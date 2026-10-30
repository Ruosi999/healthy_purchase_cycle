# =============================================================================
# 08 — Aggregate manually classified cereal purchases to shopping-trip level
# Healthy Purchase Cycle | NielsenIQ household panel | 2008–2017
#
# Inputs (restricted, outside GitHub):
#   intermediate/cereal_purchases/cereal_purchases_{source_year}.tsv
#   intermediate/cereal_classification/cereal_upc_sugar_labels.xlsx
# Output:
#   processed/cereal_trip_level/cereal_trip_level_2008_2017.tsv
# 
# We classified each cereal using publicly available nutrition information 
# matched to its brand, product name, and flavor, prioritizing U.S. Nutrition 
# Facts labels from 2008–2017 when available. 
# 
# Sugar density was calculated as grams of total sugar per serving divided by 
# serving size in grams, multiplied by 100; cereals were labeled low (≤5 g/100 g),
# medium (>5–22.5 g/100 g), or high (>22.5 g/100 g). 
# 
# When historical or product-specific information was unavailable, we used the 
# closest identifiable formulation and documented the source and assumptions; 
# products without enough evidence for a reliable match were left unclassified.

#
# Only UPC-version pairs labeled Low, Medium, or High enter the analysis.
# No trip is generated if that trip has no classified cereal purchases.
# The category is gated: Low (1) > Medium (2) > High only (3).
# Spending buckets are mutually exclusive, and each source purchase row enters
# exactly one bucket, irrespective of other cereals on the same trip.
# panel_year comes only from purchase_date; source year is never the panel year.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

# ---- Settings -------------------------------------------------------------
DATA_ROOT <- Sys.getenv("HPC_DATA_ROOT", unset =
  "C:/Users/rwen13/Documents/healthy_purchase_data")
INPUT_DIR <- file.path(DATA_ROOT, "intermediate", "cereal_purchases")
LABEL_FILE <- file.path(DATA_ROOT, "intermediate", "cereal_classification",
                        "cereal_upc_sugar_labels.xlsx")
OUTPUT_DIR <- file.path(DATA_ROOT, "processed", "cereal_trip_level")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "cereal_trip_level_2008_2017.tsv")
AUDIT_FILE <- file.path(OUTPUT_DIR, "cereal_trip_source_audit_2008_2017.tsv")
SUMMARY_FILE <- file.path(OUTPUT_DIR, "cereal_trip_year_summary_2008_2017.tsv")
SOURCE_YEARS <- 2007:2018  # Include adjacent source files if present.
REQUIRE_CORE_YEARS <- TRUE # Fail if any 2008–2017 source file is missing.
RUN_OVERWRITE <- FALSE
RUN_THREADS <- 4L
setDTthreads(RUN_THREADS)

KEYS <- c("hhid", "panel_year", "trip_id", "purchase_date")
SPENDING <- c("low_sugar_cereal_spending", "medium_sugar_cereal_spending",
              "high_sugar_cereal_spending")
OUTPUT_COLUMNS <- c(KEYS, "cereal_y", SPENDING)

# ---- Helpers --------------------------------------------------------------
check_columns <- function(actual, expected, label) {
  missing <- setdiff(expected, actual)
  if (length(missing)) stop(label, " is missing: ", paste(missing, collapse = ", "))
}

parse_dates <- function(x, context) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  iso <- !is.na(x) & grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}", x)
  normalized <- x
  normalized[iso] <- gsub("/", "-", substr(x[iso], 1L, 10L), fixed = TRUE)
  result <- suppressWarnings(as.IDate(normalized, format = "%Y-%m-%d"))
  for (fmt in c("%Y%m%d", "%m/%d/%Y", "%m/%d/%y")) {
    idx <- which(!is.na(x) & is.na(result))
    if (!length(idx)) break
    result[idx] <- suppressWarnings(as.IDate(x[idx], format = fmt))
  }
  bad <- which(!is.na(x) & is.na(result))
  if (length(bad)) stop("Cannot parse purchase_date in ", context,
                        "; example: ", x[bad[1L]])
  result
}

# ---- Load and validate the sugar classification dictionary ----------------
load_labels <- function(path) {
  if (!file.exists(path)) stop("Sugar label workbook not found: ", path)
  # read.xlsx reads workbook cells without modifying the source workbook.
  x <- as.data.table(openxlsx::read.xlsx(path, sheet = 1L,
                                       colNames = TRUE, skipEmptyRows = TRUE))
  check_columns(names(x), c("upc", "upc_ver_uc", "sugar_label"), "Sugar labels")
  # Excel may store identifiers as numeric cells. For accurate leading zeros,
  # the original UPC column MUST be stored as text in the classification file.
  x[, `:=`(upc = trimws(as.character(upc)),
           upc_ver_uc = trimws(as.character(upc_ver_uc)),
           sugar_label = tools::toTitleCase(tolower(trimws(as.character(sugar_label)))))]
  x <- x[!is.na(upc) & upc != "" & !is.na(upc_ver_uc) & upc_ver_uc != ""]
  x <- x[!is.na(sugar_label) & sugar_label %chin% c("Low", "Medium", "High"),
         .(upc, upc_ver_uc, sugar_label)]
  # Exact identical entries are fine, conflicting duplicate keys are not.
  x <- unique(x)
  if (anyDuplicated(x, by = c("upc", "upc_ver_uc"))) {
    stop("Conflicting sugar labels for the same UPC-version. Resolve the Excel file first.")
  }
  if (!nrow(x)) stop("No valid Low/Medium/High labels found in workbook.")
  setkeyv(x, c("upc", "upc_ver_uc"))
  message("Classified UPC-version pairs loaded: ", nrow(x))
  print(x[, .(upc_version_pairs = .N), by = sugar_label][order(sugar_label)])
  x
}

# ---- Read, classify, and aggregate one source file -------------------------
process_source_year <- function(year, labels) {
  path <- file.path(INPUT_DIR, sprintf("cereal_purchases_%d.tsv", year))
  message("Reading cereal source year: ", year)
  columns <- names(fread(path, sep = "\t", quote = "", nrows = 0L,
                         showProgress = FALSE))
  check_columns(columns, c("upc", "upc_ver_uc", "household_code",
                           "trip_code_uc", "purchase_date"), basename(path))
  price_col <- if ("final_price_paid" %in% columns) "final_price_paid" else ""
  if (!length(price_col) || !nzchar(price_col)) {
    check_columns(columns, c("total_price_paid", "coupon_value"), basename(path))
    price_cols <- c("total_price_paid", "coupon_value")
  } else {
    price_cols <- price_col
  }
  required <- c("upc", "upc_ver_uc", "household_code", "trip_code_uc",
                "purchase_date", price_cols)
  dt <- fread(path, select = required, sep = "\t", quote = "",
              colClasses = list(character = c("upc", "upc_ver_uc",
                                              "household_code", "trip_code_uc",
                                              "purchase_date")),
              showProgress = FALSE)
  n_source <- nrow(dt)
  # Important: inner-join keeps ONLY manually classified purchases.
  # Label keys are unique, so no purchase row is multiplied.
  dt <- labels[dt, on = .(upc, upc_ver_uc), nomatch = 0L]
  n_classified <- nrow(dt)
  dates <- parse_dates(dt$purchase_date, basename(path))
  in_period <- !is.na(dates) & dates >= as.IDate("2008-01-01") &
    dates <= as.IDate("2017-12-31")
  n_retained <- sum(in_period)
  dt <- dt[in_period]
  dates <- dates[in_period]
  if (!nrow(dt)) {
    return(list(trips = NULL,
      audit = data.table(source_year = year, source_rows = n_source,
                         classified_rows = n_classified, in_period_rows = n_retained,
                         trip_rows = 0L, net_spending = 0)))
  }
  if (anyNA(dt$household_code) || any(dt$household_code == "") ||
      anyNA(dt$trip_code_uc) || any(dt$trip_code_uc == "")) {
    stop("Missing household or trip identifier in ", basename(path))
  }
  if (nzchar(price_col)) {
    net <- suppressWarnings(as.numeric(dt[[price_col]]))
  } else {
    gross <- suppressWarnings(as.numeric(dt$total_price_paid))
    coupon <- suppressWarnings(as.numeric(dt$coupon_value))
    coupon[is.na(coupon)] <- 0
    net <- gross - coupon
  }
  # Missing/invalid spending cannot silently understate trip totals.
  if (anyNA(net) || any(!is.finite(net))) {
    stop("Invalid or missing prices in classified rows of ", basename(path))
  }
  # Match the FV/yogurt trip-level convention: floor negative net prices at 0.
  net <- pmax(net, 0)
  dt[, `:=`(hhid = household_code, trip_id = trip_code_uc,
            purchase_date = format(dates, "%Y-%m-%d"),
            panel_year = as.integer(format(dates, "%Y")))]

  # Each classified purchase has exactly one sugar class and one spend bucket.
  dt[, `:=`(low_piece = fifelse(sugar_label == "Low", net, 0),
            medium_piece = fifelse(sugar_label == "Medium", net, 0),
            high_piece = fifelse(sugar_label == "High", net, 0))]
  if (!isTRUE(all.equal(sum(net), sum(dt$low_piece) +
                        sum(dt$medium_piece) + sum(dt$high_piece),
                        tolerance = 1e-9))) {
    stop("Spending conservation failed in source year ", year)
  }

  # Gated response uses purchases, NOT positive spending, so free items count.
  trips <- dt[, .(
    cereal_y = if (any(sugar_label == "Low")) 1L else
               if (any(sugar_label == "Medium")) 2L else 3L,
    low_sugar_cereal_spending = sum(low_piece),
    medium_sugar_cereal_spending = sum(medium_piece),
    high_sugar_cereal_spending = sum(high_piece)
  ), by = .(hhid, panel_year, trip_id, purchase_date)]

  audit <- data.table(source_year = year, source_rows = n_source,
                      classified_rows = n_classified, in_period_rows = n_retained,
                      trip_rows = nrow(trips), net_spending = sum(net))
  list(trips = trips, audit = audit)
}

# ---- Main driver ----------------------------------------------------------
run_cereal_trip_aggregation <- function() {
  if (file.exists(OUTPUT_FILE) && !RUN_OVERWRITE)
    stop("Output exists. Set RUN_OVERWRITE <- TRUE to replace: ", OUTPUT_FILE)
  core_paths <- file.path(INPUT_DIR, sprintf("cereal_purchases_%d.tsv", 2008:2017))
  missing_core <- core_paths[!file.exists(core_paths)]
  if (length(missing_core)) {
    msg <- paste("Missing core-year cereal files:",
                 paste(basename(missing_core), collapse = ", "))
    if (REQUIRE_CORE_YEARS) stop(msg) else warning(msg)
  }
  paths <- file.path(INPUT_DIR, sprintf("cereal_purchases_%d.tsv", SOURCE_YEARS))
  boundary_missing <- paths[SOURCE_YEARS %in% c(2007, 2018) & !file.exists(paths)]
  if (length(boundary_missing)) warning("Boundary source file(s) missing; ",
    "some eligible purchases may be omitted: ",
    paste(basename(boundary_missing), collapse = ", "))
  available_years <- SOURCE_YEARS[file.exists(paths)]
  if (!length(available_years)) stop("No cereal purchase extracts found.")
  labels <- load_labels(LABEL_FILE)
  partials <- list()
  audits <- list()
  for (year in available_years) {
    result <- process_source_year(year, labels)
    if (!is.null(result$trips)) partials[[as.character(year)]] <- result$trips
    audits[[as.character(year)]] <- result$audit
    rm(result)
    invisible(gc(verbose = FALSE))
  }
  if (!length(partials)) stop("No classified cereal purchases dated 2008–2017.")
  trips <- rbindlist(partials, use.names = TRUE)
  rm(partials)

  # Do not silently add repeated trip totals across annual source files:
  # without purchase-line identifiers, this could double-count spending.
  dup <- trips[, .N, by = KEYS][N > 1L]
  if (nrow(dup)) {
    message("Example duplicate cross-source trip keys:")
    print(head(dup, 10L))
    stop("Same trip key appears in multiple source files. Review source-year ",
         "overlap before aggregating spending (no files were written).")
  }
  setcolorder(trips, OUTPUT_COLUMNS)
  setorder(trips, panel_year, hhid, purchase_date, trip_id)
  summary <- trips[, .(
    n_cereal_trips = .N,
    n_low_gate = sum(cereal_y == 1L),
    n_medium_gate = sum(cereal_y == 2L),
    n_high_only_gate = sum(cereal_y == 3L),
    low_sugar_cereal_spending = sum(low_sugar_cereal_spending),
    medium_sugar_cereal_spending = sum(medium_sugar_cereal_spending),
    high_sugar_cereal_spending = sum(high_sugar_cereal_spending)
  ), by = panel_year][order(panel_year)]
  audit <- rbindlist(audits, use.names = TRUE)
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  # Do not overwrite inadvertently. Main table written after all checks pass.
  tmp <- paste0(OUTPUT_FILE, ".partial")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  fwrite(trips, tmp, sep = "\t", quote = FALSE, na = "", nThread = RUN_THREADS)
  if (file.exists(OUTPUT_FILE) && !file.remove(OUTPUT_FILE))
    stop("Cannot replace existing output: ", OUTPUT_FILE)
  if (!file.rename(tmp, OUTPUT_FILE)) stop("Could not finalize ", OUTPUT_FILE)
  fwrite(audit, AUDIT_FILE, sep = "\t")
  fwrite(summary, SUMMARY_FILE, sep = "\t")
  message("Saved ", nrow(trips), " classified cereal trip rows to: ", OUTPUT_FILE)
  print(audit)
  print(summary)
  invisible(trips)
}

# Run the full aggregation.
run_cereal_trip_aggregation()
