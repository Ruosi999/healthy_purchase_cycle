# =============================================================================
# Step 06: Aggregate labeled yogurt purchases to trip level (2008–2017)
#
# Source: healthy_purchase_data/intermediate/yogurt_purchases/
#         yogurt_purchases_{SOURCE_YEAR}.tsv
# Output: healthy_purchase_data/processed/yogurt_trip_level/
#         yogurt_trip_level_2008_2017.tsv
#
# Year rule: panel_year = year(purchase_date), not the source-file panel_year.
# Keep purchase dates from 2008-01-01 through 2017-12-31 inclusive.
# Read adjacent source years (2007 and 2018) when available to capture trips
# recorded in a different annual file from their actual purchase date.
#
# Spending categories are MUTUALLY EXCLUSIVE for every purchase row:
#   1. fat_free_yogurt == 1 -> skimmed_yogurt_spending
#   2. otherwise, low_fat_yogurt == 1 -> low_fat_yogurt_spending
#   3. otherwise -> other_yogurt_spending
# yogurt_y = 1 if ANY fat-free yogurt purchase occurs on the trip, even if
# its net spending is zero. Other trips containing yogurt have yogurt_y = 0.
# Unclassified types belong in 'other'; they are not assumed to be regular.
# Coupon-adjusted net prices are floored at zero as in the FV trip pipeline.
# Household and trip IDs stay character, preserving any leading zeros.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# ---- Configuration ---------------------------------------------------------
DATA_ROOT <- Sys.getenv("HPC_DATA_ROOT", unset =
  "C:/Users/rwen13/Documents/healthy_purchase_data")
INPUT_DIR <- file.path(DATA_ROOT, "intermediate", "yogurt_purchases")
OUTPUT_DIR <- file.path(DATA_ROOT, "processed", "yogurt_trip_level")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "yogurt_trip_level_2008_2017.tsv")
AUDIT_FILE <- file.path(OUTPUT_DIR, "yogurt_trip_source_audit_2008_2017.tsv")
YEAR_AUDIT_FILE <- file.path(OUTPUT_DIR, "yogurt_trip_year_audit_2008_2017.tsv")

SOURCE_YEARS <- 2007:2018
TARGET_YEARS <- 2008:2017
REQUIRE_ALL_SOURCE_FILES <- FALSE # TRUE requires all 2007–2018 extracts.
RUN_OVERWRITE <- FALSE
RUN_THREADS <- 4L
WRITE_YEARLY_FILES <- FALSE
setDTthreads(RUN_THREADS)

TRIP_KEYS <- c("hhid", "panel_year", "trip_id", "purchase_date")
OUTPUT_COLS <- c(TRIP_KEYS, "yogurt_y", "skimmed_yogurt_spending",
                 "low_fat_yogurt_spending", "other_yogurt_spending")

# ---- Validation / parsing --------------------------------------------------
require_headers <- function(path, cols) {
  cols_available <- names(fread(path, nrows = 0L, sep = "\t", quote = "",
                               showProgress = FALSE))
  missing <- setdiff(cols, cols_available)
  if (length(missing)) stop("Missing column(s) in ", path, ": ",
                            paste(missing, collapse = ", "))
  invisible(cols_available)
}

parse_dates <- function(raw, path) {
  raw <- trimws(as.character(raw))
  raw[raw %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  # ISO date and timestamp; the date portion determines the purchase year.
  is_iso <- !is.na(raw) & grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}", raw)
  normalized <- raw
  normalized[is_iso] <- gsub("/", "-", substr(raw[is_iso], 1L, 10L), fixed = TRUE)
  out <- suppressWarnings(as.IDate(normalized, format = "%Y-%m-%d"))
  for (fmt in c("%Y%m%d", "%m/%d/%Y", "%m/%d/%y")) {
    missing_idx <- which(!is.na(raw) & is.na(out))
    if (!length(missing_idx)) break
    out[missing_idx] <- suppressWarnings(as.IDate(raw[missing_idx], format = fmt))
  }
  bad <- !is.na(raw) & is.na(out)
  if (any(bad)) stop("Unrecognized purchase_date in ", path, "; example: ",
                     raw[which(bad)[1L]])
  out
}

parse_flag <- function(x, column_name, path) {
  text <- toupper(trimws(as.character(x)))
  text[is.na(text) | text == ""] <- "0"
  result <- integer(length(text))
  result[text %in% c("1", "TRUE", "T", "YES", "Y")] <- 1L
  invalid <- !(text %in% c("0", "1", "TRUE", "FALSE", "T", "F", "YES", "NO", "Y", "N"))
  if (any(invalid)) stop("Invalid ", column_name, " flag in ", path,
                         "; example: ", text[which(invalid)[1L]])
  result
}

# ---- Aggregate one annual extract -----------------------------------------
aggregate_file <- function(source_year, path) {
  header <- require_headers(path, c("household_code", "trip_code_uc",
                                     "purchase_date", "final_price_paid",
                                     "fat_free_yogurt", "low_fat_yogurt"))
  # Read only six columns instead of the full purchase-level yogurt output.
  dt <- fread(path, select = c("household_code", "trip_code_uc",
                               "purchase_date", "final_price_paid",
                               "fat_free_yogurt", "low_fat_yogurt"),
              colClasses = list(character = c("household_code", "trip_code_uc",
                                              "purchase_date", "fat_free_yogurt",
                                              "low_fat_yogurt")),
              sep = "\t", quote = "", showProgress = FALSE)
  input_rows <- nrow(dt)
  dates <- parse_dates(dt$purchase_date, path)
  missing_date_rows <- sum(is.na(dates))
  in_window <- !is.na(dates) & dates >= as.IDate("2008-01-01") &
    dates <= as.IDate("2017-12-31")
  retained_rows <- sum(in_window)
  dt <- dt[in_window]
  dates <- dates[in_window]

  if (!nrow(dt)) {
    audit <- data.table(source_year, input_rows, missing_date_rows,
                        retained_rows = 0L, trip_rows = 0L, skimmed_trips = 0L,
                        missing_price_rows = 0L, negative_price_rows = 0L)
    return(list(trips = NULL, audit = audit))
  }

  if (anyNA(dt$household_code) || any(dt$household_code == "") ||
      anyNA(dt$trip_code_uc) || any(dt$trip_code_uc == "")) {
    stop("Missing household_code/trip_code_uc in: ", path)
  }

  dt[, `:=`(hhid = household_code, trip_id = trip_code_uc,
            purchase_date = format(dates, "%Y-%m-%d"),
            panel_year = as.integer(format(dates, "%Y")))]

  fat <- parse_flag(dt$fat_free_yogurt, "fat_free_yogurt", path)
  low <- parse_flag(dt$low_fat_yogurt, "low_fat_yogurt", path)
  if (any(fat == 1L & low == 1L)) {
    warning("Both fat-free and low-fat flags are 1 in ", basename(path),
            "; classifying these purchases as fat-free only.")
  }

  spend <- suppressWarnings(as.numeric(dt$final_price_paid))
  if (any(!is.na(dt$final_price_paid) & is.na(spend))) {
    stop("Non-numeric final_price_paid value in: ", path)
  }
  if (any(is.infinite(spend))) stop("Non-finite price in ", path)
  missing_price_rows <- sum(is.na(spend))
  negative_price_rows <- sum(spend < 0, na.rm = TRUE)
  spend[is.na(spend)] <- 0
  spend <- pmax(spend, 0)

  # Exclusive priority: fat-free > low-fat > other; each row contributes once.
  dt[, `:=`(
    skim_piece = fifelse(fat == 1L, spend, 0),
    low_piece = fifelse(fat == 0L & low == 1L, spend, 0),
    other_piece = fifelse(fat == 0L & low == 0L, spend, 0),
    skim_flag = fat
  )]
  stopifnot(isTRUE(all.equal(sum(spend), sum(dt$skim_piece) +
                             sum(dt$low_piece) + sum(dt$other_piece),
                             tolerance = 1e-10)))

  # Group by the complete stable trip key, not just trip_id or source year.
  trips <- dt[, .(
    yogurt_y = as.integer(any(skim_flag == 1L)),
    skimmed_yogurt_spending = sum(skim_piece),
    low_fat_yogurt_spending = sum(low_piece),
    other_yogurt_spending = sum(other_piece)
  ), by = TRIP_KEYS]

  audit <- data.table(source_year, input_rows, missing_date_rows,
                      retained_rows, trip_rows = nrow(trips),
                      skimmed_trips = sum(trips$yogurt_y),
                      missing_price_rows, negative_price_rows)
  list(trips = trips, audit = audit)
}

# ---- Main runner -----------------------------------------------------------
run_yogurt_trip <- function() {
  if (file.exists(OUTPUT_FILE) && !RUN_OVERWRITE) {
    stop("Output already exists. Set RUN_OVERWRITE <- TRUE: ", OUTPUT_FILE)
  }
  paths <- file.path(INPUT_DIR, sprintf("yogurt_purchases_%d.tsv", SOURCE_YEARS))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    msg <- paste0("Missing source extract(s): ", paste(basename(missing), collapse = ", "),
                  ". An incomplete source range could omit cross-year purchases.")
    if (REQUIRE_ALL_SOURCE_FILES) stop(msg) else warning(msg)
  }
  available_idx <- which(file.exists(paths))
  if (!length(available_idx)) stop("No yogurt purchase files found in ", INPUT_DIR)

  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  partials <- vector("list", length(available_idx))
  audits <- vector("list", length(available_idx))
  for (i in seq_along(available_idx)) {
    index <- available_idx[i]
    y <- SOURCE_YEARS[index]
    message("Aggregating yogurt source year: ", y)
    result <- aggregate_file(y, paths[index])
    partials[[i]] <- result$trips
    audits[[i]] <- result$audit
    rm(result)
    invisible(gc(verbose = FALSE))
  }
  partials <- Filter(Negate(is.null), partials)
  if (!length(partials)) stop("No yogurt purchases dated 2008–2017 were found.")
  all_trips <- rbindlist(partials, use.names = TRUE)
  rm(partials)

  # Cross-source duplicate trip keys may indicate repeated purchase records.
  # Without purchase-line IDs, summing them risks double-counting. Fail safely.
  duplicates <- all_trips[, .N, by = TRIP_KEYS][N > 1L]
  if (nrow(duplicates)) {
    issue_path <- file.path(OUTPUT_DIR, "yogurt_duplicate_trip_keys_review.tsv")
    fwrite(duplicates, issue_path, sep = "\t")
    stop("A trip appears in multiple source-year files. Review ", issue_path,
         " and resolve source duplication before summing spending.")
  }

  setcolorder(all_trips, OUTPUT_COLS)
  setorder(all_trips, panel_year, hhid, purchase_date, trip_id)
  year_audit <- all_trips[, .(
    n_trips = .N,
    n_skimmed_trips = sum(yogurt_y),
    skimmed_yogurt_spending = sum(skimmed_yogurt_spending),
    low_fat_yogurt_spending = sum(low_fat_yogurt_spending),
    other_yogurt_spending = sum(other_yogurt_spending)
  ), by = panel_year][order(panel_year)]

  # Atomic main output; no partially-written production file on processing error.
  temporary <- paste0(OUTPUT_FILE, ".partial")
  if (file.exists(temporary)) unlink(temporary)
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  fwrite(all_trips, temporary, sep = "\t", na = "")
  if (file.exists(OUTPUT_FILE)) unlink(OUTPUT_FILE)
  if (!file.rename(temporary, OUTPUT_FILE)) stop("Could not finalize ", OUTPUT_FILE)

  fwrite(rbindlist(audits), AUDIT_FILE, sep = "\t", na = "")
  fwrite(year_audit, YEAR_AUDIT_FILE, sep = "\t", na = "")
  if (WRITE_YEARLY_FILES) {
    for (year in TARGET_YEARS) {
      fwrite(all_trips[panel_year == year],
             file.path(OUTPUT_DIR, sprintf("yogurt_trip_level_%d.tsv", year)),
             sep = "\t", na = "")
    }
  }
  message("Finished: ", nrow(all_trips), " yogurt trips. Output: ", OUTPUT_FILE)
  invisible(year_audit)
}

if (sys.nframe() == 0L) run_yogurt_trip()
