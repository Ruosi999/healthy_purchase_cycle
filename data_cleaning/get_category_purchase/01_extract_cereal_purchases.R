# Extract purchase-level cereal data for the Healthy Purchase Cycle project.
# Code belongs in: data_cleaning/get_category_purchase/
# Raw and intermediate data remain outside the GitHub repository.
# Requires data.table. Run this script with source() or Rscript.

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Install data.table first: install.packages('data.table')")
}
library(data.table)

# ---- Configuration -------------------------------------------------------
# Start with 2008, then change RUN_YEARS to 2007:2018 after checking the audit.
# Source years are retained here; purchase-date filtering belongs downstream.
RUN_YEARS <- 2009:2018L
RUN_OVERWRITE <- FALSE
RUN_THREADS <- 4L
RUN_BATCH <- TRUE

# Environment overrides allow the same code to run on different computers.
HMS_ROOT <- Sys.getenv("HPC_HMS_ROOT", unset =
  "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS")
DATA_ROOT <- Sys.getenv("HPC_DATA_ROOT", unset =
  "C:/Users/rwen13/Documents/healthy_purchase_data")
MASTER_FILE <- Sys.getenv("HPC_MASTER_FILE", unset =
  file.path(HMS_ROOT, "Master_Files", "Latest", "products.tsv"))
OUTPUT_DIR <- file.path(DATA_ROOT, "intermediate", "cereal_purchases")

# Use exactly the eight requested modules; do not filter descriptions by keyword.
# Some valid modules (e.g., HOMINY GRITS) do not contain the word CEREAL.
CEREAL_MODULES <- c("1344", "1346", "1348", "1349", "1366",
                    "1619", "1679", "6075")
KEY_COLUMNS <- c("upc", "upc_ver_uc", "trip_code_uc", "household_code",
                 "product_module_code")

# ---- Shared utilities ----------------------------------------------------
read_selected <- function(path, required, optional = character()) {
  if (!file.exists(path)) stop("Missing input: ", path)
  # Nielsen descriptions can contain literal unmatched quotation marks.
  header <- names(fread(path, sep = "\t", quote = "", nrows = 0L,
                        showProgress = FALSE))
  missing <- setdiff(required, header)
  if (length(missing)) stop("Missing columns in ", path, ": ",
                            paste(missing, collapse = ", "))
  selected <- unique(c(required, intersect(optional, header)))
  fread(path, sep = "\t", quote = "", select = selected,
        colClasses = list(character = intersect(KEY_COLUMNS, selected)),
        na.strings = c("", "NA"), showProgress = interactive())
}

validate_lookup <- function(dt, keys, label) {
  # Identical duplicates are harmless. Conflicting lookup rows must be resolved.
  dt <- unique(dt)
  invalid <- dt[, Reduce(`|`, lapply(.SD, function(x) is.na(x) | x == "")),
                .SDcols = keys]
  if (any(invalid)) stop(label, " contains missing join keys.")
  if (anyDuplicated(dt, by = keys)) {
    stop(label, " has conflicting duplicate keys; join stopped to prevent row inflation.")
  }
  dt
}

safe_print <- function(x, rows = 20L) {
  tryCatch(print(as.data.frame(head(x, rows)), row.names = FALSE),
           error = function(e) message("Console preview skipped: ", conditionMessage(e)))
}

write_tsv_safely <- function(dt, destination, overwrite = FALSE) {
  if (file.exists(destination) && !overwrite) stop("Output exists: ", destination)
  temp <- tempfile(pattern = "cereal_", tmpdir = dirname(destination), fileext = ".tmp")
  on.exit(unlink(temp), add = TRUE)
  fwrite(dt, temp, sep = "\t", quote = "auto", na = "", nThread = RUN_THREADS)
  # Validate serialization before replacing any previous file.
  if (!file.exists(temp) || file.info(temp)$size == 0) stop("Output write failed.")
  if (file.exists(destination) && !file.remove(destination)) {
    stop("Cannot replace output (check whether it is open): ", destination)
  }
  if (!file.rename(temp, destination)) stop("Cannot finalize output: ", destination)
}

# ---- Load the cereal dictionary once per batch ----------------------------
load_cereal_dictionary <- function() {
  products <- read_selected(
    MASTER_FILE,
    required = c("upc", "upc_ver_uc", "product_module_code", "product_module_descr"),
    optional = c("upc_descr", "brand_code_uc", "brand_descr", "size1_code_uc",
                 "size1_amount", "size1_units", "size1_change_flag_uc")
  )
  dictionary <- products[product_module_code %chin% CEREAL_MODULES]
  rm(products)
  dictionary <- validate_lookup(dictionary, c("upc", "upc_ver_uc"), "Cereal dictionary")
  if (!nrow(dictionary)) stop("No requested cereal modules found in the master file.")
  setkeyv(dictionary, c("upc", "upc_ver_uc"))
  message("Cereal UPC-version dictionary size: ", nrow(dictionary))
  safe_print(dictionary[, .(upc_version_pairs = .N),
                        by = .(product_module_code, product_module_descr)])
  missing_modules <- setdiff(CEREAL_MODULES, dictionary$product_module_code)
  if (length(missing_modules)) message("Modules absent from master: ",
                                      paste(missing_modules, collapse = ", "))
  dictionary
}

# ---- Extract one source year ---------------------------------------------
process_cereal_purchases <- function(year, dictionary, overwrite = RUN_OVERWRITE) {
  destination <- file.path(OUTPUT_DIR, sprintf("cereal_purchases_%d.tsv", year))
  if (file.exists(destination) && !overwrite) {
    message("Skipping existing output: ", destination)
    return(data.table(source_year = year, status = "skipped", error = NA_character_))
  }
  message("Processing cereal purchases: ", year)
  started <- proc.time()[["elapsed"]]
  annual <- file.path(HMS_ROOT, as.character(year), "Annual_Files")

  # Read only necessary columns. A full selected-column annual purchase table
  # is faster than repeatedly scanning file offsets, but still requires RAM.
  purchases <- read_selected(
    file.path(annual, sprintf("purchases_%d.tsv", year)),
    required = c("upc", "upc_ver_uc", "trip_code_uc", "quantity",
                 "total_price_paid", "coupon_value"),
    optional = "deal_flag_uc"
  )
  input_rows <- nrow(purchases)
  # Filter first; unrelated categories never enter the extracted cereal table.
  keep <- dictionary[purchases, on = .(upc, upc_ver_uc), which = TRUE]
  cereal <- purchases[!is.na(keep)]
  rm(purchases, keep)
  gc(verbose = FALSE)
  cereal <- merge(cereal, dictionary, by = c("upc", "upc_ver_uc"),
                  all.x = TRUE, sort = FALSE)
  cereal_rows <- nrow(cereal)

  trips <- read_selected(
    file.path(annual, sprintf("trips_%d.tsv", year)),
    required = c("trip_code_uc", "household_code", "purchase_date"),
    optional = "panel_year"
  )
  # Only trip metadata needed by cereal purchases are retained for validation.
  trips <- trips[unique(cereal[, .(trip_code_uc)]), on = "trip_code_uc", nomatch = 0L]
  trips <- validate_lookup(trips, "trip_code_uc", "Trip lookup")
  # Preserve the original panel year as source metadata, not the analysis year.
  if ("panel_year" %in% names(trips)) setnames(trips, "panel_year", "source_panel_year")
  trips[, trip_lookup_matched := TRUE]
  cereal <- merge(cereal, trips, by = "trip_code_uc", all.x = TRUE, sort = FALSE)
  if (nrow(cereal) != cereal_rows) stop("Trip join changed the purchase row count.")
  unmatched <- cereal[is.na(trip_lookup_matched), .N]
  if (unmatched > 0L) {
    stop(unmatched, " cereal purchase rows lack trip metadata; no output saved.")
  }
  cereal[, trip_lookup_matched := NULL]
  rm(trips)

  # Match the other category extracts: missing coupon value is treated as zero.
  # Missing total_price_paid remains NA; negative net spending is retained.
  for (column in c("total_price_paid", "coupon_value")) {
    if (!is.numeric(cereal[[column]])) stop("Non-numeric monetary column: ", column)
  }
  missing_coupon_rows <- cereal[is.na(coupon_value), .N]
  cereal[is.na(coupon_value), coupon_value := 0]
  cereal[, `:=`(final_price_paid = total_price_paid - coupon_value,
                has_coupon = as.integer(coupon_value > 0), source_year = as.integer(year))]
  if ("deal_flag_uc" %in% names(cereal)) {
    cereal[, is_deal := as.integer(deal_flag_uc == 1)]
  }
  stopifnot(all(cereal$product_module_code %chin% CEREAL_MODULES))
  stopifnot(all(vapply(cereal[, intersect(KEY_COLUMNS, names(cereal)), with = FALSE],
                       is.character, logical(1))))
  front <- c("trip_code_uc", "household_code", "purchase_date", "source_year",
             "upc", "upc_ver_uc", "product_module_code", "product_module_descr")
  setcolorder(cereal, c(front, setdiff(names(cereal), front)))

  module_audit <- cereal[, .(purchase_rows = .N,
    upc_version_pairs = uniqueN(.SD),
    net_spending_observed = sum(final_price_paid, na.rm = TRUE),
    missing_spending_rows = sum(is.na(final_price_paid))),
    by = .(product_module_code, product_module_descr), .SDcols = c("upc", "upc_ver_uc")]
  audit <- data.table(source_year = year, input_purchase_rows = input_rows,
    cereal_rows_before_trip_join = cereal_rows, output_purchase_rows = nrow(cereal),
    unmatched_trip_rows = unmatched, missing_coupon_treated_as_zero = missing_coupon_rows,
    missing_price_rows = cereal[is.na(total_price_paid), .N],
    negative_net_price_rows = cereal[final_price_paid < 0, .N],
    elapsed_seconds = proc.time()[["elapsed"]] - started)

  # Save the data before console previews; printing cannot interrupt saving.
  write_tsv_safely(cereal, destination, overwrite)
  write_tsv_safely(audit, file.path(OUTPUT_DIR,
    sprintf("cereal_extraction_audit_%d.tsv", year)), overwrite = TRUE)
  write_tsv_safely(module_audit, file.path(OUTPUT_DIR,
    sprintf("cereal_module_audit_%d.tsv", year)), overwrite = TRUE)
  message("Year ", year, ": ", nrow(cereal), " cereal rows saved.")
  safe_print(audit)
  safe_print(module_audit)
  data.table(source_year = year, status = "success", error = NA_character_)
}

# ---- Sequential batch runner --------------------------------------------
# Avoid parallel years: each worker would allocate another large purchase table.
run_batch_cereal <- function(years = RUN_YEARS, overwrite = RUN_OVERWRITE) {
  setDTthreads(RUN_THREADS)
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(OUTPUT_DIR)) stop("Cannot create output directory: ", OUTPUT_DIR)
  dictionary <- load_cereal_dictionary()
  results <- lapply(years, function(year) {
    result <- tryCatch(process_cereal_purchases(year, dictionary, overwrite),
      error = function(e) {
        message("FAILED year ", year, ": ", conditionMessage(e))
        data.table(source_year = year, status = "failed", error = conditionMessage(e))
      })
    gc(verbose = FALSE)
    result
  })
  results <- rbindlist(results)
  safe_print(results, rows = length(years))
  if (any(results$status == "failed")) warning("Some years failed; review the batch summary.")
  invisible(results)
}

if (RUN_BATCH) run_batch_cereal()
