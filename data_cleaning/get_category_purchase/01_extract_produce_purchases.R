# =============================================================================
# Step 01: Extract non-Magnet fruit and vegetable purchases (memory-safe).
# =============================================================================
# Source data: Nielsen HMS annual purchases, annual trips, and Latest products.
# Output: One TSV per year outside the GitHub repository.
# Product join: upc + upc_ver_uc (both character).
# Trip join: trip_code_uc (character).
# Classification: 1 = fresh, 2 = frozen, 3 = canned produce.
# This version streams purchases in bounded chunks and processes years in order.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# -----------------------------------------------------------------------------
# Run settings: edit this block only.
# -----------------------------------------------------------------------------
RUN_YEARS <- 2006:2018              # Or 2008, 2008:2017, c(2008, 2010, 2015)
CHUNK_ROWS <- 50000L                # Reduce to 20000L if memory is constrained.
SKIP_COMPLETED <- TRUE              # Reuse existing outputs (e.g., 2010).
OVERWRITE_EXISTING <- FALSE         # TRUE explicitly rebuilds completed years.
STOP_ON_ERROR <- TRUE               # FALSE logs errors and continues other years.

RAW_ROOT <- Sys.getenv(
  "HPC_RAW_ROOT",
  unset = "C:/Users/rwen13/Documents/2004-2020_Panel_Data"
)
DATA_ROOT <- Sys.getenv(
  "HPC_DATA_ROOT",
  unset = "C:/Users/rwen13/Documents/healthy_purchase_data"
)
HMS_ROOT <- file.path(RAW_ROOT, "nielsen_extracts", "HMS")
PRODUCT_FILE <- file.path(HMS_ROOT, "Master_Files", "Latest", "products.tsv")
OUTPUT_DIR <- file.path(DATA_ROOT, "intermediate", "produce_purchases")
LOG_DIR <- file.path(DATA_ROOT, "temp", "produce_extraction_logs")

# -----------------------------------------------------------------------------
# Verified product-module definitions copied from the legacy extraction script.
# -----------------------------------------------------------------------------
fresh_codes <- c(
  1620,1621,1670,1688,1701,1706,4010,4015,4020,4023,4050,4055,
  4060,4085,4140,4180,4225,4230,4275,4280,4350,4355,4400,4415,
  4460,4470,4475,6049,6050,6064,6070
)
frozen_codes <- c(2608,2620,2630,2633,2634,2636,2640,2641,2642,2644,2664)
canned_codes <- c(
  1006,1007,1008,1009,1010,1011,1012,1013,1014,1016,1017,1020,
  1021,1023,1024,1026,1027,1028,1057,1058,1060,1061,1062,1064,
  1065,1066,1067,1068,1069,1070,1071,1072,1073,1074,1075,1076,
  1077,1079,1080,1081,1082,1085,1086,1087,1088,1090,1094,1095,
  1096,1097,1102,1103,1105,1107,1144,1263,1267
)
module_map <- data.table(
  product_module_code = as.character(c(fresh_codes, frozen_codes, canned_codes)),
  fv_class = c(rep(1L, length(fresh_codes)), rep(2L, length(frozen_codes)),
               rep(3L, length(canned_codes)))
)
stopifnot(!anyDuplicated(module_map$product_module_code))

PRODUCT_COLUMNS <- c(
  "upc", "upc_ver_uc", "product_module_code", "product_group_code",
  "product_group_descr", "product_module_descr", "brand_code_uc",
  "brand_descr", "size1_code_uc", "size1_amount", "size1_units",
  "size1_change_flag_uc"
)
TRIP_COLUMNS <- c("trip_code_uc", "panel_year", "purchase_date", "household_code")
OUTPUT_COLUMNS <- c(
  "household_code", "trip_code_uc", "panel_year", "purchase_date",
  "upc", "upc_ver_uc", "product_module_code", "product_module_descr",
  "product_group_code", "product_group_descr", "fv_class", "brand_code_uc",
  "brand_descr", "quantity", "size1_code_uc", "size1_amount", "size1_units",
  "size1_change_flag_uc", "total_price_paid", "coupon_value",
  "final_price_paid", "deal_flag_uc", "has_coupon", "is_deal"
)

assert_columns <- function(columns, required, source) {
  missing <- setdiff(required, columns)
  if (length(missing)) {
    stop("Missing columns in ", source, ": ", paste(missing, collapse = ", "))
  }
}

read_produce_dictionary <- function() {
  if (!file.exists(PRODUCT_FILE)) stop("Missing product master: ", PRODUCT_FILE)
  message("Loading selected columns from the product master...")
  # quote="" treats literal quote characters in product descriptions as text.
  products <- fread(PRODUCT_FILE, sep = "\t", quote = "", select = PRODUCT_COLUMNS,
                    colClasses = list(character = c("upc", "upc_ver_uc",
                                                    "product_module_code")),
                    showProgress = TRUE)
  assert_columns(names(products), PRODUCT_COLUMNS, PRODUCT_FILE)
  dictionary <- merge(products, module_map, by = "product_module_code",
                      all = FALSE, sort = FALSE)
  rm(products); gc(verbose = FALSE)
  keys <- c("upc", "upc_ver_uc")
  if (anyNA(dictionary[, ..keys]) ||
      any(dictionary$upc == "" | dictionary$upc_ver_uc == "")) {
    stop("Missing UPC/version values among selected products")
  }
  if (anyDuplicated(dictionary, by = keys)) {
    stop("Product dictionary contains duplicate UPC/version keys; check product versions")
  }
  message("Selected distinct UPC-version pairs: ", nrow(dictionary))
  dictionary
}

# Read one purchase chunk from a single file connection. Each chunk includes
# the original TSV header; keys must remain character regardless of digits.
read_purchase_chunk <- function(con, header, chunk_rows) {
  lines <- readLines(con, n = chunk_rows, warn = FALSE)
  if (!length(lines)) return(NULL)
  chunk_text <- paste(c(header, lines), collapse = "\n")
  fread(text = chunk_text, sep = "\t", quote = "", showProgress = FALSE,
        colClasses = list(character = c("upc", "upc_ver_uc", "trip_code_uc")))
}

process_year <- function(year, dictionary) {
  annual_dir <- file.path(HMS_ROOT, as.character(year), "Annual_Files")
  purchase_file <- file.path(annual_dir, sprintf("purchases_%d.tsv", year))
  trip_file <- file.path(annual_dir, sprintf("trips_%d.tsv", year))
  output_file <- file.path(OUTPUT_DIR, sprintf("fv_purchases_%d.tsv", year))
  temporary_file <- paste0(output_file, ".partial")

  if (file.exists(output_file) && !OVERWRITE_EXISTING) {
    if (SKIP_COMPLETED) {
      message("Skipping existing output for ", year, ": ", output_file,
              " (verify legacy files before relying on them)")
      return(data.table(year = year, status = "skipped", rows = NA_integer_))
    }
    stop("Output exists. Set OVERWRITE_EXISTING=TRUE to replace: ", output_file)
  }
  for (p in c(purchase_file, trip_file)) {
    if (!file.exists(p)) stop("Missing source file: ", p)
  }
  if (file.exists(temporary_file)) {
    message("Removing incomplete earlier attempt: ", temporary_file)
    unlink(temporary_file)
  }
  on.exit(if (file.exists(temporary_file)) unlink(temporary_file), add = TRUE)

  # Trips are read with only four columns, once per year. The full purchases
  # file is NEVER loaded into RAM. No parallel workers duplicate these tables.
  message("Year ", year, ": loading trip lookup...")
  trips <- fread(trip_file, sep = "\t", quote = "", select = TRIP_COLUMNS,
                 colClasses = list(character = c("trip_code_uc", "household_code")),
                 showProgress = FALSE)
  assert_columns(names(trips), TRIP_COLUMNS, trip_file)
  if (anyNA(trips$trip_code_uc) || any(trips$trip_code_uc == "") ||
      anyDuplicated(trips, by = "trip_code_uc")) {
    stop("Trip lookup has empty or duplicate trip_code_uc: ", trip_file)
  }
  setkey(trips, trip_code_uc)

  con <- file(purchase_file, open = "rt")
  on.exit(close(con), add = TRUE)
  header <- readLines(con, n = 1L, warn = FALSE)
  if (!length(header)) stop("Empty purchase file: ", purchase_file)
  assert_columns(strsplit(header, "\t", fixed = TRUE)[[1L]],
                 c("upc", "upc_ver_uc", "trip_code_uc", "quantity",
                   "total_price_paid", "coupon_value", "deal_flag_uc"), purchase_file)

  input_rows <- 0L
  selected_rows <- 0L
  missing_trip_rows <- 0L
  chunks <- 0L
  header_written <- FALSE
  repeat {
    chunk <- read_purchase_chunk(con, header, CHUNK_ROWS)
    if (is.null(chunk)) break
    chunks <- chunks + 1L
    input_rows <- input_rows + nrow(chunk)

    # Inner join to the verified product dictionary; one product per key.
    selected <- merge(chunk, dictionary, by = c("upc", "upc_ver_uc"),
                      all = FALSE, sort = FALSE)
    rm(chunk)
    if (!nrow(selected)) next
    before_trip <- nrow(selected)
    selected <- merge(selected, trips, by = "trip_code_uc", all = FALSE,
                      sort = FALSE)
    missing_trip_rows <- missing_trip_rows + before_trip - nrow(selected)
    if (!nrow(selected)) next

    for (field in c("total_price_paid", "coupon_value", "quantity")) {
      set(selected, j = field, value = suppressWarnings(as.numeric(selected[[field]])))
    }
    selected[, final_price_paid := fifelse(is.na(total_price_paid), NA_real_,
                                          total_price_paid - fcoalesce(coupon_value, 0))]
    selected[, has_coupon := as.integer(!is.na(coupon_value) & coupon_value > 0)]
    selected[, is_deal := as.integer(!is.na(deal_flag_uc) &
                                      as.character(deal_flag_uc) == "1")]
    selected <- selected[, ..OUTPUT_COLUMNS]

    # Appending chunks retains one purchase row per matching source record.
    # Write headers once. The final file is published only after success.
    fwrite(selected, temporary_file, sep = "\t", na = "",
           append = header_written, col.names = !header_written)
    header_written <- TRUE
    selected_rows <- selected_rows + nrow(selected)
    rm(selected)
    if (chunks %% 20L == 0L) {
      message(sprintf("Year %s | chunks=%s | scanned=%s | retained=%s",
                      year, chunks, format(input_rows, big.mark = ","),
                      format(selected_rows, big.mark = ",")))
      gc(verbose = FALSE)
    }
  }

  if (!header_written) {
    empty <- as.data.table(setNames(replicate(length(OUTPUT_COLUMNS),
                                              character(), simplify = FALSE),
                                     OUTPUT_COLUMNS))
    fwrite(empty, temporary_file, sep = "\t")
  }
  # Atomic rename when destination is on the same drive (as configured).
  if (file.exists(output_file) && OVERWRITE_EXISTING) unlink(output_file)
  if (!file.rename(temporary_file, output_file)) {
    stop("Failed to publish completed output: ", output_file)
  }
  if (missing_trip_rows > 0L) {
    warning(sprintf("Year %d: %d selected purchase rows lacked trip matches",
                    year, missing_trip_rows))
  }
  message(sprintf("Year %d complete | scanned=%s | retained=%s | output=%s",
                  year, input_rows, selected_rows, output_file))
  data.table(year = year, status = "completed", rows = selected_rows,
             scanned = input_rows, missing_trip_rows = missing_trip_rows)
}

run_produce_extraction <- function(years = RUN_YEARS) {
  if (!is.numeric(years) || !length(years) || anyNA(years) ||
      any(!is.finite(years)) || any(years != as.integer(years)) ||
      any(years < 2004 | years > 2020)) {
    stop("years must be whole numbers within 2004-2020")
  }
  if (!is.numeric(CHUNK_ROWS) || length(CHUNK_ROWS) != 1L ||
      is.na(CHUNK_ROWS) || CHUNK_ROWS < 1L || CHUNK_ROWS != as.integer(CHUNK_ROWS)) {
    stop("CHUNK_ROWS must be a positive integer")
  }
  years <- unique(as.integer(years))
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
  message("Selected extraction years: ", paste(years, collapse = ", "))
  message("Single-worker chunked mode; chunk rows: ", CHUNK_ROWS)
  dictionary <- read_produce_dictionary()
  results <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[[i]]
    results[[i]] <- tryCatch(
      process_year(y, dictionary),
      error = function(e) {
        message("ERROR in year ", y, ": ", conditionMessage(e))
        if (STOP_ON_ERROR) stop(e)
        data.table(year = y, status = "failed", rows = NA_integer_)
      }
    )
    gc(verbose = FALSE)
  }
  summary <- rbindlist(results, use.names = TRUE, fill = TRUE)
  log_file <- file.path(LOG_DIR, "produce_extraction_run_summary.csv")
  fwrite(summary, log_file)
  message("Run summary: ", log_file)
  invisible(summary)
}

# Running this file with Rscript starts the selected extraction.
# When sourced interactively, call run_produce_extraction() explicitly.
if (sys.nframe() == 0L) {
  run_produce_extraction()
}
