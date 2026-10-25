# =============================================================================
# Extract and label Nielsen snack purchases (purchase-level intermediate data).
# Dependencies: install.packages(c("data.table", "readr"))
# Run from RStudio, or Rscript data_cleaning/get_category_purchase/01_extract_snack_purchases.R
#
# Products and purchases are streamed once in chunks. Only the snack product
# lookup and four columns of one year's trips remain in memory. Years run
# sequentially; RAM does not grow with the number of purchase rows or years.
# Labels reproduce the supplied module-based research definition, rather than
# making product-specific nutritional claims. No products_extra/food_list needed.
# =============================================================================

# Configuration ---------------------------------------------------------------
HMS_ROOT <- "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS"
PRODUCTS_FILE <- file.path(HMS_ROOT, "Master_Files", "Latest", "products.tsv")
DATA_ROOT <- "C:/Users/rwen13/Documents/healthy_purchase_data"
OUTPUT_ROOT <- file.path(DATA_ROOT, "intermediate", "snack_purchases")

RUN_YEARS <- 2010:2018L                    # After validation: 2008:2017
RUN_DEFINITION <- "broad"             # "clean" or "broad"
RUN_OVERWRITE <- FALSE
CHUNK_ROWS <- 100000L                 # Reduce to 25000L if RAM is limited.
DT_THREADS <- 4L
RUN_SELF_CHECKS <- TRUE

# Nielsen raw extracts are tab-delimited text; quotation marks in product and
# brand descriptions are literal characters, not CSV-style field wrappers.
# This applies only to raw inputs. Output TSVs still use fwrite's normal quoting.
RAW_TSV_QUOTE <- ""
RAW_TSV_ENCODING <- "UTF-8"            # Change only if the source encoding is known.

# Extraction retains every date in each source file. For a later trip-level
# panel covering purchase dates in 2008-2017, also extract source year 2018.
# The later aggregation step must filter purchase_date, not source_file_year
# or the original panel_year. Do not change purchase dates at extraction time.

for (pkg in c("data.table", "readr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing package: ", pkg, ". Install it before running this script.")
  }
}
suppressPackageStartupMessages(library(data.table))

# Module definitions ----------------------------------------------------------
# Store codes as character; normalize a separate classification field only.
SNACK_MAP <- data.table(
  product_module_code_norm = c(
    "1270", "1271", "1318", "1323", "1324", "1325", "1326", "1327",
    "1330", "1332", "1333", "1452", "1608", "1660", "1666", "1687", "1708",
    "1341", "1360", "1425", "3556", "1506", "1507", "1508", "1509", "1362"
  ),
  snack_category = c(
    "pork_rinds", "meat_snacks", "puffed_cheese", "potato_snacks",
    "potato_snacks", "corn_tortilla_chips", "corn_tortilla_chips", "other_snacks",
    "pretzels", "caramel_corn", "variety_packs", "snack_bars", "snack_bars",
    "meat_snacks", "other_snacks", "pork_rinds", "other_snacks",
    "cracker_snacks", "cracker_snacks", "dried_fruit_snacks",
    "processed_cheese_snacks", "nuts", "nuts", "nuts", "nuts", "cookies"
  ),
  in_clean = c(rep(TRUE, 17L), rep(FALSE, 9L))
)
SNACK_MAP[, snack_health_label := fifelse(
  snack_category %in% c("nuts", "dried_fruit_snacks"),
  "healthy_snack", "unhealthy_snack"
)]

PRODUCT_COLS <- c(
  "upc", "upc_ver_uc", "product_module_code", "product_module_descr",
  "product_group_code", "product_group_descr", "brand_code_uc", "brand_descr",
  "size1_amount", "size1_units"
)
PURCHASE_COLS <- c(
  "trip_code_uc", "upc", "upc_ver_uc", "quantity",
  "total_price_paid", "coupon_value", "deal_flag_uc"
)
TRIP_COLS <- c("trip_code_uc", "household_code", "purchase_date", "panel_year")
OUTPUT_COLS <- c(
  "trip_code_uc", "household_code", "purchase_date", "panel_year",
  "upc", "upc_ver_uc", "product_module_code", "product_module_descr",
  "product_group_code", "product_group_descr", "snack_definition",
  "snack_category", "snack_health_label", "brand_code_uc", "brand_descr",
  "quantity", "size1_amount", "size1_units", "total_price_paid", "coupon_value",
  "final_price_paid", "deal_flag_uc", "has_coupon", "is_deal",
  "product_module_code_norm", "source_file_year"
)

# Shared helpers --------------------------------------------------------------
normalize_module <- function(x) {
  # Never call this on UPC, UPC version, household, or trip identifiers.
  out <- trimws(as.character(x))
  out[out %in% c("", "NA")] <- NA_character_
  numeric_like <- !is.na(out) & grepl("^[0-9]+([.]0+)?$", out)
  out[numeric_like] <- sub("[.]0+$", "", out[numeric_like])
  out[numeric_like] <- sub("^0+(?=[0-9])", "", out[numeric_like], perl = TRUE)
  out
}

check_columns <- function(path, required) {
  if (!file.exists(path)) stop("Input file missing: ", path)
  header <- names(fread(path, sep = "\t", quote = RAW_TSV_QUOTE,
                        nrows = 0L, showProgress = FALSE))
  if (anyDuplicated(header)) stop("Duplicated column names in: ", path)
  missing <- setdiff(required, header)
  if (length(missing)) stop("Missing columns in ", path, ": ", paste(missing, collapse = ", "))
  invisible(header)
}

stop_parse_problem <- function(problems, path, pos, header) {
  # Report the actual parser diagnostics rather than only the chunk start.
  # Parser rows are data-row numbers. With literal quotes and no skipped blank
  # rows, the physical file line is data_row + 1 (the header occupies line 1).
  details <- as.data.frame(head(problems, 5L))
  if ("row" %in% names(details)) {
    names(details)[names(details) == "row"] <- "data_row"
    if (identical(RAW_TSV_QUOTE, "")) details$file_line <- details$data_row + 1L
  }
  if ("col" %in% names(details)) {
    idx <- suppressWarnings(as.integer(details$col))
    details$column_name <- header[idx]
  }
  diagnostic <- paste(capture.output(print(details, row.names = FALSE)), collapse = "\n")
  stop("TSV parsing problem in ", path, "\nChunk starts at data row ", pos,
       "; parser details (up to 5):\n", diagnostic,
       "\nNo malformed rows were skipped. Check field counts and the raw input format.",
       call. = FALSE)
}

stream_selected <- function(path, columns, callback, chunk_rows) {
  header <- check_columns(path, columns)
  # Character parsing from the outset protects leading zeros and long IDs.
  # SideEffectChunkCallback writes/processes each chunk without collecting it.
  spec <- do.call(readr::cols_only, setNames(
    lapply(columns, function(x) readr::col_character()), columns
  ))
  readr::read_tsv_chunked(
    path, callback = readr::SideEffectChunkCallback$new(function(x, pos) {
      problems <- readr::problems(x)
      if (nrow(problems)) stop_parse_problem(problems, path, pos, header)
      callback(as.data.table(x), pos)
      invisible(NULL)
    }),
    chunk_size = chunk_rows, col_types = spec, quote = RAW_TSV_QUOTE,
    locale = readr::locale(encoding = RAW_TSV_ENCODING),
    trim_ws = FALSE, na = c("", "NA"), skip_empty_rows = FALSE, progress = FALSE
  )
  invisible(NULL)
}

assert_keys <- function(dt, keys, label) {
  for (k in keys) {
    if (!is.character(dt[[k]])) stop(label, ": key is not character: ", k)
    if (anyNA(dt[[k]]) || any(!nzchar(trimws(dt[[k]])))) {
      stop(label, ": missing/blank key: ", k)
    }
  }
}

unique_lookup <- function(dt, keys, label) {
  assert_keys(dt, keys, label)
  # Collapse only exact duplicates in the selected lookup fields. Conflicting
  # records require a data decision; never select an arbitrary first record.
  dt <- unique(dt)
  if (anyDuplicated(dt, by = keys)) stop(label, ": conflicting duplicate join keys.")
  setkeyv(dt, keys)
  dt
}

to_numeric <- function(x, label) {
  out <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & (is.na(out) | !is.finite(out)))) {
    stop("Invalid numeric value in ", label)
  }
  out
}

safe_print <- function(x, n = 25L) {
  tryCatch(print(as.data.frame(head(x, n)), row.names = FALSE),
           error = function(e) message("Diagnostic print skipped: ", conditionMessage(e)))
  invisible(NULL)
}

commit_file <- function(staged, destination, overwrite) {
  # The previous completed file survives an extraction failure. On Windows,
  # move it aside before renaming, and restore it if the final rename fails.
  backup <- NULL
  if (file.exists(destination)) {
    if (!overwrite) stop("Output already exists: ", destination)
    backup <- tempfile("previous_", tmpdir = dirname(destination))
    if (!file.rename(destination, backup)) stop("Cannot move existing output; close it in other applications.")
  }
  if (!file.rename(staged, destination)) {
    if (!is.null(backup) && !file.rename(backup, destination)) {
      stop("Output commit failed; previous output remains at: ", backup)
    }
    stop("Cannot commit output: ", destination)
  }
  if (!is.null(backup)) unlink(backup)
  invisible(destination)
}

# Build and reuse one small product lookup ------------------------------------
build_snack_dictionary <- function(products_file, definition, chunk_rows) {
  definition <- match.arg(definition, c("clean", "broad"))
  mapping <- if (definition == "clean") SNACK_MAP[in_clean == TRUE] else copy(SNACK_MAP)
  parts <- list()
  stream_selected(products_file, PRODUCT_COLS, function(x, pos) {
    x[, product_module_code_norm := normalize_module(product_module_code)]
    x <- x[product_module_code_norm %chin% mapping$product_module_code_norm]
    if (nrow(x)) parts[[length(parts) + 1L]] <<- x
  }, chunk_rows)
  if (!length(parts)) stop("No snack products found. Check the master file and module definitions.")
  dictionary <- rbindlist(parts, use.names = TRUE)
  rm(parts)
  dictionary <- unique_lookup(dictionary, c("upc", "upc_ver_uc"), "Snack dictionary")
  idx <- match(dictionary$product_module_code_norm, mapping$product_module_code_norm)
  dictionary[, `:=`(
    snack_definition = definition,
    snack_category = mapping$snack_category[idx],
    snack_health_label = mapping$snack_health_label[idx],
    size1_amount = to_numeric(size1_amount, "size1_amount")
  )]
  message("Unique snack UPC-version pairs: ", nrow(dictionary))
  safe_print(dictionary[, .(n_upc_version = .N), by = .(
    product_module_code_norm, snack_category, snack_health_label
  )][order(product_module_code_norm)], n = 30L)
  dictionary
}

# Process one source year -----------------------------------------------------
process_snack_purchases <- function(year, dictionary, definition,
                                    chunk_rows = CHUNK_ROWS, overwrite = FALSE,
                                    hms_root = HMS_ROOT, output_root = OUTPUT_ROOT) {
  output_dir <- file.path(output_root, definition)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(output_dir, paste0("snack_purchases_", year, ".tsv"))
  if (file.exists(destination) && !overwrite) {
    message("SKIPPED existing output: ", destination)
    return(invisible(data.table(source_file_year = year, status = "skipped", output = destination)))
  }
  annual_dir <- file.path(hms_root, as.character(year), "Annual_Files")
  purchases_file <- file.path(annual_dir, paste0("purchases_", year, ".tsv"))
  trips_file <- file.path(annual_dir, paste0("trips_", year, ".tsv"))
  check_columns(purchases_file, PURCHASE_COLS)
  check_columns(trips_file, TRIP_COLS)
  message("Processing source year ", year, " [", definition, "]")
  
  trips <- fread(trips_file, sep = "\t", quote = RAW_TSV_QUOTE, select = TRIP_COLS,
                 colClasses = "character", na.strings = c("", "NA"),
                 strip.white = FALSE, showProgress = FALSE)
  trips <- unique_lookup(trips, "trip_code_uc", "Trips lookup")
  assert_keys(trips, "household_code", "Trips lookup")
  message("Unique trip IDs: ", nrow(trips))
  
  staged <- tempfile(paste0("snack_", year, "_"), tmpdir = output_dir, fileext = ".partial")
  on.exit(if (file.exists(staged)) unlink(staged), add = TRUE)
  # Write a header even if no snack purchases occur in this year.
  empty <- as.data.table(setNames(rep(list(character()), length(OUTPUT_COLS)), OUTPUT_COLS))
  fwrite(empty, staged, sep = "\t", quote = "auto", na = "NA")
  rows_read <- rows_matched <- rows_written <- 0
  missing_spending <- negative_spending <- 0
  seen_products <- rep(FALSE, nrow(dictionary))
  counts <- NULL
  started <- proc.time()[[3L]]
  
  stream_selected(purchases_file, PURCHASE_COLS, function(x, pos) {
    rows_read <<- rows_read + nrow(x)
    # One lookup position per purchase row; duplicate purchases are retained.
    idx <- dictionary[x, on = .(upc, upc_ver_uc), which = TRUE]
    keep <- which(!is.na(idx))
    if (!length(keep)) return(invisible(NULL))
    x <- x[keep]
    idx <- idx[keep]
    rows_matched <<- rows_matched + nrow(x)
    seen_products[unique(idx)] <<- TRUE
    assert_keys(x, c("upc", "upc_ver_uc", "trip_code_uc"), "Selected purchases")
    trip_idx <- trips[x, on = .(trip_code_uc), which = TRUE]
    if (anyNA(trip_idx)) {
      stop(sum(is.na(trip_idx)), " snack rows have no matching trip in ", year,
           ". Output not committed; inspect trips input.")
    }
    for (col in setdiff(names(dictionary), c("upc", "upc_ver_uc"))) {
      set(x, j = col, value = dictionary[[col]][idx])
    }
    for (col in setdiff(TRIP_COLS, "trip_code_uc")) {
      set(x, j = col, value = trips[[col]][trip_idx])
    }
    for (col in c("quantity", "total_price_paid", "coupon_value")) {
      set(x, j = col, value = to_numeric(x[[col]], col))
    }
    # Preserve the original net-spending definition, including negatives and NA.
    # Missing coupons are NOT silently changed to zero.
    x[, `:=`(
      final_price_paid = total_price_paid - coupon_value,
      has_coupon = as.integer(!is.na(coupon_value) & coupon_value > 0),
      is_deal = as.integer(!is.na(deal_flag_uc) & deal_flag_uc == "1"),
      source_file_year = as.integer(year)
    )]
    missing_spending <<- missing_spending + sum(is.na(x$final_price_paid))
    negative_spending <<- negative_spending + sum(x$final_price_paid < 0, na.rm = TRUE)
    setcolorder(x, OUTPUT_COLS)
    fwrite(x, staged, sep = "\t", append = TRUE, col.names = FALSE,
           quote = "auto", na = "NA")
    rows_written <<- rows_written + nrow(x)
    chunk_counts <- x[, .(n_purchase_rows = .N,
                          net_spending_observed = sum(final_price_paid, na.rm = TRUE)),
                      by = .(product_module_code_norm, product_module_descr,
                             snack_category, snack_health_label)]
    counts <<- rbindlist(list(counts, chunk_counts))[, .(
      n_purchase_rows = sum(as.double(n_purchase_rows)),
      net_spending_observed = sum(net_spending_observed)
    ), by = .(product_module_code_norm, product_module_descr, snack_category, snack_health_label)]
    if ((rows_read %/% chunk_rows) %% 20L == 0L) {
      message("  Read ", rows_read, " purchases; wrote ", rows_written, " snack rows.")
    }
    invisible(NULL)
  }, chunk_rows)
  
  stopifnot(rows_written == rows_matched)
  commit_file(staged, destination, overwrite)
  message("SAVED: ", destination)
  message("All purchase rows scanned: ", rows_read,
          "; snack rows before trip join: ", rows_matched,
          "; rows after trip join: ", rows_written)
  message("Purchased unique snack UPC-version pairs: ", sum(seen_products),
          "; dictionary pairs: ", nrow(dictionary))
  
  audit <- data.table(
    source_file_year = year, snack_definition = definition,
    rows_scanned = rows_read, snack_rows_before_trip_join = rows_matched,
    rows_written = rows_written, unmatched_snack_trip_rows = 0,
    purchased_upc_versions = sum(seen_products), dictionary_upc_versions = nrow(dictionary),
    missing_net_spending_rows = missing_spending,
    negative_net_spending_rows = negative_spending,
    elapsed_seconds = proc.time()[[3L]] - started
  )
  # Diagnostics run AFTER the main data are saved; a print/audit failure must
  # never turn a completed extraction into a missing purchase output.
  tryCatch({
    fwrite(audit, file.path(output_dir, paste0("snack_purchase_audit_", year, ".tsv")), sep = "\t")
    safe_print(audit)
    if (!is.null(counts)) {
      fwrite(counts, file.path(output_dir, paste0("snack_module_audit_", year, ".tsv")), sep = "\t")
      safe_print(counts[order(-n_purchase_rows)], n = 30L)
      safe_print(counts[, .(n_purchase_rows = sum(n_purchase_rows)), by = snack_health_label])
    }
  }, error = function(e) message("Data saved; optional diagnostics failed: ", conditionMessage(e)))
  invisible(data.table(source_file_year = year, status = "saved", output = destination))
}

# Small rule checks; these do not substitute for testing on the source data. ---
run_self_checks <- function() {
  stopifnot(identical(normalize_module(c("01506", "1506.0", "001362", NA)),
                      c("1506", "1506", "1362", NA_character_)))
  stopifnot(nrow(SNACK_MAP) == 26L, sum(SNACK_MAP$in_clean) == 17L,
            !anyDuplicated(SNACK_MAP$product_module_code_norm))
  stopifnot(all(SNACK_MAP[snack_health_label == "healthy_snack", product_module_code_norm] %in%
                  c("1425", "1506", "1507", "1508", "1509")))
  stopifnot(SNACK_MAP[product_module_code_norm == "1362", snack_health_label] == "unhealthy_snack",
            all(SNACK_MAP[in_clean == TRUE, snack_health_label] == "unhealthy_snack"))
  # A join must preserve long keys, leading zeros, and repeated purchase rows.
  lookup <- data.table(upc = c("000123", "12345678901234567890"), upc_ver_uc = c("01", "02"))
  incoming <- data.table(upc = c("000123", "000123", "12345678901234567890", "123"),
                         upc_ver_uc = c("01", "01", "02", "01"))
  stopifnot(identical(lookup[incoming, on = .(upc, upc_ver_uc), which = TRUE],
                      c(1L, 1L, 2L, NA_integer_)))
  invisible(TRUE)
}

run_batch_snack <- function(years = RUN_YEARS, definition = RUN_DEFINITION,
                            overwrite = RUN_OVERWRITE, chunk_rows = CHUNK_ROWS) {
  definition <- match.arg(definition, c("clean", "broad"))
  if (!length(years) || anyNA(years) || any(years != as.integer(years))) stop("Invalid years.")
  if (length(chunk_rows) != 1L || is.na(chunk_rows) || chunk_rows < 1 ||
      chunk_rows != as.integer(chunk_rows)) stop("chunk_rows must be a positive integer.")
  old_threads <- getDTthreads()
  on.exit(setDTthreads(old_threads), add = TRUE)
  setDTthreads(DT_THREADS)
  if (RUN_SELF_CHECKS) run_self_checks()
  dictionary <- build_snack_dictionary(PRODUCTS_FILE, definition, chunk_rows)
  results <- lapply(unique(as.integer(years)), function(year) {
    result <- tryCatch(
      process_snack_purchases(year, dictionary, definition, chunk_rows, overwrite),
      error = function(e) {
        message("FAILED year ", year, ": ", conditionMessage(e))
        data.table(source_file_year = year, status = "failed", output = conditionMessage(e))
      }
    )
    gc(verbose = FALSE)
    result
  })
  results <- rbindlist(results)
  safe_print(results, n = nrow(results))
  if (any(results$status == "failed")) stop("Some years failed. See messages above; successful years are saved.")
  invisible(results)
}

# Test 2008 first. Then edit RUN_YEARS to 2008:2017 and run again.
# Existing outputs are skipped unless RUN_OVERWRITE is TRUE.
# Source without executing: source("01_extract_snack_purchases.R")
# Then call run_batch_snack() explicitly. Rscript runs the batch automatically.
if (sys.nframe() == 0L) run_batch_snack()
