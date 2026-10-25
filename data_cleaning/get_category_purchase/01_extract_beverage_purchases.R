# =============================================================================
# Script: Extract non-alcoholic beverage purchases
#
# Purpose:
#   Extract soft drinks and healthier drinks from Nielsen Consumer Panel data.
#   Alcohol is intentionally excluded from the product dictionary and output.
#
# Data and performance rules:
#   - Read only columns needed by this step.
#   - Keep UPC, UPC-version, trip, household, and module codes as character.
#   - Normalize module codes for classification only; never normalize UPC keys.
#   - Stream purchases in chunks and write each matched chunk immediately.
#   - Process years sequentially to keep peak memory use predictable.
#   - Validate trip-key uniqueness and protect against silent empty scans.
#
# Output:
#   C:/Users/rwen13/Documents/healthy_purchase_data/intermediate/
#     beverage_purchases/beverage_purchases_YYYY.tsv
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# -----------------------------------------------------------------------------
# Run settings
# -----------------------------------------------------------------------------

PANEL_ROOT <- "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS"
OUTPUT_DIR <- "C:/Users/rwen13/Documents/healthy_purchase_data/intermediate/beverage_purchases"

# Test one year first; after validation, change to 2008:2017.
RUN_YEARS <- 2009:2018L
CHUNK_ROWS <- 250000L
RUN_OVERWRITE <- FALSE

# Only non-alcoholic categories are defined here. Alcohol modules are excluded.
SOFT_DRINK_MODULES <- c(
  68, 76, 452,
  1041, 1042,
  1482, 1484, 1553,
  1638, 1713,
  2669, 2670,
  6086
)

HEALTHIER_DRINK_MODULES <- c(
  12, 108, 448,
  1030, 1031, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039,
  1040, 1044, 1045, 1049,
  1054, 1055,
  1461, 1466, 1487,
  1635, 1636, 1637,
  2662, 2663, 2666, 2667, 2668, 2674,
  6079
)

NONALCOHOLIC_MODULES <- as.character(c(SOFT_DRINK_MODULES, HEALTHIER_DRINK_MODULES))

OUTPUT_COLUMNS <- c(
  "trip_code_uc", "household_code", "purchase_date", "panel_year",
  "upc", "upc_ver_uc",
  "product_module_code", "product_module_descr",
  "product_group_code", "product_group_descr", "beverage_type",
  "brand_code_uc", "brand_descr", "quantity",
  "size1_code_uc", "size1_amount", "size1_units",
  "total_price_paid", "coupon_value", "final_price_paid",
  "deal_flag_uc", "has_coupon", "is_deal"
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "NaN")] <- NA_character_
  x
}

# Normalize numeric classification codes only. Do not use for UPC identifiers.
normalize_numeric_code <- function(x) {
  x <- clean_text(x)
  x <- sub("\\.0+$", "", x)
  numeric_code <- !is.na(x) & grepl("^[0-9]+$", x)
  x[numeric_code] <- sub("^0+(?=[0-9])", "", x[numeric_code], perl = TRUE)
  x
}

read_header <- function(path) {
  names(fread(path, sep = "\t", nrows = 0, check.names = FALSE, showProgress = FALSE))
}

read_selected_character <- function(path, required_cols, optional_cols = character()) {
  available <- read_header(path)
  missing_required <- setdiff(required_cols, available)
  if (length(missing_required)) {
    stop(
      "Required column(s) missing from ", path, ": ",
      paste(missing_required, collapse = ", ")
    )
  }

  selected <- intersect(c(required_cols, optional_cols), available)
  out <- fread(
    path, sep = "\t", select = selected, colClasses = "character",
    na.strings = c("", "NA"), check.names = FALSE, showProgress = TRUE
  )

  for (col in setdiff(optional_cols, names(out))) {
    set(out, j = col, value = NA_character_)
  }
  out
}

safe_print <- function(x, title, max_rows = 40L) {
  # Diagnostics should not prevent the main output from being saved.
  tryCatch({
    message(title)
    if (!nrow(x)) message("(no rows)") else print(x[seq_len(min(nrow(x), max_rows))])
  }, error = function(e) {
    message("Diagnostic printing skipped: ", conditionMessage(e))
  })
}

load_beverage_dictionary <- function(products_ref_path) {
  required <- c("upc", "upc_ver_uc", "product_module_code")
  optional <- c(
    "product_module_descr", "product_group_code", "product_group_descr",
    "brand_code_uc", "brand_descr", "size1_code_uc", "size1_amount",
    "size1_units", "size1_change_flag_uc"
  )

  products <- read_selected_character(products_ref_path, required, optional)
  products[, product_module_code_norm := normalize_numeric_code(product_module_code)]
  products <- products[product_module_code_norm %in% NONALCOHOLIC_MODULES]

  if (!nrow(products)) {
    stop("No products found for the selected non-alcoholic beverage modules.")
  }

  products[, beverage_type := fcase(
    product_module_code_norm %in% as.character(SOFT_DRINK_MODULES), "soft_drinks",
    product_module_code_norm %in% as.character(HEALTHIER_DRINK_MODULES), "healthier_drinks",
    default = NA_character_
  )]
  products <- products[!is.na(beverage_type)]

  # Prevent duplicate UPC-version pairs from multiplying purchase rows.
  setorder(products, upc, upc_ver_uc, product_module_code_norm)
  duplicate_pairs <- products[, .N, by = .(upc, upc_ver_uc)][N > 1L, .N]
  if (duplicate_pairs > 0L) {
    message(
      "Product master has ", duplicate_pairs,
      " duplicated UPC-version pair(s) across included modules; retaining one row per pair."
    )
  }
  products <- unique(products, by = c("upc", "upc_ver_uc"))

  keep <- c(
    "upc", "upc_ver_uc", "product_module_code", "product_module_code_norm",
    "product_module_descr", "product_group_code", "product_group_descr",
    "brand_code_uc", "brand_descr", "size1_code_uc", "size1_amount",
    "size1_units", "size1_change_flag_uc", "beverage_type"
  )
  products[, ..keep]
}

# Sequentially read a bounded number of source lines from one open connection.
# This avoids repeated fread(skip=...) offsets, which can be unreliable.
read_purchase_chunk <- function(con, header_line, selected_cols, chunk_rows) {
  lines <- readLines(con, n = chunk_rows, warn = FALSE)
  if (!length(lines)) return(data.table())

  fread(
    text = paste(c(header_line, lines), collapse = "\n"),
    sep = "\t", header = TRUE, select = selected_cols,
    colClasses = "character", na.strings = c("", "NA"),
    check.names = FALSE, showProgress = FALSE
  )
}

# -----------------------------------------------------------------------------
# Process one source year
# -----------------------------------------------------------------------------

process_beverage_year <- function(year, beverage_dict) {
  message("\n============================================================")
  message("Processing non-alcoholic beverage purchases: ", year)
  message("============================================================")

  annual_dir <- file.path(PANEL_ROOT, as.character(year), "Annual_Files")
  purchases_file <- file.path(annual_dir, paste0("purchases_", year, ".tsv"))
  trips_file <- file.path(annual_dir, paste0("trips_", year, ".tsv"))

  for (path in c(purchases_file, trips_file)) {
    if (!file.exists(path)) stop("Required input file missing: ", path)
  }

  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(OUTPUT_DIR, paste0("beverage_purchases_", year, ".tsv"))
  if (file.exists(output_file) && !RUN_OVERWRITE) {
    stop("Output already exists and RUN_OVERWRITE is FALSE: ", output_file)
  }

  # Keep only trip fields needed for household/date linkage.
  trips <- read_selected_character(
    trips_file,
    required_cols = c("trip_code_uc", "household_code", "purchase_date", "panel_year")
  )
  duplicate_trip_ids <- trips[, .N, by = "trip_code_uc"][N > 1L, .N]
  if (duplicate_trip_ids > 0L) {
    stop(
      "Trips file has ", duplicate_trip_ids,
      " duplicated trip_code_uc value(s) in ", year,
      "; resolve duplicate keys before joining."
    )
  }
  setkey(trips, trip_code_uc)

  purchase_header <- read_header(purchases_file)
  required_purchase <- c("upc", "upc_ver_uc", "trip_code_uc")
  missing_purchase <- setdiff(required_purchase, purchase_header)
  if (length(missing_purchase)) {
    stop("Purchases file is missing: ", paste(missing_purchase, collapse = ", "))
  }

  optional_purchase <- c("quantity", "total_price_paid", "coupon_value", "deal_flag_uc")
  selected_purchase <- intersect(c(required_purchase, optional_purchase), purchase_header)

  # Stream the large purchase file from a single connection.
  purchase_con <- file(purchases_file, open = "r")
  header_line <- readLines(purchase_con, n = 1L, warn = FALSE)
  if (!length(header_line)) {
    close(purchase_con)
    stop("Purchases file is empty: ", purchases_file)
  }
  on.exit(if (isOpen(purchase_con)) close(purchase_con), add = TRUE)

  output_tmp <- paste0(output_file, ".tmp")
  if (file.exists(output_tmp)) unlink(output_tmp)
  on.exit(if (file.exists(output_tmp)) unlink(output_tmp), add = TRUE)

  source_rows <- 0
  matched_product_rows <- 0
  matched_trip_rows <- 0
  soft_rows <- 0
  healthier_rows <- 0
  chunk_index <- 0L
  audit_parts <- list()

  repeat {
    purchase_chunk <- read_purchase_chunk(
      con = purchase_con, header_line = header_line,
      selected_cols = selected_purchase, chunk_rows = CHUNK_ROWS
    )
    if (!nrow(purchase_chunk)) break

    chunk_index <- chunk_index + 1L
    source_rows <- source_rows + nrow(purchase_chunk)

    for (col in setdiff(optional_purchase, names(purchase_chunk))) {
      set(purchase_chunk, j = col, value = NA_character_)
    }

    # Join identifiers remain character to preserve exact key values.
    for (key in c("upc", "upc_ver_uc", "trip_code_uc")) {
      set(purchase_chunk, j = key, value = as.character(purchase_chunk[[key]]))
    }

    # The dictionary contains only soft-drink and healthier-drink modules.
    beverage_chunk <- beverage_dict[
      purchase_chunk, on = .(upc, upc_ver_uc), nomatch = 0L
    ]
    if (!nrow(beverage_chunk)) {
      rm(purchase_chunk, beverage_chunk)
      next
    }
    matched_product_rows <- matched_product_rows + nrow(beverage_chunk)

    if (any(!beverage_chunk$beverage_type %in% c("soft_drinks", "healthier_drinks"))) {
      stop("Unexpected beverage_type found; only non-alcoholic categories are allowed.")
    }

    # Attach household, date, and panel metadata.
    beverage_chunk <- trips[beverage_chunk, on = .(trip_code_uc), nomatch = 0L]
    matched_trip_rows <- matched_trip_rows + nrow(beverage_chunk)
    if (!nrow(beverage_chunk)) {
      rm(purchase_chunk, beverage_chunk)
      next
    }

    beverage_chunk[, quantity := suppressWarnings(as.numeric(quantity))]
    beverage_chunk[, total_price_paid := suppressWarnings(as.numeric(total_price_paid))]
    beverage_chunk[, coupon_value := suppressWarnings(as.numeric(coupon_value))]
    beverage_chunk[is.na(coupon_value), coupon_value := 0]
    beverage_chunk[, final_price_paid := fifelse(
      is.na(total_price_paid), NA_real_, total_price_paid - coupon_value
    )]
    beverage_chunk[, has_coupon := as.integer(coupon_value > 0)]
    beverage_chunk[, is_deal := as.integer(!is.na(deal_flag_uc) & deal_flag_uc == "1")]

    soft_rows <- soft_rows + sum(beverage_chunk$beverage_type == "soft_drinks", na.rm = TRUE)
    healthier_rows <- healthier_rows +
      sum(beverage_chunk$beverage_type == "healthier_drinks", na.rm = TRUE)

    audit_parts[[length(audit_parts) + 1L]] <- beverage_chunk[
      , .N, by = .(product_module_code, product_module_descr, beverage_type)
    ]

    for (col in setdiff(OUTPUT_COLUMNS, names(beverage_chunk))) {
      set(beverage_chunk, j = col, value = NA_character_)
    }
    output_chunk <- beverage_chunk[, ..OUTPUT_COLUMNS]

    fwrite(
      output_chunk, file = output_tmp, sep = "\t",
      append = file.exists(output_tmp), col.names = !file.exists(output_tmp), na = ""
    )

    message(
      "Chunk ", chunk_index,
      " | source rows read: ", format(source_rows, big.mark = ","),
      " | beverage rows written: ", format(matched_trip_rows, big.mark = ",")
    )

    rm(purchase_chunk, beverage_chunk, output_chunk)
    if (chunk_index %% 4L == 0L) invisible(gc())
  }

  # Never finalize a silently empty scan of a non-empty source file.
  if (source_rows == 0L && file.info(purchases_file)$size > 0) {
    stop("No purchase rows were read from a non-empty file; output was not finalized.")
  }

  if (!file.exists(output_tmp)) {
    empty_output <- as.data.table(
      setNames(replicate(length(OUTPUT_COLUMNS), character(), simplify = FALSE), OUTPUT_COLUMNS)
    )
    fwrite(empty_output, output_tmp, sep = "\t", na = "")
  }

  if (file.exists(output_file)) unlink(output_file)
  if (!file.rename(output_tmp, output_file)) stop("Could not finalize output: ", output_file)

  message("\nYear ", year, " summary")
  message("Source purchase rows scanned: ", format(source_rows, big.mark = ","))
  message("Rows matched to non-alcoholic beverage dictionary: ", format(matched_product_rows, big.mark = ","))
  message("Rows after trip join / written: ", format(matched_trip_rows, big.mark = ","))
  message("Soft-drink purchase rows: ", format(soft_rows, big.mark = ","))
  message("Healthier-drink purchase rows: ", format(healthier_rows, big.mark = ","))
  message("Output: ", output_file)

  # Audits are secondary outputs; failures here do not invalidate the main TSV.
  tryCatch({
    if (length(audit_parts)) {
      module_audit <- rbindlist(audit_parts)[
        , .(N = sum(N)),
        by = .(product_module_code, product_module_descr, beverage_type)
      ][order(beverage_type, product_module_code)]

      safe_print(module_audit, "Module-by-beverage-type counts:")
      fwrite(
        module_audit,
        file.path(OUTPUT_DIR, paste0("beverage_module_audit_", year, ".tsv")),
        sep = "\t", na = ""
      )
    }
  }, error = function(e) {
    message("Module audit could not be completed: ", conditionMessage(e))
    message("The main purchase output has already been saved.")
  })

  invisible(output_file)
}

# -----------------------------------------------------------------------------
# Batch runner
# -----------------------------------------------------------------------------

run_beverage_batch <- function(years = RUN_YEARS) {
  if (!length(years)) stop("RUN_YEARS is empty.")

  products_ref_path <- file.path(PANEL_ROOT, "Master_Files", "Latest", "products.tsv")
  if (!file.exists(products_ref_path)) stop("Product master file missing: ", products_ref_path)

  message("Loading shared product dictionary once...")
  beverage_dict <- load_beverage_dictionary(products_ref_path)
  setkey(beverage_dict, upc, upc_ver_uc)

  message(
    "Unique non-alcoholic beverage UPC-version pairs: ",
    format(nrow(beverage_dict), big.mark = ",")
  )
  message("Alcohol modules are excluded from the dictionary.")

  for (year in years) {
    process_beverage_year(as.integer(year), beverage_dict)
    invisible(gc())
  }

  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Run
# -----------------------------------------------------------------------------

run_beverage_batch(RUN_YEARS)
