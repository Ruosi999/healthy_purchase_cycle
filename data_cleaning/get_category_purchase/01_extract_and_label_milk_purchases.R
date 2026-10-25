# =============================================================================
# Script: Extract and label milk purchases in one memory-efficient step
#
# Purpose:
#   1. Identify milk products from the Nielsen master product file.
#   2. Match purchases to trips and annual products_extra type descriptions.
#   3. Create whole_milk and low_fat_milk indicators.
#   4. Write one labeled TSV per source year.
#
# Product definition:
#   3625 = DAIRY-MILK-REFRIGERATED
#   1297 = MILK - CANNED
#   Buttermilk (3626) is intentionally excluded.
#
# Important data rules:
#   - UPC and UPC-version keys are always read and joined as character.
#   - Household, trip, and module codes are kept as character.
#   - Purchase files are processed in chunks to avoid loading a full year
#     of purchase records into R memory.
#   - The product master and products_extra are reduced to the fields needed.
#   - Lookup tables are made unique before joining to prevent row multiplication.
#
# Output:
#   C:/Users/rwen13/Documents/healthy_purchase_data/intermediate/
#     milk_purchases_with_labels/milk_purchases_YYYY_with_labels.tsv
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------------------------------------------------------
# Run settings
# -----------------------------------------------------------------------------

# Root containing HMS annual files and the shared master product file.
PANEL_ROOT <- "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS"

# Keep generated data outside the GitHub repository.
OUTPUT_DIR <- "C:/Users/rwen13/Documents/healthy_purchase_data/intermediate/milk_purchases_with_labels"

# Run one year first, then switch to 2008:2017 for the full batch.
RUN_YEARS <- 2010:2018L
# Larger chunks reduce repeated parsing and join overhead. Lower this if RAM is constrained.
CHUNK_ROWS <- 500000L
RUN_OVERWRITE <- TRUE

# Process years sequentially to keep peak memory predictable.
# This script intentionally does not use parallel workers.
MILK_MODULES <- c("3625", "1297")

# Output column order is fixed for reproducible downstream use.
OUTPUT_COLUMNS <- c(
  "trip_code_uc",
  "household_code",
  "purchase_date",
  "panel_year",
  "upc",
  "upc_ver_uc",
  "product_module_code",
  "product_module_descr",
  "milk_definition",
  "milk_module_group",
  "milk_main_flag",
  "milk_expanded_flag",
  "expanded_regular_milk_flag",
  "brand_code_uc",
  "brand_descr",
  "quantity",
  "size1_code_uc",
  "size1_amount",
  "size1_units",
  "total_price_paid",
  "coupon_value",
  "final_price_paid",
  "has_coupon",
  "is_deal",
  "deal_flag_uc",
  "milk_type_descr",
  "whole_milk",
  "low_fat_milk",
  "fully_skimmed_milk"
)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

clean_missing_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "NaN")] <- NA_character_
  x
}

# Normalize numeric category codes only. Never use this on UPC join keys.
normalize_numeric_code <- function(x) {
  x <- clean_missing_text(x)
  x <- sub("\\.0+$", "", x)
  is_numeric_code <- !is.na(x) & grepl("^[0-9]+$", x)
  x[is_numeric_code] <- sub("^0+(?=[0-9])", "", x[is_numeric_code], perl = TRUE)
  x
}

read_header <- function(path) {
  names(fread(
    path,
    sep = "\t",
    nrows = 0,
    check.names = FALSE,
    showProgress = FALSE
  ))
}

read_selected_character <- function(path, required_cols, optional_cols = character()) {
  available <- read_header(path)
  missing_required <- setdiff(required_cols, available)
  
  if (length(missing_required) > 0L) {
    stop(
      "Required column(s) missing from file: ", path, "\n",
      paste(missing_required, collapse = ", ")
    )
  }
  
  selected <- intersect(c(required_cols, optional_cols), available)
  
  out <- fread(
    path,
    sep = "\t",
    select = selected,
    colClasses = "character",
    na.strings = c("", "NA"),
    check.names = FALSE,
    showProgress = TRUE
  )
  
  # Add optional columns absent from a particular annual file.
  for (col in setdiff(optional_cols, names(out))) {
    set(out, j = col, value = NA_character_)
  }
  
  out
}

first_non_missing <- function(x) {
  x <- clean_missing_text(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_character_ else x[[1L]]
}

safe_print <- function(x, title = NULL, max_rows = 30L) {
  # Diagnostics should never prevent the main output from being saved.
  tryCatch(
    {
      if (!is.null(title)) message(title)
      if (is.null(x) || nrow(as.data.table(x)) == 0L) {
        message("(no rows)")
      } else {
        print(as.data.table(x)[seq_len(min(nrow(as.data.table(x)), max_rows))])
      }
    },
    error = function(e) {
      message("Diagnostic printing skipped: ", conditionMessage(e))
    }
  )
}

# Read only milk modules from the shared master product file.
load_milk_dictionary <- function(products_ref_path) {
  required <- c("upc", "upc_ver_uc", "product_module_code")
  optional <- c(
    "product_module_descr",
    "brand_code_uc",
    "brand_descr",
    "size1_code_uc",
    "size1_amount",
    "size1_units",
    "size1_change_flag_uc"
  )
  
  products <- read_selected_character(
    products_ref_path,
    required_cols = required,
    optional_cols = optional
  )
  
  products[, product_module_code_norm := normalize_numeric_code(product_module_code)]
  products <- products[product_module_code_norm %in% MILK_MODULES]
  
  if (nrow(products) == 0L) {
    stop("No products found for milk modules: ", paste(MILK_MODULES, collapse = ", "))
  }
  
  products[, milk_definition := fifelse(
    product_module_code_norm == "3625",
    "hinnosaar_clean_milk",
    "expanded_regular_milk"
  )]
  
  products[, milk_module_group := fifelse(
    product_module_code_norm == "3625",
    "refrigerated_dairy_milk",
    "canned_milk"
  )]
  
  products[, milk_main_flag := as.integer(product_module_code_norm == "3625")]
  products[, milk_expanded_flag := 1L]
  products[, expanded_regular_milk_flag := as.integer(product_module_code_norm == "1297")]
  
  # Match the old extraction behavior: retain one product row per UPC-version.
  # Sorting makes the selected row deterministic if the master contains duplicates.
  setorder(products, upc, upc_ver_uc, product_module_code_norm)
  duplicate_pairs <- products[, .N, by = .(upc, upc_ver_uc)][N > 1L, .N]
  
  if (duplicate_pairs > 0L) {
    message(
      "Master product dictionary contains ", duplicate_pairs,
      " duplicated UPC-version pair(s) within the milk modules; keeping one row per pair."
    )
  }
  
  products <- unique(products, by = c("upc", "upc_ver_uc"))
  
  keep_cols <- c(
    "upc", "upc_ver_uc", "product_module_code", "product_module_code_norm",
    "product_module_descr", "milk_definition", "milk_module_group",
    "milk_main_flag", "milk_expanded_flag", "expanded_regular_milk_flag",
    "brand_code_uc", "brand_descr", "size1_code_uc", "size1_amount",
    "size1_units", "size1_change_flag_uc"
  )
  out <- products[, ..keep_cols]
  # Key the small lookup once; do not sort every purchase chunk before joining.
  setkeyv(out, c("upc", "upc_ver_uc"))
  out
}

# Create one type_descr per UPC-version. The lookup is intentionally narrow.
load_milk_type_lookup <- function(products_extra_path, milk_dict) {
  extra <- read_selected_character(
    products_extra_path,
    required_cols = c("upc", "upc_ver_uc", "type_descr")
  )
  
  # Keep only UPC-version pairs in the milk dictionary before grouping.
  # This reduces the aggregation table to milk products instead of all products.
  milk_keys <- unique(milk_dict[, .(upc, upc_ver_uc)])
  extra <- extra[milk_keys, on = .(upc, upc_ver_uc), nomatch = 0L]
  
  extra[, type_descr := clean_missing_text(type_descr)]
  extra_lookup <- extra[
    ,
    .(type_descr = first_non_missing(type_descr)),
    by = .(upc, upc_ver_uc)
  ]
  
  if (anyDuplicated(extra_lookup, by = c("upc", "upc_ver_uc"))) {
    stop("Internal error: products_extra lookup is not unique by UPC-version.")
  }
  
  setkeyv(extra_lookup, c("upc", "upc_ver_uc"))
  extra_lookup
}

# Read a bounded number of purchase rows without loading the annual file.
read_purchase_chunk <- function(con, header_line, selected_cols, chunk_rows) {
  # Read the next chunk sequentially from an already-open connection.
  # This avoids fragile repeated skip offsets in fread().
  lines <- readLines(con, n = chunk_rows, warn = FALSE)
  
  if (length(lines) == 0L) {
    return(data.table())
  }
  
  # Reattach the original header so fread can select columns by name.
  chunk_text <- paste(c(header_line, lines), collapse = "\n")
  
  fread(
    text = chunk_text,
    sep = "\t",
    header = TRUE,
    select = selected_cols,
    colClasses = "character",
    na.strings = c("", "NA"),
    check.names = FALSE,
    showProgress = FALSE
  )
}

# Create milk labels using the requested text rules.
add_milk_labels <- function(dt) {
  desc <- toupper(ifelse(is.na(dt$milk_type_descr), "", dt$milk_type_descr))
  
  # Canned milk is not assigned refrigerated-milk fat-type labels.
  canned <- dt$product_module_code_norm == "1297" |
    grepl("EVAPORATED|CONDENSED", desc)
  
  whole <- grepl("WHOLE", desc, fixed = TRUE) & !canned
  
  # Low-fat includes skim/nonfat products as well as reduced-fat products.
  low_fat_pattern <- paste0(
    "(^|[^A-Z])NF([^A-Z]|$)|",
    "NONFAT|NON[[:space:]]+FAT|",
    "(^|[^A-Z])FF([^A-Z]|$)|",
    "FAT[[:space:]-]*FREE|",
    "SKIM|LOWFAT|LOW[[:space:]]+FAT|",
    "(^|[^A-Z])RD[[:space:]]+FAT|REDUCED[[:space:]]+FAT|",
    "\\.5%|1%|2%"
  )
  low_fat <- grepl(low_fat_pattern, desc, perl = TRUE) & !whole & !canned
  
  # Fully skimmed is a narrower subset of low-fat milk:
  # NF / NONFAT / NON FAT / FF / FAT FREE / FAT-FREE / SKIM.
  # Reduced-fat products such as 0.5%, 1%, and 2% are not fully skimmed.
  fully_skimmed_pattern <- paste0(
    "(^|[^A-Z])NF([^A-Z]|$)|",
    "NONFAT|NON[[:space:]]+FAT|",
    "(^|[^A-Z])FF([^A-Z]|$)|",
    "FAT[[:space:]-]*FREE|",
    "SKIM"
  )
  fully_skimmed <- grepl(fully_skimmed_pattern, desc, perl = TRUE) &
    !whole & !canned
  
  dt[, whole_milk := as.integer(whole)]
  dt[, low_fat_milk := as.integer(low_fat)]
  dt[, fully_skimmed_milk := as.integer(fully_skimmed)]
  dt
}

# -----------------------------------------------------------------------------
# Process one source year
# -----------------------------------------------------------------------------

process_milk_year <- function(year, milk_dict) {
  message("\n============================================================")
  message("Processing milk purchases from source year: ", year)
  message("============================================================")
  
  annual_dir <- file.path(PANEL_ROOT, as.character(year), "Annual_Files")
  purchases_file <- file.path(annual_dir, paste0("purchases_", year, ".tsv"))
  trips_file <- file.path(annual_dir, paste0("trips_", year, ".tsv"))
  products_extra_file <- file.path(annual_dir, paste0("products_extra_", year, ".tsv"))
  
  for (path in c(purchases_file, trips_file, products_extra_file)) {
    if (!file.exists(path)) stop("Required input file missing: ", path)
  }
  
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(OUTPUT_DIR, paste0("milk_purchases_", year, "_with_labels.tsv"))
  
  if (file.exists(output_file) && !RUN_OVERWRITE) {
    stop(
      "Output already exists and RUN_OVERWRITE is FALSE: ", output_file,
      "\nSet RUN_OVERWRITE <- TRUE to replace it."
    )
  }
  
  # Read only the required trip fields; all join keys remain character.
  trips <- read_selected_character(
    trips_file,
    required_cols = c("trip_code_uc", "household_code", "purchase_date", "panel_year")
  )
  
  # Fail rather than silently deduplicating trips: duplicate trip IDs could
  # otherwise make the purchase-to-trip join ambiguous or multiply records.
  trip_duplicate_count <- trips[, .N, by = "trip_code_uc"][N > 1L, .N]
  if (trip_duplicate_count > 0L) {
    stop(
      "Trips file contains ", trip_duplicate_count,
      " duplicated trip_code_uc value(s) for year ", year,
      ". Resolve these before joining purchases."
    )
  }
  setkey(trips, trip_code_uc)
  
  type_lookup <- load_milk_type_lookup(products_extra_file, milk_dict)
  setkey(type_lookup, upc, upc_ver_uc)
  
  # Prepare the purchase file schema and select only columns used downstream.
  purchase_header <- read_header(purchases_file)
  required_purchase_cols <- c("upc", "upc_ver_uc", "trip_code_uc")
  missing_purchase_cols <- setdiff(required_purchase_cols, purchase_header)
  if (length(missing_purchase_cols) > 0L) {
    stop("Purchases file missing required columns: ", paste(missing_purchase_cols, collapse = ", "))
  }
  
  optional_purchase_cols <- c(
    "quantity", "total_price_paid", "coupon_value", "deal_flag_uc"
  )
  selected_purchase_cols <- intersect(
    c(required_purchase_cols, optional_purchase_cols),
    purchase_header
  )
  
  # Open the large purchase file once and stream through it sequentially.
  purchase_con <- file(purchases_file, open = "r")
  purchase_header_line <- readLines(purchase_con, n = 1L, warn = FALSE)
  if (length(purchase_header_line) == 0L) {
    close(purchase_con)
    stop("Purchases file is empty: ", purchases_file)
  }
  on.exit(if (isOpen(purchase_con)) close(purchase_con), add = TRUE)
  
  # Add absent optional purchase fields to each chunk as missing values.
  output_tmp <- paste0(output_file, ".tmp")
  if (file.exists(output_tmp)) unlink(output_tmp)
  on.exit(if (file.exists(output_tmp)) unlink(output_tmp), add = TRUE)
  
  # Small accumulators for diagnostics.
  source_rows <- 0
  matched_product_rows <- 0
  matched_extra_rows <- 0
  matched_trip_rows <- 0
  missing_type_rows <- 0
  whole_rows <- 0
  low_fat_rows <- 0
  fully_skimmed_rows <- 0
  # Collect small per-chunk key sets and deduplicate once at the end.
  # Repeatedly rbind()+unique() on the growing table becomes increasingly expensive.
  unique_pair_parts <- list()
  type_audit_parts <- list()
  module_audit_parts <- list()
  chunk_index <- 0L
  output_has_rows <- FALSE
  repeat {
    purchase_chunk <- read_purchase_chunk(
      con = purchase_con,
      header_line = purchase_header_line,
      selected_cols = selected_purchase_cols,
      chunk_rows = CHUNK_ROWS
    )
    
    if (nrow(purchase_chunk) == 0L) break
    
    chunk_index <- chunk_index + 1L
    chunk_n <- nrow(purchase_chunk)
    source_rows <- source_rows + chunk_n
    
    for (col in setdiff(optional_purchase_cols, names(purchase_chunk))) {
      set(purchase_chunk, j = col, value = NA_character_)
    }
    
    # Preserve all identifiers as character before joining.
    for (key in c("upc", "upc_ver_uc", "trip_code_uc")) {
      set(purchase_chunk, j = key, value = as.character(purchase_chunk[[key]]))
    }
    
    # First join: keep only purchases matching the milk product dictionary.
    # Join against the pre-keyed dictionary. Avoid sorting each purchase chunk.
    milk_chunk <- milk_dict[purchase_chunk, on = .(upc, upc_ver_uc), nomatch = 0L]
    
    # If no milk purchases are present in this chunk, move to the next chunk.
    if (nrow(milk_chunk) == 0L) {
      rm(purchase_chunk, milk_chunk)
      next
    }
    
    matched_product_rows <- matched_product_rows + nrow(milk_chunk)
    
    # Second join: attach type_descr without allowing duplicate extra rows.
    milk_chunk <- type_lookup[milk_chunk, on = .(upc, upc_ver_uc)]
    
    # The lookup's type_descr is renamed to the requested output name.
    setnames(milk_chunk, "type_descr", "milk_type_descr")
    matched_extra_rows <- matched_extra_rows + nrow(milk_chunk)
    missing_type_rows <- missing_type_rows + sum(is.na(milk_chunk$milk_type_descr))
    
    # Third join: attach household and trip information.
    before_trip_join <- nrow(milk_chunk)
    milk_chunk <- trips[milk_chunk, on = .(trip_code_uc), nomatch = 0L]
    matched_trip_rows <- matched_trip_rows + nrow(milk_chunk)
    
    if (nrow(milk_chunk) < before_trip_join) {
      message(
        "Warning: ", before_trip_join - nrow(milk_chunk),
        " milk purchase row(s) had no matching trip in chunk ", chunk_index, "."
      )
    }
    
    if (nrow(milk_chunk) == 0L) {
      rm(purchase_chunk, milk_chunk)
      next
    }
    
    # Convert numeric analysis fields only after the joins.
    milk_chunk[, quantity := suppressWarnings(as.numeric(quantity))]
    milk_chunk[, total_price_paid := suppressWarnings(as.numeric(total_price_paid))]
    milk_chunk[, coupon_value := suppressWarnings(as.numeric(coupon_value))]
    milk_chunk[, final_price_paid := fifelse(
      is.na(total_price_paid),
      NA_real_,
      total_price_paid - fifelse(is.na(coupon_value), 0, coupon_value)
    )]
    milk_chunk[, has_coupon := as.integer(!is.na(coupon_value) & coupon_value > 0)]
    milk_chunk[, is_deal := as.integer(!is.na(deal_flag_uc) & deal_flag_uc == "1")]
    
    milk_chunk <- add_milk_labels(milk_chunk)
    
    whole_rows <- whole_rows + sum(milk_chunk$whole_milk == 1L, na.rm = TRUE)
    low_fat_rows <- low_fat_rows + sum(milk_chunk$low_fat_milk == 1L, na.rm = TRUE)
    fully_skimmed_rows <- fully_skimmed_rows +
      sum(milk_chunk$fully_skimmed_milk == 1L, na.rm = TRUE)
    
    # Track unique matched UPC-version pairs for a year-level diagnostic.
    pair_chunk <- unique(milk_chunk[, .(upc, upc_ver_uc)])
    unique_pair_parts[[length(unique_pair_parts) + 1L]] <- pair_chunk
    
    # Compact diagnostics are accumulated by chunk, not by retaining purchase rows.
    type_audit_parts[[length(type_audit_parts) + 1L]] <- milk_chunk[
      ,
      .N,
      by = .(milk_type_descr, whole_milk, low_fat_milk, fully_skimmed_milk)
    ]
    
    module_audit_parts[[length(module_audit_parts) + 1L]] <- milk_chunk[
      ,
      .N,
      by = .(product_module_code, product_module_descr, whole_milk, low_fat_milk, fully_skimmed_milk)
    ]
    
    # Keep a fixed output schema and write each chunk immediately.
    # milk_chunk is no longer needed in its original shape; reuse it to avoid a full copy.
    output_chunk <- milk_chunk
    output_chunk[, product_module_code := as.character(product_module_code)]
    output_chunk[, household_code := as.character(household_code)]
    output_chunk[, trip_code_uc := as.character(trip_code_uc)]
    output_chunk[, panel_year := as.character(panel_year)]
    output_chunk[, upc := as.character(upc)]
    output_chunk[, upc_ver_uc := as.character(upc_ver_uc)]
    
    for (col in setdiff(OUTPUT_COLUMNS, names(output_chunk))) {
      set(output_chunk, j = col, value = NA)
    }
    output_chunk <- output_chunk[, ..OUTPUT_COLUMNS]
    
    fwrite(
      output_chunk,
      file = output_tmp,
      sep = "\t",
      append = output_has_rows,
      col.names = !output_has_rows,
      na = ""
    )
    
    output_has_rows <- TRUE
    
    message(
      "Chunk ", chunk_index,
      " | source rows read: ", format(source_rows, big.mark = ","),
      " | milk rows written: ", format(matched_trip_rows, big.mark = ",")
    )
    
    rm(purchase_chunk, milk_chunk, output_chunk, pair_chunk)
    if (chunk_index %% 4L == 0L) invisible(gc())
  }
  
  # Guard against silently accepting a failed scan of a non-empty source.
  if (source_rows == 0L && file.info(purchases_file)$size > 0) {
    stop(
      "No purchase rows were read from a non-empty file. ",
      "The output will not be finalized. Check the TSV format and chunk reader."
    )
  }
  
  # Write a valid header-only file if no milk purchases were found.
  if (!file.exists(output_tmp)) {
    empty_output <- as.data.table(
      setNames(replicate(length(OUTPUT_COLUMNS), character(), simplify = FALSE), OUTPUT_COLUMNS)
    )
    fwrite(empty_output, output_tmp, sep = "\t", na = "")
  }
  
  # Only replace the final output after all chunks have completed.
  if (file.exists(output_file)) unlink(output_file)
  if (!file.rename(output_tmp, output_file)) {
    stop("Could not move temporary output into place: ", output_file)
  }
  
  message("\nYear ", year, " extraction summary")
  message("Source purchase rows scanned: ", format(source_rows, big.mark = ","))
  message("Rows matched to milk product dictionary: ", format(matched_product_rows, big.mark = ","))
  message("Rows after products_extra join: ", format(matched_extra_rows, big.mark = ","))
  message("Rows after trips join / written: ", format(matched_trip_rows, big.mark = ","))
  message("Rows without milk_type_descr: ", format(missing_type_rows, big.mark = ","))
  unique_purchase_pair_count <- if (length(unique_pair_parts)) {
    nrow(unique(rbindlist(unique_pair_parts, use.names = TRUE), by = c("upc", "upc_ver_uc")))
  } else 0L
  message("Unique UPC-version pairs in matched milk purchases: ", format(unique_purchase_pair_count, big.mark = ","))
  message("Unique UPC-version pairs in milk dictionary: ", format(nrow(milk_dict), big.mark = ","))
  message("whole_milk purchase rows: ", format(whole_rows, big.mark = ","))
  message("low_fat_milk purchase rows: ", format(low_fat_rows, big.mark = ","))
  message("fully_skimmed_milk purchase rows: ", format(fully_skimmed_rows, big.mark = ","))
  message("Output: ", output_file)
  
  # Summarize the small diagnostic accumulators after the data output is safe.
  tryCatch({
    if (length(type_audit_parts) > 0L) {
      type_audit <- rbindlist(type_audit_parts)[
        ,
        .(N = sum(N)),
        by = .(milk_type_descr, whole_milk, low_fat_milk, fully_skimmed_milk)
      ][order(-N)]
      safe_print(type_audit, "Top milk_type_descr values and labels:", max_rows = 30L)
      
      module_audit <- rbindlist(module_audit_parts)[
        ,
        .(N = sum(N)),
        by = .(product_module_code, product_module_descr, whole_milk, low_fat_milk, fully_skimmed_milk)
      ][order(product_module_code, -N)]
      safe_print(module_audit, "Module-by-label counts:", max_rows = 50L)
      
      type_audit_file <- file.path(
        OUTPUT_DIR,
        paste0("milk_type_audit_", year, ".tsv")
      )
      module_audit_file <- file.path(
        OUTPUT_DIR,
        paste0("milk_module_label_audit_", year, ".tsv")
      )
      fwrite(type_audit, type_audit_file, sep = "\t", na = "")
      fwrite(module_audit, module_audit_file, sep = "\t", na = "")
    }
  }, error = function(e) {
    message("Audit diagnostics could not be completed: ", conditionMessage(e))
    message("The main purchase output has already been saved.")
  })
  
  invisible(output_file)
}

# -----------------------------------------------------------------------------
# Batch runner
# -----------------------------------------------------------------------------

run_milk_batch <- function(years = RUN_YEARS) {
  if (length(years) == 0L) stop("RUN_YEARS is empty.")
  
  products_ref_path <- file.path(PANEL_ROOT, "Master_Files", "Latest", "products.tsv")
  if (!file.exists(products_ref_path)) {
    stop("Master product file missing: ", products_ref_path)
  }
  
  # Load and filter the shared master dictionary once for the entire batch.
  message("Loading shared milk product dictionary once...")
  milk_dict <- load_milk_dictionary(products_ref_path)
  message("Unique milk UPC-version pairs: ", format(nrow(milk_dict), big.mark = ","))
  
  for (year in years) {
    tryCatch(
      process_milk_year(as.integer(year), milk_dict),
      error = function(e) {
        message("ERROR for year ", year, ": ", conditionMessage(e))
        stop(e)
      }
    )
    invisible(gc())
  }
  
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Run
# -----------------------------------------------------------------------------

run_milk_batch(RUN_YEARS)
