# =============================================================================
# Step 04 | Extract Nielsen yogurt purchases — fast and audited
#
# Inputs:
#   HMS/Master_Files/Latest/products.tsv
#   HMS/YYYY/Annual_Files/{purchases,trips,products_extra}_YYYY.tsv
#
# Output (local private data, NOT the GitHub repository):
#   healthy_purchase_data/intermediate/yogurt_purchases/
#     yogurt_purchases_YYYY.tsv
#     yogurt_extraction_audit_YYYY.tsv
#     yogurt_module_audit_YYYY.tsv
#     yogurt_fat_label_audit_YYYY.tsv
#
# Classification:
#   Historical yogurt modules: 1634, 1738, 3603, 3612.
#   Module codes are normalized, but UPC / UPC-version keys are NEVER modified.
#   An independent module-description check excludes non-yogurt products.
#
# Performance:
#   One shared product dictionary, yearly processing, data.table::fread()
#   selected columns, vectorized keyed joins, and data.table::fwrite().
#   products_extra reads only UPC, UPC-version, and type_descr for the year.
#   No parallel copies of large purchase tables are created.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# ---- Configuration ----------------------------------------------------------
RUN_YEARS <- 2009:2018L                # Verify 2008; later use 2007:2018.
RUN_OVERWRITE <- TRUE             # Replace the incorrect earlier 2008 extract.
RUN_THREADS <- 4L                 # Keep <= 5 threads on a 32 GB machine.
STRICT_YOGURT_DESCRIPTION <- TRUE # Require yogurt wording in module description.
YOGURT_MODULES <- c("1634", "1738", "3603", "3612")

# fast = fread selected purchase columns into RAM for one year (recommended).
# streaming = use readr callback with bounded memory, but is slower.
READ_MODE <- "fast"
CHUNK_ROWS <- 250000L

HMS_ROOT <- Sys.getenv("HPC_HMS_ROOT", unset =
                         "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS")
DATA_ROOT <- Sys.getenv("HPC_DATA_ROOT", unset =
                          "C:/Users/rwen13/Documents/healthy_purchase_data")
OUTPUT_DIR <- file.path(DATA_ROOT, "intermediate", "yogurt_purchases")
MASTER_FILE <- file.path(HMS_ROOT, "Master_Files", "Latest", "products.tsv")
setDTthreads(RUN_THREADS)

# ---- General helpers --------------------------------------------------------
normalize_module <- function(x) {
  # Only module codes undergo numeric-like normalization; never call on UPCs.
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  numeric <- !is.na(x) & grepl("^[0-9]+$", x)
  x[numeric] <- sub("^0+(?=[0-9])", "", x[numeric], perl = TRUE)
  x
}

require_columns <- function(path, cols) {
  if (!file.exists(path)) stop("Required input file does not exist: ", path)
  header <- names(fread(path, nrows = 0L, sep = "\t", quote = "", showProgress = FALSE))
  missing <- setdiff(cols, header)
  if (length(missing)) {
    stop("Missing columns in ", path, ": ", paste(missing, collapse = ", "))
  }
  header
}

write_atomic <- function(dt, path) {
  # Write to a temporary file before committing the complete result.
  tmp <- paste0(path, ".partial")
  if (file.exists(tmp)) unlink(tmp)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  fwrite(dt, file = tmp, sep = "\t", na = "", quote = TRUE)
  if (file.exists(path)) {
    if (!RUN_OVERWRITE) stop("Output exists: ", path)
    unlink(path)
  }
  if (!file.rename(tmp, path)) stop("Failed to finalize output: ", path)
  invisible(path)
}

# ---- Shared master product dictionary --------------------------------------
load_yogurt_dictionary <- function() {
  needed <- c("upc", "upc_ver_uc", "product_module_code", "product_module_descr")
  optional <- c("brand_code_uc", "brand_descr", "size1_code_uc", "size1_amount",
                "size1_units", "size1_change_flag_uc")
  header <- require_columns(MASTER_FILE, needed)
  selected <- intersect(c(needed, optional), header)
  
  message("Reading master product dictionary (selected columns only)...")
  # Important: some brand descriptions contain embedded unbalanced quotes,
  # e.g. '"MEGA" GIANTS'. They are literal text, not TSV quoting syntax.
  # quote="" avoids fread's improper-quoting recovery and warning.
  master <- fread(MASTER_FILE, sep = "\t", quote = "", select = selected,
                  colClasses = list(character = intersect(
                    c("upc", "upc_ver_uc", "product_module_code", "product_module_descr"), selected)),
                  showProgress = TRUE)
  for (nm in setdiff(optional, names(master))) set(master, j = nm, value = NA_character_)
  master[, module_norm := normalize_module(product_module_code)]
  
  # Only the four historical module codes enter candidate classification.
  candidates <- master[module_norm %in% YOGURT_MODULES]
  if (!nrow(candidates)) stop("No products in specified yogurt modules.")
  
  # Surface actual Nielsen module descriptions before any purchase matching.
  module_summary <- candidates[, .(n_products = .N, n_upc = uniqueN(upc)),
                               by = .(module_norm, product_module_descr)]
  setorder(module_summary, module_norm, product_module_descr)
  message("\nCandidate yogurt modules from the master file:")
  print(module_summary)
  
  # Fail closed on inconsistent module labels rather than admitting nuts, etc.
  desc <- toupper(trimws(as.character(candidates$product_module_descr)))
  described_as_yogurt <- !is.na(desc) & grepl("YOGURT|YOGHURT|YOGHOURT", desc)
  excluded <- candidates[!described_as_yogurt, .N,
                         by = .(module_norm, product_module_descr)]
  if (nrow(excluded)) {
    message("\nModule-code matches NOT described as yogurt:")
    print(excluded)
    if (STRICT_YOGURT_DESCRIPTION) {
      message("Excluding ", sum(excluded$N), " candidate product rows not described as yogurt.")
      candidates <- candidates[described_as_yogurt]
    } else {
      warning("STRICT_YOGURT_DESCRIPTION is FALSE: non-yogurt descriptions could enter the output.")
    }
  }
  if (!nrow(candidates)) stop("No yogurt products left after description verification. Review historical module codes.")
  
  # The same UPC-version pair must never refer to multiple modules in master.
  selected_pairs <- unique(candidates[, .(upc, upc_ver_uc)])
  conflicts <- master[selected_pairs, on = .(upc, upc_ver_uc), nomatch = 0L][
    , .(n_modules = uniqueN(module_norm),
        modules = paste(unique(module_norm), collapse = ",")),
    by = .(upc, upc_ver_uc)][n_modules > 1L]
  if (nrow(conflicts)) {
    message("Example UPC-version conflicts:")
    print(head(conflicts, 15L))
    stop(nrow(conflicts), " yogurt UPC-version key(s) have conflicting product-module codes in master.")
  }
  rm(master, selected_pairs, conflicts); invisible(gc())
  
  if (anyNA(candidates$upc) || anyNA(candidates$upc_ver_uc) ||
      any(candidates$upc == "" | candidates$upc_ver_uc == "")) {
    stop("Blank UPC or UPC-version in yogurt dictionary.")
  }
  # Identical pairs within one module are deduplicated reproducibly.
  setorder(candidates, upc, upc_ver_uc, product_module_descr, brand_descr)
  candidates <- unique(candidates, by = c("upc", "upc_ver_uc"))
  setkey(candidates, upc, upc_ver_uc)
  message("Verified yogurt UPC-version dictionary size: ", nrow(candidates))
  candidates
}

# ---- Annual trips -----------------------------------------------------------
load_trips <- function(year) {
  path <- file.path(HMS_ROOT, year, "Annual_Files", sprintf("trips_%s.tsv", year))
  cols <- c("trip_code_uc", "household_code", "purchase_date", "panel_year")
  require_columns(path, cols)
  x <- fread(path, select = cols, sep = "\t", quote = "",
             colClasses = list(character = c("trip_code_uc", "household_code", "panel_year")),
             showProgress = FALSE)
  if (anyNA(x$trip_code_uc) || any(x$trip_code_uc == "") ||
      anyDuplicated(x, by = "trip_code_uc")) {
    stop("Invalid/duplicate trip_code_uc in ", path)
  }
  setkey(x, trip_code_uc)
  x
}

PURCHASE_COLS <- c("upc", "upc_ver_uc", "trip_code_uc", "quantity",
                   "total_price_paid", "coupon_value", "deal_flag_uc")
OUTPUT_COLS <- c("trip_code_uc", "household_code", "purchase_date", "panel_year",
                 "upc", "upc_ver_uc", "product_module_code", "product_module_descr",
                 "brand_code_uc", "brand_descr", "quantity", "size1_code_uc",
                 "size1_amount", "size1_units", "size1_change_flag_uc",
                 "total_price_paid", "coupon_value", "final_price_paid",
                 "deal_flag_uc", "has_coupon", "is_deal",
                 "yogurt_type_descr", "fat_free_yogurt", "low_fat_yogurt")

# ---- Annual fat-type lookup -----------------------------------------------
load_yogurt_type_lookup <- function(year, dictionary) {
  path <- file.path(HMS_ROOT, year, "Annual_Files",
                    sprintf("products_extra_%s.tsv", year))
  cols <- c("upc", "upc_ver_uc", "type_descr")
  require_columns(path, cols)
  message("Loading annual products_extra: 3 columns only (", year, ")...")
  
  # Read a narrow three-column table; all join keys remain character.
  extra <- fread(path, sep = "\t", quote = "", select = cols,
                 colClasses = list(character = cols), showProgress = FALSE)
  # Reduce immediately to yogurt UPC-version pairs; do not retain the complete
  # products_extra table while scanning the annual purchase file.
  extra <- extra[dictionary[, .(upc, upc_ver_uc)],
                 on = .(upc, upc_ver_uc), nomatch = 0L]
  if (!nrow(extra)) {
    warning("No yogurt products match products_extra for ", year)
    return(data.table(upc = character(), upc_ver_uc = character(),
                      yogurt_type_descr = character()))
  }
  extra[, type_descr := trimws(type_descr)]
  extra[type_descr == "", type_descr := NA_character_]
  
  # Annual extra files can contain repeated records. Conflicting nonblank
  # descriptors must not be silently assigned to the same UPC-version pair.
  conflicts <- extra[!is.na(type_descr),
                     .(n_types = uniqueN(toupper(type_descr))),
                     by = .(upc, upc_ver_uc)][n_types > 1L]
  if (nrow(conflicts)) {
    stop("Conflicting type_descr for ", nrow(conflicts),
         " yogurt UPC-version pairs in products_extra_", year,
         ". Please audit these pairs before assigning fat labels.")
  }
  extra[, has_type := as.integer(!is.na(type_descr))]
  setorder(extra, upc, upc_ver_uc, -has_type)
  extra <- unique(extra, by = c("upc", "upc_ver_uc"))
  extra[, has_type := NULL]
  setnames(extra, "type_descr", "yogurt_type_descr")
  setkey(extra, upc, upc_ver_uc)
  message("Annual matched yogurt descriptors: ", nrow(extra), " UPC-version pairs")
  extra
}

add_yogurt_fat_labels <- function(x) {
  # Labels are mutually exclusive. LIGHT, NATURAL, and LACTOSE FREE alone
  # do not convey fat content and therefore receive two zeros.
  d <- toupper(trimws(fcoalesce(as.character(x$yogurt_type_descr), "")))
  fat_free <- grepl("NON[ -]?FAT|FAT[ -]?FREE", d, perl = TRUE)
  low_fat <- grepl("LOW[ -]?FAT", d, perl = TRUE) & !fat_free
  x[, fat_free_yogurt := as.integer(fat_free)]
  x[, low_fat_yogurt := as.integer(low_fat)]
  x
}

# ---- Single-year purchase extraction ---------------------------------------
process_year <- function(year, dictionary) {
  purchase_file <- file.path(HMS_ROOT, year, "Annual_Files",
                             sprintf("purchases_%s.tsv", year))
  require_columns(purchase_file, PURCHASE_COLS)
  output <- file.path(OUTPUT_DIR, sprintf("yogurt_purchases_%s.tsv", year))
  audit_path <- file.path(OUTPUT_DIR, sprintf("yogurt_extraction_audit_%s.tsv", year))
  module_audit_path <- file.path(OUTPUT_DIR, sprintf("yogurt_module_audit_%s.tsv", year))
  label_audit_path <- file.path(OUTPUT_DIR, sprintf("yogurt_fat_label_audit_%s.tsv", year))
  if (!RUN_OVERWRITE && any(file.exists(c(output, audit_path, module_audit_path,
                                          label_audit_path)))) {
    stop("Existing output for ", year, ". Set RUN_OVERWRITE <- TRUE to replace it.")
  }
  
  message("\nProcessing yogurt purchases: ", year, " [", READ_MODE, "]")
  trips <- load_trips(year)
  yogurt_types <- load_yogurt_type_lookup(year, dictionary)
  
  transform_chunk <- function(p) {
    # Both joins have unique right-hand keys; output cannot multiply purchase rows.
    if (!nrow(p)) return(data.table())
    x <- dictionary[p, on = .(upc, upc_ver_uc), nomatch = 0L]
    if (!nrow(x)) return(data.table())
    matched <- nrow(x)
    x <- trips[x, on = "trip_code_uc", nomatch = 0L]
    lost <- matched - nrow(x)
    if (!nrow(x)) return(list(rows = data.table(), matched = matched, lost = lost))
    
    x[, total_price_paid := suppressWarnings(as.numeric(total_price_paid))]
    x[, coupon_value := suppressWarnings(as.numeric(coupon_value))]
    x[, quantity := suppressWarnings(as.numeric(quantity))]
    x[, final_price_paid := total_price_paid - fcoalesce(coupon_value, 0)]
    x[, has_coupon := as.integer(!is.na(coupon_value) & coupon_value > 0)]
    x[, is_deal := as.integer(!is.na(deal_flag_uc) & as.character(deal_flag_uc) == "1")]
    
    # Preserve every yogurt purchase (left lookup). Missing annual descriptor
    # means both labels remain zero; it never drops a purchase record.
    x <- yogurt_types[x, on = .(upc, upc_ver_uc)]
    x <- add_yogurt_fat_labels(x)
    # Independent invariant: exported rows MUST be in yogurt modules.
    if (any(!x$module_norm %in% YOGURT_MODULES)) stop("Non-yogurt module detected after join.")
    if (STRICT_YOGURT_DESCRIPTION && any(!grepl("YOGURT|YOGHURT|YOGHOURT",
                                                toupper(x$product_module_descr)))) {
      stop("Non-yogurt description detected after join.")
    }
    list(rows = x[, ..OUTPUT_COLS], matched = matched, lost = lost)
  }
  
  purchase_rows <- 0L
  yogurt_rows <- 0L
  unmatched_trips <- 0L
  module_parts <- list()
  label_parts <- list()
  temp <- paste0(output, ".partial")
  if (file.exists(temp)) unlink(temp)
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  
  consume <- function(p) {
    purchase_rows <<- purchase_rows + nrow(p)
    a <- transform_chunk(p)
    if (!length(a) || is.null(a$rows)) return(invisible(NULL))
    unmatched_trips <<- unmatched_trips + a$lost
    if (!nrow(a$rows)) return(invisible(NULL))
    yogurt_rows <<- yogurt_rows + nrow(a$rows)
    part <- a$rows[, .(N = .N, spend = sum(final_price_paid, na.rm = TRUE)),
                   by = .(product_module_code, product_module_descr)]
    module_parts[[length(module_parts) + 1L]] <<- part
    label_parts[[length(label_parts) + 1L]] <<- a$rows[
      , .(N = .N), by = .(yogurt_type_descr, fat_free_yogurt, low_fat_yogurt)]
    fwrite(a$rows, file = temp, sep = "\t", append = file.exists(temp),
           col.names = !file.exists(temp), na = "")
    invisible(NULL)
  }
  
  if (READ_MODE == "fast") {
    # Single fread scan of seven columns is substantially faster than parsing
    # every column of the annual file through a readr callback. One year only.
    p <- fread(purchase_file, select = PURCHASE_COLS, sep = "\t", quote = "",
               colClasses = list(character = c("upc", "upc_ver_uc", "trip_code_uc")),
               showProgress = TRUE)
    consume(p)
    rm(p); invisible(gc())
  } else if (READ_MODE == "streaming") {
    if (!requireNamespace("readr", quietly = TRUE)) stop("Install readr for streaming mode.")
    callback <- readr::SideEffectChunkCallback$new(function(chunk, pos) {
      p <- as.data.table(chunk)[, ..PURCHASE_COLS]
      consume(p)
      invisible(NULL)
    })
    readr::read_tsv_chunked(purchase_file, callback = callback, chunk_size = CHUNK_ROWS,
                            col_types = readr::cols(.default = readr::col_character()),
                            progress = FALSE)
  } else stop("READ_MODE must be 'fast' or 'streaming'.")
  
  if (!file.exists(temp)) {
    blank <- as.data.table(setNames(rep(list(character()), length(OUTPUT_COLS)), OUTPUT_COLS))
    fwrite(blank, temp, sep = "\t", na = "")
  }
  # The main output is finalized only after every input row has been processed.
  if (file.exists(output)) unlink(output)
  if (!file.rename(temp, output)) stop("Could not finalize ", output)
  
  audit <- data.table(source_year = year, purchase_rows_read = purchase_rows,
                      yogurt_rows_saved = yogurt_rows,
                      yogurt_rows_without_trip = unmatched_trips,
                      yogurt_dictionary_upc_version = nrow(dictionary))
  module_audit <- if (length(module_parts)) rbindlist(module_parts)[
    , .(N = sum(N), spending = sum(spend)),
    by = .(product_module_code, product_module_descr)][order(product_module_code)]
  else data.table(product_module_code = character(), product_module_descr = character(),
                  N = integer(), spending = numeric())
  write_atomic(audit, audit_path)
  write_atomic(module_audit, module_audit_path)
  label_audit <- if (length(label_parts)) rbindlist(label_parts)[
    , .(N = sum(N)),
    by = .(yogurt_type_descr, fat_free_yogurt, low_fat_yogurt)][order(-N)]
  else data.table(yogurt_type_descr = character(), fat_free_yogurt = integer(),
                  low_fat_yogurt = integer(), N = integer())
  write_atomic(label_audit, label_audit_path)
  message("Year ", year, " completed: ", yogurt_rows, " yogurt purchase rows; ",
          unmatched_trips, " missing trip matches.")
  message("Source module audit:")
  print(module_audit)
  message("Top yogurt fat-type descriptions:")
  print(head(label_audit, 25L))
  invisible(output)
}

run_yogurt_batch <- function(years = RUN_YEARS) {
  if (!length(years) || anyNA(years) || anyDuplicated(years)) stop("Invalid RUN_YEARS.")
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dictionary <- load_yogurt_dictionary()
  for (year in as.integer(years)) {
    process_year(year, dictionary)
    invisible(gc())
  }
  invisible(NULL)
}

if (sys.nframe() == 0L) run_yogurt_batch()
