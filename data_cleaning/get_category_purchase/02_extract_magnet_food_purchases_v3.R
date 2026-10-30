# Step 02: Memory-efficient Magnet / Reference Card food extraction.
# Read-only source: NIQ annual purchases/trips and master products.
# Restricted TSVs and all audit files are written OUTSIDE the Git repository.
# Preserve the original reference-card brand rules and modules 453 / 460 patch.
# Required R packages: data.table, readr, dplyr, tibble, stringr, duckdb, DBI.

RUN_YEARS <- 2007L           # Examples: 2008L, 2008:2017, c(2008, 2011, 2017)
RUN_WORKERS <- 1L           # Intentionally serial: limit peak memory use.
RUN_OVERWRITE <- FALSE      # TRUE overwrites outputs for specified years.
CHUNK_ROWS <- 25000L       # Reduce to 25000L if memory remains tight.

RAW_ROOT <- Sys.getenv("HPC_RAW_ROOT", unset = "C:/Users/rwen13/Documents/2004-2020_Panel_Data")
DATA_ROOT <- Sys.getenv("HPC_DATA_ROOT", unset = "C:/Users/rwen13/Documents/healthy_purchase_data")
OUTPUT_DIR <- file.path(DATA_ROOT, "intermediate", "magnet_food_purchases")

for (pkg in c("data.table", "readr", "dplyr", "tibble", "stringr", "duckdb", "DBI")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Please install R package: ", pkg)
}

magnet_brand_map <- tibble::tribble(
  ~brand_descr,                          ~magnet_food_class,        ~is_magnet_food, ~is_magnet_fresh_fv,
  
  # Fresh FV identified by brand description
  "REFERENCE CARD FRUITS",               "fresh_fv_fruits",          1L,              1L,
  "REFERENCE CARD VEGETABLES",           "fresh_fv_vegetables",      1L,              1L,
  
  # Other edible food identified by brand description
  "REFERENCE CARD COFFEE",               "coffee",                  1L,              0L,
  "REFERENCE CARD BAKED GOODS",          "baked_goods",             1L,              0L,
  "REFERENCE CD MEAT/POULTRY/FISH",      "meat_poultry_fish",       1L,              0L,
  "REFERENCE CD CANDY/NUTS/SEEDS",       "candy_nuts_seeds",        1L,              0L,
  "REFERENCE CARD PREPARED FOODS",       "prepared_foods",          1L,              0L,
  "REFERENCE CARD CHEESE",               "cheese",                  1L,              0L,
  "REFERENCE CARD TAKE OUT",             "take_out",                1L,              0L,
  "REFERENCE CARD COLD CUTS",            "cold_cuts",               1L,              0L
)

nonfood_magnet_brands <- c(
  "REFERENCE CARD GAS",
  "REFERENCE CARD FILM",
  "REFERENCE CARD RX",
  "REFERENCE CARD FLORAL",
  "REFERENCE CARD APPAREL",
  "RC APPAREL"
)

# Product-module rule for reference-card fresh FV.
# These module codes should be captured even when brand_descr is incomplete.
reference_card_fv_module_codes <- c("453", "460")

reference_card_fv_module_descrs <- c(
  "REFERENCE CARD FRUITS",
  "REFERENCE CARD VEGETABLES"
)


# Normalize descriptions and module codes. UPC identifiers are NEVER numeric.
clean_text <- function(x) stringr::str_squish(stringr::str_to_upper(as.character(x)))
clean_code <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[!is.na(x) & x %in% c("", "NA")] <- NA_character_
  sub("\\.0+$", "", x)
}

# Normalize numeric-like MODULE codes only; never normalize UPC or UPC version.
# Examples: 0453, 00453, 453.0 -> 453; 0460 -> 460.
normalize_numeric_code <- function(x) {
  x <- clean_code(x)
  numeric_like <- !is.na(x) & grepl("^[0-9]+$", x)
  x[numeric_like] <- sub("^0+(?=[0-9])", "", x[numeric_like], perl = TRUE)
  x
}

# Fail quickly if the historical leading-zero patch is lost.
validate_module_normalization <- function() {
  input <- c("453", "0453", "00453", "453.0", "460", "0460", "00460", "460.0", "000", "ABC")
  expected <- c("453", "453", "453", "453", "460", "460", "460", "460", "0", "ABC")
  stopifnot(identical(normalize_numeric_code(input), expected))
  invisible(TRUE)
}

required_columns <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
}

# Read selected source columns only; missing optional columns become NA.
read_selected <- function(path, required, optional = character()) {
  header <- names(data.table::fread(path, nrows = 0L, showProgress = FALSE))
  missing <- setdiff(required, header)
  if (length(missing)) stop(path, " lacks: ", paste(missing, collapse = ", "))
  present <- intersect(c(required, optional), header)
  key_cols <- intersect(c("upc", "upc_ver_uc", "trip_code_uc", "household_code",
                          "panel_year", "product_module_code", "product_group_code",
                          "department_code", "brand_code_uc"), present)
  result <- data.table::fread(path, select = present,
                              colClasses = if (length(key_cols)) list(character = key_cols) else NULL,
                              showProgress = FALSE)
  for (cc in setdiff(optional, names(result))) result[, (cc) := NA]
  result
}

product_columns <- c("upc_descr", "brand_descr", "product_module_code", "product_module_descr",
                     "product_group_code", "product_group_descr", "department_code", "department_descr",
                     "brand_code_uc", "multi", "size1_code_uc", "size1_amount", "size1_units",
                     "size1_change_flag_uc")

# Apply exactly the legacy union of brand-based rules and reference-card FV module rules.
build_magnet_dictionary <- function(products) {
  product <- data.table::copy(products)
  product[, brand_clean := clean_text(brand_descr)]
  product[, module_clean := normalize_numeric_code(product_module_code)]
  product[, module_descr_clean := clean_text(product_module_descr)]
  product[, product_module_code_norm := module_clean]
  product[, product_group_code_norm := normalize_numeric_code(product_group_code)]
  product[, department_code_norm := normalize_numeric_code(department_code)]
  # Early filtering avoids merging the full product master into the brand rules.
  product <- product[brand_clean %in% clean_text(magnet_brand_map$brand_descr) |
    module_clean %in% reference_card_fv_module_codes |
    module_descr_clean %in% reference_card_fv_module_descrs]
  if (!nrow(product)) stop("No Magnet products found in master reference")
  rule <- data.table::as.data.table(magnet_brand_map)
  rule[, brand_clean := clean_text(brand_descr)]
  rule <- rule[, .(brand_clean, magnet_food_class_brand = magnet_food_class,
                   is_magnet_food_brand = is_magnet_food,
                   is_magnet_fresh_fv_brand = is_magnet_fresh_fv)]
  if (anyDuplicated(rule$brand_clean)) stop("Duplicate brand-based mapping rules")
  product <- merge(product, rule, by = "brand_clean", all.x = TRUE, sort = FALSE)
  product[, is_reference_card_fv_module := as.integer(
    module_clean %in% reference_card_fv_module_codes |
      module_descr_clean %in% reference_card_fv_module_descrs)]
  product[, magnet_food_class_module := data.table::fcase(
    module_clean == "453" | module_descr_clean == "REFERENCE CARD FRUITS", "fresh_fv_fruits",
    module_clean == "460" | module_descr_clean == "REFERENCE CARD VEGETABLES", "fresh_fv_vegetables",
    default = NA_character_)]
  product[, magnet_match_source := data.table::fcase(
    is_reference_card_fv_module == 1L & !is.na(magnet_food_class_brand), "brand_and_module",
    is_reference_card_fv_module == 1L & is.na(magnet_food_class_brand), "module_453_460",
    is_reference_card_fv_module == 0L & !is.na(magnet_food_class_brand), "brand_descr",
    default = NA_character_)]
  product[, magnet_food_class := data.table::fifelse(
    !is.na(magnet_food_class_module), magnet_food_class_module, magnet_food_class_brand)]
  product[, is_magnet_food := as.integer(!is.na(magnet_food_class_brand) |
                                           is_reference_card_fv_module == 1L)]
  product[, is_magnet_fresh_fv := as.integer(
    (!is.na(is_magnet_fresh_fv_brand) & is_magnet_fresh_fv_brand == 1L) |
      is_reference_card_fv_module == 1L)]
  product <- product[is_magnet_food == 1L]
  # Unlike distinct(..., .keep_all=TRUE), conflicting duplicate UPC versions are explicit errors.
  key <- c("upc", "upc_ver_uc")
  if (anyNA(product[, ..key])) stop("Missing UPC-version join keys in the Magnet dictionary")
  if (anyDuplicated(product, by = key)) {
    stop("Duplicate (upc, upc_ver_uc) in Magnet dictionary. Resolve the master-data ambiguity before joining.")
  }
  keep <- c(key, product_columns, "product_module_code_norm",
            "product_group_code_norm", "department_code_norm", "magnet_food_class", "magnet_match_source",
            "is_reference_card_fv_module", "is_magnet_food", "is_magnet_fresh_fv")
  product[, ..keep]
}

# Purchase columns are read as strings in each chunk, preventing UPC precision loss.
# Numeric conversions are performed only on amount/quantity fields AFTER filtering.
output_columns <- c("household_code", "trip_code_uc", "panel_year", "purchase_date",
  "upc", "upc_ver_uc", "upc_descr", "product_module_code", "product_module_code_norm", "product_module_descr",
  "product_group_code", "product_group_code_norm", "product_group_descr", "department_code", "department_code_norm", "department_descr",
  "brand_code_uc", "brand_descr", "magnet_food_class", "magnet_match_source",
  "is_reference_card_fv_module", "is_magnet_food", "is_magnet_fresh_fv", "quantity",
  "size1_code_uc", "size1_amount", "size1_units", "size1_change_flag_uc",
  "total_price_paid", "coupon_value", "final_price_paid", "final_price_paid_capped",
  "magnet_food_spend_increment", "magnet_food_spend_increment_capped",
  "magnet_fresh_fv_spend_increment", "magnet_fresh_fv_spend_increment_capped",
  "deal_flag_uc", "has_coupon", "is_deal")

# Use DuckDB on-disk scan for audits to avoid loading the entire output into RAM.
# The file is read as varchar to keep identifiers unmodified in audit operations.
write_purchase_audits <- function(purchase_path, output_dir, year) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, "SET memory_limit = '1GB'")
  DBI::dbExecute(con, paste0("SET temp_directory = ", as.character(DBI::dbQuoteString(con,
    normalizePath(file.path(DATA_ROOT, "temp"), winslash = "/", mustWork = TRUE)))))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  quoted_path <- as.character(DBI::dbQuoteString(con, normalizePath(purchase_path, winslash = "/")))
  DBI::dbExecute(con, paste0("CREATE VIEW purchases AS SELECT * FROM read_csv(", quoted_path,
    ", delim='\t', header=true, all_varchar=true)"))
  group_fields <- c("panel_year", "magnet_match_source", "product_module_code_norm",
    "product_module_descr", "product_group_code_norm", "product_group_descr",
    "department_code_norm", "department_descr", "magnet_food_class", "is_magnet_fresh_fv")
  grouped <- paste(group_fields, collapse = ", ")
  query <- paste0("SELECT ", grouped, ", COUNT(*) AS rows, ",
    "COUNT(DISTINCT household_code) AS households, COUNT(DISTINCT trip_code_uc) AS trips, ",
    "SUM(TRY_CAST(magnet_food_spend_increment AS DOUBLE)) AS spend_raw, ",
    "SUM(TRY_CAST(magnet_food_spend_increment_capped AS DOUBLE)) AS spend_capped, ",
    "SUM(TRY_CAST(magnet_fresh_fv_spend_increment AS DOUBLE)) AS fresh_fv_spend_raw, ",
    "SUM(TRY_CAST(magnet_fresh_fv_spend_increment_capped AS DOUBLE)) AS fresh_fv_spend_capped ",
    "FROM purchases GROUP BY ", grouped)
  data.table::fwrite(DBI::dbGetQuery(con, query), file.path(output_dir,
    sprintf("magnet_food_purchase_audit_%d.tsv", year)), sep = "\t")
  specific <- paste0("SELECT panel_year, product_module_code_norm, product_module_descr, ",
    "product_group_code_norm, product_group_descr, department_code_norm, department_descr, magnet_match_source, ",
    "COUNT(*) AS rows, COUNT(DISTINCT household_code) AS households, ",
    "COUNT(DISTINCT trip_code_uc) AS trips, ",
    "SUM(TRY_CAST(magnet_fresh_fv_spend_increment AS DOUBLE)) AS spend_raw, ",
    "SUM(TRY_CAST(magnet_fresh_fv_spend_increment_capped AS DOUBLE)) AS spend_capped ",
    "FROM purchases WHERE product_module_code_norm IN ('453','460') GROUP BY ALL")
  data.table::fwrite(DBI::dbGetQuery(con, specific), file.path(output_dir,
    sprintf("reference_card_fv_purchase_audit_%d.tsv", year)), sep = "\t")
}

process_magnet_food_purchases <- function(year) {
  purchases_path <- file.path(RAW_ROOT, "nielsen_extracts", "HMS", year,
                              "Annual_Files", sprintf("purchases_%d.tsv", year))
  trips_path <- file.path(RAW_ROOT, "nielsen_extracts", "HMS", year,
                          "Annual_Files", sprintf("trips_%d.tsv", year))
  products_path <- file.path(RAW_ROOT, "nielsen_extracts", "HMS", "Master_Files",
                             "Latest", "products.tsv")
  for (path in c(purchases_path, trips_path, products_path)) {
    if (!file.exists(path)) stop("Missing required file: ", path)
  }
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(DATA_ROOT, "temp"), recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(OUTPUT_DIR, sprintf("magnet_food_purchases_%d.tsv", year))
  audit_names <- sprintf(c("magnet_nonfood_reference_card_audit_%d.tsv",
                           "reference_card_fv_module_audit_%d.tsv",
                           "magnet_food_dictionary_audit_%d.tsv",
                           "magnet_food_purchase_audit_%d.tsv",
                           "reference_card_fv_purchase_audit_%d.tsv"), year)
  all_outputs <- c(destination, file.path(OUTPUT_DIR, audit_names))
  if (!RUN_OVERWRITE && any(file.exists(all_outputs))) {
    stop("Outputs already exist for ", year, "; set RUN_OVERWRITE <- TRUE to replace them")
  }
  message("Preparing master dictionary for ", year)
  products <- read_selected(products_path, c("upc", "upc_ver_uc", "brand_descr",
                                              "product_module_code"), product_columns)
  product <- build_magnet_dictionary(products)
  message("Matched reference-card UPC-versions in master: ", nrow(product))
  data.table::setkeyv(product, c("upc", "upc_ver_uc"))
  nonfood <- products[clean_text(brand_descr) %in% clean_text(nonfood_magnet_brands),
                      .N, by = .(brand_descr, upc_descr)]
  data.table::fwrite(nonfood, file.path(OUTPUT_DIR, audit_names[1]), sep = "\t")
  fv_modules <- products[normalize_numeric_code(product_module_code) %in% reference_card_fv_module_codes |
                           clean_text(product_module_descr) %in% reference_card_fv_module_descrs,
                         .N, by = .(product_module_code, product_module_descr,
                                    product_group_code, product_group_descr,
                                    department_code, department_descr, brand_descr, upc_descr)]
  data.table::fwrite(fv_modules, file.path(OUTPUT_DIR, audit_names[2]), sep = "\t")
  dictionary_audit <- product[, .N, by = .(magnet_match_source, product_module_code,
    product_module_descr, product_group_code, product_group_descr, department_code,
    department_descr, magnet_food_class, is_magnet_fresh_fv)]
  data.table::fwrite(dictionary_audit, file.path(OUTPUT_DIR, audit_names[3]), sep = "\t")
  rm(products, nonfood, fv_modules, dictionary_audit)
  gc(verbose = FALSE)

  # The trips file is generally much smaller than purchases; read only four columns.
  trips <- read_selected(trips_path, c("trip_code_uc", "panel_year",
                                        "purchase_date", "household_code"))
  if (anyNA(trips$trip_code_uc) || anyDuplicated(trips, by = "trip_code_uc")) {
    stop("Trips have missing or duplicate trip_code_uc values for year ", year)
  }
  data.table::setkey(trips, trip_code_uc)
  # read_tsv_chunked invokes a callback with at most CHUNK_ROWS purchase records.
  # Do not convert identifiers to numeric; every source field is initially character.
  purchase_names <- names(data.table::fread(purchases_path, nrows = 0L, showProgress = FALSE))
  needed <- c("upc", "upc_ver_uc", "trip_code_uc", "quantity",
              "total_price_paid", "coupon_value", "deal_flag_uc")
  required_columns(as.list(stats::setNames(vector("list", length(purchase_names)), purchase_names)),
                   needed, "Purchases")
  cols <- readr::cols(.default = readr::col_character())
  temp_path <- paste0(destination, ".incomplete")
  if (file.exists(temp_path)) unlink(temp_path)
  on.exit(if (file.exists(temp_path)) unlink(temp_path), add = TRUE)
  first <- TRUE
  count <- 0L
  callback <- readr::SideEffectChunkCallback$new(function(x, pos) {
    part <- data.table::as.data.table(x)
    part <- part[, ..needed]
    matched <- product[part, on = .(upc, upc_ver_uc), nomatch = NULL]
    if (!nrow(matched)) return(invisible(NULL))
    before <- nrow(matched)
    matched <- trips[matched, on = "trip_code_uc", nomatch = NULL]
    if (nrow(matched) > before) stop("Trip join unexpectedly expanded purchase rows")
    if (!nrow(matched)) return(invisible(NULL))
    for (name in c("quantity", "total_price_paid", "coupon_value")) {
      data.table::set(matched, j = name, value = suppressWarnings(as.numeric(matched[[name]])))
    }
    matched[, final_price_paid := total_price_paid - data.table::fcoalesce(coupon_value, 0)]
    matched[, final_price_paid_capped := pmax(final_price_paid, 0)]
    matched[, has_coupon := as.integer(!is.na(coupon_value) & coupon_value > 0)]
    matched[, is_deal := as.integer(!is.na(deal_flag_uc) & deal_flag_uc == "1")]
    matched[, magnet_food_spend_increment := data.table::fcoalesce(final_price_paid, 0)]
    matched[, magnet_food_spend_increment_capped := data.table::fcoalesce(final_price_paid_capped, 0)]
    matched[, magnet_fresh_fv_spend_increment := data.table::fifelse(
      is_magnet_fresh_fv == 1L, magnet_food_spend_increment, 0)]
    matched[, magnet_fresh_fv_spend_increment_capped := data.table::fifelse(
      is_magnet_fresh_fv == 1L, magnet_food_spend_increment_capped, 0)]
    absent <- setdiff(output_columns, names(matched))
    if (length(absent)) stop("Missing output columns after joins: ", paste(absent, collapse = ", "))
    matched <- matched[, ..output_columns]
    data.table::fwrite(matched, temp_path, sep = "\t", append = !first,
                       col.names = first, na = "")
    first <<- FALSE
    count <<- count + nrow(matched)
    invisible(NULL)
  })
  message("Streaming annual purchases in chunks of ", CHUNK_ROWS, " records")
  readr::read_tsv_chunked(purchases_path, callback = callback, chunk_size = CHUNK_ROWS,
                          col_types = cols, progress = FALSE)
  if (first) {
    empty <- as.data.frame(matrix(nrow = 0L, ncol = length(output_columns)))
    names(empty) <- output_columns
    data.table::fwrite(empty, temp_path, sep = "\t")
  }
  if (file.exists(destination)) unlink(destination)
  if (!file.rename(temp_path, destination)) stop("Could not finalize: ", destination)
  message("Matched purchase records in ", year, ": ", count)
  write_purchase_audits(destination, OUTPUT_DIR, year)
  message("Finished ", year, "; output: ", destination)
  invisible(destination)
}

run_batch_magnet_food <- function(years = RUN_YEARS, workers = RUN_WORKERS) {
  if (length(years) == 0L || anyNA(years) || any(years != as.integer(years)) ||
      any(years < 2004 | years > 2020)) stop("RUN_YEARS must be within 2004:2020")
  if (workers != 1L) stop("Low-memory version supports RUN_WORKERS <- 1L only")
  validate_module_normalization()
  for (year in unique(as.integer(years))) {
    process_magnet_food_purchases(year)
    gc(verbose = FALSE)
  }
  invisible(NULL)
}

if (sys.nframe() == 0L) run_batch_magnet_food()
