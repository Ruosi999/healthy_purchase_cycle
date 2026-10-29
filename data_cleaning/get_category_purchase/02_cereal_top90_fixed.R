# Build the non-control-brand cereal UPC-version identification master.
# Code: healthy_purchase_cycle/data_cleaning/get_category_purchase/
# Data: healthy_purchase_data/intermediate/cereal_classification/ (outside GitHub)
# Requires: data.table, openxlsx. Run: source("07_build_cereal_top90_upc_master.R")

if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table")
if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Install openxlsx")
library(data.table)

# ---- Configuration -------------------------------------------------------
HMS_ROOT <- Sys.getenv("HPC_HMS_ROOT", unset =
  "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS")
DATA_ROOT <- Sys.getenv("HPC_DATA_ROOT", unset =
  "C:/Users/rwen13/Documents/healthy_purchase_data")
INPUT_DIR <- file.path(DATA_ROOT, "intermediate", "cereal_purchases")
OUTPUT_DIR <- file.path(DATA_ROOT, "intermediate", "cereal_classification")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "cereal_noncontrol_top90_upc_master_2008_2017.xlsx")
SOURCE_YEARS <- 2007:2018  # Include boundary sources; date controls sample eligibility.
ANALYSIS_YEARS <- 2008:2017
COVERAGE_TARGET <- 0.90
THREADS <- 4L
# Private-label marker. Review Brand_Audit for any other private-label names.
CONTROL_BRAND_REGEX <- "^\\s*(CTL[ ._-]*BR(?:AND)?|CONTROL[ -]*BRAND)\\b"
# Complete core years are required by default; boundary years are reported if absent.
REQUIRE_CORE_YEARS <- TRUE

setDTthreads(THREADS)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Utilities -----------------------------------------------------------
read_narrow <- function(path, required, optional = character()) {
  if (!file.exists(path)) stop("Missing file: ", path)
  header <- names(fread(path, nrows = 0L, sep = "\t", quote = "", showProgress = FALSE))
  missing <- setdiff(required, header)
  if (length(missing)) stop("Missing columns in ", basename(path), ": ", paste(missing, collapse = ", "))
  cols <- unique(c(required, intersect(optional, header)))
  fread(path, sep = "\t", quote = "", select = cols,
        colClasses = list(character = intersect(cols,
          c("upc", "upc_ver_uc", "trip_code_uc", "household_code", "product_module_code"))),
        showProgress = FALSE)
}

purchase_year <- function(x) {
  # Source purchase_date is usually ISO; handle YYYYMMDD and MM/DD/YYYY as well.
  s <- trimws(as.character(x))
  y <- suppressWarnings(as.integer(substr(s, 1L, 4L)))
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)
  compact <- grepl("^[0-9]{8}$", s)
  american <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", s)
  valid <- iso | compact | american
  y[american] <- suppressWarnings(as.integer(sub("^.*/", "", s[american])))
  y[!valid] <- NA_integer_
  y
}

# ---- Load annual cereal rows and aggregate to UPC-version ----------------
source_paths <- file.path(INPUT_DIR, sprintf("cereal_purchases_%d.tsv", SOURCE_YEARS))
available <- file.exists(source_paths)
missing_years <- SOURCE_YEARS[!available]
if (length(missing_years)) {
  warning("Missing cereal source years: ", paste(missing_years, collapse = ", "))
  if (REQUIRE_CORE_YEARS && any(ANALYSIS_YEARS %in% missing_years)) {
    stop("Core source years missing. Complete extraction or set REQUIRE_CORE_YEARS <- FALSE for an explicitly partial analysis.")
  }
}

sales_by_source <- list()
identity_by_source <- list()
audit_rows <- list()
unknown_brand_rows <- list()
source_idx <- 0L

for (yr in SOURCE_YEARS[available]) {
  message("Reading cereal source year ", yr)
  path <- file.path(INPUT_DIR, sprintf("cereal_purchases_%d.tsv", yr))
  d <- read_narrow(path,
    required = c("upc", "upc_ver_uc", "purchase_date", "brand_descr"),
    optional = c("upc_descr", "product_module_code", "product_module_descr",
                 "size1_amount", "size1_units", "final_price_paid",
                 "total_price_paid", "coupon_value", "trip_code_uc"))
  input_n <- nrow(d)
  d[, analysis_year := purchase_year(purchase_date)]
  d <- d[analysis_year %in% ANALYSIS_YEARS]
  kept_n <- nrow(d)
  if (!kept_n) {
    audit_rows[[length(audit_rows) + 1L]] <- data.table(source_year = yr, input_rows = input_n,
      dated_2008_2017 = 0L, noncontrol_rows = 0L, control_rows = 0L,
      unknown_brand_rows = 0L, missing_price_rows = 0L, negative_net_rows = 0L)
    next
  }
  if (anyNA(d$upc) || anyNA(d$upc_ver_uc) || any(d$upc == "" | d$upc_ver_uc == "")) {
    stop("Missing UPC keys in source year ", yr)
  }
  d[, control_brand := grepl(CONTROL_BRAND_REGEX,
    toupper(trimws(fifelse(is.na(brand_descr), "", brand_descr))), perl = TRUE)]
  d[is.na(brand_descr) | trimws(brand_descr) == "", control_brand := NA]
  # Unknown brands cannot safely be treated as national brands. Exclude them
  # from both the ranking and denominator, and retain a separate audit.
  missing_brand_n <- d[is.na(control_brand), .N]
  if (missing_brand_n > 0L) {
    message("Source ", yr, ": excluding ", missing_brand_n,
            " purchase rows with missing brand descriptions (audited separately).")
    unknown_brand_rows[[length(unknown_brand_rows) + 1L]] <-
      d[is.na(control_brand), .(
        purchase_rows = .N,
        upc_descr = if ("upc_descr" %in% names(d))
          as.character(upc_descr[1L]) else NA_character_),
        by = .(upc, upc_ver_uc)][, source_year := yr]
  }
  control_n <- d[control_brand == TRUE, .N]
  d <- d[control_brand == FALSE & !is.na(control_brand)]
  noncontrol_n <- nrow(d)
  if (!"final_price_paid" %in% names(d)) {
    if (!"total_price_paid" %in% names(d)) stop("No monetary field in ", path)
    if (!"coupon_value" %in% names(d)) d[, coupon_value := 0]
    d[is.na(coupon_value), coupon_value := 0]
    d[, final_price_paid := as.numeric(total_price_paid) - as.numeric(coupon_value)]
  }
  d[, final_price_paid := suppressWarnings(as.numeric(final_price_paid))]
  missing_price <- d[is.na(final_price_paid), .N]
  negative_n <- d[!is.na(final_price_paid) & final_price_paid < 0, .N]
  # Sales metric: nonnegative coupon-adjusted spending (one counted amount per purchase row).
  d[, sales_dollars := pmax(final_price_paid, 0)]
  d[, source_year := as.integer(yr)]
  source_idx <- source_idx + 1L
  sales_by_source[[source_idx]] <- d[, .(
    sales_dollars = sum(sales_dollars, na.rm = TRUE),
    purchase_rows = .N, years_present = uniqueN(analysis_year)),
    by = .(upc, upc_ver_uc, source_year)]
  feature_cols <- intersect(c("upc", "upc_ver_uc", "brand_descr", "upc_descr",
    "product_module_code", "product_module_descr", "size1_amount", "size1_units"), names(d))
  identity_by_source[[source_idx]] <- unique(d[, ..feature_cols], by = c("upc", "upc_ver_uc"))[
    , source_year := yr]
  audit_rows[[length(audit_rows) + 1L]] <- data.table(
    source_year = yr, input_rows = input_n, dated_2008_2017 = kept_n,
    noncontrol_rows = noncontrol_n, control_rows = control_n,
    unknown_brand_rows = missing_brand_n, missing_price_rows = missing_price, negative_net_rows = negative_n)
  rm(d); gc(FALSE)
}
if (!length(sales_by_source)) stop("No qualifying non-control-brand cereal purchases")
source_audit <- rbindlist(audit_rows, fill = TRUE)
if (sum(source_audit$missing_price_rows) > 0L)
  stop("Missing prices in eligible non-control cereal purchases. Sales coverage would be biased; inspect Source_Audit before proceeding.")

source_sales <- rbindlist(sales_by_source)
# Preserve the highest-sales source year as the representative descriptor year.
setorder(source_sales, upc, upc_ver_uc, -sales_dollars, source_year)
representative_year <- source_sales[, .(representative_source_year = source_year[1L]),
  by = .(upc, upc_ver_uc)]
upc_sales <- source_sales[, .(sales_dollars = sum(sales_dollars),
  purchase_rows = sum(purchase_rows), source_year_count = .N),
  by = .(upc, upc_ver_uc)]
rm(sales_by_source, source_sales)

# ---- UPC-level market-share calculation ---------------------------------
# Denominator: ALL non-control-brand cereal sales dated 2008-2017.
# Not all cereals and not just the selected top-90% UPCs.
total_noncontrol_sales <- sum(upc_sales$sales_dollars)
if (!is.finite(total_noncontrol_sales) || total_noncontrol_sales <= 0)
  stop("Total non-control cereal sales is zero or invalid")
setorder(upc_sales, -sales_dollars, upc, upc_ver_uc)
upc_sales[, `:=`(
  rank = .I,
  UPC_Market_Share = sales_dollars / total_noncontrol_sales,
  cumulative_share = cumsum(sales_dollars) / total_noncontrol_sales)]
cutoff <- which(upc_sales$cumulative_share >= COVERAGE_TARGET)[1L]
top90 <- copy(upc_sales[seq_len(cutoff)])
stopifnot(nrow(top90) >= 1L, tail(top90$cumulative_share, 1L) >= COVERAGE_TARGET)
message(sprintf("Selected %s UPC versions; cumulative dollar share %.3f%% of non-control cereal sales",
  nrow(top90), 100 * tail(top90$cumulative_share, 1L)))

# ---- Attach master-file descriptions from the representative source year --
identities <- rbindlist(identity_by_source, fill = TRUE)
# Check whether one UPC is mapped to inconsistent descriptions in a source.
if (anyDuplicated(identities, by = c("upc", "upc_ver_uc", "source_year")))
  stop("Inconsistent cereal product dictionary for a UPC-version/source-year")
setnames(identities, "source_year", "representative_source_year")
selected <- merge(top90, representative_year,
                  by = c("upc", "upc_ver_uc"), all.x = TRUE, sort = FALSE)
selected <- merge(selected, identities,
                  by = c("upc", "upc_ver_uc", "representative_source_year"),
                  all.x = TRUE, sort = FALSE)
if (nrow(selected) != nrow(top90)) stop("Identity join multiplied UPC rows")
rm(identities, identity_by_source)

# ---- Add selected products_extra descriptors; no purchase-level extra join ----
extra_fields <- c("product_descr", "flavor_descr", "type_descr", "style_descr",
                  "variety_descr", "organic_claim_descr", "usda_organic_seal_descr")
extra_by_source <- list()
for (yr in sort(unique(selected$representative_source_year))) {
  path <- file.path(HMS_ROOT, as.character(yr), "Annual_Files",
                    sprintf("products_extra_%d.tsv", yr))
  if (!file.exists(path)) {
    warning("Missing products_extra for source year ", yr, "; related feature fields will be blank")
    next
  }
  x <- read_narrow(path, c("upc", "upc_ver_uc"), extra_fields)
  keys <- selected[representative_source_year == yr, .(upc, upc_ver_uc)]
  x <- x[keys, on = .(upc, upc_ver_uc), nomatch = 0L]
  if (anyDuplicated(x, by = c("upc", "upc_ver_uc"))) {
    x <- unique(x)
    if (anyDuplicated(x, by = c("upc", "upc_ver_uc")))
      stop("Conflicting product-extra UPC-version keys for year ", yr)
  }
  x[, representative_source_year := yr]
  extra_by_source[[length(extra_by_source) + 1L]] <- x
  rm(x); gc(FALSE)
}
if (length(extra_by_source)) {
  all_extra <- rbindlist(extra_by_source, fill = TRUE)
  setnames(all_extra, intersect(extra_fields, names(all_extra)),
           paste0("extra_", intersect(extra_fields, names(all_extra))))
  selected <- merge(selected, all_extra,
    by = c("upc", "upc_ver_uc", "representative_source_year"),
    all.x = TRUE, sort = FALSE)
  if (nrow(selected) != nrow(top90)) stop("Products-extra join multiplied UPC rows")
}
setorder(selected, rank)

# A human-readable feature string plus independent columns for matching/research.
feature_parts <- intersect(c("upc_descr", "product_module_descr", "extra_product_descr",
  "extra_flavor_descr", "extra_type_descr", "extra_style_descr", "extra_variety_descr",
  "size1_amount", "size1_units"), names(selected))
selected[, Feature := apply(.SD, 1L, function(vals) {
  pieces <- trimws(as.character(vals))
  pieces <- pieces[!is.na(pieces) & nzchar(pieces) & pieces != "NA"]
  paste(unique(pieces), collapse = " | ")
}), .SDcols = feature_parts]
selected[, Brand := brand_descr]
setcolorder(selected, c("rank", "upc", "upc_ver_uc", "Brand", "Feature",
  "UPC_Market_Share", "cumulative_share", "sales_dollars", "purchase_rows",
  setdiff(names(selected), c("rank", "upc", "upc_ver_uc", "Brand", "Feature",
    "UPC_Market_Share", "cumulative_share", "sales_dollars", "purchase_rows"))))

# Make the exclusion denominator and boundary rule inspectable in Excel.
summary_table <- data.table(
  metric = c("Analysis period (purchase dates)", "Source file years requested",
    "Definition of sales", "Control brand exclusion pattern", "Target share",
    "Selected UPC-version count", "Total non-control UPC-version count",
    "Selected non-control dollar share", "Non-control total dollars",
    "Selected top-90% dollars", "Unknown-brand purchase rows excluded", "Missing source years",
    "NOTE: UPC market share denominator"),
  value = c("2008-01-01 through 2017-12-31", "2007-2018",
    "max(final_price_paid,0) per purchase row, after coupon adjustment",
    CONTROL_BRAND_REGEX, as.character(COVERAGE_TARGET),
    as.character(nrow(selected)), as.character(nrow(upc_sales)),
    as.character(tail(selected$cumulative_share, 1L)),
    as.character(total_noncontrol_sales), as.character(sum(selected$sales_dollars)),
    as.character(sum(source_audit$unknown_brand_rows)), paste(missing_years, collapse = ", "),
    "ALL eligible non-control-brand cereal sales, excluding unknown-brand rows"))

# ---- Write ONE workbook in restricted local data storage -----------------
wb <- openxlsx::createWorkbook()
add_table <- function(sheet_name, dat) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(wb, sheet_name, as.data.frame(dat),
                           tableStyle = "TableStyleMedium2", withFilter = TRUE)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2)
}
# Excel handles numeric identifiers poorly: force UPC keys to TEXT format.
add_table("Top90_UPCs", selected)
key_style <- openxlsx::createStyle(numFmt = "@")
openxlsx::addStyle(wb, "Top90_UPCs", key_style,
  rows = seq_len(nrow(selected)) + 1L,
  cols = match(c("upc", "upc_ver_uc"), names(selected)), gridExpand = TRUE, stack = TRUE)
percent_style <- openxlsx::createStyle(numFmt = "0.0000%")
openxlsx::addStyle(wb, "Top90_UPCs", percent_style,
  rows = seq_len(nrow(selected)) + 1L,
  cols = match(c("UPC_Market_Share", "cumulative_share"), names(selected)),
  gridExpand = TRUE, stack = TRUE)
add_table("Coverage_Summary", summary_table)
add_table("Source_Audit", source_audit)
if (length(unknown_brand_rows)) {
  add_table("Unknown_Brand_UPCs", rbindlist(unknown_brand_rows, fill = TRUE))
}
# Remaining UPCs are not discarded from the extracted purchase data.
add_table("Remaining_UPCs", upc_sales[-seq_len(cutoff)])
openxlsx::saveWorkbook(wb, OUTPUT_FILE, overwrite = TRUE)
message("Saved restricted research workbook: ", OUTPUT_FILE)
message("This workbook is OUTSIDE the GitHub repository.")
