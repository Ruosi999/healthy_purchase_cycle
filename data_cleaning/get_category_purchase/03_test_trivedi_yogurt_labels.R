# =============================================================================
# Minimal non-destructive robustness test: Trivedi et al. (2016) yogurt labels
#
# Source: existing annual yogurt purchase extracts (NO raw Nielsen rescan).
# Method: identify fat-free, low-fat, or reduced-fat wording in type_descr.
# This reproduces the article's disclosed keyword approach, not its SKU roster.
#
# Inputs (restricted, local only):
#   healthy_purchase_data/intermediate/yogurt_purchases/yogurt_purchases_YYYY.tsv
# Outputs (restricted, local only):
#   healthy_purchase_data/intermediate/yogurt_trivedi_test/
#     yogurt_trivedi_summary.csv
#     yogurt_trivedi_yearly.csv
#     yogurt_trivedi_label_changes.csv
#     yogurt_trivedi_trip_comparison.csv
#
# No existing file is overwritten. Identifiers remain character/string.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

DATA_ROOT <- Sys.getenv("HPC_DATA_ROOT", unset =
  "C:/Users/rwen13/Documents/healthy_purchase_data")
INPUT_DIR <- file.path(DATA_ROOT, "intermediate", "yogurt_purchases")
OUTPUT_DIR <- file.path(DATA_ROOT, "intermediate", "yogurt_trivedi_test")
SOURCE_YEARS <- 2007:2018
DATE_START <- as.IDate("2008-01-01")
DATE_END <- as.IDate("2017-12-31")
setDTthreads(4L)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# The paper names Low Fat, Fat Free and Reduced Fat. Keep the existing NONFAT
# spelling recognized by the project's extraction script.
classify_trivedi <- function(descr) {
  d <- toupper(trimws(fcoalesce(as.character(descr), "")))
  grepl("NON[ -]?FAT|FAT[ -]?FREE|LOW[ -]?FAT|REDUCED[ -]?FAT", d, perl = TRUE)
}

parse_purchase_dates <- function(x) {
  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  iso <- !is.na(s) & grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}", s)
  norm <- s
  norm[iso] <- gsub("/", "-", substr(s[iso], 1L, 10L), fixed = TRUE)
  out <- suppressWarnings(as.IDate(norm, format = "%Y-%m-%d"))
  for (fmt in c("%Y%m%d", "%m/%d/%Y", "%m/%d/%y")) {
    ix <- which(!is.na(s) & is.na(out))
    if (!length(ix)) break
    out[ix] <- suppressWarnings(as.IDate(s[ix], format = fmt))
  }
  if (any(!is.na(s) & is.na(out))) stop("Unrecognized purchase_date: ", s[which(!is.na(s) & is.na(out))[1]])
  out
}

parse_existing_flag <- function(x, name) {
  s <- toupper(trimws(as.character(x)))
  if (anyNA(s) || any(!s %in% c("0", "1", "TRUE", "FALSE", "T", "F")))
    stop("Invalid/missing existing label in ", name)
  as.integer(s %in% c("1", "TRUE", "T"))
}

# This function reads only the 8 needed columns and returns grouped summaries;
# it does not accumulate full purchase-level records across all years.
process_source <- function(source_year) {
  path <- file.path(INPUT_DIR, sprintf("yogurt_purchases_%d.tsv", source_year))
  if (!file.exists(path)) return(NULL)
  message("Testing source file: ", basename(path))
  cols <- c("upc", "upc_ver_uc", "household_code", "trip_code_uc",
            "purchase_date", "yogurt_type_descr", "fat_free_yogurt",
            "low_fat_yogurt", "final_price_paid")
  headers <- names(fread(path, nrows = 0L, sep = "\t", quote = ""))
  if (length(setdiff(cols, headers))) stop("Missing columns in ", path, ": ",
                                            paste(setdiff(cols, headers), collapse = ", "))
  x <- fread(path, sep = "\t", quote = "", select = cols,
             colClasses = list(character = cols), showProgress = FALSE)
  dt <- parse_purchase_dates(x$purchase_date)
  x <- x[!is.na(dt) & dt >= DATE_START & dt <= DATE_END]
  dt <- dt[!is.na(dt) & dt >= DATE_START & dt <= DATE_END]
  if (!nrow(x)) return(NULL)
  if (anyNA(x$upc) || anyNA(x$upc_ver_uc) ||
      anyNA(x$household_code) || anyNA(x$trip_code_uc) ||
      any(x$upc == "" | x$upc_ver_uc == "" |
          x$household_code == "" | x$trip_code_uc == ""))
    stop("Missing identifier in ", path)
  x[, `:=`(panel_year = as.integer(format(dt, "%Y")),
           purchase_date = format(dt, "%Y-%m-%d"))]
  x[, `:=`(old_fat_free = parse_existing_flag(fat_free_yogurt, "fat_free_yogurt"),
           old_low_fat = parse_existing_flag(low_fat_yogurt, "low_fat_yogurt"),
           trivedi_healthy = as.integer(classify_trivedi(yogurt_type_descr)))]
  x[, old_healthy := old_fat_free]
  # Negative coupon-adjusted prices are floored at zero, matching the existing
  # trip aggregation logic. Missing prices contribute zero to spending only.
  raw_spend <- suppressWarnings(as.numeric(x$final_price_paid))
  if (any(!is.na(x$final_price_paid) & x$final_price_paid != "" & is.na(raw_spend)) ||
      any(is.infinite(raw_spend))) stop("Invalid final_price_paid in ", path)
  x[, spend := pmax(fcoalesce(raw_spend, 0), 0)]
  if (any(x$old_healthy == 1L & x$trivedi_healthy == 0L))
    stop("Previously fat-free yogurt lost its healthy label in ", path)

  # SKU-year is defined by UPC + UPC version + calendar year. Annual source
  # duplicates will be deduplicated only after cross-file consistency checks.
  sku <- x[, .(trivedi_min = min(trivedi_healthy),
               trivedi_max = max(trivedi_healthy),
               old_min = min(old_healthy), old_max = max(old_healthy)),
           by = .(panel_year, upc, upc_ver_uc)]
  if (any(sku$trivedi_min != sku$trivedi_max | sku$old_min != sku$old_max))
    stop("Conflicting labels within an SKU-year in ", path)
  sku[, `:=`(trivedi_healthy = trivedi_min, old_healthy = old_min)]
  sku[, c("trivedi_min", "trivedi_max", "old_min", "old_max") := NULL]

  trip_keys <- c("panel_year", "household_code", "trip_code_uc", "purchase_date")
  trips <- x[, .(old_healthy_trip = as.integer(any(old_healthy == 1L)),
                 trivedi_healthy_trip = as.integer(any(trivedi_healthy == 1L)),
                 total_yogurt_spend = sum(spend),
                 trivedi_healthy_spend = sum(spend[trivedi_healthy == 1L])),
             by = trip_keys]
  trips[, source_year := source_year]

  sales <- x[, .(purchase_lines = .N, sales = sum(spend)),
             by = .(panel_year, old_healthy, trivedi_healthy)]
  rm(x); invisible(gc(verbose = FALSE))
  list(sku = sku, trips = trips, sales = sales)
}

results <- lapply(SOURCE_YEARS, process_source)
results <- Filter(Negate(is.null), results)
if (!length(results)) stop("No annual yogurt purchase extracts found.")

sku <- rbindlist(lapply(results, `[[`, "sku"))
trips <- rbindlist(lapply(results, `[[`, "trips"))
sales <- rbindlist(lapply(results, `[[`, "sales"))
rm(results); invisible(gc(verbose = FALSE))

# The same SKU-year in adjacent source files must have identical labels.
sku_check <- sku[, .(n_old = uniqueN(old_healthy),
                     n_trivedi = uniqueN(trivedi_healthy)),
                 by = .(panel_year, upc, upc_ver_uc)]
if (any(sku_check$n_old != 1L | sku_check$n_trivedi != 1L))
  stop("Conflicting SKU-year labels across annual source files.")
sku <- unique(sku, by = c("panel_year", "upc", "upc_ver_uc"))
rm(sku_check)

# Same trip occurring in two source files would cause spending double counts.
trip_keys <- c("panel_year", "household_code", "trip_code_uc", "purchase_date")
if (anyDuplicated(trips, by = trip_keys))
  stop("Duplicate trip keys across source files; resolve before using these statistics.")

# Small yearly sales cross-tabs, including original-vs-new label switches.
sales <- sales[, .(purchase_lines = sum(purchase_lines), sales = sum(sales)),
               by = .(panel_year, old_healthy, trivedi_healthy)]
changes <- sales[old_healthy != trivedi_healthy][order(panel_year)]
changes[, change := "Old regular -> Trivedi healthy"]

summarize_year <- function(yr) {
  s <- sku[panel_year == yr]
  t <- trips[panel_year == yr]
  z <- sales[panel_year == yr]
  total_sales <- sum(z$sales)
  data.table(
    panel_year = yr,
    n_sku_versions = nrow(s),
    n_yogurt_trips = nrow(t),
    old_healthy_sku_pct = 100 * mean(s$old_healthy),
    trivedi_healthy_sku_pct = 100 * mean(s$trivedi_healthy),
    old_healthy_sales_pct = if (total_sales > 0) 100 * sum(z$sales[z$old_healthy == 1L]) / total_sales else NA_real_,
    trivedi_healthy_sales_pct = if (total_sales > 0) 100 * sum(z$sales[z$trivedi_healthy == 1L]) / total_sales else NA_real_,
    old_healthy_trip_pct = 100 * mean(t$old_healthy_trip),
    trivedi_healthy_trip_pct = 100 * mean(t$trivedi_healthy_trip),
    trivedi_mean_conditional_spend_share_pct = 100 * mean(
      fifelse(t$total_yogurt_spend > 0,
              t$trivedi_healthy_spend / t$total_yogurt_spend, NA_real_), na.rm = TRUE),
    new_healthy_trips = sum(t$old_healthy_trip == 0L & t$trivedi_healthy_trip == 1L),
    n_purchase_lines = sum(z$purchase_lines),
    total_yogurt_sales = total_sales
  )
}

yearly <- rbindlist(lapply(sort(unique(trips$panel_year)), summarize_year))
# All-years summary weights each underlying observation rather than averaging
# yearly percentages. SKU entries are unique SKU-version-year observations.
total_sales <- sum(sales$sales)
summary <- data.table(
  n_sku_versions_years = nrow(sku), n_yogurt_trips = nrow(trips),
  old_healthy_sku_pct = 100 * mean(sku$old_healthy),
  trivedi_healthy_sku_pct = 100 * mean(sku$trivedi_healthy),
  old_healthy_sales_pct = 100 * sum(sales$sales[sales$old_healthy == 1L]) / total_sales,
  trivedi_healthy_sales_pct = 100 * sum(sales$sales[sales$trivedi_healthy == 1L]) / total_sales,
  old_healthy_trip_pct = 100 * mean(trips$old_healthy_trip),
  trivedi_healthy_trip_pct = 100 * mean(trips$trivedi_healthy_trip),
  new_healthy_trips = sum(trips$old_healthy_trip == 0L & trips$trivedi_healthy_trip == 1L),
  n_purchase_lines = sum(sales$purchase_lines), total_yogurt_sales = total_sales
)

trip_comparison <- trips[, .(n_trips = .N,
                             yogurt_spend = sum(total_yogurt_spend),
                             trivedi_healthy_spend = sum(trivedi_healthy_spend)),
                         by = .(panel_year, old_healthy_trip, trivedi_healthy_trip)]

fwrite(summary, file.path(OUTPUT_DIR, "yogurt_trivedi_summary.csv"))
fwrite(yearly, file.path(OUTPUT_DIR, "yogurt_trivedi_yearly.csv"))
fwrite(changes, file.path(OUTPUT_DIR, "yogurt_trivedi_label_changes.csv"))
fwrite(trip_comparison, file.path(OUTPUT_DIR, "yogurt_trivedi_trip_comparison.csv"))

message("\n=== Trivedi yogurt label robustness test ===")
print(summary)
message("\nYearly results:")
print(yearly)
message("\nOutputs written outside GitHub: ", OUTPUT_DIR)
