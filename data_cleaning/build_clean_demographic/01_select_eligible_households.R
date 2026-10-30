# ============================================================
# 01_select_eligible_households.R
# Healthy Purchase Cycle | Demographic Construction
# ============================================================
# Purpose:
#   Define the household sample from the UNION of the six
#   category-level shopping-trip datasets (2008–2017).
#   Retain households observed in at least THREE distinct
#   purchase years; years do not need to be consecutive.
#
# Inputs:
#   Local restricted category-level TSV files (see inputs below).
#
# Outputs (local restricted-data directory; NOT GitHub):
#   1) eligible_households_2008_2017.tsv
#   2) category_household_dates_2008_2017.tsv
#   3) household_sample_summary.csv
#   4) household_year_count_distribution.csv
#
# Identification:
#   hhid is always character. For FV, rename household_code
#   to hhid. Count years from purchase_date, NOT source year.
#   A household qualifies if present in ANY of six categories
#   in >= 3 unique calendar years between 2008 and 2017.
# ============================================================

library(data.table)

# 1. Paths and parameters -------------------------------------
data_root <- "C:/Users/rwen13/Documents/healthy_purchase_data"
analysis_years <- 2008:2017
min_observed_years <- 3L

input_files <- c(
  fv       = file.path(data_root, "processed/fv_trip_level/fv_trip_level_2008_2017.tsv"),
  milk     = file.path(data_root, "processed/milk_trip_level/milk_trip_level_2008_2017.tsv"),
  yogurt   = file.path(data_root, "processed/yogurt_trip_level/yogurt_trip_level_2008_2017.tsv"),
  cereal   = file.path(data_root, "processed/cereal_trip_level/cereal_trip_level_2008_2017.tsv"),
  snack    = file.path(data_root, "processed/snack_trip_level/snack_trip_level_2008_2017.tsv"),
  beverage = file.path(data_root, "intermediate/beverage_trip_level/beverage_trip_level_2008_2017.tsv")
)

output_dir <- file.path(data_root, "processed/demographics")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

missing_files <- input_files[!file.exists(input_files)]
if (length(missing_files) > 0L) {
  stop("Missing input file(s):\n", paste(missing_files, collapse = "\n"),
       "\nVerify actual locations before running this script.")
}

# 2. Read one category, retaining household-date pairs only ----
read_household_dates <- function(category, path) {
  message("Reading ", category, ": ", path)
  
  # Read only the header first to verify the expected identifiers.
  columns <- names(fread(path, sep = "\t", nrows = 0L, showProgress = FALSE))
  id_col <- if (category == "fv") "household_code" else "hhid"
  required <- c(id_col, "purchase_date")
  absent <- setdiff(required, columns)
  if (length(absent) > 0L) {
    stop("Missing required column(s) in ", category, ": ",
         paste(absent, collapse = ", "))
  }
  
  # IDs and dates are explicitly read as strings to avoid coercion.
  dat <- fread(
    path, sep = "\t", select = required,
    colClasses = list(character = required),
    showProgress = TRUE
  )
  setnames(dat, id_col, "hhid")
  dat[, hhid := trimws(hhid)]
  dat[hhid == "", hhid := NA_character_]
  dat[, purchase_date := trimws(purchase_date)]
  
  # The processed trip files should use ISO YYYY-MM-DD dates.
  # Fail on nonempty unexpected date formats instead of silently
  # assigning a household to the wrong observation year.
  nonempty_date <- !is.na(dat$purchase_date) & nzchar(dat$purchase_date)
  bad_format <- nonempty_date & !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dat$purchase_date)
  if (any(bad_format)) {
    stop(category, ": unexpected purchase_date format, e.g. ",
         dat$purchase_date[which(bad_format)[1L]],
         ". Expected YYYY-MM-DD.")
  }
  dat[, purchase_date := as.IDate(purchase_date, format = "%Y-%m-%d")]
  if (any(nonempty_date & is.na(dat$purchase_date))) {
    stop(category, ": some nonempty purchase dates could not be parsed.")
  }
  
  # Derive year from the actual shopping date, regardless of the
  # original Nielsen source-file year or panel_year column.
  dat[, panel_year := as.integer(format(purchase_date, "%Y"))]
  invalid_rows <- dat[is.na(hhid) | is.na(purchase_date), .N]
  out_of_window <- dat[!is.na(panel_year) &
                         !(panel_year %in% analysis_years), .N]
  dat <- dat[!is.na(hhid) & panel_year %in% analysis_years,
             .(hhid, panel_year, purchase_date)]
  unique(dat, by = c("hhid", "panel_year", "purchase_date"))
  
  message(sprintf("  Unique household-dates: %s | invalid ID/date rows: %s | outside window: %s",
                  format(nrow(dat), big.mark = ","),
                  format(invalid_rows, big.mark = ","),
                  format(out_of_window, big.mark = ",")))
  dat[]
}

# 3. Union of categories and distinct years -------------------
category_dates <- lapply(names(input_files), function(category) {
  read_household_dates(category, input_files[[category]])
})
names(category_dates) <- names(input_files)

# UNION, not an intersection: the same household-date can occur
# in multiple categories but contributes only once to the frame.
all_dates <- unique(
  rbindlist(category_dates, use.names = TRUE),
  by = c("hhid", "panel_year", "purchase_date")
)
setorder(all_dates, hhid, purchase_date)
if (nrow(all_dates) == 0L) stop("No valid household-date observations found.")

# n_years_observed counts distinct years, NOT trips or category rows.
household_years <- unique(all_dates[, .(hhid, panel_year)])
hh_counts <- household_years[, .(n_years_observed = .N), by = hhid]
hh_counts[, eligible := n_years_observed >= min_observed_years]
setorder(hh_counts, hhid)
eligible_hh <- hh_counts[eligible == TRUE, .(hhid, n_years_observed)]

# 4. Restrict household-date backbone to eligible households --
# The join must not duplicate dates: eligible_hh has one row/hhid.
stopifnot(!anyDuplicated(eligible_hh$hhid))
selected_dates <- all_dates[eligible_hh, on = "hhid", nomatch = 0L,
                            .(hhid, panel_year, purchase_date)]
setorder(selected_dates, hhid, purchase_date)

# 5. Validation ------------------------------------------------
stopifnot(
  !anyDuplicated(selected_dates, by = c("hhid", "panel_year", "purchase_date")),
  !anyDuplicated(eligible_hh$hhid),
  all(eligible_hh$n_years_observed >= min_observed_years),
  uniqueN(selected_dates$hhid) == nrow(eligible_hh),
  all(selected_dates$panel_year %in% analysis_years)
)
recount <- unique(selected_dates[, .(hhid, panel_year)])[
  , .(recounted_years = .N), by = hhid
]
validation <- merge(eligible_hh, recount, by = "hhid", all.x = TRUE)
stopifnot(all(validation$n_years_observed == validation$recounted_years))

# 6. Sample-selection reports ----------------------------------
# These report actual counts after running on local Nielsen data.
n_all <- nrow(hh_counts)
n_excluded <- hh_counts[eligible == FALSE, .N]
n_retained <- nrow(eligible_hh)
stopifnot(n_all == n_excluded + n_retained)

summary_dt <- data.table(
  selection_step = c(
    "Unique households across six categories (2008-2017)",
    "Excluded: observed in fewer than 3 distinct years",
    "Retained: observed in at least 3 distinct years"
  ),
  n_households = c(n_all, n_excluded, n_retained)
)

# Include zero-count lengths for a complete 1-to-10-year distribution.
distribution_dt <- data.table(n_years_observed = seq_along(analysis_years))
distribution_dt <- merge(
  distribution_dt,
  hh_counts[, .(n_households = .N), by = n_years_observed],
  by = "n_years_observed", all.x = TRUE, sort = TRUE
)
distribution_dt[is.na(n_households), n_households := 0L]
distribution_dt[, retained := n_years_observed >= min_observed_years]

# 7. Export outside the GitHub repository ----------------------
fwrite(eligible_hh,
       file.path(output_dir, "eligible_households_2008_2017.tsv"),
       sep = "\t", na = "")
fwrite(selected_dates,
       file.path(output_dir, "category_household_dates_2008_2017.tsv"),
       sep = "\t", na = "")
fwrite(summary_dt,
       file.path(output_dir, "household_sample_summary.csv"))
fwrite(distribution_dt,
       file.path(output_dir, "household_year_count_distribution.csv"))

message("\nHousehold sample selection summary:")
print(summary_dt)
message("\nYears observed distribution:")
print(distribution_dt)
message("\nOutputs saved to: ", normalizePath(output_dir, winslash = "/"))
message("Step 01 completed successfully.")
