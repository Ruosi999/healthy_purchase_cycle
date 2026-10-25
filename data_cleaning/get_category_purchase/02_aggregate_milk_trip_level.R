# =============================================================================
# Script: Aggregate labeled milk purchases to trip level
#
# Purpose:
#   Convert yearly purchase-level milk files into a trip-level dataset aligned
#   with the FV trip-level structure.
#
# Input:
#   healthy_purchase_data/intermediate/milk_purchases_with_labels/
#   milk_purchases_YYYY_with_labels.tsv
#
# Output:
#   healthy_purchase_data/processed/milk_trip_level/
#   - milk_trip_level_2008_2017.tsv
#   - milk_trip_level_YYYY.tsv
#   - milk_trip_aggregation_audit.tsv
#
# Year assignment:
#   panel_year is derived from purchase_date, not from the source filename.
#   Only purchase dates in 2008-2017 are retained. This also means that
#   2007-dated purchases are excluded even if they appear in another source file.
#
# Key conventions:
#   - household_code and trip_code_uc are always read and retained as character.
#   - hhid = household_code
#   - trip_id = trip_code_uc
#   - milk_y = 1 if at least one fully skimmed milk purchase occurs on the trip.
#   - Spending uses final_price_paid when available; otherwise it is calculated
#     as total_price_paid - coupon_value, with missing coupons treated as zero.
#   - low_fat_milk_spending follows the existing low_fat_milk label. Because
#     fully skimmed milk is a subset of low-fat milk in the labeling rules,
#     low_fat_milk_spending includes skimmed purchases and overlaps with
#     skimmed_milk_spending by design.
#
# The script reads one source year at a time to limit memory use.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------------------------------------------------------
# Project configuration
# -----------------------------------------------------------------------------

DATA_ROOT <- "C:/Users/rwen13/Documents/healthy_purchase_data"

INPUT_DIR <- file.path(
  DATA_ROOT,
  "intermediate",
  "milk_purchases_with_labels"
)

OUTPUT_DIR <- file.path(
  DATA_ROOT,
  "processed",
  "milk_trip_level"
)

# Include adjacent source years because source-file year can differ from the
# calendar year in purchase_date. Missing source files are logged and skipped.
SOURCE_YEARS <- 2007:2018

# Keep only purchases whose actual purchase date falls in these years.
KEEP_PANEL_YEARS <- 2008:2017

# Save both the combined file and one file per calendar year.
WRITE_YEARLY_FILES <- TRUE

# data.table thread count; lower this if the machine is under heavy load.
DATA_TABLE_THREADS <- 4L
setDTthreads(DATA_TABLE_THREADS)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

parse_purchase_date <- function(x) {
  # Parse common Nielsen date formats without relying on the source filename.
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_

  parsed <- suppressWarnings(as.IDate(x, format = "%Y-%m-%d"))

  remaining <- which(is.na(parsed) & !is.na(x))
  date_formats <- c(
    "%m/%d/%Y",
    "%m/%d/%y",
    "%Y/%m/%d",
    "%Y%m%d"
  )

  for (date_format in date_formats) {
    if (length(remaining) == 0L) {
      break
    }

    candidate <- suppressWarnings(
      as.IDate(x[remaining], format = date_format)
    )
    valid <- !is.na(candidate)

    if (any(valid)) {
      parsed[remaining[valid]] <- candidate[valid]
    }

    remaining <- which(is.na(parsed) & !is.na(x))
  }

  parsed
}


to_binary <- function(x) {
  # Normalize common numeric/text encodings of binary labels.
  value <- toupper(trimws(as.character(x)))
  as.integer(value %in% c("1", "TRUE", "T", "YES", "Y"))
}


read_labeled_milk_file <- function(file_path) {
  # Read the header first so only columns required for aggregation are loaded.
  header <- names(
    fread(
      file_path,
      sep = "\t",
      nrows = 0L,
      showProgress = FALSE
    )
  )

  required_columns <- c(
    "household_code",
    "trip_code_uc",
    "purchase_date",
    "whole_milk",
    "low_fat_milk",
    "fully_skimmed_milk"
  )

  missing_columns <- setdiff(required_columns, header)
  if (length(missing_columns) > 0L) {
    stop(
      "Required column(s) missing from ", basename(file_path), ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  price_columns <- intersect(
    c("final_price_paid", "total_price_paid", "coupon_value"),
    header
  )

  if (!("final_price_paid" %in% price_columns) &&
      !("total_price_paid" %in% price_columns)) {
    stop(
      "Neither final_price_paid nor total_price_paid is available in ",
      basename(file_path)
    )
  }

  selected_columns <- unique(c(required_columns, price_columns))
  character_columns <- intersect(
    c("household_code", "trip_code_uc", "purchase_date"),
    selected_columns
  )

  dt <- fread(
    file_path,
    sep = "\t",
    select = selected_columns,
    colClasses = list(character = character_columns),
    showProgress = TRUE
  )

  # Defensive conversion keeps trip and household identifiers as strings.
  dt[, household_code := as.character(household_code)]
  dt[, trip_code_uc := as.character(trip_code_uc)]
  dt[, purchase_date := parse_purchase_date(purchase_date)]

  # Use the supplied coupon-adjusted price when available. If it is missing,
  # fall back to total price less coupon where those source fields are present.
  if ("final_price_paid" %in% names(dt)) {
    spend <- suppressWarnings(as.numeric(dt[["final_price_paid"]]))
  } else {
    spend <- rep(NA_real_, nrow(dt))
  }

  if ("total_price_paid" %in% names(dt)) {
    total_spend <- suppressWarnings(as.numeric(dt[["total_price_paid"]]))

    if ("coupon_value" %in% names(dt)) {
      coupon <- suppressWarnings(as.numeric(dt[["coupon_value"]]))
      coupon[is.na(coupon)] <- 0
    } else {
      coupon <- rep(0, nrow(dt))
    }

    fallback_spend <- total_spend - coupon
    fill_rows <- is.na(spend) & !is.na(fallback_spend)
    spend[fill_rows] <- fallback_spend[fill_rows]
  }

  dt[, spend := spend]

  # Unrecognized or missing label values are treated as not labeled.
  dt[, whole_flag := to_binary(whole_milk)]
  dt[, low_fat_flag := to_binary(low_fat_milk)]
  dt[, skimmed_flag := to_binary(fully_skimmed_milk)]

  dt
}


aggregate_one_source_file <- function(source_year) {
  # Aggregate one source file, filtering on purchase_date rather than source year.
  input_file <- file.path(
    INPUT_DIR,
    paste0("milk_purchases_", source_year, "_with_labels.tsv")
  )

  if (!file.exists(input_file)) {
    message("Skipping missing source file: ", basename(input_file))
    return(list(
      trips = NULL,
      audit = data.table(
        source_file_year = as.integer(source_year),
        input_file = basename(input_file),
        file_found = FALSE,
        input_rows = NA_integer_,
        missing_purchase_date_rows = NA_integer_,
        excluded_date_rows = NA_integer_,
        retained_purchase_rows = NA_integer_,
        output_trip_rows = NA_integer_,
        skimmed_milk_trips = NA_integer_
      )
    ))
  }

  message("Reading source year ", source_year, ": ", basename(input_file))
  dt <- read_labeled_milk_file(input_file)

  input_rows <- nrow(dt)
  missing_date_rows <- sum(is.na(dt$purchase_date))

  # panel_year is derived from the actual purchase date.
  dt[, panel_year := as.integer(format(purchase_date, "%Y"))]

  # Reference the column explicitly outside the data.table expression.
  keep_rows <- !is.na(dt$panel_year) & dt$panel_year %in% KEEP_PANEL_YEARS
  excluded_date_rows <- sum(!keep_rows)
  dt <- dt[keep_rows]

  if (nrow(dt) == 0L) {
    message("No in-window purchase dates in source year ", source_year)

    return(list(
      trips = NULL,
      audit = data.table(
        source_file_year = as.integer(source_year),
        input_file = basename(input_file),
        file_found = TRUE,
        input_rows = as.integer(input_rows),
        missing_purchase_date_rows = as.integer(missing_date_rows),
        excluded_date_rows = as.integer(excluded_date_rows),
        retained_purchase_rows = 0L,
        output_trip_rows = 0L,
        skimmed_milk_trips = 0L
      )
    ))
  }

  # Retain only purchase-level fields needed to construct trip-level outcomes.
  # The low-fat spending measure follows the broad low_fat_milk label.
  dt[, low_fat_spend_row := fifelse(low_fat_flag == 1L, spend, 0)]
  dt[, whole_spend_row := fifelse(whole_flag == 1L, spend, 0)]
  dt[, skimmed_spend_row := fifelse(skimmed_flag == 1L, spend, 0)]

  # Aggregate all milk purchase rows to one row per household-trip-date.
  trip_data <- dt[
    ,
    .(
      hhid = household_code[1L],
      panel_year = panel_year[1L],
      trip_id = trip_code_uc[1L],
      purchase_date = purchase_date[1L],
      milk_y = as.integer(any(skimmed_flag == 1L)),
      low_fat_milk_spending = sum(low_fat_spend_row, na.rm = TRUE),
      whole_milk_spending = sum(whole_spend_row, na.rm = TRUE),
      skimmed_milk_spending = sum(skimmed_spend_row, na.rm = TRUE),
      purchase_rows = .N
    ),
    by = .(household_code, panel_year, trip_code_uc, purchase_date)
  ]

  # Preserve source-year provenance temporarily for duplicate checks and audits.
  trip_data[, source_file_year := as.integer(source_year)]

  audit <- data.table(
    source_file_year = as.integer(source_year),
    input_file = basename(input_file),
    file_found = TRUE,
    input_rows = as.integer(input_rows),
    missing_purchase_date_rows = as.integer(missing_date_rows),
    excluded_date_rows = as.integer(excluded_date_rows),
    retained_purchase_rows = as.integer(nrow(dt)),
    output_trip_rows = as.integer(nrow(trip_data)),
    skimmed_milk_trips = as.integer(sum(trip_data$milk_y == 1L))
  )

  message(
    "Source year ", source_year,
    " | retained purchase rows: ", format(nrow(dt), big.mark = ","),
    " | trip rows: ", format(nrow(trip_data), big.mark = ","),
    " | skimmed-milk trips: ",
    format(sum(trip_data$milk_y == 1L), big.mark = ",")
  )

  rm(dt)
  gc(verbose = FALSE)

  list(trips = trip_data, audit = audit)
}


# -----------------------------------------------------------------------------
# Batch processing
# -----------------------------------------------------------------------------

trip_results <- vector("list", length(SOURCE_YEARS))
audit_results <- vector("list", length(SOURCE_YEARS))

for (i in seq_along(SOURCE_YEARS)) {
  result <- aggregate_one_source_file(SOURCE_YEARS[i])
  trip_results[[i]] <- result$trips
  audit_results[[i]] <- result$audit
  rm(result)
}

trip_results <- Filter(Negate(is.null), trip_results)
audit_table <- rbindlist(audit_results, use.names = TRUE, fill = TRUE)

if (length(trip_results) == 0L) {
  stop(
    "No eligible milk trips were found. Check INPUT_DIR and confirm that ",
    "labeled milk purchase files exist."
  )
}

all_trips <- rbindlist(trip_results, use.names = TRUE, fill = TRUE)
setorder(all_trips, panel_year, hhid, purchase_date, trip_id)

# Each trip should occur in only one source file. Fail rather than silently
# double-count spending if the same trip key appears in multiple source files.
trip_key_columns <- c("hhid", "panel_year", "trip_id", "purchase_date")
duplicate_trip_keys <- all_trips[
  ,
  .N,
  by = trip_key_columns
][N > 1L]

if (nrow(duplicate_trip_keys) > 0L) {
  duplicate_file <- file.path(OUTPUT_DIR, "duplicate_milk_trip_keys.tsv")
  fwrite(duplicate_trip_keys, duplicate_file, sep = "\t")
  stop(
    "Duplicate trip keys were found across source files. ",
    "See: ", duplicate_file,
    ". No final trip-level dataset was written."
  )
}

# Validate that trip_id does not map to multiple household/date combinations.
trip_id_conflicts <- all_trips[
  ,
  .(n_keys = .N),
  by = "trip_id"
][n_keys > 1L]

if (nrow(trip_id_conflicts) > 0L) {
  conflict_file <- file.path(OUTPUT_DIR, "milk_trip_id_conflicts.tsv")
  fwrite(trip_id_conflicts, conflict_file, sep = "\t")
  stop(
    "Some trip_id values map to multiple trip keys. Review: ",
    conflict_file,
    ". No final trip-level dataset was written."
  )
}

# Save the combined analysis-ready dataset with the same core fields as FV trips.
output_columns <- c(
  "hhid",
  "panel_year",
  "trip_id",
  "purchase_date",
  "milk_y",
  "low_fat_milk_spending",
  "whole_milk_spending",
  "skimmed_milk_spending"
)

final_trips <- all_trips[, ..output_columns]
setorder(final_trips, panel_year, hhid, purchase_date, trip_id)

combined_output <- file.path(
  OUTPUT_DIR,
  "milk_trip_level_2008_2017.tsv"
)

fwrite(
  final_trips,
  combined_output,
  sep = "\t",
  na = ""
)

message("Saved combined trip-level data: ", combined_output)

# Optionally save one trip-level file for each calendar year.
if (WRITE_YEARLY_FILES) {
  for (year in KEEP_PANEL_YEARS) {
    yearly_data <- final_trips[panel_year == year]
    yearly_output <- file.path(
      OUTPUT_DIR,
      paste0("milk_trip_level_", year, ".tsv")
    )

    fwrite(yearly_data, yearly_output, sep = "\t", na = "")
    message(
      "Saved ", year, " trip-level data: ",
      format(nrow(yearly_data), big.mark = ","),
      " trips"
    )
  }
}

# Save a source-file audit so skipped years and date filtering are transparent.
audit_output <- file.path(
  OUTPUT_DIR,
  "milk_trip_aggregation_audit.tsv"
)

fwrite(audit_table, audit_output, sep = "\t", na = "")

# Final checks and summary.
stopifnot(
  all(final_trips$panel_year %in% KEEP_PANEL_YEARS),
  !anyDuplicated(final_trips[, ..trip_key_columns])
)

message("------------------------------------------------------------")
message("Milk trip-level aggregation complete.")
message("Calendar years retained: 2008-2017")
message("Total trip rows: ", format(nrow(final_trips), big.mark = ","))
message(
  "Trips with fully skimmed milk (milk_y = 1): ",
  format(sum(final_trips$milk_y == 1L), big.mark = ",")
)
message(
  "Total low-fat milk spending: ",
  round(sum(final_trips$low_fat_milk_spending, na.rm = TRUE), 2)
)
message(
  "Total whole milk spending: ",
  round(sum(final_trips$whole_milk_spending, na.rm = TRUE), 2)
)
message(
  "Total skimmed milk spending: ",
  round(sum(final_trips$skimmed_milk_spending, na.rm = TRUE), 2)
)
message("Audit file: ", audit_output)
message("------------------------------------------------------------")
