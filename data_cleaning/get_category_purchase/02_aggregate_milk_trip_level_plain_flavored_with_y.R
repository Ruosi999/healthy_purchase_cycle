# =============================================================================
# Script: Aggregate plain and flavored milk purchases to trip level
#
# Purpose:
#   Convert yearly purchase-level plain/flavored milk files into a trip-level
#   dataset while preserving milk as a standalone cleaning output.
#
# Product definition:
#   3625 = DAIRY-MILK-REFRIGERATED
#       -> Plain refrigerated milk -> healthy_beverage
#   3592 = DAIRY-FLAVORED MILK-REFRIGERATED
#       -> Flavored refrigerated milk -> unhealthy_beverage
#
#   Canned milk (1297) and buttermilk (3626) are excluded upstream and must not
#   appear in the labeled purchase files consumed by this script.
#
# Input:
#   healthy_purchase_data/intermediate/milk_purchases_beverage_classified/
#   milk_purchases_YYYY_beverage_classified.tsv
#
# Output:
#   healthy_purchase_data/processed/milk_trip_level_beverage_classified/
#   - milk_trip_level_2008_2017.tsv
#   - milk_trip_level_YYYY.tsv
#   - milk_trip_aggregation_audit.tsv
#
# Key conventions:
#   - household_code and trip_code_uc are always read and retained as character.
#   - hhid = household_code
#   - trip_id = trip_code_uc
#   - plain_milk_purchased = 1 if any plain milk is purchased on the trip.
#   - flavored_milk_purchased = 1 if any flavored milk is purchased on the trip.
#   - Spending is classified by product module, not by fat content.
#   - Spending uses final_price_paid when available; missing values fall back
#     to total_price_paid - coupon_value, with missing coupons treated as zero.
#   - Plain and flavored spending are mutually exclusive and exhaustive across
#     all included milk purchase rows.
#   - milk_y = 1 if any plain refrigerated milk is purchased (including mixed
#     plain/flavored trips); otherwise milk_y = 2 for flavored-only trips.
#   - Trips without eligible milk purchases are absent from this dataset.
#   - Milk remains a standalone dataset and is combined with beverage only
#     when the HMM master panel is built.
#
# Year assignment:
#   panel_year is derived from purchase_date, not from the source filename.
#   Only purchase dates in 2008-2017 are retained. Source year 2018 is included
#   to capture purchases dated in late 2017.
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
  "milk_purchases_beverage_classified"
)

# Use a new output directory so the previous fat-based milk trip file is
# preserved until the new output has been checked.
OUTPUT_DIR <- file.path(
  DATA_ROOT,
  "processed",
  "milk_trip_level_beverage_classified"
)

# The extraction script creates source-year files from 2008 through 2018.
# Source year 2018 can contain purchases dated in calendar year 2017.
SOURCE_YEARS <- 2008:2018

KEEP_PANEL_YEARS <- 2008:2017

WRITE_YEARLY_FILES <- TRUE

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


normalize_module_code <- function(x) {
  # Normalize module codes only. Never apply this function to UPC identifiers.
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  x <- sub("\\.0+$", "", x)
  is_numeric_code <- !is.na(x) & grepl("^[0-9]+$", x)
  x[is_numeric_code] <- sub("^0+(?=[0-9])", "", x[is_numeric_code], perl = TRUE)
  x
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
    "product_module_code",
    "milk_beverage_class",
    "plain_milk_flag",
    "flavored_milk_flag"
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
    c(
      "household_code",
      "trip_code_uc",
      "purchase_date",
      "product_module_code",
      "milk_beverage_class",
      "plain_milk_flag",
      "flavored_milk_flag"
    ),
    selected_columns
  )

  dt <- fread(
    file_path,
    sep = "\t",
    select = selected_columns,
    colClasses = list(character = character_columns),
    showProgress = TRUE
  )

  # Preserve household, trip, and module identifiers as character strings.
  dt[, household_code := as.character(household_code)]
  dt[, trip_code_uc := as.character(trip_code_uc)]
  dt[, purchase_date := parse_purchase_date(purchase_date)]
  dt[, product_module_code := normalize_module_code(product_module_code)]

  if (anyNA(dt$product_module_code)) {
    stop(
      "Missing product_module_code values found in ",
      basename(file_path)
    )
  }

  dt[, plain_flag := to_binary(plain_milk_flag)]
  dt[, flavored_flag := to_binary(flavored_milk_flag)]
  dt[, milk_beverage_class := trimws(as.character(milk_beverage_class))]

  # Each included purchase must belong to exactly one of the two milk modules.
  invalid_flags <- is.na(dt$plain_flag) |
    is.na(dt$flavored_flag) |
    (dt$plain_flag + dt$flavored_flag != 1L)

  if (any(invalid_flags)) {
    stop(
      "Invalid plain/flavored milk flags found in ",
      basename(file_path),
      ". Each purchase must have exactly one flag equal to 1."
    )
  }

  # Verify that module codes, binary flags, and beverage labels agree.
  module_matches_flags <- (
    dt$plain_flag == 1L & dt$product_module_code == "3625"
  ) | (
    dt$flavored_flag == 1L & dt$product_module_code == "3592"
  )

  if (any(!module_matches_flags)) {
    stop(
      "Product-module codes do not agree with plain/flavored flags in ",
      basename(file_path)
    )
  }

  expected_class <- ifelse(
    dt$plain_flag == 1L,
    "healthy_beverage",
    "unhealthy_beverage"
  )

  class_matches_flags <- !is.na(dt$milk_beverage_class) &
    dt$milk_beverage_class == expected_class

  if (any(!class_matches_flags)) {
    stop(
      "milk_beverage_class does not agree with the module-based classification in ",
      basename(file_path)
    )
  }

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

  dt
}


aggregate_one_source_file <- function(source_year) {
  # Aggregate one source file, filtering on purchase_date rather than source year.
  input_file <- file.path(
    INPUT_DIR,
    paste0("milk_purchases_", source_year, "_beverage_classified.tsv")
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
        milk_y_1_trips = NA_integer_,
        milk_y_2_trips = NA_integer_,
        plain_milk_trips = NA_integer_,
        flavored_milk_trips = NA_integer_,
        mixed_milk_trips = NA_integer_,
        plain_milk_spending = NA_real_,
        flavored_milk_spending = NA_real_,
        total_milk_spending = NA_real_,
        missing_price_rows = NA_integer_
      )
    ))
  }

  message("Reading source year ", source_year, ": ", basename(input_file))
  dt <- read_labeled_milk_file(input_file)

  input_rows <- nrow(dt)
  missing_date_rows <- sum(is.na(dt$purchase_date))

  # Derive panel_year from the actual purchase date.
  dt[, panel_year := as.integer(format(purchase_date, "%Y"))]

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
        milk_y_1_trips = 0L,
        milk_y_2_trips = 0L,
        plain_milk_trips = 0L,
        flavored_milk_trips = 0L,
        mixed_milk_trips = 0L,
        plain_milk_spending = 0,
        flavored_milk_spending = 0,
        total_milk_spending = 0,
        missing_price_rows = 0L
      )
    ))
  }

  # Assign every included purchase to exactly one spending component.
  dt[, plain_spend_row := fifelse(plain_flag == 1L, spend, 0)]
  dt[, flavored_spend_row := fifelse(flavored_flag == 1L, spend, 0)]

  # Verify purchase-level spending is fully allocated across the two components.
  source_total <- sum(dt$spend, na.rm = TRUE)
  allocated_total <- sum(dt$plain_spend_row, na.rm = TRUE) +
    sum(dt$flavored_spend_row, na.rm = TRUE)

  if (!isTRUE(all.equal(source_total, allocated_total, tolerance = 1e-8))) {
    stop("Milk spending reconciliation failed in source year ", source_year)
  }

  # Aggregate purchase records to one row per household-trip-date.
  trip_data <- dt[
    ,
    .(
      # A plain-milk purchase takes precedence over flavored milk.
      milk_y = if (any(plain_flag == 1L)) 1L else 2L,
      plain_milk_purchased = as.integer(any(plain_flag == 1L)),
      flavored_milk_purchased = as.integer(any(flavored_flag == 1L)),
      plain_milk_spending = sum(plain_spend_row, na.rm = TRUE),
      flavored_milk_spending = sum(flavored_spend_row, na.rm = TRUE),
      total_milk_spending = sum(plain_spend_row, na.rm = TRUE) +
        sum(flavored_spend_row, na.rm = TRUE),
      milk_purchase_rows = .N,
      missing_price_rows = sum(is.na(spend))
    ),
    by = .(
      hhid = household_code,
      panel_year,
      trip_id = trip_code_uc,
      purchase_date
    )
  ]

  # Reconcile purchase-level and trip-level spending totals.
  trip_total <- sum(trip_data$total_milk_spending, na.rm = TRUE)

  if (!isTRUE(all.equal(source_total, trip_total, tolerance = 1e-8))) {
    stop("Trip-level milk spending reconciliation failed in source year ", source_year)
  }

  # Preserve source-year provenance temporarily for duplicate checks and audits.
  trip_data[, source_file_year := as.integer(source_year)]

  plain_trips <- sum(trip_data$plain_milk_purchased == 1L)
  flavored_trips <- sum(trip_data$flavored_milk_purchased == 1L)
  mixed_trips <- sum(
    trip_data$plain_milk_purchased == 1L &
      trip_data$flavored_milk_purchased == 1L
  )

  audit <- data.table(
    source_file_year = as.integer(source_year),
    input_file = basename(input_file),
    file_found = TRUE,
    input_rows = as.integer(input_rows),
    missing_purchase_date_rows = as.integer(missing_date_rows),
    excluded_date_rows = as.integer(excluded_date_rows),
    retained_purchase_rows = as.integer(nrow(dt)),
    output_trip_rows = as.integer(nrow(trip_data)),
    milk_y_1_trips = as.integer(sum(trip_data$milk_y == 1L)),
    milk_y_2_trips = as.integer(sum(trip_data$milk_y == 2L)),
    plain_milk_trips = as.integer(plain_trips),
    flavored_milk_trips = as.integer(flavored_trips),
    mixed_milk_trips = as.integer(mixed_trips),
    plain_milk_spending = sum(trip_data$plain_milk_spending, na.rm = TRUE),
    flavored_milk_spending = sum(trip_data$flavored_milk_spending, na.rm = TRUE),
    total_milk_spending = trip_total,
    missing_price_rows = as.integer(sum(is.na(dt$spend)))
  )

  message(
    "Source year ", source_year,
    " | retained purchase rows: ", format(nrow(dt), big.mark = ","),
    " | trip rows: ", format(nrow(trip_data), big.mark = ","),
    " | plain-milk trips: ", format(plain_trips, big.mark = ","),
    " | flavored-milk trips: ", format(flavored_trips, big.mark = ","),
    " | mixed trips: ", format(mixed_trips, big.mark = ",")
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
    "beverage-classified milk purchase files exist."
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

# Keep Milk as a standalone trip-level dataset. Do not merge with Beverage here.
output_columns <- c(
  "hhid",
  "panel_year",
  "trip_id",
  "purchase_date",
  "milk_y",
  "plain_milk_purchased",
  "flavored_milk_purchased",
  "plain_milk_spending",
  "flavored_milk_spending",
  "total_milk_spending",
  "milk_purchase_rows",
  "missing_price_rows"
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

# Final integrity checks.
stopifnot(
  all(final_trips$panel_year %in% KEEP_PANEL_YEARS),
  !anyDuplicated(final_trips[, ..trip_key_columns]),
  all(final_trips$milk_y %in% c(1L, 2L)),
  all(final_trips$milk_y == ifelse(final_trips$plain_milk_purchased == 1L, 1L, 2L)),
  all(final_trips$plain_milk_purchased %in% c(0L, 1L)),
  all(final_trips$flavored_milk_purchased %in% c(0L, 1L)),
  all(
    final_trips$plain_milk_purchased +
      final_trips$flavored_milk_purchased >= 1L
  ),
  isTRUE(all.equal(
    sum(final_trips$total_milk_spending, na.rm = TRUE),
    sum(final_trips$plain_milk_spending, na.rm = TRUE) +
      sum(final_trips$flavored_milk_spending, na.rm = TRUE),
    tolerance = 1e-8
  ))
)

message("------------------------------------------------------------")
message("Plain/flavored milk trip-level aggregation complete.")
message("Calendar years retained: 2008-2017")
message("Total trip rows: ", format(nrow(final_trips), big.mark = ","))
message("Milk outcome 1 (any plain milk): ",
        format(sum(final_trips$milk_y == 1L), big.mark = ","))
message("Milk outcome 2 (flavored milk only): ",
        format(sum(final_trips$milk_y == 2L), big.mark = ","))
message(
  "Trips with plain milk: ",
  format(sum(final_trips$plain_milk_purchased == 1L), big.mark = ",")
)
message(
  "Trips with flavored milk: ",
  format(sum(final_trips$flavored_milk_purchased == 1L), big.mark = ",")
)
message(
  "Trips with both plain and flavored milk: ",
  format(sum(
    final_trips$plain_milk_purchased == 1L &
      final_trips$flavored_milk_purchased == 1L
  ), big.mark = ",")
)
message(
  "Total plain milk spending: ",
  round(sum(final_trips$plain_milk_spending, na.rm = TRUE), 2)
)
message(
  "Total flavored milk spending: ",
  round(sum(final_trips$flavored_milk_spending, na.rm = TRUE), 2)
)
message(
  "Total milk spending: ",
  round(sum(final_trips$total_milk_spending, na.rm = TRUE), 2)
)
message("Audit file: ", audit_output)
message("------------------------------------------------------------")
