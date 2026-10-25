# =============================================================================
# Script: Aggregate non-alcoholic beverage purchases to trip level
#
# Purpose:
#   Convert line-level beverage purchases into one record per household-trip.
#   The output schema is aligned with the FV trip-level data.
#
# Input:
#   healthy_purchase_data/intermediate/beverage_purchases/
#     beverage_purchases_YYYY.tsv
#
# Output:
#   healthy_purchase_data/intermediate/beverage_trip_level/
#     beverage_trip_level_2008_2017.tsv
#
# Year assignment:
#   panel_year is derived from purchase_date, not from the source filename or
#   the source panel_year column. Only purchase dates from 2008 through 2017
#   are retained. Dates in 2007 or 2018 are excluded regardless of source file.
#
# Category definitions:
#   beverage_y = 1 if at least one healthier_drinks item is purchased on a trip;
#                0 for a trip with beverage purchases but no healthier drinks.
#   healthy_beverage_spending   = spending on healthier_drinks.
#   unhealthy_beverage_spending = spending on soft_drinks.
#
# Spending:
#   Use final_price_paid (after coupons) when available. If it is missing for
#   a row, fall back to total_price_paid - coupon_value.
#
# Data integrity:
#   - Keep household and trip identifiers as character strings.
#   - Read and aggregate one source-year file at a time.
#   - Include source years 2008-2018 so late-2017 purchases recorded in the 2018 source are captured.
#   - A missing source file causes a clear error instead of a partial output.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# -----------------------------------------------------------------------------
# Run settings
# -----------------------------------------------------------------------------

DATA_ROOT <- "C:/Users/rwen13/Documents/healthy_purchase_data"

INPUT_DIR <- file.path(DATA_ROOT, "intermediate", "beverage_purchases")
OUTPUT_DIR <- file.path(DATA_ROOT, "intermediate", "beverage_trip_level")

# Source-file years can differ from the actual purchase-date year.
SOURCE_YEARS <- 2008:2018

TARGET_YEAR_MIN <- 2008L
TARGET_YEAR_MAX <- 2017L

OUTPUT_FILE <- file.path(
  OUTPUT_DIR,
  paste0("beverage_trip_level_", TARGET_YEAR_MIN, "_", TARGET_YEAR_MAX, ".tsv")
)

RUN_OVERWRITE <- FALSE

OUTPUT_COLUMNS <- c(
  "hhid",
  "panel_year",
  "trip_id",
  "purchase_date",
  "beverage_y",
  "healthy_beverage_spending",
  "unhealthy_beverage_spending"
)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

read_header <- function(path) {
  names(fread(
    path,
    sep = "\t",
    nrows = 0,
    check.names = FALSE,
    showProgress = FALSE
  ))
}

# Parse common date formats found in flat-file exports.
# Invalid or unrecognized dates remain NA and are excluded from the analysis.
parse_purchase_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  parsed <- as.Date(rep(NA_character_, length(x)))

  date_formats <- c(
    "%Y-%m-%d",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%m/%d/%Y",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%y",
    "%Y%m%d"
  )

  for (fmt in date_formats) {
    missing_idx <- which(is.na(parsed) & !is.na(x))
    if (!length(missing_idx)) break

    candidate <- suppressWarnings(as.Date(x[missing_idx], format = fmt))
    valid_idx <- !is.na(candidate)

    if (any(valid_idx)) {
      parsed[missing_idx[valid_idx]] <- candidate[valid_idx]
    }
  }

  parsed
}

# Read only the columns needed for aggregation.
read_beverage_file <- function(path) {
  available <- read_header(path)

  required <- c(
    "household_code",
    "trip_code_uc",
    "purchase_date",
    "beverage_type"
  )

  missing_required <- setdiff(required, available)
  if (length(missing_required)) {
    stop(
      "Required column(s) missing from ", path, ": ",
      paste(missing_required, collapse = ", ")
    )
  }

  spending_candidates <- intersect(
    c("final_price_paid", "total_price_paid", "coupon_value"),
    available
  )

  if (!("final_price_paid" %in% spending_candidates) &&
      !("total_price_paid" %in% spending_candidates)) {
    stop(
      "Neither final_price_paid nor total_price_paid exists in: ", path
    )
  }

  selected <- unique(c(required, spending_candidates))

  dt <- fread(
    path,
    sep = "\t",
    select = selected,
    colClasses = list(
      character = intersect(
        c("household_code", "trip_code_uc", "purchase_date", "beverage_type"),
        selected
      )
    ),
    na.strings = c("", "NA"),
    check.names = FALSE,
    showProgress = TRUE
  )

  # Standardize identifier types without changing their contents.
  dt[, household_code := as.character(household_code)]
  dt[, trip_code_uc := as.character(trip_code_uc)]
  dt[, purchase_date := as.character(purchase_date)]
  dt[, beverage_type := tolower(trimws(as.character(beverage_type)))]

  dt
}

# Calculate row-level spending using the net price when available.
add_spending <- function(dt) {
  if ("final_price_paid" %in% names(dt)) {
    net_spend <- suppressWarnings(as.numeric(dt[["final_price_paid"]]))
  } else {
    net_spend <- rep(NA_real_, nrow(dt))
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
    use_fallback <- is.na(net_spend) & !is.na(fallback_spend)
    net_spend[use_fallback] <- fallback_spend[use_fallback]
  }

  dt[, row_spending := net_spend]
  dt
}

# Aggregate purchases within a source file to the target trip-level schema.
aggregate_one_source <- function(source_year) {
  input_file <- file.path(
    INPUT_DIR,
    paste0("beverage_purchases_", source_year, ".tsv")
  )

  if (!file.exists(input_file)) {
    stop(
      "Required source file is missing: ", input_file,
      "\nRun the beverage extraction step for all SOURCE_YEARS, including 2018 ",
      "to capture purchases dated in late 2017."
    )
  }

  message("\n------------------------------------------------------------")
  message("Aggregating beverage purchases from source file year: ", source_year)
  message("Input: ", input_file)

  dt <- read_beverage_file(input_file)
  source_rows <- nrow(dt)

  if (source_rows == 0L) {
    message("Source file contains zero beverage purchase rows.")
    return(list(
      trips = data.table(),
      audit = data.table(
        source_file_year = as.integer(source_year),
        source_rows = 0L,
        invalid_date_rows = 0L,
        excluded_date_rows = 0L,
        retained_purchase_rows = 0L,
        healthy_purchase_rows = 0L,
        unhealthy_purchase_rows = 0L,
        trip_rows = 0L,
        rows_missing_spending = 0L
      )
    ))
  }

  # Derive panel_year from purchase_date; do not trust source year fields.
  dt[, parsed_purchase_date := parse_purchase_date(purchase_date)]
  dt[, purchase_year := as.integer(format(parsed_purchase_date, "%Y"))]

  invalid_date_rows <- sum(is.na(dt$parsed_purchase_date))
  date_in_range <- !is.na(dt$purchase_year) &
    dt$purchase_year >= TARGET_YEAR_MIN &
    dt$purchase_year <= TARGET_YEAR_MAX

  excluded_date_rows <- sum(!is.na(dt$purchase_year) & !date_in_range)
  eligible <- dt[date_in_range]

  # The upstream extraction should already contain only these two categories.
  # Keep this validation so unexpected labels cannot silently enter spending.
  unexpected_types <- setdiff(
    unique(eligible$beverage_type),
    c("soft_drinks", "healthier_drinks", NA_character_)
  )
  unexpected_types <- unexpected_types[!is.na(unexpected_types)]

  if (length(unexpected_types)) {
    stop(
      "Unexpected beverage_type value(s) in ", input_file, ": ",
      paste(unexpected_types, collapse = ", ")
    )
  }

  eligible <- eligible[
    beverage_type %in% c("soft_drinks", "healthier_drinks")
  ]

  if (!nrow(eligible)) {
    message("No purchases with eligible purchase dates in this source file.")
    return(list(
      trips = data.table(),
      audit = data.table(
        source_file_year = as.integer(source_year),
        source_rows = as.integer(source_rows),
        invalid_date_rows = as.integer(invalid_date_rows),
        excluded_date_rows = as.integer(excluded_date_rows),
        retained_purchase_rows = 0L,
        healthy_purchase_rows = 0L,
        unhealthy_purchase_rows = 0L,
        trip_rows = 0L,
        rows_missing_spending = 0L
      )
    ))
  }

  eligible <- add_spending(eligible)

  # Missing household or trip IDs cannot safely be assigned to a trip.
  missing_key_rows <- eligible[
    is.na(household_code) | household_code == "" |
      is.na(trip_code_uc) | trip_code_uc == "",
    .N
  ]

  if (missing_key_rows > 0L) {
    warning(
      missing_key_rows, " eligible purchase row(s) have missing household/trip IDs ",
      "in source year ", source_year, " and will be excluded."
    )
    eligible <- eligible[
      !is.na(household_code) & household_code != "" &
        !is.na(trip_code_uc) & trip_code_uc != ""
    ]
  }

  rows_missing_spending <- sum(is.na(eligible$row_spending))
  healthy_purchase_rows <- sum(eligible$beverage_type == "healthier_drinks")
  unhealthy_purchase_rows <- sum(eligible$beverage_type == "soft_drinks")

  # Each input row is a beverage purchase line. Aggregate its spending and
  # define the trip indicator as one if any healthier drink was purchased.
  trip_level <- eligible[
    ,
    .(
      beverage_y = as.integer(any(beverage_type == "healthier_drinks")),
      healthy_beverage_spending = sum(
        row_spending[beverage_type == "healthier_drinks"],
        na.rm = TRUE
      ),
      unhealthy_beverage_spending = sum(
        row_spending[beverage_type == "soft_drinks"],
        na.rm = TRUE
      )
    ),
    by = .(
      hhid = household_code,
      panel_year = purchase_year,
      trip_id = trip_code_uc,
      purchase_date = as.character(parsed_purchase_date)
    )
  ]

  # Keep a stable output schema and key types.
  trip_level[, hhid := as.character(hhid)]
  trip_level[, trip_id := as.character(trip_id)]
  trip_level[, panel_year := as.integer(panel_year)]
  trip_level[, purchase_date := as.character(purchase_date)]
  setcolorder(trip_level, OUTPUT_COLUMNS)

  audit <- data.table(
    source_file_year = as.integer(source_year),
    source_rows = as.integer(source_rows),
    invalid_date_rows = as.integer(invalid_date_rows),
    excluded_date_rows = as.integer(excluded_date_rows),
    retained_purchase_rows = as.integer(nrow(eligible)),
    healthy_purchase_rows = as.integer(healthy_purchase_rows),
    unhealthy_purchase_rows = as.integer(unhealthy_purchase_rows),
    trip_rows = as.integer(nrow(trip_level)),
    rows_missing_spending = as.integer(rows_missing_spending)
  )

  message("Source purchase rows: ", format(source_rows, big.mark = ","))
  message("Rows with invalid purchase_date: ", format(invalid_date_rows, big.mark = ","))
  message("Rows excluded by purchase-date year: ", format(excluded_date_rows, big.mark = ","))
  message("Eligible purchase rows retained: ", format(nrow(eligible), big.mark = ","))
  message("Healthier-drink purchase rows: ", format(healthy_purchase_rows, big.mark = ","))
  message("Soft-drink purchase rows: ", format(unhealthy_purchase_rows, big.mark = ","))
  message("Trip rows generated from this source file: ", format(nrow(trip_level), big.mark = ","))

  rm(dt, eligible)
  invisible(gc())

  list(trips = trip_level, audit = audit)
}

# -----------------------------------------------------------------------------
# Main runner
# -----------------------------------------------------------------------------

run_beverage_trip_aggregation <- function() {
  if (file.exists(OUTPUT_FILE) && !RUN_OVERWRITE) {
    stop(
      "Output already exists and RUN_OVERWRITE is FALSE: ", OUTPUT_FILE,
      "\nSet RUN_OVERWRITE <- TRUE to replace it."
    )
  }

  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  # Process one source year at a time. Only the smaller trip-level aggregates
  # are retained in memory across years.
  trip_parts <- vector("list", length(SOURCE_YEARS))
  audit_parts <- vector("list", length(SOURCE_YEARS))

  for (i in seq_along(SOURCE_YEARS)) {
    result <- aggregate_one_source(SOURCE_YEARS[i])
    trip_parts[[i]] <- result$trips
    audit_parts[[i]] <- result$audit
    rm(result)
    invisible(gc())
  }

  trip_parts <- trip_parts[vapply(trip_parts, nrow, integer(1)) > 0L]
  if (!length(trip_parts)) {
    stop("No eligible beverage purchase trips were generated. No final output saved.")
  }

  trip_level <- rbindlist(trip_parts, use.names = TRUE, fill = TRUE)

  # A trip could appear in more than one source-year file. Combine any such
  # records using the actual household, date, trip ID, and purchase-date year.
  trip_level <- trip_level[
    ,
    .(
      beverage_y = as.integer(any(beverage_y == 1L)),
      healthy_beverage_spending = sum(healthy_beverage_spending, na.rm = TRUE),
      unhealthy_beverage_spending = sum(unhealthy_beverage_spending, na.rm = TRUE)
    ),
    by = .(hhid, panel_year, trip_id, purchase_date)
  ]

  # Enforce the target date-year range again after combining source files.
  trip_level <- trip_level[
    panel_year >= TARGET_YEAR_MIN & panel_year <= TARGET_YEAR_MAX
  ]

  setcolorder(trip_level, OUTPUT_COLUMNS)
  setorder(trip_level, panel_year, hhid, purchase_date, trip_id)

  # Save atomically so an interrupted run does not leave a partial final file.
  output_tmp <- paste0(OUTPUT_FILE, ".tmp")
  if (file.exists(output_tmp)) unlink(output_tmp)
  on.exit(if (file.exists(output_tmp)) unlink(output_tmp), add = TRUE)

  fwrite(trip_level, output_tmp, sep = "\t", na = "")

  if (file.exists(OUTPUT_FILE)) unlink(OUTPUT_FILE)
  if (!file.rename(output_tmp, OUTPUT_FILE)) {
    stop("Could not finalize output file: ", OUTPUT_FILE)
  }

  audit <- rbindlist(audit_parts, use.names = TRUE, fill = TRUE)
  audit_file <- file.path(
    OUTPUT_DIR,
    paste0("beverage_trip_aggregation_audit_", TARGET_YEAR_MIN, "_", TARGET_YEAR_MAX, ".tsv")
  )
  fwrite(audit, audit_file, sep = "\t", na = "")

  message("\n============================================================")
  message("Beverage trip-level aggregation complete")
  message("============================================================")
  message("Final trip rows: ", format(nrow(trip_level), big.mark = ","))
  message("Unique households: ", format(uniqueN(trip_level$hhid), big.mark = ","))
  message("Unique trips: ", format(uniqueN(trip_level$trip_id), big.mark = ","))
  message(
    "Trips with healthier drinks (beverage_y = 1): ",
    format(sum(trip_level$beverage_y == 1L), big.mark = ",")
  )
  message(
    "Trips without healthier drinks (beverage_y = 0): ",
    format(sum(trip_level$beverage_y == 0L), big.mark = ",")
  )
  message("Date range: ", min(trip_level$purchase_date), " to ", max(trip_level$purchase_date))
  message("Output: ", OUTPUT_FILE)
  message("Audit: ", audit_file)

  invisible(trip_level)
}

# -----------------------------------------------------------------------------
# Run
# -----------------------------------------------------------------------------

run_beverage_trip_aggregation()
