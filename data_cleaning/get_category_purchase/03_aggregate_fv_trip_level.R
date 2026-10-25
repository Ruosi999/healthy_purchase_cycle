# Step 03: Build a trip-level fresh / non-fresh fruit-and-vegetable dataset.
#
# Inputs (restricted intermediate data; never stored in the Git repository):
#   1. produce_purchases/fv_purchases_{source_year}.tsv
#   2. magnet_food_purchases/magnet_food_purchases_{source_year}.tsv
#
# Date rule:
#   Keep purchase dates in 2008-2017, regardless of the source file year or
#   the original panel_year. Source files through 2018 are required because
#   purchases dated in late 2017 may be recorded in the 2018 annual file.
#
# Classification:
#   - Non-Magnet fv_class == 1: fresh FV.
#   - Non-Magnet fv_class in {2, 3}: non-fresh FV (frozen / canned).
#   - Magnet is_magnet_fresh_fv == 1: fresh FV. All Magnet FV is treated as fresh.
#   - If a trip contains both types, fv_y = 1 (fresh takes priority); spending
#     is still reported separately for fresh and non-fresh FV.
#
# Spending:
#   By default, use non-negative coupon-adjusted spending. Set
#   USE_CAPPED_SPENDING <- FALSE to use the uncapped net spending instead.
#   Missing spending values contribute zero, but purchase-row presence still
#   determines the trip's fv_y classification.
#
# Join keys / identifiers are kept as character strings. In particular, do not
#   convert trip_code_uc or household_code to numeric.

suppressPackageStartupMessages(library(data.table))

# -----------------------------------------------------------------------------
# Run settings: edit this section if needed.
# -----------------------------------------------------------------------------
SOURCE_YEARS <- 2008:2018       # Include 2018 to capture 2017-dated purchases.
TARGET_DATE_YEARS <- 2008:2017  # Final sample is selected by purchase_date year.
RUN_OVERWRITE <- FALSE          # TRUE replaces existing output files.
USE_CAPPED_SPENDING <- TRUE     # TRUE: truncate negative net spending at zero.

DATA_ROOT <- Sys.getenv(
  "HPC_DATA_ROOT",
  unset = "C:/Users/rwen13/Documents/healthy_purchase_data"
)
PRODUCE_DIR <- file.path(DATA_ROOT, "intermediate", "produce_purchases")
MAGNET_DIR <- file.path(DATA_ROOT, "intermediate", "magnet_food_purchases")
OUTPUT_DIR <- file.path(DATA_ROOT, "processed", "fv_trip_level")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "fv_trip_level_2008_2017.tsv")
SOURCE_AUDIT_FILE <- file.path(OUTPUT_DIR, "fv_trip_level_source_audit_2008_2017.tsv")
SUMMARY_AUDIT_FILE <- file.path(OUTPUT_DIR, "fv_trip_level_summary_2008_2017.tsv")

TRIP_KEYS <- c("household_code", "purchase_date", "panel_year", "trip_id")

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

read_selected <- function(path, columns, character_columns) {
  # Read only needed columns to keep peak memory use low.
  header <- names(fread(path, nrows = 0L, showProgress = FALSE))
  missing <- setdiff(columns, header)
  if (length(missing)) {
    stop("Input file is missing required columns: ", paste(missing, collapse = ", "),
         "\nFile: ", path)
  }
  fread(
    path,
    select = columns,
    colClasses = list(character = intersect(character_columns, columns)),
    showProgress = FALSE
  )
}

parse_purchase_date <- function(x, path) {
  # Support common Nielsen date encodings while returning a Date vector.
  raw <- trimws(as.character(x))
  blank <- is.na(raw) | raw == "" | toupper(raw) %in% c("NA", "N/A", "NULL")
  out <- as.Date(rep(NA_character_, length(raw)))

  # ISO-like dates: YYYY-MM-DD, YYYY/MM/DD, optionally followed by a time.
  idx <- !blank & grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}", raw)
  if (any(idx)) {
    normalized <- gsub("/", "-", substr(raw[idx], 1L, 10L), fixed = TRUE)
    out[idx] <- as.Date(normalized, format = "%Y-%m-%d")
  }

  # Compact YYYYMMDD.
  idx <- is.na(out) & !blank & grepl("^\\d{8}$", raw)
  if (any(idx)) out[idx] <- as.Date(raw[idx], format = "%Y%m%d")

  # US-style MM/DD/YYYY, optionally followed by a time.
  idx <- is.na(out) & !blank & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", raw)
  if (any(idx)) out[idx] <- as.Date(substr(raw[idx], 1L, 10L), format = "%m/%d/%Y")

  # Fail rather than silently dropping non-empty dates that could not be parsed.
  invalid <- !blank & is.na(out)
  if (any(invalid)) {
    examples <- paste(utils::head(unique(raw[invalid]), 5L), collapse = ", ")
    stop("Could not parse non-empty purchase_date value(s) in ", path,
         ". Example(s): ", examples)
  }

  list(date = out, missing_count = sum(blank), invalid_count = sum(invalid))
}

empty_trip_summary <- function() {
  data.table(
    household_code = character(), purchase_date = character(), panel_year = integer(),
    trip_id = character(), fresh_spending = numeric(), non_fresh_spending = numeric(),
    has_fresh = logical(), has_non_fresh = logical()
  )
}

summarize_trip_rows <- function(dt, source_type, source_year, input_rows,
                                missing_date_rows, date_window_rows,
                                eligible_rows, spend_column) {
  # The input is already restricted to eligible FV rows and target date years.
  if (!nrow(dt)) {
    summary <- empty_trip_summary()
    total_spend <- 0
  } else {
    # Keep identifiers as strings and derive panel_year from purchase_date.
    dt[, household_code := as.character(household_code)]
    dt[, trip_id := as.character(trip_code_uc)]
    dt[, panel_year := as.integer(format(as.Date(purchase_date), "%Y"))]

    if (anyNA(dt$household_code) || any(dt$household_code == "") ||
        anyNA(dt$trip_id) || any(dt$trip_id == "")) {
      stop(source_type, " input for source year ", source_year,
           " has missing household_code or trip_code_uc among eligible records.")
    }

    spending <- suppressWarnings(as.numeric(dt[[spend_column]]))
    if (USE_CAPPED_SPENDING) spending <- pmax(spending, 0)
    spending[is.na(spending) | !is.finite(spending)] <- 0
    dt[, spend_used := spending]

    # Magnet records are always fresh. Non-Magnet rows use fv_class to decide.
    if (source_type == "produce") {
      dt[, has_fresh_row := as.integer(fv_class) == 1L]
      dt[, has_non_fresh_row := as.integer(fv_class) %in% c(2L, 3L)]
    } else {
      dt[, has_fresh_row := TRUE]
      dt[, has_non_fresh_row := FALSE]
    }

    dt[, fresh_piece := fifelse(has_fresh_row, spend_used, 0)]
    dt[, non_fresh_piece := fifelse(has_non_fresh_row, spend_used, 0)]

    summary <- dt[, .(
      fresh_spending = sum(fresh_piece),
      non_fresh_spending = sum(non_fresh_piece),
      has_fresh = any(has_fresh_row),
      has_non_fresh = any(has_non_fresh_row)
    ), by = TRIP_KEYS]
    total_spend <- sum(summary$fresh_spending) + sum(summary$non_fresh_spending)
  }

  audit <- data.table(
    source_year = as.integer(source_year),
    source_type = source_type,
    input_rows = as.numeric(input_rows),
    missing_purchase_date_rows = as.numeric(missing_date_rows),
    rows_with_target_date_year = as.numeric(date_window_rows),
    eligible_fv_purchase_rows = as.numeric(eligible_rows),
    trip_rows_before_final_merge = as.numeric(nrow(summary)),
    spending_sum = as.numeric(total_spend)
  )
  list(summary = summary, audit = audit)
}

process_produce_file <- function(source_year, path) {
  required <- c("household_code", "trip_code_uc", "purchase_date", "fv_class", "final_price_paid")
  dt <- read_selected(path, required,
                      character_columns = c("household_code", "trip_code_uc", "purchase_date", "fv_class", "final_price_paid"))
  input_rows <- nrow(dt)
  parsed <- parse_purchase_date(dt$purchase_date, path)
  missing_date_rows <- parsed$missing_count
  if (missing_date_rows > 0L) {
    warning(source_year, " produce file has ", missing_date_rows,
            " rows with missing purchase_date; these rows cannot be assigned to a date year.")
  }
  dt[, parsed_date := parsed$date]
  dt <- dt[!is.na(parsed_date)]
  dt[, purchase_year := as.integer(format(parsed_date, "%Y"))]
  dt <- dt[purchase_year %in% TARGET_DATE_YEARS]
  date_window_rows <- nrow(dt)
  if (nrow(dt)) dt[, purchase_date := format(parsed_date, "%Y-%m-%d")]

  dt[, fv_class_int := suppressWarnings(as.integer(as.character(fv_class)))]
  dt <- dt[fv_class_int %in% c(1L, 2L, 3L)]
  eligible_rows <- nrow(dt)
  dt[, fv_class := fv_class_int]
  dt[, final_price_paid := suppressWarnings(as.numeric(final_price_paid))]

  # Non-fresh spending includes frozen and canned FV (classes 2 and 3).
  result <- summarize_trip_rows(
    dt = dt, source_type = "produce", source_year = source_year,
    input_rows = input_rows, missing_date_rows = missing_date_rows,
    date_window_rows = date_window_rows, eligible_rows = eligible_rows,
    spend_column = "final_price_paid"
  )
  rm(dt); gc(verbose = FALSE)
  result
}

process_magnet_file <- function(source_year, path) {
  required <- c("household_code", "trip_code_uc", "purchase_date", "is_magnet_fresh_fv")
  spend_column <- if (USE_CAPPED_SPENDING) {
    "magnet_fresh_fv_spend_increment_capped"
  } else {
    "magnet_fresh_fv_spend_increment"
  }
  required <- c(required, spend_column)
  dt <- read_selected(path, required,
                      character_columns = c("household_code", "trip_code_uc", "purchase_date", "is_magnet_fresh_fv", spend_column))
  input_rows <- nrow(dt)
  parsed <- parse_purchase_date(dt$purchase_date, path)
  missing_date_rows <- parsed$missing_count
  if (missing_date_rows > 0L) {
    warning(source_year, " Magnet file has ", missing_date_rows,
            " rows with missing purchase_date; these rows cannot be assigned to a date year.")
  }
  dt[, parsed_date := parsed$date]
  dt <- dt[!is.na(parsed_date)]
  dt[, purchase_year := as.integer(format(parsed_date, "%Y"))]
  dt <- dt[purchase_year %in% TARGET_DATE_YEARS]
  date_window_rows <- nrow(dt)
  if (nrow(dt)) dt[, purchase_date := format(parsed_date, "%Y-%m-%d")]

  # Every Magnet FV row is fresh, but only rows flagged as Magnet fresh FV enter.
  dt[, fresh_flag := suppressWarnings(as.integer(as.character(is_magnet_fresh_fv)))]
  dt <- dt[!is.na(fresh_flag) & fresh_flag == 1L]
  eligible_rows <- nrow(dt)
  dt[, (spend_column) := suppressWarnings(as.numeric(get(spend_column)))]

  result <- summarize_trip_rows(
    dt = dt, source_type = "magnet", source_year = source_year,
    input_rows = input_rows, missing_date_rows = missing_date_rows,
    date_window_rows = date_window_rows, eligible_rows = eligible_rows,
    spend_column = spend_column
  )
  rm(dt); gc(verbose = FALSE)
  result
}

write_atomic <- function(x, path, overwrite = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".partial")
  if (file.exists(temporary)) unlink(temporary)
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  fwrite(x, temporary, sep = "\t", na = "")
  if (file.exists(path)) {
    if (!overwrite) stop("Output exists (set RUN_OVERWRITE <- TRUE to replace): ", path)
    unlink(path)
  }
  if (!file.rename(temporary, path)) stop("Could not finalize output file: ", path)
  invisible(path)
}

# -----------------------------------------------------------------------------
# Main pipeline
# -----------------------------------------------------------------------------

run_fv_trip_aggregation <- function() {
  if (!all(TARGET_DATE_YEARS %in% SOURCE_YEARS)) {
    stop("SOURCE_YEARS must include every TARGET_DATE_YEARS value.")
  }
  if (!length(SOURCE_YEARS) || anyDuplicated(SOURCE_YEARS)) {
    stop("SOURCE_YEARS must be a non-empty vector without duplicates.")
  }
  if (file.exists(OUTPUT_FILE) && !RUN_OVERWRITE) {
    stop("Output exists (set RUN_OVERWRITE <- TRUE to replace): ", OUTPUT_FILE)
  }

  # Require all source-year files before processing so missing 2018 files cannot
  # silently drop purchases dated in 2017 but recorded in the 2018 annual files.
  expected_files <- unlist(lapply(SOURCE_YEARS, function(y) c(
    file.path(PRODUCE_DIR, sprintf("fv_purchases_%d.tsv", y)),
    file.path(MAGNET_DIR, sprintf("magnet_food_purchases_%d.tsv", y))
  )), use.names = FALSE)
  missing_files <- expected_files[!file.exists(expected_files)]
  if (length(missing_files)) {
    stop(
      "Missing required intermediate file(s). Run Step 01 and Step 02 for source years ",
      paste(SOURCE_YEARS, collapse = ", "),
      ". In particular, 2018 source files are needed to capture purchases dated in 2017.\n",
      paste(missing_files, collapse = "\n")
    )
  }

  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  if (!RUN_OVERWRITE && any(file.exists(c(SOURCE_AUDIT_FILE, SUMMARY_AUDIT_FILE)))) {
    stop("One or more audit output files already exist. Set RUN_OVERWRITE <- TRUE to replace outputs.")
  }

  message("Source years: ", paste(SOURCE_YEARS, collapse = ", "))
  message("Keeping purchase_date years: ", paste(TARGET_DATE_YEARS, collapse = ", "))
  message("Capped spending: ", USE_CAPPED_SPENDING)

  trip_parts <- list()
  audit_parts <- list()
  part_index <- 0L

  for (source_year in SOURCE_YEARS) {
    produce_path <- file.path(PRODUCE_DIR, sprintf("fv_purchases_%d.tsv", source_year))
    magnet_path <- file.path(MAGNET_DIR, sprintf("magnet_food_purchases_%d.tsv", source_year))

    message("Aggregating non-Magnet FV source file: ", source_year)
    produce_result <- process_produce_file(source_year, produce_path)
    if (nrow(produce_result$summary)) {
      part_index <- part_index + 1L
      trip_parts[[part_index]] <- produce_result$summary
    }
    audit_parts[[length(audit_parts) + 1L]] <- produce_result$audit
    rm(produce_result); gc(verbose = FALSE)

    message("Aggregating Magnet FV source file: ", source_year)
    magnet_result <- process_magnet_file(source_year, magnet_path)
    if (nrow(magnet_result$summary)) {
      part_index <- part_index + 1L
      trip_parts[[part_index]] <- magnet_result$summary
    }
    audit_parts[[length(audit_parts) + 1L]] <- magnet_result$audit
    rm(magnet_result); gc(verbose = FALSE)
  }

  if (!length(trip_parts)) stop("No eligible FV purchase rows found in the target date window.")

  # Only trip-level summaries are combined here, not purchase-level records.
  trip_data <- rbindlist(trip_parts, use.names = TRUE, fill = TRUE)
  rm(trip_parts); gc(verbose = FALSE)
  trip_data <- trip_data[, .(
    fresh_spending = sum(fresh_spending),
    non_fresh_spending = sum(non_fresh_spending),
    has_fresh = any(has_fresh),
    has_non_fresh = any(has_non_fresh)
  ), by = TRIP_KEYS]

  # Fresh takes priority when a trip contains both fresh and non-fresh FV.
  trip_data[, fv_y := fifelse(has_fresh, 1L,
                              fifelse(has_non_fresh, 2L, NA_integer_))]
  trip_data <- trip_data[!is.na(fv_y)]
  setcolorder(trip_data, c("household_code", "purchase_date", "panel_year", "trip_id",
                           "fv_y", "fresh_spending", "non_fresh_spending"))
  setorder(trip_data, purchase_date, household_code, trip_id)

  # Derive compact summary diagnostics before writing the final dataset.
  summary_audit <- trip_data[, .(
    trips = .N,
    trips_with_fresh = sum(fv_y == 1L),
    trips_nonfresh_only = sum(fv_y == 2L),
    trips_with_both_types = sum(has_fresh & has_non_fresh),
    fresh_spending = sum(fresh_spending),
    non_fresh_spending = sum(non_fresh_spending)
  ), by = panel_year][order(panel_year)]

  source_audit <- rbindlist(audit_parts, use.names = TRUE, fill = TRUE)
  setorder(source_audit, source_year, source_type)

  # Keep the final production file to exactly the seven requested columns.
  output_columns <- c("household_code", "purchase_date", "panel_year", "trip_id",
                      "fv_y", "fresh_spending", "non_fresh_spending")
  final_output <- trip_data[, ..output_columns]

  write_atomic(final_output, OUTPUT_FILE, overwrite = RUN_OVERWRITE)
  write_atomic(source_audit, SOURCE_AUDIT_FILE, overwrite = RUN_OVERWRITE)
  write_atomic(summary_audit, SUMMARY_AUDIT_FILE, overwrite = RUN_OVERWRITE)

  message("Finished trip-level FV aggregation.")
  message("Trips: ", nrow(trip_data))
  message("Purchase-date years: ", min(trip_data$panel_year), "-", max(trip_data$panel_year))
  message("Fresh spending: ", round(sum(trip_data$fresh_spending), 2))
  message("Non-fresh spending: ", round(sum(trip_data$non_fresh_spending), 2))
  message("Output: ", OUTPUT_FILE)
  invisible(trip_data)
}

# Run from the repository root or source this file and call the function.
# Output is written to healthy_purchase_data/processed/fv_trip_level/, outside Git.
if (sys.nframe() == 0L) {
  run_fv_trip_aggregation()
}
