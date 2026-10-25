# =============================================================================
# Step 02: Aggregate snack purchases to trips, aligned with the FV trip panel.
# Required: install.packages("data.table")
# Optional for READ_MODE="stream": install.packages("readr")
#
# One row per hhid / trip_id / purchase_date, among trips with snack purchases.
# Output: hhid, panel_year, trip_id, purchase_date, snack_y,
#         healthy_snack_spending, unhealthy_snack_spending.
# hhid is the source household_code; trip_id is the source trip_code_uc.
# Both identifiers remain character, including leading zeros and long IDs.
#
# Calendar rule: panel_year ALWAYS comes from purchase_date. Source file years
# and the original panel_year are retained only in diagnostics. Keep 2008-2017.
# Require source files through 2018 to capture late-recorded 2017 purchases.
# If the 2007 source file exists, scan it too for any 2008-dated purchases.
# A purchase dated in 2007 is excluded even when recorded in a later file.
#
# Outcome: 1 = any healthy snack; 2 = only unhealthy snacks. Healthy wins for a
# mixed trip, but spending on both types is retained. This is FV-style coding,
# not a 0/1 dummy. Trips without snack purchases are not created by this step.
# Labels are taken from Step 01; nuts / dried fruit are healthy under broad.
#
# Spending matches the supplied FV aggregation defaults: cap negative net amounts
# at zero, and let missing amounts contribute zero. Both are audited. Set
# USE_CAPPED_SPENDING <- FALSE to retain negative net amounts instead.
# Classification depends on purchase presence, including zero/missing spending.
#
# Speed: read six columns from one annual snack extract with multithreaded fread;
# parse distinct dates once and use optimized grouped sums. Uncompressed RDS
# partials avoid writing and reparsing intermediate TSV text. READ_MODE="stream"
# bounds purchase-row memory by CHUNK_ROWS instead of a whole source year.
# Both modes consolidate one purchase-date year at a time.
# Temporary files are removed on success or failure. No raw Nielsen files are
# needed here; finish Step 01 extraction for the required source years first.
# =============================================================================

DATA_ROOT <- Sys.getenv(
  "HPC_DATA_ROOT", unset = "C:/Users/rwen13/Documents/healthy_purchase_data"
)
RUN_DEFINITION <- "broad"
INPUT_DIR <- file.path(DATA_ROOT, "intermediate", "snack_purchases", RUN_DEFINITION)
OUTPUT_DIR <- file.path(DATA_ROOT, "processed", "snack_trip_level")
TARGET_DATE_YEARS <- 2008:2017
SOURCE_YEARS <- 2008:2018
INCLUDE_PREVIOUS_SOURCE_IF_PRESENT <- TRUE
USE_CAPPED_SPENDING <- TRUE
RUN_OVERWRITE <- FALSE
READ_MODE <- "fread"                 # Fast default; "stream" uses less RAM.
CHUNK_ROWS <- 500000L                 # Used only in streaming mode.
DT_THREADS <- 4L

# Repeated rows WITHIN a source file remain purchases, as in Step 01. If the
# same trip appears in MULTIPLE annual files, stop before committing output:
# purchase totals alone cannot establish whether these are duplicate extracts
# or different purchases. Enable only after verifying disjoint purchase rows.
ALLOW_CROSS_SOURCE_TRIPS <- FALSE

for (pkg in "data.table") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing package: ", pkg, ". Install it before running this script.")
  }
}
suppressPackageStartupMessages(library(data.table))

OUTPUT_COLS <- c("hhid", "panel_year", "trip_id", "purchase_date", "snack_y",
                 "healthy_snack_spending", "unhealthy_snack_spending")
PART_COLS <- c("hhid", "trip_id", "purchase_date", "source_year", "has_healthy",
               "healthy_snack_spending", "unhealthy_snack_spending")

safe_print <- function(x, n = 20L) {
  tryCatch(print(as.data.frame(head(x, n)), row.names = FALSE),
           error = function(e) message("Diagnostic print skipped: ", conditionMessage(e)))
  invisible(NULL)
}

check_years <- function(x, label) {
  if (!length(x) || anyNA(x) || !is.numeric(x) ||
      any(x != as.integer(x)) || any(x < 1900 | x > 2200)) {
    stop(label, " must contain valid integer calendar years.")
  }
  sort(unique(as.integer(x)))
}

parse_date_values <- function(x, path) {
  # Recognize ISO dates (also with a time), YYYY/MM/DD, YYYYMMDD, and US dates.
  # Strip time only after recognizing a date. Round-trip validation rejects
  # impossible dates rather than rolling them into a different month.
  raw <- trimws(as.character(x))
  out <- rep(NA_character_, length(raw))
  patterns <- c(
    "^([0-9]{4})[-/]([0-9]{1,2})[-/]([0-9]{1,2})($|[ T])",
    "^([0-9]{4})([0-9]{2})([0-9]{2})$",
    "^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})($|[ T])"
  )
  for (j in seq_along(patterns)) {
    idx <- which(!is.na(raw) & is.na(out) & grepl(patterns[j], raw))
    if (!length(idx)) next
    # Remove a recognized time suffix before extracting components: sub() on
    # the prefix alone would otherwise retain the remaining timestamp text.
    values <- sub("[ T].*$", "", raw[idx])
    a <- as.integer(sub(patterns[j], "\\1", values))
    b <- as.integer(sub(patterns[j], "\\2", values))
    d <- as.integer(sub(patterns[j], "\\3", values))
    if (j == 1L) values <- gsub("/", "-", values, fixed = TRUE)
    fmt <- c("%Y-%m-%d", "%Y%m%d", "%m/%d/%Y")[[j]]
    parsed <- suppressWarnings(as.IDate(values, format = fmt))
    canonical <- if (j < 3L) sprintf("%04d-%02d-%02d", a, b, d) else
      sprintf("%04d-%02d-%02d", d, a, b)
    normalized <- as.character(parsed)
    valid <- !is.na(parsed) & !is.na(canonical) & normalized == canonical
    out[idx[valid]] <- normalized[valid]
  }
  if (anyNA(out)) {
    examples <- paste(head(unique(raw[is.na(out)]), 5L), collapse = ", ")
    stop("Missing or invalid purchase_date in ", path, "; examples: ", examples)
  }
  out
}

parse_purchase_date <- function(x, path) {
  # Annual files repeat a small number of dates across millions of purchases.
  # Validate every distinct value once, including all out-of-window dates.
  values <- unique(x)
  parse_date_values(values, path)[match(x, values)]
}

strict_fread <- function(...) {
  # fread can return a prefix after warning about malformed input. Reject that
  # result after native reader cleanup; never silently use a truncated file.
  messages <- character()
  ans <- withCallingHandlers(fread(...), warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  if (length(messages)) stop("TSV parsing problem: ", paste(messages, collapse = "\n"))
  ans
}

parse_spending <- function(x, path) {
  out <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & (is.na(out) | !is.finite(out)))) {
    stop("Invalid non-missing final_price_paid in ", path)
  }
  out
}

assert_id <- function(x, name, path) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x)))) {
    stop("Missing/invalid character ", name, " in ", path)
  }
}

commit_file <- function(staged, destination, overwrite) {
  # Preserve the previous completed result until the new file is ready.
  backup <- NULL
  if (file.exists(destination)) {
    if (!overwrite) stop("Output exists: ", destination, "; set RUN_OVERWRITE <- TRUE.")
    backup <- tempfile("previous_snack_", tmpdir = dirname(destination))
    if (!file.rename(destination, backup)) stop("Cannot move existing output; close it in other apps.")
  }
  if (!file.rename(staged, destination)) {
    if (!is.null(backup) && !file.rename(backup, destination)) {
      stop("Commit failed; previous output remains at: ", backup)
    }
    stop("Cannot commit output: ", destination)
  }
  if (!is.null(backup)) unlink(backup)
  invisible(destination)
}

aggregate_chunk <- function(x, source_year, target_years, path, cap_spending) {
  # Parse all dates before filtering; do not silently omit an unknown date.
  x[, purchase_date := parse_purchase_date(purchase_date, path)]
  x[, date_year := as.integer(substr(purchase_date, 1L, 4L))]
  n_input <- nrow(x)
  n_before <- sum(x$date_year < min(target_years))
  n_after <- sum(x$date_year > max(target_years))
  n_other <- sum(!x$date_year %in% target_years) - n_before - n_after
  x <- x[date_year %in% target_years]
  n_kept <- nrow(x)
  n_reassigned_source <- sum(x$date_year != source_year)
  n_original_panel_missing <- sum(is.na(x$panel_year))
  n_reassigned_panel <- sum(as.character(x$date_year) != x$panel_year, na.rm = TRUE)
  if (n_kept) {
    assert_id(x$household_code, "household_code", path)
    assert_id(x$trip_code_uc, "trip_code_uc", path)
    if (anyNA(x$snack_health_label) ||
        any(!x$snack_health_label %chin% c("healthy_snack", "unhealthy_snack"))) {
      stop("Unknown snack_health_label in ", path)
    }
  }
  spend <- parse_spending(x$final_price_paid, path)
  n_missing <- sum(is.na(spend))
  n_negative <- sum(spend < 0, na.rm = TRUE)
  net_observed <- sum(spend, na.rm = TRUE)
  if (cap_spending) spend <- pmax(spend, 0)
  spend[is.na(spend)] <- 0
  x[, `:=`(hhid = household_code, trip_id = trip_code_uc,
           healthy = as.integer(snack_health_label == "healthy_snack"), spend_used = spend)]
  # Precompute contributions so data.table can use its optimized group engine.
  # Integer presence is independent of spending, including zero and missing.
  x[, `:=`(healthy_piece = fifelse(healthy == 1L, spend_used, 0),
           unhealthy_piece = fifelse(healthy == 0L, spend_used, 0))]
  partial <- if (n_kept) x[, .(
    has_healthy = max(healthy),
    healthy_snack_spending = sum(healthy_piece),
    unhealthy_snack_spending = sum(unhealthy_piece)
  ), by = .(hhid, trip_id, purchase_date, date_year)] else data.table(
    hhid = character(), trip_id = character(), purchase_date = character(),
    date_year = integer(), has_healthy = integer(), healthy_snack_spending = numeric(),
    unhealthy_snack_spending = numeric()
  )
  partial[, source_year := as.integer(source_year)]
  counts <- data.table(
    input_rows = n_input, kept_rows = n_kept, excluded_before_window = n_before,
    excluded_after_window = n_after, excluded_other_years = n_other,
    reassigned_from_source_year = n_reassigned_source,
    reassigned_from_original_panel_year = n_reassigned_panel,
    missing_original_panel_year = n_original_panel_missing,
    missing_net_spending_rows = n_missing, negative_net_spending_rows = n_negative,
    uncapped_net_spending_observed = net_observed,
    healthy_spending_used = sum(x$healthy_piece),
    unhealthy_spending_used = sum(x$unhealthy_piece)
  )
  list(partial = partial, counts = counts)
}

consolidate_year <- function(parts, year, allow_cross_source) {
  # Chunk boundaries may split a trip. Always combine all partial totals first.
  # Conflicting dates for the same household/trip within a year require review.
  dates <- unique(parts[, .(hhid, trip_id, purchase_date)])
  if (anyDuplicated(dates, by = c("hhid", "trip_id"))) {
    stop("Conflicting purchase dates for a trip in ", year)
  }
  rm(dates)
  sources <- unique(parts[, .(hhid, trip_id, source_year)])
  repeated <- duplicated(sources, by = c("hhid", "trip_id"))
  overlaps <- uniqueN(sources[repeated, .(hhid, trip_id)])
  if (overlaps && !allow_cross_source) {
    safe_print(sources[repeated], n = 5L)
    stop(overlaps, " trips in ", year, " occur in multiple source files. Check for duplicate ",
         "extracts. Set ALLOW_CROSS_SOURCE_TRIPS <- TRUE only if their purchase rows are disjoint.")
  }
  rm(sources, repeated)
  out <- parts[, .(
    has_healthy = max(has_healthy),
    healthy_snack_spending = sum(healthy_snack_spending),
    unhealthy_snack_spending = sum(unhealthy_snack_spending)
  ), by = .(hhid, trip_id, purchase_date)]
  out[, snack_y := fifelse(has_healthy > 0L, 1L, 2L)]
  out[, has_healthy := NULL]
  out[, panel_year := as.integer(year)]
  setcolorder(out, OUTPUT_COLS)
  setorderv(out, c("hhid", "purchase_date", "trip_id"))
  if (any(!is.finite(out$healthy_snack_spending)) ||
      any(!is.finite(out$unhealthy_snack_spending))) stop("Non-finite trip spending in ", year)
  list(data = out, cross_source_trips = overlaps)
}

run_batch_snack_trip <- function(target_years = TARGET_DATE_YEARS,
                                source_years = SOURCE_YEARS,
                                input_dir = INPUT_DIR, output_dir = OUTPUT_DIR,
                                overwrite = RUN_OVERWRITE, chunk_rows = CHUNK_ROWS,
                                cap_spending = USE_CAPPED_SPENDING,
                                allow_cross_source = ALLOW_CROSS_SOURCE_TRIPS,
                                include_previous = INCLUDE_PREVIOUS_SOURCE_IF_PRESENT,
                                read_mode = READ_MODE) {
  read_mode <- match.arg(read_mode, c("fread", "stream"))
  if (read_mode == "stream" && !requireNamespace("readr", quietly = TRUE)) {
    stop("Streaming mode requires readr: install.packages(\"readr\").")
  }
  target_years <- check_years(target_years, "target_years")
  source_years <- check_years(source_years, "source_years")
  if (length(chunk_rows) != 1L || is.na(chunk_rows) || chunk_rows < 1 ||
      chunk_rows != as.integer(chunk_rows)) stop("chunk_rows must be a positive integer.")
  # Include each target's next annual file, including the final boundary year.
  required <- sort(unique(c(target_years, target_years + 1L)))
  if (length(setdiff(required, source_years))) {
    stop("source_years must include target years and their next years: ",
         paste(required, collapse = ", "))
  }
  source_path <- function(y) file.path(input_dir, paste0("snack_purchases_", y, ".tsv"))
  previous <- min(target_years) - 1L
  if (include_previous && file.exists(source_path(previous))) {
    source_years <- sort(unique(c(previous, source_years)))
  }
  paths <- vapply(source_years, source_path, character(1L))
  if (any(!file.exists(paths))) {
    stop("Missing snack extraction files. Run Step 01 for these source years first:\n",
         paste(paths[!file.exists(paths)], collapse = "\n"),
         "\nInclude 2018 for a complete 2008-2017 purchase-date panel.")
  }
  required_cols <- c("household_code", "trip_code_uc", "purchase_date", "panel_year",
                     "snack_health_label", "final_price_paid")
  # Preflight every header before beginning a long run.
  for (path in paths) {
    header <- names(strict_fread(file = path, sep = "\t", quote = '"',
                                 nrows = 0L, header = TRUE, showProgress = FALSE))
    if (anyDuplicated(header) || length(setdiff(required_cols, header))) {
      stop("Missing/duplicated required columns in ", path)
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tag <- paste0(min(target_years), "_", max(target_years))
  destination <- file.path(output_dir, paste0("snack_trip_level_", tag, ".tsv"))
  if (file.exists(destination) && !overwrite) stop("Output exists: ", destination,
                                                "; set RUN_OVERWRITE <- TRUE.")
  work_dir <- tempfile("snack_trip_work_", tmpdir = output_dir)
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  old_threads <- getDTthreads()
  on.exit(setDTthreads(old_threads), add = TRUE)
  setDTthreads(DT_THREADS)
  started <- proc.time()[[3L]]
  source_audits <- list()
  partial_files <- setNames(vector("list", length(target_years)), as.character(target_years))
  spec <- if (read_mode == "stream") do.call(readr::cols_only, setNames(
    lapply(required_cols, function(z) readr::col_character()), required_cols
  )) else NULL

  for (i in seq_along(source_years)) {
    year <- source_years[[i]]
    path <- paths[[i]]
    message("Reading source year ", year, " [", read_mode, "] ...")
    source_started <- proc.time()[[3L]]
    audit <- NULL
    part_number <- 0L
    consume <- function(chunk) {
      result <- aggregate_chunk(chunk, year, target_years, path, cap_spending)
      audit <<- if (is.null(audit)) result$counts else
        as.data.table(Map(`+`, audit, result$counts))
      p <- result$partial
      part_number <<- part_number + 1L
      for (y in unique(p$date_year)) {
        f <- file.path(work_dir, paste0("partial_", y, "_", year, "_", part_number, ".rds"))
        saveRDS(p[date_year == y, ..PART_COLS], f, compress = FALSE)
        key <- as.character(y)
        partial_files[[key]] <<- c(partial_files[[key]], f)
      }
      invisible(NULL)
    }
    # Step 01 (including ultrafast) writes quote="auto" with fwrite. Standard
    # quote handling is required HERE, even though its raw inputs use quote="".
    # Unneeded product descriptions are skipped; IDs are parsed as character.
    if (read_mode == "fread") {
      x <- strict_fread(file = path, sep = "\t", header = TRUE, skip = 0L,
                        select = required_cols, colClasses = "character", quote = '"',
                        na.strings = c("", "NA"), strip.white = FALSE,
                        encoding = "UTF-8", showProgress = TRUE)
      consume(x)
      rm(x)
    } else {
      callback <- readr::SideEffectChunkCallback$new(function(chunk, pos) {
        problems <- readr::problems(chunk)
        if (nrow(problems)) {
          safe_print(problems, n = 5L)
          stop("TSV parsing problem in ", path, "; chunk starts at data row ", pos)
        }
        consume(as.data.table(chunk))
      })
      readr::read_tsv_chunked(path, callback = callback, chunk_size = chunk_rows,
                              col_types = spec, quote = '"', trim_ws = FALSE,
                              locale = readr::locale(encoding = "UTF-8"),
                              na = c("", "NA"), skip_empty_rows = FALSE, progress = FALSE)
    }
    if (is.null(audit)) {
      empty <- as.data.table(setNames(rep(list(character()), length(required_cols)), required_cols))
      audit <- aggregate_chunk(empty, year, target_years, path, cap_spending)$counts
    }
    audit[, `:=`(source_file_year = year, read_mode = read_mode,
                 elapsed_seconds = proc.time()[[3L]] - source_started)]
    source_audits[[i]] <- audit
    message("  Rows read: ", audit$input_rows, "; kept by purchase date: ", audit$kept_rows,
            "; seconds: ", round(audit$elapsed_seconds, 1L))
    gc(verbose = FALSE)
  }
  audit <- rbindlist(source_audits)
  staged <- file.path(work_dir, "snack_trip_level.tsv")
  empty_out <- data.table(hhid = character(), panel_year = integer(), trip_id = character(),
                          purchase_date = character(), snack_y = integer(),
                          healthy_snack_spending = numeric(), unhealthy_snack_spending = numeric())
  fwrite(empty_out, staged, sep = "\t", quote = "auto", na = "NA")
  summaries <- list()
  for (year in target_years) {
    files <- partial_files[[as.character(year)]]
    out <- copy(empty_out)
    overlaps <- 0L
    if (length(files)) {
      p <- rbindlist(lapply(files, readRDS), use.names = TRUE)
      result <- consolidate_year(p, year, allow_cross_source)
      out <- result$data
      overlaps <- result$cross_source_trips
      rm(p, result)
      unlink(files)
    }
    fwrite(out, staged, sep = "\t", append = TRUE, col.names = FALSE, quote = "auto", na = "NA")
    summaries[[length(summaries) + 1L]] <- data.table(
      panel_year = year, n_trips = nrow(out), n_households = uniqueN(out$hhid),
      healthy_trips = sum(out$snack_y == 1L), unhealthy_only_trips = sum(out$snack_y == 2L),
      healthy_snack_spending = sum(out$healthy_snack_spending),
      unhealthy_snack_spending = sum(out$unhealthy_snack_spending),
      min_purchase_date = if (nrow(out)) min(out$purchase_date) else NA_character_,
      max_purchase_date = if (nrow(out)) max(out$purchase_date) else NA_character_,
      cross_source_trips = overlaps
    )
    message("Consolidated purchase-date year ", year, ": ", nrow(out), " snack trips.")
    rm(out)
    gc(verbose = FALSE)
  }
  summary <- rbindlist(summaries)
  # Reconcile spending after all chunk/year reductions, before replacing output.
  if (!isTRUE(all.equal(sum(summary$healthy_snack_spending),
                       sum(audit$healthy_spending_used), tolerance = 1e-8)) ||
      !isTRUE(all.equal(sum(summary$unhealthy_snack_spending),
                       sum(audit$unhealthy_spending_used), tolerance = 1e-8))) {
    stop("Spending reconciliation failed; output not committed.")
  }
  commit_file(staged, destination, overwrite)
  message("SAVED: ", destination)
  # Optional audit writes/printing cannot prevent saving the completed dataset.
  tryCatch({
    audit[, `:=`(capped_spending = cap_spending, snack_definition = RUN_DEFINITION)]
    fwrite(audit, file.path(output_dir, paste0("snack_trip_level_source_audit_", tag, ".tsv")), sep = "\t")
    fwrite(summary, file.path(output_dir, paste0("snack_trip_level_summary_", tag, ".tsv")), sep = "\t")
    safe_print(summary, n = length(target_years))
    if (any(summary$n_trips == 0L)) message("Review summary: some target years contain no snack trips.")
    message("Elapsed seconds: ", round(proc.time()[[3L]] - started, 1L))
  }, error = function(e) message("Data saved; optional diagnostics failed: ", conditionMessage(e)))
  invisible(list(output = destination, source_audit = audit, summary = summary))
}

# RStudio: source this file, then call run_batch_snack_trip().
# Optional 2008-only test (requires the 2008 and 2009 extraction files):
# run_batch_snack_trip(target_years = 2008L, source_years = 2008:2009)
# Full run: run_batch_snack_trip() using the settings above.
# Lower-memory run: run_batch_snack_trip(read_mode = "stream")
# Command line: Rscript data_cleaning/get_category_purchase/02_aggregate_snack_trip_level.R
if (sys.nframe() == 0L) run_batch_snack_trip()
