# ============================================================
# 03_construct_seven_life_events.R
# Healthy Purchase Cycle | NielsenIQ HMS | 2008-2017
# ============================================================
# PURPOSE
#   Construct seven year-to-year life events from the cleaned,
#   calendar-year-aligned household-year panel created in Step 02.
#
# INPUT
#   processed/demographics/household_demographics_2007_2017.tsv
#   Unique key: hhid + panel_year (hhid must remain character).
#
# TIMING
#   Step 02 has already aligned non-income demographics from
#   reporting year t+1 to calendar year t and income from t+2 to t.
#   DO NOT shift these data again. Events compare adjacent target
#   calendar years (t-1, t). 2007 is used only as history for 2008.
#
# EVENTS
#   event_new_child:        no children -> children, OR no under-6
#                           children -> under-6 children (proxy for
#                           a new young child; not verified birth).
#   getting_married:        nonmarried -> married.
#   getting_divorce_or_separation: married -> nonmarried
#                           (proxy; may include widowhood unless distinguished).
#   moving:                 interregional move detected by a change
#                           in Nielsen Region_Cd (nine broad regions).
#   getting_job:            no employed head -> >=1 employed head.
#   retirement:             employed -> no employed head, with a
#                           head aged 65+ in target year.
#   other_unemployment:     employed -> no employed head, with no
#                           head aged 65+ in target year.
#
# MISSINGNESS
#   Event = NA if years are not consecutive or status cannot be
#   established. Zero only means a valid comparison without event.
#   ZIP is not used to define moving. If either adjacent-year
#   Region_Cd is missing or invalid, moving is NA.
#
# No household exclusions are applied in Step 03.
# All restricted outputs stay outside the GitHub repository.
# ============================================================

library(data.table)

# 1. Inputs and outputs ------------------------------------------------------
data_root <- "C:/Users/rwen13/Documents/healthy_purchase_data"
input_dir <- file.path(data_root, "processed", "demographics")
input_file <- file.path(input_dir, "household_demographics_2007_2017.tsv")
output_file <- file.path(input_dir, "household_demographics_with_events_2008_2017.tsv")
audit_file <- file.path(input_dir, "life_event_summary_by_year.csv")
validation_file <- file.path(input_dir, "life_event_validation_summary.csv")
source_years <- 2007L:2017L
analysis_years <- 2008L:2017L

if (!file.exists(input_file)) stop("Missing Step 02 output: ", input_file)

# 2. Read only the necessary fields -----------------------------------------
header <- names(fread(input_file, sep = "\t", nrows = 0L))
required <- c(
  "hhid", "panel_year", "has_kids", "is_married", "household_employed",
  "household_retired", "household_other_unemployed",
  "Age_And_Presence_Of_Children", "Region_Cd"
)
missing_required <- setdiff(required, header)
if (length(missing_required)) {
  stop("Step 02 output lacks required columns: ", paste(missing_required, collapse = ", "))
}
# Read full panel to preserve all controls and raw fields in the output.
# Force IDs and region labels to character; preserve raw codes.
dt <- fread(
  input_file, sep = "\t",
  colClasses = list(character = intersect(c("hhid", "Panelist_ZipCd", "Region_Cd"), header)),
  showProgress = TRUE
)
dt[, `:=`(hhid = trimws(hhid), panel_year = as.integer(panel_year))]
if (anyNA(dt$hhid) || any(!nzchar(dt$hhid)) || anyNA(dt$panel_year)) {
  stop("Missing household identifiers or panel years.")
}
if (anyDuplicated(dt, by = c("hhid", "panel_year"))) {
  stop("Duplicate household-year records in Step 02 output.")
}
if (any(!dt$panel_year %in% source_years)) {
  stop("Unexpected year in Step 02 input.")
}
setorder(dt, hhid, panel_year)

# 3. Validate / standardize state variables ---------------------------------
valid_binary <- function(x, name) {
  v <- suppressWarnings(as.integer(x))
  if (any(!is.na(v) & !v %in% 0L:1L)) stop("Invalid binary status in ", name)
  v
}
for (v in c("has_kids", "is_married", "household_employed",
            "household_retired", "household_other_unemployed")) {
  dt[, (v) := valid_binary(get(v), v)]
}
child_code <- suppressWarnings(as.integer(dt$Age_And_Presence_Of_Children))
# Codebook: 1,4,5,7 signal at least one child under age six.
dt[, has_under6 := fifelse(child_code %in% c(1L, 4L, 5L, 7L), 1L,
                          fifelse(child_code %in% c(2L, 3L, 6L, 9L), 0L,
                                  NA_integer_))]

# No-head codes must already have been respected by Step 02's classification.
# Check that the two nonemployment subtypes cannot both equal one.
if (dt[household_retired == 1L & household_other_unemployed == 1L, .N] > 0L) {
  stop("Step 02 classifications conflict: retired and other-unemployed both 1.")
}
if (dt[household_employed == 1L &
       (household_retired == 1L | household_other_unemployed == 1L), .N] > 0L) {
  stop("Step 02 classifications conflict: employed and unemployed subtype.")
}

# Normalize Nielsen regional codes for interregional moving detection.
# Only codes 1-9 correspond to the nine documented Nielsen regions.
# Keep the original Region_Cd unchanged for auditing.
dt[, region_clean := trimws(as.character(Region_Cd))]
dt[!region_clean %in% as.character(1L:9L), region_clean := NA_character_]

# 4. Create previous-year fields --------------------------------------------
prior_vars <- c("has_kids", "has_under6", "is_married", "household_employed",
                "region_clean")
for (v in prior_vars) {
  dt[, (paste0("prev_", v)) := shift(get(v), type = "lag"), by = hhid]
}
dt[, previous_year := shift(panel_year), by = hhid]
dt[, valid_transition := !is.na(previous_year) & panel_year - previous_year == 1L]

# Utility: distinguish observed no-event (0) from unknown (NA).
binary_transition <- function(prev, current, eligible) {
  fifelse(eligible & !is.na(prev) & !is.na(current),
          as.integer(prev == 0L & current == 1L), NA_integer_)
}

# 5. Seven household-year life events -----------------------------------------
# A family can already have older children when a new under-six child appears.
# The under-six onset is a proxy, NOT a documented birth date.
dt[, event_new_child := fifelse(
  valid_transition & !is.na(prev_has_kids) & !is.na(has_kids) &
    !is.na(prev_has_under6) & !is.na(has_under6),
  as.integer((prev_has_kids == 0L & has_kids == 1L) |
             (prev_has_under6 == 0L & has_under6 == 1L)),
  NA_integer_
)]
dt[, getting_married := binary_transition(prev_is_married, is_married,
                                          valid_transition)]

# Marriage exit: married -> nonmarried between consecutive observed years.
# This is an observable marital-status transition, not independently verified
# divorce/separation. If nonmarried includes widowhood, interpret as a proxy.
# Use a consistent, correctly spelled event variable name.
dt[, getting_divorce_or_separation := fifelse(
  valid_transition & !is.na(prev_is_married) & !is.na(is_married),
  as.integer(prev_is_married == 1L & is_married == 0L),
  NA_integer_
)]
dt[, getting_job := binary_transition(prev_household_employed,
                                     household_employed, valid_transition)]

# Exiting employment: 1 -> 0. The target year's cleaned household
# retirement subtype is determined using the presence of a 65+ head.
known_employment_transition <- dt$valid_transition &
  !is.na(dt$prev_household_employed) & !is.na(dt$household_employed)
left_employment <- known_employment_transition &
  dt$prev_household_employed == 1L & dt$household_employed == 0L

dt[, retirement := fifelse(
  known_employment_transition &
    (household_employed == 1L | !is.na(household_retired)),
  as.integer(left_employment & household_retired == 1L),
  NA_integer_
)]
dt[, other_unemployment := fifelse(
  known_employment_transition &
    (household_employed == 1L | !is.na(household_other_unemployed)),
  as.integer(left_employment & household_other_unemployed == 1L),
  NA_integer_
)]

# Moving means a change between Nielsen regions in consecutive years.
# An unchanged region is coded 0, including households that might have
# moved within the same region (not observed by this measure).
# Missing or invalid region information gives NA, not 0.
dt[, moving := fifelse(
  valid_transition & !is.na(prev_region_clean) & !is.na(region_clean),
  as.integer(prev_region_clean != region_clean),
  NA_integer_
)]
dt[, moving_source := fifelse(
  !valid_transition, "nonconsecutive_or_no_prior_year",
  fifelse(is.na(prev_region_clean) | is.na(region_clean),
          "missing_region", "region_comparison")
)]

# Identical to moving, retained with an explicit interpretive name.
dt[, interregional_move := moving]

# 6. Drop intermediate lag fields, retain source codes -----------------------
intermediate <- c("previous_year", paste0("prev_", prior_vars),
                  "region_clean")
dt[, (intersect(intermediate, names(dt))) := NULL]

# 7. Export only analysis years, retain missing event indicators -------------
final <- dt[panel_year %in% analysis_years]
if (anyDuplicated(final, by = c("hhid", "panel_year"))) {
  stop("Life event output has nonunique household-year keys.")
}
if (nrow(final) == 0L) stop("No 2008-2017 records in aligned demographic input.")

events <- c("event_new_child", "getting_married",
            "getting_divorce_or_separation", "moving",
            "getting_job", "retirement", "other_unemployment")
for (v in events) {
  if (any(!is.na(final[[v]]) & !final[[v]] %in% c(0L, 1L))) {
    stop("Invalid event indicator: ", v)
  }
}

# Mutually exclusive employment exits.
if (final[retirement == 1L & other_unemployment == 1L, .N] > 0L) {
  stop("The two employment exit events are not mutually exclusive.")
}

# Place join keys, event indicators and controls first.
front <- c("hhid", "panel_year", events, "K_1", "income10k",
           "family_size_numeric", "valid_transition", "moving_source",
           "interregional_move")
setcolorder(final, c(intersect(front, names(final)), setdiff(names(final), front)))
setorder(final, hhid, panel_year)

# Counts are household-year events, not unique households with an event.
audit <- rbindlist(lapply(events, function(v) {
  final[, .(
    event = v,
    household_years = .N,
    observed_event_status = sum(!is.na(get(v))),
    missing_event_status = sum(is.na(get(v))),
    event_household_years = sum(get(v) == 1L, na.rm = TRUE),
    households_with_event = uniqueN(hhid[get(v) == 1L])
  ), by = panel_year]
}), use.names = TRUE)
setorder(audit, panel_year, event)
audit[, event_rate_among_observed := fifelse(
  observed_event_status > 0L,
  event_household_years / observed_event_status, NA_real_
)]

checks <- data.table(
  metric = c(
    "output_households", "output_household_years",
    "valid_consecutive_year_transitions", "unknown_year_transitions",
    "region_comparable_transitions", "missing_region_transitions",
    "moving_unidentified_transitions", "new_child_events",
    "marriage_events", "marriage_exit_events", "moving_events", "job_gain_events",
    "retirement_events", "other_unemployment_events"
  ),
  value = c(
    uniqueN(final$hhid), nrow(final),
    sum(final$valid_transition), sum(!final$valid_transition),
    sum(final$moving_source == "region_comparison"),
    sum(final$moving_source == "missing_region"),
    sum(is.na(final$moving)),
    vapply(events, function(v) sum(final[[v]] == 1L, na.rm = TRUE), numeric(1))
  )
)

fwrite(final, output_file, sep = "\t", na = "")
fwrite(audit, audit_file)
fwrite(checks, validation_file)

cat("\nStep 03 life-event validation summary:\n")
print(checks)
cat("\nEvents by year (first rows):\n")
print(head(audit, 12L))
cat("\nSaved: ", output_file, "\n", sep = "")
cat("Audit: ", audit_file, "\n", sep = "")
cat("Validation: ", validation_file, "\n", sep = "")
