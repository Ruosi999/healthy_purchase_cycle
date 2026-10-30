# ============================================================
# 02_build_clean_demographics_final.R
# Healthy Purchase Cycle | NielsenIQ HMS | 2008-2017
# ============================================================
# PURPOSE
#   Build a clean household-year demographic panel for households
#   selected in Step 01 (at least three purchase-observed years).
#
# NIELSEN TIME ALIGNMENT (target calendar year t):
#   Non-income demographics: panelists_(t+1).tsv; reflect late t.
#   Household income:       panelists_(t+2).tsv; refers to income in t.
#   For 2008-2017 targets, read source years 2009-2019.
#   Target 2007 is also retained for 2008 transition comparisons.
#   A source missing for a household is NA, never imputed from
#   an adjacent reporting year.
#
# STEP 02 DOES NOT YET CONSTRUCT THE SIX LIFE EVENT INDICATORS.
#   It constructs cleaned status variables for Step 04.
#
# SECURITY: All restricted household data are written outside GitHub.
# ============================================================

library(data.table)

# 1. Paths and analysis window -----------------------------------------------
data_root <- "C:/Users/rwen13/Documents/healthy_purchase_data"
hms_root <- "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS"
output_dir <- file.path(data_root, "processed", "demographics")
eligible_file <- file.path(output_dir, "eligible_households_2008_2017.tsv")
analysis_years <- 2008L:2017L
target_years <- 2007L:2017L
source_years <- 2008L:2019L

# Verified NielsenIQ employment codes:
#   0 = no male/female head; 1 = under 30 hours;
#   2 = 30-34 hours; 3 = 35+ hours; 9 = not employed for pay.
employed_codes <- c(1L, 2L, 3L)
nonemployed_codes <- c(9L)

# Representative incomes for open-ended brackets are modeling assumptions.
assumed_top_100k <- 125000
assumed_top_200k <- 225000

# ANALYTICAL ASSUMPTION (not verified in the available codebook):
# For 2011-2019, treat code 27 as the open-ended $100,000+ group,
# continuing the documented 2010 definition. Assign $125,000.
# Preserve income_mapping_status so that this can be audited and revised.
assume_post2010_code27_is_top_100k <- TRUE

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(eligible_file)) stop("Step 01 eligibility not found: ", eligible_file)
eligible <- fread(eligible_file, select = "hhid", colClasses = "character")
eligible[, hhid := trimws(hhid)]
if (anyNA(eligible$hhid) || any(!nzchar(eligible$hhid)) || anyDuplicated(eligible$hhid)) {
  stop("Eligibility IDs are empty or duplicated.")
}
setkey(eligible, hhid)

# 2. Source columns -----------------------------------------------------------
# Preserve original coded fields (as character) for future review.
general_cols <- c(
  "Household_Cd", "Panel_Year", "Household_Size", "Household_Composition",
  "Marital_Status", "Age_And_Presence_Of_Children",
  "Male_Head_Age", "Female_Head_Age",
  "Male_Head_Birth", "Female_Head_Birth",
  "Male_Head_Employment", "Female_Head_Employment",
  "Male_Head_Occupation", "Female_Head_Occupation",
  "Region_Cd", "Panelist_ZipCd", "Fips_State_Cd", "Fips_County_Cd",
  "Type_Of_Residence", "Household_Income",
  unlist(lapply(1:7, function(k) paste0("Member_", k, "_",
                                        c("Birth", "Relationship_Sex", "Employment"))))
)
needed_general <- c("Household_Cd", "Panel_Year", "Household_Size",
                    "Marital_Status", "Age_And_Presence_Of_Children",
                    "Male_Head_Age", "Female_Head_Age",
                    "Male_Head_Employment", "Female_Head_Employment")
int_code <- function(x) suppressWarnings(as.integer(x))

# Read one reporting year once. Filter to Step-01 households immediately.
read_panelists_year <- function(year) {
  path <- file.path(hms_root, as.character(year), "Annual_Files",
                    sprintf("panelists_%d.tsv", year))
  if (!file.exists(path)) stop("Missing required Nielsen file: ", path)
  header <- names(fread(path, sep = "\t", nrows = 0L, showProgress = FALSE))
  missing_cols <- setdiff(c(needed_general, "Household_Income"), header)
  if (length(missing_cols)) stop(year, ": missing required columns: ",
                                 paste(missing_cols, collapse = ", "))
  keep <- intersect(general_cols, header)
  x <- fread(path, sep = "\t", select = keep,
             colClasses = list(character = keep), showProgress = TRUE)
  setnames(x, c("Household_Cd", "Panel_Year"), c("hhid", "source_panel_year_raw"))
  x[, `:=`(hhid = trimws(hhid), source_panel_year = int_code(source_panel_year_raw))]
  n_bad_source_year <- x[is.na(source_panel_year) | source_panel_year != year, .N]
  x <- x[!is.na(hhid) & nzchar(hhid) & source_panel_year == year]
  x <- x[eligible, on = "hhid", nomatch = 0L]
  if (anyDuplicated(x, by = c("hhid", "source_panel_year"))) {
    stop("Duplicate household/source-year keys in panelists_", year,
         "; resolve source conflicts before continuing.")
  }
  message("Panelists ", year, ": matched ", uniqueN(x$hhid),
          " households; wrong-year source rows = ", n_bad_source_year)
  x[]
}

source_list <- lapply(source_years, read_panelists_year)
names(source_list) <- as.character(source_years)
source_dt <- rbindlist(source_list, use.names = TRUE, fill = TRUE)
rm(source_list)

# 3. Calendar-year alignment -------------------------------------------------
# The non-income annual values in reporting year s describe approximately
# Q4 of calendar year s-1. Keep them intact and label target year s-1.
annual <- copy(source_dt[source_panel_year %in% (target_years + 1L)])
annual[, `:=`(panel_year = source_panel_year - 1L,
              demographics_source_panel_year = source_panel_year)]

# Preserve income FROM the general source under a descriptive raw name;
# it is not the income for the target panel_year.
setnames(annual, "Household_Income", "Household_Income_general_source_raw")

# Income in source reporting year s explicitly refers to calendar year s-2.
# Join by exact household ID + target calendar year; no nearest-year fill.
income_source <- source_dt[, .(
  hhid, panel_year = source_panel_year - 2L,
  income_source_panel_year = source_panel_year,
  Household_Income = Household_Income
)]
income_source <- income_source[panel_year %in% target_years]
if (anyDuplicated(income_source, by = c("hhid", "panel_year"))) {
  stop("Nonunique household/year in aligned income source.")
}
annual <- merge(annual, income_source, by = c("hhid", "panel_year"),
                all.x = TRUE, sort = FALSE)
annual[, income_reference_year := panel_year]

# Re-read identifier types and preserve zero-prefixed ZIP codes as strings.
annual[, hhid := as.character(hhid)]

# 4. Clean coded status variables --------------------------------------------
child_code <- int_code(annual$Age_And_Presence_Of_Children)
annual[, has_kids := fifelse(child_code %in% 1L:7L, 1L,
                             fifelse(child_code == 9L, 0L, NA_integer_))]
annual[, K_1 := has_kids]  # Previously agreed definition: any child under 18.

marital_code <- int_code(annual$Marital_Status)
annual[, is_married := fifelse(marital_code == 1L, 1L,
                               fifelse(marital_code %in% 2L:4L, 0L, NA_integer_))]
size_code <- int_code(annual$Household_Size)
annual[, family_size_numeric := fifelse(size_code %in% 1L:9L, size_code, NA_integer_)]
annual[, family_size_topcoded := fifelse(size_code == 9L, 1L,
                                         fifelse(size_code %in% 1L:8L, 0L, NA_integer_))]

# Nielsen age category 0 = no head; 9 = 65+.
# Birth years are not shifted numerically; the values originate from t+1.
# Age computed from birth is assessed at target t, not reporting year t+1.
for (sex in c("Male", "Female")) {
  birth <- int_code(annual[[paste0(sex, "_Head_Birth")]])
  age_var <- paste0(tolower(sex), "_head_age_from_birth")
  annual[, (age_var) := fifelse(!is.na(birth) & birth >= 1900L &
                                  birth <= panel_year, panel_year - birth, NA_integer_)]
}

head_employment <- function(raw_employment, raw_age) {
  e <- int_code(raw_employment)
  a <- int_code(raw_age)
  fifelse(a == 0L, NA_integer_,
          fifelse(e %in% employed_codes, 1L,
                  fifelse(e %in% nonemployed_codes, 0L, NA_integer_)))
}
annual[, male_head_employed := head_employment(Male_Head_Employment, Male_Head_Age)]
annual[, female_head_employed := head_employment(Female_Head_Employment, Female_Head_Age)]
male_age_code <- int_code(annual$Male_Head_Age)
female_age_code <- int_code(annual$Female_Head_Age)
male_present <- male_age_code %in% 1L:9L
female_present <- female_age_code %in% 1L:9L
any_employed <- (!is.na(annual$male_head_employed) & annual$male_head_employed == 1L) |
  (!is.na(annual$female_head_employed) & annual$female_head_employed == 1L)
all_nonemployed <- (male_present | female_present) &
  (!male_present | (!is.na(annual$male_head_employed) & annual$male_head_employed == 0L)) &
  (!female_present | (!is.na(annual$female_head_employed) & annual$female_head_employed == 0L))
annual[, household_employed := fifelse(any_employed, 1L,
                                       fifelse(all_nonemployed, 0L, NA_integer_))]
head_65plus <- (male_present & male_age_code == 9L) |
  (female_present & female_age_code == 9L)
annual[, household_retired := fifelse(household_employed == 0L & head_65plus, 1L,
                                      fifelse(!is.na(household_employed), 0L, NA_integer_))]
annual[, household_other_unemployed := fifelse(household_employed == 0L & !head_65plus, 1L,
                                               fifelse(!is.na(household_employed), 0L, NA_integer_))]

# 5. Convert income using source reporting-year-specific codes ---------------
# Most bands are fixed in the supplied Nielsen excerpt.
income_midpoints <- c(
  "3" = 2500, "4" = 6500, "6" = 9000, "8" = 11000,
  "10" = 13500, "11" = 17500, "13" = 22500,
  "15" = 27500, "16" = 32500, "17" = 37500,
  "18" = 42500, "19" = 47500, "21" = 55000,
  "23" = 65000, "26" = 85000
)
annual[, income_code := trimws(as.character(Household_Income))]
annual[income_code == "", income_code := NA_character_]
annual[, income_dollars := as.numeric(income_midpoints[income_code])]
annual[, income_mapping_status := fifelse(!is.na(income_dollars),
                                          "documented_midpoint", "unmapped")]
# IMPORTANT: interpret code 27 based on SOURCE PANEL YEAR, never target year.
annual[income_code == "27" & income_source_panel_year %between% c(2006L, 2009L),
       `:=`(income_dollars = 112500, income_mapping_status = "documented_midpoint")]
annual[income_code == "28" & income_source_panel_year %between% c(2006L, 2009L),
       `:=`(income_dollars = 137500, income_mapping_status = "documented_midpoint")]
annual[income_code == "29" & income_source_panel_year %between% c(2006L, 2009L),
       `:=`(income_dollars = 175000, income_mapping_status = "documented_midpoint")]
annual[income_code == "30" & income_source_panel_year %between% c(2006L, 2009L),
       `:=`(income_dollars = assumed_top_200k,
            income_mapping_status = "assumed_open_top_200k")]
annual[income_code == "27" & income_source_panel_year == 2010L,
       `:=`(income_dollars = assumed_top_100k,
            income_mapping_status = "assumed_open_top_100k")]
if (assume_post2010_code27_is_top_100k) {
  annual[income_code == "27" & income_source_panel_year >= 2011L,
         `:=`(income_dollars = assumed_top_100k,
              income_mapping_status = "unverified_post2010_assumption")]
}
annual[, `:=`(
  income10k = as.numeric(income_dollars / 10000),
  income_mapping_assumed = grepl("^(assumed_|unverified_)", income_mapping_status),
  income_conversion_pending = !is.na(income_code) &
    income_mapping_status == "unmapped"
)]

# 6. Flag implausible trajectories AFTER alignment ---------------------------
# Only compare adjacent target years. Do not bridge panel gaps.
setorder(annual, hhid, panel_year)
annual[, `:=`(
  previous_year = shift(panel_year), next_year = shift(panel_year),
  prev_married = shift(is_married), next_married = shift(is_married),
  prev_kids = shift(has_kids), next_kids = shift(has_kids),
  prev_male_age = shift(male_head_age_from_birth),
  prev_female_age = shift(female_head_age_from_birth),
  prev_male_age_code = shift(int_code(Male_Head_Age)),
  prev_female_age_code = shift(int_code(Female_Head_Age))
), by = hhid]
annual[, adjacent_prev := !is.na(previous_year) & (panel_year - previous_year == 1L)]
annual[, consecutive_three := adjacent_prev & !is.na(next_year) &
         (next_year - panel_year == 1L)]

annual[, age_decrease_flag := adjacent_prev & (
  (!is.na(male_head_age_from_birth) & !is.na(prev_male_age) &
     male_head_age_from_birth < prev_male_age) |
    (!is.na(female_head_age_from_birth) & !is.na(prev_female_age) &
       female_head_age_from_birth < prev_female_age) |
    (int_code(Male_Head_Age) %in% 1L:9L & prev_male_age_code %in% 1L:9L &
       int_code(Male_Head_Age) < prev_male_age_code) |
    (int_code(Female_Head_Age) %in% 1L:9L & prev_female_age_code %in% 1L:9L &
       int_code(Female_Head_Age) < prev_female_age_code)
)]
annual[, marriage_flicker_flag := consecutive_three &
         !is.na(prev_married) & !is.na(is_married) & !is.na(next_married) &
         prev_married == next_married & is_married != prev_married]
annual[, children_flicker_flag := consecutive_three &
         !is.na(prev_kids) & !is.na(has_kids) & !is.na(next_kids) &
         prev_kids == 1L & has_kids == 0L & next_kids == 1L]

hh_flags <- annual[, .(
  age_decrease = any(age_decrease_flag, na.rm = TRUE),
  marriage_flicker = any(marriage_flicker_flag, na.rm = TRUE),
  children_flicker = any(children_flicker_flag, na.rm = TRUE)
), by = hhid]
hh_flags[, exclude_any := age_decrease | marriage_flicker | children_flicker]
hh_flags[, exclusion_reason := fifelse(age_decrease, "age_decrease",
                                       fifelse(marriage_flicker, "marriage_flicker",
                                               fifelse(children_flicker, "children_flicker", "retained")))]

exclusion_summary <- data.table(
  metric = c("step01_eligible_households", "matched_households",
             "flagged_age_decrease", "flagged_marriage_flicker",
             "flagged_children_flicker", "removed_age_first",
             "removed_marriage_after_age", "removed_children_after_both",
             "removed_any_reason", "final_retained_households"),
  n_households = c(nrow(eligible), nrow(hh_flags),
                   sum(hh_flags$age_decrease), sum(hh_flags$marriage_flicker),
                   sum(hh_flags$children_flicker),
                   sum(hh_flags$exclusion_reason == "age_decrease"),
                   sum(hh_flags$exclusion_reason == "marriage_flicker"),
                   sum(hh_flags$exclusion_reason == "children_flicker"),
                   sum(hh_flags$exclude_any), sum(!hh_flags$exclude_any))
)

fwrite(hh_flags, file.path(output_dir, "demographic_exclusion_flags.tsv"), sep = "\t", na = "")
fwrite(exclusion_summary, file.path(output_dir, "demographic_exclusion_summary.csv"))
cat("\nHousehold exclusion summary (counts by reason may overlap):\n")
print(exclusion_summary)

annual <- annual[hh_flags[exclude_any == FALSE, .(hhid)], on = "hhid", nomatch = 0L]
internal_cols <- c("previous_year", "next_year", "prev_married", "next_married",
                   "prev_kids", "next_kids", "prev_male_age", "prev_female_age",
                   "prev_male_age_code", "prev_female_age_code", "adjacent_prev",
                   "consecutive_three", "age_decrease_flag", "marriage_flicker_flag",
                   "children_flicker_flag")
annual[, (intersect(internal_cols, names(annual))) := NULL]
setcolorder(annual, c("hhid", "panel_year", setdiff(names(annual), c("hhid", "panel_year"))))
setorder(annual, hhid, panel_year)
if (anyDuplicated(annual, by = c("hhid", "panel_year"))) stop("Nonunique output household-year keys")

# 7. Export audits and aligned demographics ----------------------------------
source_audit <- source_dt[, .(n_households = uniqueN(hhid)), by = source_panel_year]
setorder(source_audit, source_panel_year)
fwrite(source_audit, file.path(output_dir, "demographic_extraction_audit.csv"))

income_audit <- annual[, .(n_household_years = .N),
                       by = .(income_source_panel_year, income_code, income_mapping_status)]
setorder(income_audit, income_source_panel_year, income_code)
fwrite(income_audit, file.path(output_dir, "demographic_income_code_audit.csv"))

fwrite(annual, file.path(output_dir, "household_demographics_2007_2017.tsv"),
       sep = "\t", na = "")
analysis_panel <- annual[panel_year %in% analysis_years]
fwrite(analysis_panel, file.path(output_dir, "household_demographics_2008_2017.tsv"),
       sep = "\t", na = "")

cat("\nFinal matched households (2008-2017): ", uniqueN(analysis_panel$hhid), "\n", sep = "")
cat("Final household-years (2008-2017): ", nrow(analysis_panel), "\n", sep = "")
cat("Unmapped income household-years: ", sum(analysis_panel$income_conversion_pending), "\n", sep = "")
cat("Missing income10k household-years: ", sum(is.na(analysis_panel$income10k)), "\n", sep = "")
cat("Assumption-based income household-years: ", sum(analysis_panel$income_mapping_assumed, na.rm = TRUE), "\n", sep = "")
cat("Unknown employment household-years: ", sum(is.na(analysis_panel$household_employed)), "\n", sep = "")
cat("Employment mapping verified: 1-3 employed; 9 not employed; 0 no head.\n")
cat("NOTE: 2011-2019 code 27 is ASSUMED to mean $100,000+; confirm against annual Nielsen codebooks.\n")
cat("Finished. Restricted outputs: ", output_dir, "\n", sep = "")