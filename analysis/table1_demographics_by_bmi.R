# ============================================================================
# Script: table1_demographics_by_bmi.R
# Purpose: Create Table 1 - Demographics and activity by BMI class
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("========================================\n")
cat("Creating Table 1: Demographics by BMI Class\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(gtsummary)
library(lubridate)
library(gt)  # For HTML output

# ============================================================================
# ENSURE OUTPUT DIRECTORIES EXIST
# ============================================================================

cat("Creating output directories...\n")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/datasets", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
cat("  - Output directories ready\n\n")

# ============================================================================
# STEP 1: LOAD ALL PROCESSED DATA
# ============================================================================

cat("Step 1: Loading processed data...\n")

load("outputs/datasets/01_raw_data.RData")
load("outputs/datasets/03c_baseline_bmi.RData")
load("outputs/datasets/04c_baseline_activity.RData")
load("outputs/datasets/04b_person_valid_days.RData")
load("outputs/datasets/05c_treatment_categories.RData")

cat("  - Data loaded successfully\n\n")

# ============================================================================
# STEP 2: EXTRACT DEMOGRAPHICS (AGE, SEX, RACE, ETHNICITY)
# ============================================================================

cat("Step 2: Extracting demographics...\n")

person_demographics <- dataset_06597753_person_df %>%
  mutate(
    birth_date = as.Date(date_of_birth),
    age = as.numeric(difftime(Sys.Date(), birth_date, units = "days")) / 365.25,
    sex = case_when(
      sex_at_birth == "Male" ~ "Male",
      sex_at_birth == "Female" ~ "Female",
      TRUE ~ "Other/Unknown"
    ),
    # Race - simplify categories
    race_clean = case_when(
      grepl("White", race, ignore.case = TRUE) ~ "White",
      grepl("Black|African", race, ignore.case = TRUE) ~ "Black/African American",
      grepl("Asian", race, ignore.case = TRUE) ~ "Asian",
      grepl("Hispanic|Latino", race, ignore.case = TRUE) ~ "Hispanic/Latino",
      grepl("Native|American Indian|Alaska", race, ignore.case = TRUE) ~ "American Indian/Alaska Native",
      grepl("Pacific|Hawaiian", race, ignore.case = TRUE) ~ "Native Hawaiian/Pacific Islander",
      grepl("More than one|Multiple|Two or more", race, ignore.case = TRUE) ~ "More than one race",
      TRUE ~ "Other/Unknown"
    ),
    # Ethnicity
    ethnicity_clean = case_when(
      grepl("Hispanic|Latino", ethnicity, ignore.case = TRUE) ~ "Hispanic/Latino",
      grepl("Not Hispanic", ethnicity, ignore.case = TRUE) ~ "Not Hispanic/Latino",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(person_id, age, sex, race_clean, ethnicity_clean)

cat("  - Demographics extracted for", nrow(person_demographics), "persons\n\n")

# ============================================================================
# STEP 3: EXTRACT ANTHROPOMETRIC MEASURES
# ============================================================================

cat("Step 3: Extracting anthropometric measures...\n")

# Get waist circumference
waist_circ <- dataset_06597753_measurement_df %>%
  filter(
    grepl("waist circumference|waist circ", standard_concept_name, ignore.case = TRUE),
    !grepl("hip|ratio", standard_concept_name, ignore.case = TRUE)
  ) %>%
  group_by(person_id) %>%
  summarize(
    waist_cm = mean(value_as_number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(waist_cm), waist_cm > 50, waist_cm < 200)  # Plausible range

cat("  - Waist circumference for", nrow(waist_circ), "persons\n")

# Get hip circumference
hip_circ <- dataset_06597753_measurement_df %>%
  filter(
    grepl("hip circumference|hip circ", standard_concept_name, ignore.case = TRUE),
    !grepl("waist|ratio", standard_concept_name, ignore.case = TRUE)
  ) %>%
  group_by(person_id) %>%
  summarize(
    hip_cm = mean(value_as_number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(hip_cm), hip_cm > 60, hip_cm < 200)  # Plausible range

cat("  - Hip circumference for", nrow(hip_circ), "persons\n\n")

# ============================================================================
# STEP 4: EXTRACT DIAGNOSES FROM CONDITION DATA
# ============================================================================

cat("Step 4: Extracting diagnoses...\n")

# Diabetes Mellitus (DM)
dm_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("diabetes mellitus|diabetic|type 2 diabetes|type 1 diabetes|T2DM|T1DM",
          standard_concept_name, ignore.case = TRUE),
    !grepl("gestational|prediabetes|pre-diabetes|insipidus", standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_dm = TRUE)

cat("  - Diabetes: ", nrow(dm_patients), "patients\n")

# Hypertension (HTN)
htn_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("hypertension|hypertensive|high blood pressure|elevated blood pressure",
          standard_concept_name, ignore.case = TRUE),
    !grepl("pulmonary|portal|intracranial|ocular|gestational|pregnancy",
          standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_htn = TRUE)

cat("  - Hypertension: ", nrow(htn_patients), "patients\n")

# Ischemic Heart Disease (IHD) / Coronary Artery Disease
ihd_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("ischemic heart|coronary artery disease|coronary heart|myocardial infarction|angina|CAD|CHD|MI|heart attack|acute coronary",
          standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_ihd = TRUE)

cat("  - IHD/CAD: ", nrow(ihd_patients), "patients\n")

# Cerebrovascular Accident (CVA) / Stroke
cva_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("stroke|cerebrovascular|CVA|cerebral infarction|transient ischemic|TIA|brain infarct",
          standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_cva = TRUE)

cat("  - CVA/Stroke: ", nrow(cva_patients), "patients\n")

# Osteoarthritis (OA)
oa_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("osteoarthritis|degenerative joint|degenerative arthritis|OA",
          standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_oa = TRUE)

cat("  - Osteoarthritis: ", nrow(oa_patients), "patients\n")

# Sleep Apnea
sleep_apnea_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("sleep apnea|obstructive sleep|OSA", standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_sleep_apnea = TRUE)

cat("  - Sleep Apnea: ", nrow(sleep_apnea_patients), "patients\n")

# Dyslipidemia / Hyperlipidemia
dyslipidemia_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("hyperlipidemia|dyslipidemia|hypercholesterolemia|high cholesterol|elevated cholesterol",
          standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_dyslipidemia = TRUE)

cat("  - Dyslipidemia: ", nrow(dyslipidemia_patients), "patients\n")

# Heart Failure
hf_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("heart failure|cardiac failure|CHF|congestive heart",
          standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_hf = TRUE)

cat("  - Heart Failure: ", nrow(hf_patients), "patients\n")

# NAFLD/NASH
nafld_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("fatty liver|NAFLD|NASH|steatohepatitis|hepatic steatosis",
          standard_concept_name, ignore.case = TRUE),
    !grepl("alcohol", standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_nafld = TRUE)

cat("  - NAFLD/NASH: ", nrow(nafld_patients), "patients\n")

# CKD - Chronic Kidney Disease
ckd_patients <- dataset_06597753_condition_df %>%
  filter(
    grepl("chronic kidney|CKD|renal insufficiency|kidney disease stage",
          standard_concept_name, ignore.case = TRUE),
    !grepl("acute", standard_concept_name, ignore.case = TRUE)
  ) %>%
  distinct(person_id) %>%
  mutate(has_ckd = TRUE)

cat("  - CKD: ", nrow(ckd_patients), "patients\n\n")

# ============================================================================
# STEP 5: MERGE ALL DATA FOR TABLE 1
# ============================================================================

cat("Step 5: Merging all data...\n")

# Start with baseline BMI
table1_data <- baseline_bmi %>%
  # Join demographics
  left_join(person_demographics, by = "person_id") %>%
  # Join activity data
  left_join(baseline_activity, by = "person_id") %>%
  # Join treatment categories
  left_join(treatment_categories %>%
              select(person_id, glp1_user, treatment_category),
            by = "person_id") %>%
  # Join Fitbit valid days
  left_join(person_valid_days %>% select(person_id, total_valid_days),
            by = "person_id") %>%
  # Join anthropometrics
  left_join(waist_circ, by = "person_id") %>%
  left_join(hip_circ, by = "person_id") %>%
  # Join diagnoses
  left_join(dm_patients, by = "person_id") %>%
  left_join(htn_patients, by = "person_id") %>%
  left_join(ihd_patients, by = "person_id") %>%
  left_join(cva_patients, by = "person_id") %>%
  left_join(oa_patients, by = "person_id") %>%
  left_join(sleep_apnea_patients, by = "person_id") %>%
  left_join(dyslipidemia_patients, by = "person_id") %>%
  left_join(hf_patients, by = "person_id") %>%
  left_join(nafld_patients, by = "person_id") %>%
  left_join(ckd_patients, by = "person_id")

# Replace NA with FALSE for diagnosis flags
table1_data <- table1_data %>%
  mutate(
    across(starts_with("has_"), ~replace_na(., FALSE))
  )

cat("  - Merged dataset has", nrow(table1_data), "persons\n\n")

# ============================================================================
# STEP 6: CALCULATE DERIVED VARIABLES
# ============================================================================

cat("Step 6: Calculating derived variables...\n")

# Get height for ratio calculations (from baseline_bmi or recalculate)
# baseline_bmi should have baseline_height_m

table1_data <- table1_data %>%
  mutate(
    # Waist-to-hip ratio
    waist_hip_ratio = waist_cm / hip_cm,

    # Waist-to-height ratio (waist in cm, height in m -> convert height to cm)
    waist_height_ratio = waist_cm / (baseline_height_m * 100),

    # Flag for WHtR >= 0.5 (central obesity indicator)
    whtr_elevated = waist_height_ratio >= 0.5,

    # Combined light + fairly active minutes
    mean_light_fairly_active_min = mean_lightly_active_min + mean_fairly_active_min
  )

cat("  - Derived variables calculated\n\n")

# ============================================================================
# STEP 7: FILTER TO ANALYSIS COHORT
# ============================================================================

cat("Step 7: Filtering to analysis cohort...\n")

# Cohort criteria:
# - Has baseline BMI
# - Has Fitbit data with >= 7 valid days
# - Age 18-90
# - BMI >= 18.5 (to exclude underweight)
# - BMI >= 27 ONLY required for GLP-1 users (FDA indication)
# - Not bariatric surgery

n_start <- nrow(table1_data)

table1_cohort <- table1_data %>%
  filter(
    !is.na(baseline_bmi),                    # Has BMI
    baseline_bmi >= 18.5,                    # Exclude underweight
    !is.na(total_valid_days),                # Has Fitbit data
    total_valid_days >= 7,                   # >= 7 valid days
    age >= 18 & age <= 90,                   # Age 18-90
    treatment_category != "Bariatric_only",  # No bariatric only
    treatment_category != "GLP1_and_Bariatric", # No GLP-1 + bariatric
    # BMI >= 27 only required for GLP-1 users
    !(glp1_user == TRUE & baseline_bmi < 27)
  )

n_end <- nrow(table1_cohort)

cat("  - Started with:", n_start, "persons\n")
cat("  - After filtering:", n_end, "persons\n")
cat("  - Excluded:", n_start - n_end, "persons\n\n")

# ============================================================================
# STEP 8: PREPARE VARIABLES FOR TABLE
# ============================================================================

cat("Step 8: Preparing variables for table...\n")

table1_cohort <- table1_cohort %>%
  mutate(
    # Create BMI class variable (full range)
    bmi_class = case_when(
      baseline_bmi >= 18.5 & baseline_bmi < 25 ~ "18.5-<25",
      baseline_bmi >= 25 & baseline_bmi < 30 ~ "25-<30",
      baseline_bmi >= 30 & baseline_bmi < 35 ~ "30-<35",
      baseline_bmi >= 35 & baseline_bmi < 40 ~ "35-<40",
      baseline_bmi >= 40 ~ ">=40",
      TRUE ~ NA_character_
    ),
    bmi_class = factor(
      bmi_class,
      levels = c("18.5-<25", "25-<30", "30-<35", "35-<40", ">=40")
    ),

    # GLP-1 user variable
    glp1_use = ifelse(glp1_user, "Yes", "No"),

    # Convert diagnosis flags to Yes/No for display
    dm = ifelse(has_dm, "Yes", "No"),
    htn = ifelse(has_htn, "Yes", "No"),
    ihd = ifelse(has_ihd, "Yes", "No"),
    cva = ifelse(has_cva, "Yes", "No"),
    oa = ifelse(has_oa, "Yes", "No"),
    sleep_apnea = ifelse(has_sleep_apnea, "Yes", "No"),
    dyslipidemia = ifelse(has_dyslipidemia, "Yes", "No"),
    heart_failure = ifelse(has_hf, "Yes", "No"),
    nafld = ifelse(has_nafld, "Yes", "No"),
    ckd = ifelse(has_ckd, "Yes", "No"),
    whtr_high = ifelse(whtr_elevated, "Yes", "No")
  )

cat("  - Variables prepared\n\n")

cat("BMI Class Distribution:\n")
print(table(table1_cohort$bmi_class, useNA = "ifany"))
cat("\n\n")

# ============================================================================
# STEP 9: CREATE TABLE 1 USING GTSUMMARY
# ============================================================================

cat("Step 9: Creating Table 1...\n\n")

table1 <- table1_cohort %>%
  select(
    bmi_class,
    # Demographics
    age,
    sex,
    race_clean,
    ethnicity_clean,
    glp1_use,
    # Anthropometrics
    baseline_bmi,
    baseline_weight_kg,
    waist_cm,
    hip_cm,
    waist_hip_ratio,
    waist_height_ratio,
    whtr_high,
    # Diagnoses
    dm,
    htn,
    ihd,
    cva,
    oa,
    sleep_apnea,
    dyslipidemia,
    heart_failure,
    nafld,
    ckd,
    # Activity
    mean_sedentary_min,
    mean_lightly_active_min,
    mean_fairly_active_min,
    mean_light_fairly_active_min,
    mean_very_active_min,
    mean_steps,
    mean_calories
  ) %>%
  tbl_summary(
    by = bmi_class,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      # Demographics
      age ~ "Age (years)",
      sex ~ "Sex",
      race_clean ~ "Race",
      ethnicity_clean ~ "Ethnicity",
      glp1_use ~ "GLP-1 Use",
      # Anthropometrics
      baseline_bmi ~ "BMI (kg/m2)",
      baseline_weight_kg ~ "Weight (kg)",
      waist_cm ~ "Waist Circumference (cm)",
      hip_cm ~ "Hip Circumference (cm)",
      waist_hip_ratio ~ "Waist-to-Hip Ratio",
      waist_height_ratio ~ "Waist-to-Height Ratio",
      whtr_high ~ "WHtR >= 0.5",
      # Diagnoses
      dm ~ "Diabetes Mellitus",
      htn ~ "Hypertension",
      ihd ~ "Ischemic Heart Disease",
      cva ~ "Stroke/CVA",
      oa ~ "Osteoarthritis",
      sleep_apnea ~ "Sleep Apnea",
      dyslipidemia ~ "Dyslipidemia",
      heart_failure ~ "Heart Failure",
      nafld ~ "NAFLD/NASH",
      ckd ~ "Chronic Kidney Disease",
      # Activity
      mean_sedentary_min ~ "Sedentary Minutes",
      mean_lightly_active_min ~ "Lightly Active Minutes",
      mean_fairly_active_min ~ "Fairly Active Minutes",
      mean_light_fairly_active_min ~ "Light + Fairly Active Minutes",
      mean_very_active_min ~ "Very Active Minutes",
      mean_steps ~ "Steps per Day",
      mean_calories ~ "Calories per Day"
    ),
    missing = "no"
  ) %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",  # Non-parametric for skewed data
      all_categorical() ~ "chisq.test"    # Chi-square for categorical
    )
  ) %>%
  add_overall() %>%
  modify_caption("**Table 1. Baseline Characteristics by BMI Class**") %>%
  modify_spanning_header(all_stat_cols() ~ "**BMI Class (kg/m2)**") %>%
  bold_labels()

# Print table
print(table1)

# ============================================================================
# STEP 10: SAVE TABLE
# ============================================================================

cat("\n\nStep 10: Saving table...\n")

# Save as RDS
saveRDS(table1, file = "outputs/tables/table1_demographics_by_bmi.rds")

# Save underlying data for reference
saveRDS(table1_cohort, file = "outputs/tables/table1_cohort_data.rds")

# Save as CSV (flattened version with key statistics)
table1_df <- table1_cohort %>%
  group_by(bmi_class) %>%
  summarize(
    n = n(),
    # Demographics
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    pct_male = 100 * mean(sex == "Male", na.rm = TRUE),
    pct_glp1 = 100 * mean(glp1_use == "Yes", na.rm = TRUE),
    # Anthropometrics
    bmi_mean = mean(baseline_bmi, na.rm = TRUE),
    bmi_sd = sd(baseline_bmi, na.rm = TRUE),
    weight_mean = mean(baseline_weight_kg, na.rm = TRUE),
    weight_sd = sd(baseline_weight_kg, na.rm = TRUE),
    waist_mean = mean(waist_cm, na.rm = TRUE),
    waist_sd = sd(waist_cm, na.rm = TRUE),
    hip_mean = mean(hip_cm, na.rm = TRUE),
    hip_sd = sd(hip_cm, na.rm = TRUE),
    whr_mean = mean(waist_hip_ratio, na.rm = TRUE),
    whr_sd = sd(waist_hip_ratio, na.rm = TRUE),
    whtr_mean = mean(waist_height_ratio, na.rm = TRUE),
    whtr_sd = sd(waist_height_ratio, na.rm = TRUE),
    pct_whtr_high = 100 * mean(whtr_elevated, na.rm = TRUE),
    # Diagnoses
    pct_dm = 100 * mean(has_dm, na.rm = TRUE),
    pct_htn = 100 * mean(has_htn, na.rm = TRUE),
    pct_ihd = 100 * mean(has_ihd, na.rm = TRUE),
    pct_cva = 100 * mean(has_cva, na.rm = TRUE),
    pct_oa = 100 * mean(has_oa, na.rm = TRUE),
    pct_sleep_apnea = 100 * mean(has_sleep_apnea, na.rm = TRUE),
    pct_dyslipidemia = 100 * mean(has_dyslipidemia, na.rm = TRUE),
    pct_hf = 100 * mean(has_hf, na.rm = TRUE),
    pct_nafld = 100 * mean(has_nafld, na.rm = TRUE),
    pct_ckd = 100 * mean(has_ckd, na.rm = TRUE),
    # Activity
    sedentary_mean = mean(mean_sedentary_min, na.rm = TRUE),
    sedentary_sd = sd(mean_sedentary_min, na.rm = TRUE),
    lightly_active_mean = mean(mean_lightly_active_min, na.rm = TRUE),
    lightly_active_sd = sd(mean_lightly_active_min, na.rm = TRUE),
    fairly_active_mean = mean(mean_fairly_active_min, na.rm = TRUE),
    fairly_active_sd = sd(mean_fairly_active_min, na.rm = TRUE),
    light_fairly_active_mean = mean(mean_light_fairly_active_min, na.rm = TRUE),
    light_fairly_active_sd = sd(mean_light_fairly_active_min, na.rm = TRUE),
    very_active_mean = mean(mean_very_active_min, na.rm = TRUE),
    very_active_sd = sd(mean_very_active_min, na.rm = TRUE),
    steps_mean = mean(mean_steps, na.rm = TRUE),
    steps_sd = sd(mean_steps, na.rm = TRUE),
    calories_mean = mean(mean_calories, na.rm = TRUE),
    calories_sd = sd(mean_calories, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(table1_df, "outputs/tables/table1_summary_stats.csv")

# Save as HTML
table1 %>%
  as_gt() %>%
  gt::gtsave("outputs/tables/table1_demographics_by_bmi.html")

cat("  - Table saved to:\n")
cat("    - outputs/tables/table1_demographics_by_bmi.rds\n")
cat("    - outputs/tables/table1_cohort_data.rds\n")
cat("    - outputs/tables/table1_summary_stats.csv\n")
cat("    - outputs/tables/table1_demographics_by_bmi.html\n\n")

cat("========================================\n")
cat("Table 1 creation complete!\n")
cat("========================================\n\n")
