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
# STEP 2: MERGE ALL DATA FOR TABLE 1
# ============================================================================

cat("Step 2: Merging data for Table 1...\n")

# Calculate age from person data
person_age_sex <- dataset_06597753_person_df %>%
  mutate(
    birth_date = as.Date(date_of_birth),
    age = as.numeric(difftime(Sys.Date(), birth_date, units = "days")) / 365.25,
    sex = case_when(
      sex_at_birth == "Male" ~ "Male",
      sex_at_birth == "Female" ~ "Female",
      TRUE ~ "Other/Unknown"
    )
  ) %>%
  select(person_id, age, sex)

# Merge all datasets
table1_data <- baseline_bmi %>%
  # Join demographics
  left_join(person_age_sex, by = "person_id") %>%
  # Join activity data
  left_join(baseline_activity, by = "person_id") %>%
  # Join treatment categories
  left_join(treatment_categories %>%
              select(person_id, glp1_user, treatment_category),
            by = "person_id") %>%
  # Join Fitbit valid days
  left_join(person_valid_days %>% select(person_id, total_valid_days),
            by = "person_id")

cat("  - Merged dataset has", nrow(table1_data), "persons\n\n")

# ============================================================================
# STEP 3: FILTER TO ANALYSIS COHORT
# ============================================================================

cat("Step 3: Filtering to analysis cohort...\n")

# Filter criteria:
# - Has baseline BMI
# - Has Fitbit data with >= 7 valid days
# - Age 18-90
# - BMI >= 27
# - Not bariatric surgery

n_start <- nrow(table1_data)

table1_cohort <- table1_data %>%
  filter(
    !is.na(baseline_bmi),                    # Has BMI
    !is.na(total_valid_days),                # Has Fitbit data
    total_valid_days >= 7,                   # >= 7 valid days
    age >= 18 & age <= 90,                   # Age 18-90
    baseline_bmi >= 27,                      # Overweight/obese
    treatment_category != "Bariatric_only",  # No bariatric only
    treatment_category != "GLP1_and_Bariatric" # No GLP-1 + bariatric
  )

n_end <- nrow(table1_cohort)

cat("  - Started with:", n_start, "persons\n")
cat("  - After filtering:", n_end, "persons\n")
cat("  - Excluded:", n_start - n_end, "persons\n\n")

# ============================================================================
# STEP 4: PREPARE VARIABLES FOR TABLE
# ============================================================================

cat("Step 4: Preparing variables for table...\n")

table1_cohort <- table1_cohort %>%
  mutate(
    # Create BMI class variable (only >= 27)
    bmi_class_analysis = case_when(
      baseline_bmi >= 27 & baseline_bmi < 30 ~ "27-30 (Overweight)",
      baseline_bmi >= 30 & baseline_bmi < 35 ~ "30-35 (Obesity I)",
      baseline_bmi >= 35 & baseline_bmi < 40 ~ "35-40 (Obesity II)",
      baseline_bmi >= 40 ~ ">=40 (Obesity III)"
    ),
    bmi_class_analysis = factor(
      bmi_class_analysis,
      levels = c("27-30 (Overweight)", "30-35 (Obesity I)",
                 "35-40 (Obesity II)", ">=40 (Obesity III)")
    ),

    # GLP-1 user variable
    glp1_use = ifelse(glp1_user, "Yes", "No")
  )

cat("  - Variables prepared\n\n")

cat("BMI Class Distribution:\n")
print(table(table1_cohort$bmi_class_analysis))
cat("\n\n")

# ============================================================================
# STEP 5: CREATE TABLE 1 USING GTSUMMARY
# ============================================================================

cat("Step 5: Creating Table 1...\n\n")

table1 <- table1_cohort %>%
  select(
    bmi_class_analysis,
    age,
    sex,
    glp1_use,
    baseline_bmi,
    mean_sedentary_min,
    mean_lightly_active_min,
    mean_fairly_active_min,
    mean_very_active_min,
    mean_steps,
    mean_calories
  ) %>%
  tbl_summary(
    by = bmi_class_analysis,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      glp1_use ~ "GLP-1 Use",
      baseline_bmi ~ "BMI (kg/m²)",
      mean_sedentary_min ~ "Sedentary Minutes",
      mean_lightly_active_min ~ "Lightly Active Minutes",
      mean_fairly_active_min ~ "Fairly Active Minutes",
      mean_very_active_min ~ "Very Active Minutes",
      mean_steps ~ "Steps per Day",
      mean_calories ~ "Calories per Day"
    ),
    missing = "no"
  ) %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous() ~ "aov",    # ANOVA for continuous variables
      all_categorical() ~ "chisq.test"  # Chi-square for categorical
    )
  ) %>%
  add_overall() %>%
  modify_caption("**Table 1. Baseline Characteristics by BMI Class**") %>%
  bold_labels()

# Print table
print(table1)

# ============================================================================
# STEP 6: SAVE TABLE
# ============================================================================

cat("\n\nStep 6: Saving table...\n")

# Save as RDS
saveRDS(table1, file = "outputs/tables/table1_demographics_by_bmi.rds")

# Save as CSV (flattened version)
table1_df <- table1_cohort %>%
  group_by(bmi_class_analysis) %>%
  summarize(
    n = n(),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    pct_male = 100 * mean(sex == "Male", na.rm = TRUE),
    pct_glp1 = 100 * mean(glp1_use == "Yes", na.rm = TRUE),
    bmi_mean = mean(baseline_bmi, na.rm = TRUE),
    bmi_sd = sd(baseline_bmi, na.rm = TRUE),
    sedentary_mean = mean(mean_sedentary_min, na.rm = TRUE),
    sedentary_sd = sd(mean_sedentary_min, na.rm = TRUE),
    lightly_active_mean = mean(mean_lightly_active_min, na.rm = TRUE),
    lightly_active_sd = sd(mean_lightly_active_min, na.rm = TRUE),
    fairly_active_mean = mean(mean_fairly_active_min, na.rm = TRUE),
    fairly_active_sd = sd(mean_fairly_active_min, na.rm = TRUE),
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
cat("    - outputs/tables/table1_summary_stats.csv\n")
cat("    - outputs/tables/table1_demographics_by_bmi.html\n\n")

cat("========================================\n")
cat("Table 1 creation complete!\n")
cat("========================================\n\n")
