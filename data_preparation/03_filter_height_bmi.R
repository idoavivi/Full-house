# ============================================================================
# Script: 03_filter_height_bmi.R
# Purpose: Filter and clean height measurements, compute BMI
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("========================================\n")
cat("Starting height and BMI processing...\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)

# Load raw data
load("outputs/datasets/01_raw_data.RData")

# ============================================================================
# PART 1: HEIGHT DATA
# ============================================================================

cat("PART 1: HEIGHT DATA\n")
cat("-------------------\n\n")

# Extract height measurements
cat("Step 1: Extracting height measurements...\n")

# Include patterns: "body height", "height", "stature"
# Exclude patterns: "percentile", "z-score", "fundal", "sitting"

height_raw <- dataset_06597753_measurement_df %>%
  filter(
    grepl("body height|height|stature", standard_concept_name, ignore.case = TRUE) |
    grepl("body height|height|stature", source_concept_name, ignore.case = TRUE)
  ) %>%
  filter(
    !grepl("percentile|z-score|fundal|sitting", standard_concept_name, ignore.case = TRUE),
    !grepl("percentile|z-score|fundal|sitting", source_concept_name, ignore.case = TRUE)
  )

cat("  - Found", nrow(height_raw), "height measurements\n\n")

# Convert dates and clean
cat("Step 2: Converting dates and cleaning...\n")

height_clean <- height_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    height_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date), !is.na(height_value))

cat("  - After removing missing dates/values:", nrow(height_clean), "measurements\n\n")

# Unit conversion to meters
cat("Step 3: Converting units to meters...\n")

height_clean <- height_clean %>%
  mutate(
    unit_lower = tolower(unit_concept_name),

    height_m = case_when(
      # If unit is meters
      grepl("meter|m", unit_lower, ignore.case = TRUE) & !grepl("cm|centimeter", unit_lower) ~ height_value,

      # If unit is cm/centimeters
      grepl("cm|centimeter", unit_lower, ignore.case = TRUE) ~ height_value / 100,

      # If unit is inches
      grepl("inch|in", unit_lower, ignore.case = TRUE) ~ height_value * 0.0254,

      # Unknown units - make assumptions based on value
      # Unknown 100-250: assume cm
      is.na(unit_concept_name) & height_value >= 100 & height_value <= 250 ~ height_value / 100,

      # Unknown 40-100: assume inches
      is.na(unit_concept_name) & height_value >= 40 & height_value < 100 ~ height_value * 0.0254,

      # Unknown 1-3: assume meters
      is.na(unit_concept_name) & height_value >= 1 & height_value <= 3 ~ height_value,

      # Default: assume cm
      TRUE ~ height_value / 100
    )
  )

cat("  - Unit conversion complete\n\n")

# Apply plausibility bounds (1.20 to 2.20 m)
cat("Step 4: Applying plausibility bounds (1.20-2.20 m)...\n")

n_before <- nrow(height_clean)
height_clean <- height_clean %>%
  filter(height_m >= 1.20 & height_m <= 2.20)
n_after <- nrow(height_clean)

cat("  - Removed", n_before - n_after, "implausible measurements\n")
cat("  - Remaining:", n_after, "measurements\n\n")

# Calculate mean height per person
cat("Step 5: Calculating mean height per person...\n")

height_final <- height_clean %>%
  group_by(person_id) %>%
  summarize(
    height_m = mean(height_m, na.rm = TRUE),
    n_height_measurements = n(),
    .groups = "drop"
  )

cat("  - Unique persons with height:", nrow(height_final), "\n\n")

# ============================================================================
# PART 2: BMI DATA
# ============================================================================

cat("PART 2: BMI DATA\n")
cat("----------------\n\n")

# Extract direct BMI measurements
cat("Step 1: Extracting direct BMI measurements...\n")

bmi_raw <- dataset_06597753_measurement_df %>%
  filter(
    grepl("body mass index|bmi", standard_concept_name, ignore.case = TRUE) |
    grepl("body mass index|bmi", source_concept_name, ignore.case = TRUE)
  )

cat("  - Found", nrow(bmi_raw), "direct BMI measurements\n\n")

# Clean BMI data
cat("Step 2: Cleaning BMI measurements...\n")

bmi_clean <- bmi_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    bmi_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date), !is.na(bmi_value))

# Apply plausibility bounds (12 to 80 kg/m²)
n_before <- nrow(bmi_clean)
bmi_clean <- bmi_clean %>%
  filter(bmi_value >= 12 & bmi_value <= 80)
n_after <- nrow(bmi_clean)

cat("  - Removed", n_before - n_after, "implausible BMI measurements\n")
cat("  - Remaining:", n_after, "measurements\n\n")

# ============================================================================
# PART 3: COMPUTE BMI FROM WEIGHT AND HEIGHT
# ============================================================================

cat("PART 3: COMPUTE BMI\n")
cat("-------------------\n\n")

# Load weight data
load("outputs/datasets/02_weight_clean.RData")

cat("Step 1: Merging weight and height data...\n")

# Create computed BMI for each weight measurement
weight_height <- weight_final %>%
  left_join(height_final, by = "person_id") %>%
  filter(!is.na(height_m))

cat("  - Weight measurements with height:", nrow(weight_height), "\n\n")

# Compute BMI
cat("Step 2: Computing BMI from weight and height...\n")

bmi_computed <- weight_height %>%
  mutate(
    bmi_computed = weight_kg / (height_m^2),
    bmi_source = "computed"
  ) %>%
  filter(bmi_computed >= 12 & bmi_computed <= 80) %>%
  select(person_id, measurement_date, bmi = bmi_computed, bmi_source, weight_kg, height_m)

cat("  - Computed BMI values:", nrow(bmi_computed), "\n\n")

# Combine computed and direct BMI (prefer computed)
cat("Step 3: Creating final BMI dataset (preferring computed over direct)...\n")

bmi_direct <- bmi_clean %>%
  select(person_id, measurement_date, bmi = bmi_value) %>%
  mutate(bmi_source = "direct")

# Merge and prefer computed
bmi_all <- bind_rows(
  bmi_computed %>% select(person_id, measurement_date, bmi, bmi_source),
  bmi_direct
) %>%
  arrange(person_id, measurement_date, desc(bmi_source == "computed")) %>%
  group_by(person_id, measurement_date) %>%
  slice(1) %>%
  ungroup()

cat("  - Total BMI measurements:", nrow(bmi_all), "\n")
cat("  - Unique persons with BMI:", n_distinct(bmi_all$person_id), "\n\n")

# Create baseline BMI (one per person - mean of all BMI values)
cat("Step 4: Creating baseline BMI (mean per person)...\n")

baseline_bmi <- bmi_all %>%
  group_by(person_id) %>%
  summarize(
    baseline_bmi = mean(bmi, na.rm = TRUE),
    n_bmi_measurements = n(),
    .groups = "drop"
  ) %>%
  mutate(
    bmi_class = case_when(
      baseline_bmi < 18.5 ~ "< 18.5",
      baseline_bmi >= 18.5 & baseline_bmi < 25 ~ "18.5-25",
      baseline_bmi >= 25 & baseline_bmi < 30 ~ "25-30",
      baseline_bmi >= 30 & baseline_bmi < 35 ~ "30-35",
      baseline_bmi >= 35 & baseline_bmi < 40 ~ "35-40",
      baseline_bmi >= 40 ~ ">=40"
    ),
    bmi_class = factor(bmi_class, levels = c("< 18.5", "18.5-25", "25-30", "30-35", "35-40", ">=40"))
  )

cat("  - Persons with baseline BMI:", nrow(baseline_bmi), "\n\n")

# Save all processed data
cat("Saving processed data...\n")
save(height_final, file = "outputs/datasets/03a_height_clean.RData")
save(bmi_all, file = "outputs/datasets/03b_bmi_all.RData")
save(baseline_bmi, file = "outputs/datasets/03c_baseline_bmi.RData")

cat("\n========================================\n")
cat("Height and BMI processing complete!\n")
cat("========================================\n\n")

# Display summary statistics
cat("SUMMARY STATISTICS:\n\n")
cat("Height:\n")
cat("  Mean height:", round(mean(height_final$height_m), 2), "m\n")
cat("  Median height:", round(median(height_final$height_m), 2), "m\n\n")

cat("BMI:\n")
cat("  Mean BMI:", round(mean(baseline_bmi$baseline_bmi), 2), "kg/m²\n")
cat("  Median BMI:", round(median(baseline_bmi$baseline_bmi), 2), "kg/m²\n\n")

cat("BMI Class Distribution:\n")
print(table(baseline_bmi$bmi_class))
cat("\n")
