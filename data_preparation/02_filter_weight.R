# ============================================================================
# Script: 02_filter_weight.R
# Purpose: Filter and clean weight measurements
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("========================================\n")
cat("Starting weight data filtering...\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)

# Load raw data
load("outputs/datasets/01_raw_data.RData")

# Extract weight measurements from measurement data
cat("Step 1: Extracting weight measurements...\n")

# Include patterns: "body weight", "weight measured"
# Exclude patterns: "birth", "fetal", "ideal", "dry", "dosing", "percentile"

weight_raw <- dataset_06597753_measurement_df %>%
  filter(
    # Include weight-related measurements
    grepl("body weight|weight measured", standard_concept_name, ignore.case = TRUE) |
    grepl("body weight|weight measured", source_concept_name, ignore.case = TRUE)
  ) %>%
  filter(
    # Exclude non-body weight measurements
    !grepl("birth|fetal|ideal|dry|dosing|percentile", standard_concept_name, ignore.case = TRUE),
    !grepl("birth|fetal|ideal|dry|dosing|percentile", source_concept_name, ignore.case = TRUE)
  )

cat("  - Found", nrow(weight_raw), "weight measurements\n\n")

# Step 2: Convert measurement_datetime to Date
cat("Step 2: Converting dates...\n")

weight_clean <- weight_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    weight_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date), !is.na(weight_value))

cat("  - After removing missing dates/values:", nrow(weight_clean), "measurements\n\n")

# Step 3: Unit conversion
cat("Step 3: Converting units to kg...\n")

weight_clean <- weight_clean %>%
  mutate(
    # Determine original unit
    unit_lower = tolower(unit_concept_name),

    # Convert to kg based on unit
    weight_kg = case_when(
      # If unit is kg, keep as is
      grepl("kilogram|kg", unit_lower, ignore.case = TRUE) ~ weight_value,

      # If unit is lbs/pounds, convert
      grepl("pound|lb", unit_lower, ignore.case = TRUE) ~ weight_value * 0.453592,

      # If unknown unit and value > 200, assume lbs
      is.na(unit_concept_name) & weight_value > 200 ~ weight_value * 0.453592,

      # If unknown unit and value <= 200, assume kg
      is.na(unit_concept_name) & weight_value <= 200 ~ weight_value,

      # Default: assume kg
      TRUE ~ weight_value
    )
  )

cat("  - Unit conversion complete\n\n")

# Step 4: Apply plausibility bounds (30 to 300 kg)
cat("Step 4: Applying plausibility bounds (30-300 kg)...\n")

n_before <- nrow(weight_clean)
weight_clean <- weight_clean %>%
  filter(weight_kg >= 30 & weight_kg <= 300)
n_after <- nrow(weight_clean)

cat("  - Removed", n_before - n_after, "implausible measurements\n")
cat("  - Remaining:", n_after, "measurements\n\n")

# Step 5: Handle same-day duplicates (keep first)
cat("Step 5: Handling same-day duplicates...\n")

n_before <- nrow(weight_clean)
weight_clean <- weight_clean %>%
  arrange(person_id, measurement_date, measurement_datetime) %>%
  group_by(person_id, measurement_date) %>%
  slice(1) %>%
  ungroup()
n_after <- nrow(weight_clean)

cat("  - Removed", n_before - n_after, "duplicate same-day measurements\n")
cat("  - Remaining:", n_after, "measurements\n\n")

# Step 6: Create final clean weight dataset
cat("Step 6: Creating final weight dataset...\n")

weight_final <- weight_clean %>%
  select(
    person_id,
    measurement_date,
    weight_kg,
    original_value = weight_value,
    original_unit = unit_concept_name
  ) %>%
  arrange(person_id, measurement_date)

cat("  - Final dataset:", nrow(weight_final), "measurements\n")
cat("  - Unique persons:", n_distinct(weight_final$person_id), "\n\n")

# Save processed data
cat("Saving processed weight data...\n")
save(weight_final, file = "outputs/datasets/02_weight_clean.RData")

cat("\n========================================\n")
cat("Weight data filtering complete!\n")
cat("========================================\n\n")

# Display summary statistics
cat("SUMMARY STATISTICS:\n")
cat("  Mean weight:", round(mean(weight_final$weight_kg), 2), "kg\n")
cat("  Median weight:", round(median(weight_final$weight_kg), 2), "kg\n")
cat("  Min weight:", round(min(weight_final$weight_kg), 2), "kg\n")
cat("  Max weight:", round(max(weight_final$weight_kg), 2), "kg\n\n")
