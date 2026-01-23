# ============================================================================
# Script: 04_filter_fitbit.R
# Purpose: Validate and process Fitbit activity data
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("========================================\n")
cat("Starting Fitbit data validation...\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)

# Load raw data
load("outputs/datasets/01_raw_data.RData")

# Define validation thresholds
STEPS_MIN <- 100
STEPS_MAX <- 50000
SEDENTARY_MIN <- 180  # minutes
WEAR_TIME_MIN <- 600  # minutes

cat("Validation Thresholds:\n")
cat("  Steps: ", STEPS_MIN, "-", STEPS_MAX, "\n")
cat("  Sedentary time: >=", SEDENTARY_MIN, "minutes\n")
cat("  Wear time: >=", WEAR_TIME_MIN, "minutes\n\n")

# ============================================================================
# PART 1: PROCESS FITBIT ACTIVITY DATA
# ============================================================================

cat("PART 1: PROCESS FITBIT ACTIVITY DATA\n")
cat("-------------------------------------\n\n")

cat("Step 1: Extracting and cleaning Fitbit activity data...\n")

fitbit_raw <- dataset_06597753_fitbit_activity_df %>%
  mutate(
    date = as.Date(date),
    steps = as.numeric(steps),
    sedentary_minutes = as.numeric(sedentary_minutes),
    lightly_active_minutes = as.numeric(lightly_active_minutes),
    fairly_active_minutes = as.numeric(fairly_active_minutes),
    very_active_minutes = as.numeric(very_active_minutes),
    calories_out = as.numeric(calories_out)
  ) %>%
  filter(!is.na(date))

cat("  - Raw Fitbit records:", nrow(fitbit_raw), "\n\n")

# Step 2: Calculate derived variables
cat("Step 2: Calculating derived variables...\n")

fitbit_processed <- fitbit_raw %>%
  mutate(
    # Calculate wear time
    wear_time = sedentary_minutes + lightly_active_minutes +
                fairly_active_minutes + very_active_minutes,

    # Calculate MVPA (moderate to vigorous physical activity)
    mvpa = fairly_active_minutes + very_active_minutes,

    # Replace NA with 0 for minutes if needed
    sedentary_minutes = ifelse(is.na(sedentary_minutes), 0, sedentary_minutes),
    lightly_active_minutes = ifelse(is.na(lightly_active_minutes), 0, lightly_active_minutes),
    fairly_active_minutes = ifelse(is.na(fairly_active_minutes), 0, fairly_active_minutes),
    very_active_minutes = ifelse(is.na(very_active_minutes), 0, very_active_minutes)
  )

cat("  - Derived variables calculated\n\n")

# Step 3: Apply validation criteria
cat("Step 3: Applying validation criteria...\n")

fitbit_validated <- fitbit_processed %>%
  mutate(
    valid_day = (
      !is.na(steps) &
      steps >= STEPS_MIN &
      steps <= STEPS_MAX &
      sedentary_minutes >= SEDENTARY_MIN &
      wear_time >= WEAR_TIME_MIN
    )
  )

n_valid <- sum(fitbit_validated$valid_day, na.rm = TRUE)
n_invalid <- sum(!fitbit_validated$valid_day, na.rm = TRUE)

cat("  - Valid days:", n_valid, "\n")
cat("  - Invalid days:", n_invalid, "\n")
cat("  - Percent valid:", round(100 * n_valid / nrow(fitbit_validated), 1), "%\n\n")

# Step 4: Keep only valid days
cat("Step 4: Filtering to valid days only...\n")

fitbit_valid <- fitbit_validated %>%
  filter(valid_day == TRUE) %>%
  select(
    person_id,
    date,
    steps,
    sedentary_minutes,
    lightly_active_minutes,
    fairly_active_minutes,
    very_active_minutes,
    mvpa,
    wear_time,
    calories_out
  )

cat("  - Valid Fitbit records:", nrow(fitbit_valid), "\n")
cat("  - Unique persons:", n_distinct(fitbit_valid$person_id), "\n\n")

# Step 5: Count valid days per person
cat("Step 5: Counting valid days per person...\n")

person_valid_days <- fitbit_valid %>%
  group_by(person_id) %>%
  summarize(
    total_valid_days = n(),
    first_fitbit_date = min(date),
    last_fitbit_date = max(date),
    fitbit_days_span = as.numeric(difftime(max(date), min(date), units = "days")),
    .groups = "drop"
  )

cat("  - Persons with >= 1 valid day:", nrow(person_valid_days), "\n")
cat("  - Persons with >= 7 valid days:",
    sum(person_valid_days$total_valid_days >= 7), "\n\n")

# Step 6: Calculate person-level baseline activity (mean across all valid days)
cat("Step 6: Calculating baseline activity per person...\n")

baseline_activity <- fitbit_valid %>%
  group_by(person_id) %>%
  summarize(
    mean_steps = mean(steps, na.rm = TRUE),
    mean_sedentary_min = mean(sedentary_minutes, na.rm = TRUE),
    mean_lightly_active_min = mean(lightly_active_minutes, na.rm = TRUE),
    mean_fairly_active_min = mean(fairly_active_minutes, na.rm = TRUE),
    mean_very_active_min = mean(very_active_minutes, na.rm = TRUE),
    mean_mvpa = mean(mvpa, na.rm = TRUE),
    mean_wear_time = mean(wear_time, na.rm = TRUE),
    mean_calories = mean(calories_out, na.rm = TRUE),
    .groups = "drop"
  )

cat("  - Baseline activity calculated for", nrow(baseline_activity), "persons\n\n")

# Save processed data
cat("Saving processed Fitbit data...\n")
save(fitbit_valid, file = "outputs/datasets/04a_fitbit_valid.RData")
save(person_valid_days, file = "outputs/datasets/04b_person_valid_days.RData")
save(baseline_activity, file = "outputs/datasets/04c_baseline_activity.RData")

cat("\n========================================\n")
cat("Fitbit data validation complete!\n")
cat("========================================\n\n")

# Display summary statistics
cat("SUMMARY STATISTICS:\n\n")
cat("Activity Metrics (mean across all valid days):\n")
cat("  Mean steps/day:", round(mean(fitbit_valid$steps), 0), "\n")
cat("  Mean sedentary minutes:", round(mean(fitbit_valid$sedentary_minutes), 0), "\n")
cat("  Mean lightly active minutes:", round(mean(fitbit_valid$lightly_active_minutes), 0), "\n")
cat("  Mean fairly active minutes:", round(mean(fitbit_valid$fairly_active_minutes), 0), "\n")
cat("  Mean very active minutes:", round(mean(fitbit_valid$very_active_minutes), 0), "\n")
cat("  Mean MVPA:", round(mean(fitbit_valid$mvpa), 0), "\n\n")

cat("Valid Days Distribution:\n")
cat("  Median valid days per person:", median(person_valid_days$total_valid_days), "\n")
cat("  Mean valid days per person:", round(mean(person_valid_days$total_valid_days), 1), "\n\n")
