# ============================================================================
# Jupyter Notebook: 02_filter_weight
# Purpose: Filter and clean weight measurements
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

# ============================================================================
# CELL 1: Load Required Libraries and Data
# ============================================================================

library(tidyverse)
library(lubridate)

# Load raw data (from previous notebook)
load("outputs/datasets/01_raw_data.RData")

print("Libraries and data loaded successfully!")

# ============================================================================
# CELL 2: Extract Weight Measurements
# ============================================================================

print("Extracting weight measurements...")

# Include patterns: "body weight", "weight measured"
# Exclude patterns: "birth", "fetal", "ideal", "dry", "dosing", "percentile"

weight_raw <- dataset_06597753_measurement_df %>%
  # Convert to appropriate data types
  mutate(
    value_as_number = as.numeric(value_as_number),
    measurement_datetime = as.POSIXct(measurement_datetime),
    person_id = as.character(person_id)
  ) %>%
  filter(
    grepl("body weight|weight measured", standard_concept_name, ignore.case = TRUE) |
    grepl("body weight|weight measured", source_concept_name, ignore.case = TRUE)
  ) %>%
  filter(
    !grepl("birth|fetal|ideal|dry|dosing|percentile",
           standard_concept_name, ignore.case = TRUE),
    !grepl("birth|fetal|ideal|dry|dosing|percentile",
           source_concept_name, ignore.case = TRUE)
  )

cat("Found", format(nrow(weight_raw), big.mark = ","), "weight measurements\n")
cat("Unique persons:", format(n_distinct(weight_raw$person_id), big.mark = ","), "\n")

# Show some examples
print("Sample weight measurements:")
head(weight_raw %>% select(person_id, measurement_datetime, value_as_number,
                           unit_concept_name, standard_concept_name))

# ============================================================================
# CELL 3: Convert Dates and Remove Missing Values
# ============================================================================

print("\nCleaning dates and values...")

weight_clean <- weight_raw %>%
  mutate(
    measurement_date = as.Date(measurement_datetime),
    weight_value = value_as_number
  ) %>%
  filter(!is.na(measurement_date), !is.na(weight_value))

cat("After removing missing dates/values:",
    format(nrow(weight_clean), big.mark = ","), "measurements\n")

# ============================================================================
# CELL 4: Convert Units to Kilograms
# ============================================================================

print("\nConverting units to kilograms...")

# Check unit distribution before conversion
print("Unit distribution:")
print(table(weight_clean$unit_concept_name, useNA = "ifany"))

weight_clean <- weight_clean %>%
  mutate(
    unit_lower = tolower(unit_concept_name),

    weight_kg = case_when(
      # If unit is kg/kilogram
      grepl("kg|kilogram", unit_lower, ignore.case = TRUE) ~ weight_value,

      # If unit is lbs/pounds
      grepl("lb|pound", unit_lower, ignore.case = TRUE) ~ weight_value * 0.453592,

      # Unknown units - make assumptions based on value
      # If value > 200, assume lbs
      is.na(unit_concept_name) & weight_value > 200 ~ weight_value * 0.453592,

      # If value <= 200, assume kg
      is.na(unit_concept_name) & weight_value <= 200 ~ weight_value,

      # Default: assume kg
      TRUE ~ weight_value
    )
  )

cat("Unit conversion complete!\n")

# Show conversion summary
conversion_summary <- weight_clean %>%
  mutate(
    unit_category = case_when(
      grepl("kg|kilogram", unit_lower, ignore.case = TRUE) ~ "Kilograms",
      grepl("lb|pound", unit_lower, ignore.case = TRUE) ~ "Pounds (converted)",
      is.na(unit_concept_name) & weight_value > 200 ~ "Unknown > 200 (assumed lbs)",
      is.na(unit_concept_name) & weight_value <= 200 ~ "Unknown <= 200 (assumed kg)",
      TRUE ~ "Other"
    )
  ) %>%
  count(unit_category)

print("\nConversion summary:")
print(conversion_summary)

# ============================================================================
# CELL 5: Apply Plausibility Bounds (30-300 kg)
# ============================================================================

print("\nApplying plausibility bounds (30-300 kg)...")

n_before <- nrow(weight_clean)
weight_clean <- weight_clean %>%
  filter(weight_kg >= 30 & weight_kg <= 300)
n_after <- nrow(weight_clean)

cat("Removed", format(n_before - n_after, big.mark = ","),
    "implausible measurements\n")
cat("Remaining:", format(n_after, big.mark = ","), "measurements\n")

# ============================================================================
# CELL 6: Remove Same-Day Duplicates
# ============================================================================

print("\nRemoving same-day duplicates (keeping first measurement)...")

n_before <- nrow(weight_clean)
weight_clean <- weight_clean %>%
  arrange(person_id, measurement_date, measurement_datetime) %>%
  group_by(person_id, measurement_date) %>%
  slice(1) %>%
  ungroup()
n_after <- nrow(weight_clean)

cat("Removed", format(n_before - n_after, big.mark = ","), "duplicate measurements\n")
cat("Remaining:", format(n_after, big.mark = ","), "measurements\n")

# ============================================================================
# CELL 7: Create Person-Level Summary
# ============================================================================

print("\nCreating person-level weight summary...")

weight_final <- weight_clean %>%
  select(person_id, measurement_date, weight_kg) %>%
  arrange(person_id, measurement_date)

person_weight_summary <- weight_final %>%
  group_by(person_id) %>%
  summarize(
    n_weight_measurements = n(),
    first_weight_date = min(measurement_date),
    last_weight_date = max(measurement_date),
    min_weight_kg = min(weight_kg),
    max_weight_kg = max(weight_kg),
    mean_weight_kg = mean(weight_kg),
    .groups = "drop"
  )

cat("Unique persons with weight:",
    format(nrow(person_weight_summary), big.mark = ","), "\n")

# Show distribution of measurement counts
print("\nMeasurements per person:")
print(summary(person_weight_summary$n_weight_measurements))

# ============================================================================
# CELL 8: Visualize Weight Distribution
# ============================================================================

print("\nCreating weight distribution plot...")

library(ggplot2)

p1 <- ggplot(person_weight_summary, aes(x = mean_weight_kg)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Mean Weight",
    x = "Mean Weight (kg)",
    y = "Number of Persons"
  ) +
  theme_minimal()

print(p1)

# ============================================================================
# CELL 9: Save Processed Data
# ============================================================================

print("\nSaving processed weight data...")

save(weight_final, file = "outputs/datasets/02_weight_clean.RData")
save(person_weight_summary, file = "outputs/datasets/02_weight_summary.RData")

cat("Data saved to:\n")
cat("  - outputs/datasets/02_weight_clean.RData\n")
cat("  - outputs/datasets/02_weight_summary.RData\n")

# ============================================================================
# CELL 10: Display Summary Statistics
# ============================================================================

cat("\n========================================\n")
cat("WEIGHT DATA PROCESSING COMPLETE\n")
cat("========================================\n\n")

cat("SUMMARY STATISTICS:\n\n")
cat("Total weight measurements:", format(nrow(weight_final), big.mark = ","), "\n")
cat("Unique persons:", format(nrow(person_weight_summary), big.mark = ","), "\n\n")

cat("Weight Distribution (kg):\n")
cat("  Mean:", round(mean(person_weight_summary$mean_weight_kg), 1), "\n")
cat("  Median:", round(median(person_weight_summary$mean_weight_kg), 1), "\n")
cat("  SD:", round(sd(person_weight_summary$mean_weight_kg), 1), "\n")
cat("  Range:", round(min(person_weight_summary$min_weight_kg), 1), "-",
    round(max(person_weight_summary$max_weight_kg), 1), "\n\n")

cat("Measurements per person:\n")
cat("  Mean:", round(mean(person_weight_summary$n_weight_measurements), 1), "\n")
cat("  Median:", median(person_weight_summary$n_weight_measurements), "\n")
cat("  Range:", min(person_weight_summary$n_weight_measurements), "-",
    max(person_weight_summary$n_weight_measurements), "\n\n")

cat("You can now proceed to the next notebook: 03_filter_height_bmi\n")
