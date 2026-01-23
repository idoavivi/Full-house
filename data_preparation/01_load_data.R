# ============================================================================
# Script: 01_load_data.R
# Purpose: Load All of Us exported dataframes
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("========================================\n")
cat("Starting data load process...\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)

# ============================================================================
# ENSURE OUTPUT DIRECTORIES EXIST
# ============================================================================

cat("Creating output directories...\n")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/datasets", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
cat("  - Output directories ready\n\n")

# Check if data has already been loaded from a previous session
if (file.exists("outputs/datasets/01_raw_data.RData")) {
  cat("Loading data from previous session...\n")
  load("outputs/datasets/01_raw_data.RData")
  cat("Data loaded successfully!\n\n")
} else {
  cat("NOTE: This script assumes you have already run the All of Us export code\n")
  cat("      and the following dataframes exist in your environment:\n")
  cat("      - dataset_06597753_person_df\n")
  cat("      - dataset_06597753_fitbit_activity_df\n")
  cat("      - dataset_06597753_fitbit_intraday_steps_df\n")
  cat("      - dataset_06597753_measurement_df\n")
  cat("      - dataset_06597753_condition_df\n")
  cat("      - dataset_06597753_drug_df\n")
  cat("      - dataset_06597753_procedure_df\n")
  cat("      - dataset_06597753_observation_df\n\n")

  # Check that all dataframes exist
  required_dfs <- c(
    "dataset_06597753_person_df",
    "dataset_06597753_fitbit_activity_df",
    "dataset_06597753_fitbit_intraday_steps_df",
    "dataset_06597753_measurement_df",
    "dataset_06597753_condition_df",
    "dataset_06597753_drug_df",
    "dataset_06597753_procedure_df",
    "dataset_06597753_observation_df"
  )

  missing_dfs <- required_dfs[!sapply(required_dfs, exists)]

  if (length(missing_dfs) > 0) {
    cat("ERROR: The following dataframes are missing:\n")
    cat(paste("  -", missing_dfs, collapse = "\n"))
    cat("\n\nPlease run the All of Us export code first.\n")
    stop("Missing required dataframes")
  }

  cat("All required dataframes found!\n\n")

  # Save to RData for future use
  cat("Saving raw data to outputs/datasets/01_raw_data.RData...\n")
  save(
    dataset_06597753_person_df,
    dataset_06597753_fitbit_activity_df,
    dataset_06597753_fitbit_intraday_steps_df,
    dataset_06597753_measurement_df,
    dataset_06597753_condition_df,
    dataset_06597753_drug_df,
    dataset_06597753_procedure_df,
    dataset_06597753_observation_df,
    file = "outputs/datasets/01_raw_data.RData"
  )
  cat("Data saved successfully!\n\n")
}

# Display data summaries
cat("========================================\n")
cat("DATA SUMMARY\n")
cat("========================================\n\n")

cat("Person data: ", nrow(dataset_06597753_person_df), "rows\n")
cat("Fitbit activity data: ", nrow(dataset_06597753_fitbit_activity_df), "rows\n")
cat("Fitbit intraday steps: ", nrow(dataset_06597753_fitbit_intraday_steps_df), "rows\n")
cat("Measurement data: ", nrow(dataset_06597753_measurement_df), "rows\n")
cat("Condition data: ", nrow(dataset_06597753_condition_df), "rows\n")
cat("Drug data: ", nrow(dataset_06597753_drug_df), "rows\n")
cat("Procedure data: ", nrow(dataset_06597753_procedure_df), "rows\n")
cat("Observation data: ", nrow(dataset_06597753_observation_df), "rows\n\n")

cat("========================================\n")
cat("Data load complete!\n")
cat("========================================\n\n")
