# ============================================================================
# Jupyter Notebook: 01_load_data
# Purpose: Load All of Us data from Google Cloud Storage
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

# This notebook is designed to run in All of Us Workbench Jupyter environment
# Each section is a separate cell

# ============================================================================
# CELL 1: Load Required Libraries
# ============================================================================

library(tidyverse)
library(lubridate)
library(bigrquery)

print("Libraries loaded successfully!")

# ============================================================================
# CELL 2: Define Google Cloud Storage Paths
# ============================================================================

# Base path for your data exports
base_path <- "gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123"

# Define paths for each dataset
paths <- list(
  person = paste0(base_path, "/person_06597753/person_06597753_*.csv"),
  fitbit_activity = paste0(base_path, "/fitbit_activity_06597753/fitbit_activity_06597753_*.csv"),
  fitbit_intraday_steps = paste0(base_path, "/fitbit_intraday_steps_06597753/fitbit_intraday_steps_06597753_*.csv"),
  measurement = paste0(base_path, "/measurement_06597753/measurement_06597753_*.csv"),
  procedure = paste0(base_path, "/procedure_06597753/procedure_06597753_*.csv"),
  condition = paste0(base_path, "/condition_06597753/condition_06597753_*.csv"),
  observation = paste0(base_path, "/observation_06597753/observation_06597753_*.csv"),
  drug = paste0(base_path, "/drug_06597753/drug_06597753_*.csv")
)

print("Paths defined:")
print(paths)

# ============================================================================
# CELL 3: Create Output Directories
# ============================================================================

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/datasets", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)

print("Output directories created!")

# ============================================================================
# CELL 4: Load Person Data
# ============================================================================

print("Loading person data...")
dataset_06597753_person_df <- read_csv(
  paths$person,
  col_types = cols(.default = col_character())
)

print(paste("Person data loaded:", nrow(dataset_06597753_person_df), "rows"))
head(dataset_06597753_person_df)

# ============================================================================
# CELL 5: Load Fitbit Activity Data
# ============================================================================

print("Loading Fitbit activity data...")
print("NOTE: This is a large dataset and may take several minutes...")

dataset_06597753_fitbit_activity_df <- read_csv(
  paths$fitbit_activity,
  col_types = cols(.default = col_character())
)

print(paste("Fitbit activity data loaded:",
            format(nrow(dataset_06597753_fitbit_activity_df), big.mark = ","),
            "rows"))
head(dataset_06597753_fitbit_activity_df)

# ============================================================================
# CELL 6: Load Fitbit Intraday Steps Data
# ============================================================================

print("Loading Fitbit intraday steps data...")
print("NOTE: This is a VERY large dataset and may take several minutes...")

dataset_06597753_fitbit_intraday_steps_df <- read_csv(
  paths$fitbit_intraday_steps,
  col_types = cols(.default = col_character())
)

print(paste("Fitbit intraday steps data loaded:",
            format(nrow(dataset_06597753_fitbit_intraday_steps_df), big.mark = ","),
            "rows"))
head(dataset_06597753_fitbit_intraday_steps_df)

# ============================================================================
# CELL 7: Load Measurement Data
# ============================================================================

print("Loading measurement data...")

dataset_06597753_measurement_df <- read_csv(
  paths$measurement,
  col_types = cols(.default = col_character())
)

print(paste("Measurement data loaded:",
            format(nrow(dataset_06597753_measurement_df), big.mark = ","),
            "rows"))
head(dataset_06597753_measurement_df)

# ============================================================================
# CELL 8: Load Procedure Data
# ============================================================================

print("Loading procedure data...")

dataset_06597753_procedure_df <- read_csv(
  paths$procedure,
  col_types = cols(.default = col_character())
)

print(paste("Procedure data loaded:",
            format(nrow(dataset_06597753_procedure_df), big.mark = ","),
            "rows"))
head(dataset_06597753_procedure_df)

# ============================================================================
# CELL 9: Load Condition Data
# ============================================================================

print("Loading condition data...")

dataset_06597753_condition_df <- read_csv(
  paths$condition,
  col_types = cols(.default = col_character())
)

print(paste("Condition data loaded:",
            format(nrow(dataset_06597753_condition_df), big.mark = ","),
            "rows"))
head(dataset_06597753_condition_df)

# ============================================================================
# CELL 10: Load Observation Data
# ============================================================================

print("Loading observation data...")

dataset_06597753_observation_df <- read_csv(
  paths$observation,
  col_types = cols(.default = col_character())
)

print(paste("Observation data loaded:",
            format(nrow(dataset_06597753_observation_df), big.mark = ","),
            "rows"))
head(dataset_06597753_observation_df)

# ============================================================================
# CELL 11: Load Drug Data
# ============================================================================

print("Loading drug data...")

dataset_06597753_drug_df <- read_csv(
  paths$drug,
  col_types = cols(.default = col_character())
)

print(paste("Drug data loaded:",
            format(nrow(dataset_06597753_drug_df), big.mark = ","),
            "rows"))
head(dataset_06597753_drug_df)

# ============================================================================
# CELL 12: Save All Data to RData File
# ============================================================================

print("Saving all data to RData file...")

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

print("Data saved to: outputs/datasets/01_raw_data.RData")

# ============================================================================
# CELL 13: Display Summary
# ============================================================================

cat("\n========================================\n")
cat("DATA LOADING COMPLETE\n")
cat("========================================\n\n")

summary_df <- data.frame(
  Dataset = c(
    "Person",
    "Fitbit Activity",
    "Fitbit Intraday Steps",
    "Measurement",
    "Condition",
    "Drug",
    "Procedure",
    "Observation"
  ),
  Rows = c(
    format(nrow(dataset_06597753_person_df), big.mark = ","),
    format(nrow(dataset_06597753_fitbit_activity_df), big.mark = ","),
    format(nrow(dataset_06597753_fitbit_intraday_steps_df), big.mark = ","),
    format(nrow(dataset_06597753_measurement_df), big.mark = ","),
    format(nrow(dataset_06597753_condition_df), big.mark = ","),
    format(nrow(dataset_06597753_drug_df), big.mark = ","),
    format(nrow(dataset_06597753_procedure_df), big.mark = ","),
    format(nrow(dataset_06597753_observation_df), big.mark = ",")
  )
)

print(summary_df)

cat("\nAll data successfully loaded and saved!\n")
cat("You can now proceed to the next notebook: 02_filter_weight\n")
