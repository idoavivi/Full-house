# ============================================================================
# Script: 05_identify_glp1_bariatric.R
# Purpose: Identify GLP-1 medication users and bariatric surgery patients
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("========================================\n")
cat("Identifying GLP-1 users and bariatric surgery patients...\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)

# Load raw data
load("outputs/datasets/01_raw_data.RData")

# ============================================================================
# PART 1: IDENTIFY GLP-1 MEDICATION USERS
# ============================================================================

cat("PART 1: GLP-1 MEDICATION USERS\n")
cat("-------------------------------\n\n")

# GLP-1 search patterns
glp1_patterns <- c(
  "semaglutide", "ozempic", "wegovy",
  "tirzepatide", "mounjaro", "zepbound"
)

cat("Searching for GLP-1 medications:\n")
cat("  -", paste(glp1_patterns, collapse = "\n  - "), "\n\n")

# Extract GLP-1 prescriptions
cat("Step 1: Extracting GLP-1 prescriptions...\n")

glp1_rx <- dataset_06597753_drug_df %>%
  filter(
    grepl(paste(glp1_patterns, collapse = "|"),
          standard_concept_name, ignore.case = TRUE) |
    grepl(paste(glp1_patterns, collapse = "|"),
          source_concept_name, ignore.case = TRUE) |
    grepl(paste(glp1_patterns, collapse = "|"),
          drug_source_value, ignore.case = TRUE)
  ) %>%
  mutate(
    rx_date = as.Date(drug_exposure_start_datetime),

    # Identify which GLP-1 type
    glp1_type = case_when(
      grepl("semaglutide|ozempic|wegovy", standard_concept_name, ignore.case = TRUE) ~ "Semaglutide",
      grepl("tirzepatide|mounjaro|zepbound", standard_concept_name, ignore.case = TRUE) ~ "Tirzepatide",
      TRUE ~ "Other GLP-1"
    )
  ) %>%
  filter(!is.na(rx_date)) %>%
  select(person_id, rx_date, glp1_type, standard_concept_name)

cat("  - Found", nrow(glp1_rx), "GLP-1 prescriptions\n")
cat("  - Unique persons:", n_distinct(glp1_rx$person_id), "\n\n")

# Calculate GLP-1 metrics per person
cat("Step 2: Calculating GLP-1 metrics per person...\n")

glp1_users <- glp1_rx %>%
  arrange(person_id, rx_date) %>%
  group_by(person_id) %>%
  summarize(
    glp1_first_date = min(rx_date),
    glp1_last_date = max(rx_date),
    glp1_n_fills = n(),
    glp1_duration_days = as.numeric(difftime(max(rx_date), min(rx_date), units = "days")),
    first_glp1_type = first(glp1_type),
    .groups = "drop"
  ) %>%
  mutate(
    glp1_long_term = (glp1_duration_days > 90 | glp1_n_fills >= 3),
    glp1_user = TRUE
  )

cat("  - GLP-1 users identified:", nrow(glp1_users), "\n")
cat("  - Long-term users (>90 days or >=3 fills):",
    sum(glp1_users$glp1_long_term), "\n\n")

cat("GLP-1 Type Distribution:\n")
print(table(glp1_users$first_glp1_type))
cat("\n\n")

# ============================================================================
# PART 2: IDENTIFY BARIATRIC SURGERY PATIENTS
# ============================================================================

cat("PART 2: BARIATRIC SURGERY PATIENTS\n")
cat("-----------------------------------\n\n")

# Bariatric surgery search patterns
bariatric_patterns <- c(
  "bariatric",
  "gastric bypass",
  "sleeve",
  "roux-en-y",
  "gastric band",
  "duodenal switch"
)

cat("Searching for bariatric surgery:\n")
cat("  -", paste(bariatric_patterns, collapse = "\n  - "), "\n\n")

# Extract bariatric surgery records from procedures
cat("Step 1: Extracting bariatric surgery from procedures...\n")

bariatric_proc <- dataset_06597753_procedure_df %>%
  filter(
    grepl(paste(bariatric_patterns, collapse = "|"),
          standard_concept_name, ignore.case = TRUE) |
    grepl(paste(bariatric_patterns, collapse = "|"),
          source_concept_name, ignore.case = TRUE) |
    grepl(paste(bariatric_patterns, collapse = "|"),
          procedure_source_value, ignore.case = TRUE)
  ) %>%
  mutate(
    surgery_date = as.Date(procedure_datetime)
  ) %>%
  select(person_id, surgery_date, standard_concept_name)

cat("  - Found", nrow(bariatric_proc), "bariatric surgery records\n")
cat("  - Unique persons:", n_distinct(bariatric_proc$person_id), "\n\n")

# Also check conditions for bariatric-related diagnoses
cat("Step 2: Checking conditions for bariatric-related diagnoses...\n")

bariatric_cond <- dataset_06597753_condition_df %>%
  filter(
    grepl(paste(bariatric_patterns, collapse = "|"),
          standard_concept_name, ignore.case = TRUE) |
    grepl(paste(bariatric_patterns, collapse = "|"),
          source_concept_name, ignore.case = TRUE)
  ) %>%
  mutate(
    condition_date = as.Date(condition_start_datetime)
  ) %>%
  select(person_id, condition_date, standard_concept_name)

cat("  - Found", nrow(bariatric_cond), "bariatric-related condition records\n")
cat("  - Unique persons:", n_distinct(bariatric_cond$person_id), "\n\n")

# Combine all bariatric identifications
cat("Step 3: Creating final bariatric surgery list...\n")

bariatric_patients <- bind_rows(
  bariatric_proc %>% select(person_id),
  bariatric_cond %>% select(person_id)
) %>%
  distinct(person_id) %>%
  mutate(bariatric_surgery = TRUE)

cat("  - Total persons with bariatric surgery:", nrow(bariatric_patients), "\n\n")

# ============================================================================
# PART 3: CREATE TREATMENT CATEGORIES
# ============================================================================

cat("PART 3: TREATMENT CATEGORIES\n")
cat("----------------------------\n\n")

# Get all unique persons
load("outputs/datasets/01_raw_data.RData")
all_persons <- dataset_06597753_person_df %>%
  select(person_id)

# Merge treatment information
treatment_categories <- all_persons %>%
  left_join(glp1_users %>% select(person_id, glp1_user), by = "person_id") %>%
  left_join(bariatric_patients, by = "person_id") %>%
  mutate(
    glp1_user = ifelse(is.na(glp1_user), FALSE, glp1_user),
    bariatric_surgery = ifelse(is.na(bariatric_surgery), FALSE, bariatric_surgery),

    treatment_category = case_when(
      glp1_user & bariatric_surgery ~ "GLP1_and_Bariatric",
      glp1_user & !bariatric_surgery ~ "GLP1_only",
      !glp1_user & bariatric_surgery ~ "Bariatric_only",
      !glp1_user & !bariatric_surgery ~ "Neither"
    ),

    include_in_analysis = case_when(
      treatment_category == "GLP1_only" ~ TRUE,
      treatment_category == "Neither" ~ TRUE,  # Will filter further for weight loss
      treatment_category == "Bariatric_only" ~ FALSE,
      treatment_category == "GLP1_and_Bariatric" ~ FALSE
    )
  )

cat("Treatment Category Distribution:\n")
print(table(treatment_categories$treatment_category))
cat("\n\n")

cat("Include in Analysis:\n")
print(table(treatment_categories$include_in_analysis,
            treatment_categories$treatment_category))
cat("\n\n")

# Save processed data
cat("Saving processed data...\n")
save(glp1_users, file = "outputs/datasets/05a_glp1_users.RData")
save(bariatric_patients, file = "outputs/datasets/05b_bariatric_patients.RData")
save(treatment_categories, file = "outputs/datasets/05c_treatment_categories.RData")

cat("\n========================================\n")
cat("GLP-1 and bariatric identification complete!\n")
cat("========================================\n\n")

cat("SUMMARY:\n")
cat("  Total persons:", nrow(treatment_categories), "\n")
cat("  GLP-1 only:", sum(treatment_categories$treatment_category == "GLP1_only"), "\n")
cat("  Neither:", sum(treatment_categories$treatment_category == "Neither"), "\n")
cat("  Bariatric only (EXCLUDED):",
    sum(treatment_categories$treatment_category == "Bariatric_only"), "\n")
cat("  GLP-1 + Bariatric (EXCLUDED):",
    sum(treatment_categories$treatment_category == "GLP1_and_Bariatric"), "\n\n")
