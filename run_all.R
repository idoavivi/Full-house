# ============================================================================
# Master Script: run_all.R
# Purpose: Run all analysis scripts in order
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("  FULL HOUSE RESEARCH PROJECT - MASTER ANALYSIS SCRIPT\n")
cat("  Bidirectional Relationships Between Physical Activity and Weight Loss\n")
cat("================================================================================\n\n")

cat("This script will run all analysis steps in order:\n")
cat("  1. Load data\n")
cat("  2. Filter weight measurements\n")
cat("  3. Filter height and compute BMI\n")
cat("  4. Validate Fitbit data\n")
cat("  5. Identify GLP-1 users and bariatric surgery patients\n")
cat("  6. Create Table 1 (Demographics by BMI class)\n")
cat("  7. Create Figure 1 (Steps by BMI scatter plot)\n\n")

cat("NOTE: Before running this script, make sure you have:\n")
cat("  1. Run all the All of Us export code to create the dataframes\n")
cat("  2. Installed all required packages (tidyverse, gtsummary, etc.)\n\n")

response <- readline(prompt = "Do you want to continue? (yes/no): ")

if (tolower(response) != "yes") {
  cat("\nAnalysis cancelled.\n")
  stop("User cancelled analysis")
}

cat("\n================================================================================\n\n")

# Track timing
start_time <- Sys.time()

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

cat("STEP 1 OF 7: Loading data...\n")
cat("----------------------------\n")
source("data_preparation/01_load_data.R")
cat("STEP 1 COMPLETE\n\n")

# ============================================================================
# STEP 2: FILTER WEIGHT
# ============================================================================

cat("STEP 2 OF 7: Filtering weight data...\n")
cat("--------------------------------------\n")
source("data_preparation/02_filter_weight.R")
cat("STEP 2 COMPLETE\n\n")

# ============================================================================
# STEP 3: FILTER HEIGHT AND BMI
# ============================================================================

cat("STEP 3 OF 7: Filtering height and computing BMI...\n")
cat("---------------------------------------------------\n")
source("data_preparation/03_filter_height_bmi.R")
cat("STEP 3 COMPLETE\n\n")

# ============================================================================
# STEP 4: VALIDATE FITBIT
# ============================================================================

cat("STEP 4 OF 7: Validating Fitbit data...\n")
cat("---------------------------------------\n")
source("data_preparation/04_filter_fitbit.R")
cat("STEP 4 COMPLETE\n\n")

# ============================================================================
# STEP 5: IDENTIFY GLP-1 AND BARIATRIC
# ============================================================================

cat("STEP 5 OF 7: Identifying GLP-1 users and bariatric surgery...\n")
cat("-------------------------------------------------------------\n")
source("data_preparation/05_identify_glp1_bariatric.R")
cat("STEP 5 COMPLETE\n\n")

# ============================================================================
# STEP 6: CREATE TABLE 1
# ============================================================================

cat("STEP 6 OF 7: Creating Table 1...\n")
cat("---------------------------------\n")
source("analysis/table1_demographics_by_bmi.R")
cat("STEP 6 COMPLETE\n\n")

# ============================================================================
# STEP 7: CREATE FIGURE 1
# ============================================================================

cat("STEP 7 OF 7: Creating Figure 1...\n")
cat("----------------------------------\n")
source("analysis/figure1_steps_by_bmi.R")
cat("STEP 7 COMPLETE\n\n")

# ============================================================================
# ANALYSIS COMPLETE
# ============================================================================

end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "mins")

cat("================================================================================\n")
cat("  ALL ANALYSIS STEPS COMPLETE!\n")
cat("================================================================================\n\n")

cat("Total time elapsed:", round(elapsed_time, 2), "minutes\n\n")

cat("Output files created:\n")
cat("  - outputs/datasets/*.RData (processed data files)\n")
cat("  - outputs/tables/table1_demographics_by_bmi.rds\n")
cat("  - outputs/tables/table1_summary_stats.csv\n")
cat("  - outputs/figures/figure1_steps_by_bmi.png\n")
cat("  - outputs/figures/figure1_steps_by_bmi.pdf\n")
cat("  - outputs/figures/figure1_regression_results.rds\n\n")

cat("Next steps:\n")
cat("  1. Review Table 1: readRDS('outputs/tables/table1_demographics_by_bmi.rds')\n")
cat("  2. View Figure 1: Open outputs/figures/figure1_steps_by_bmi.png\n")
cat("  3. Continue with additional analyses as needed\n\n")

cat("================================================================================\n\n")
