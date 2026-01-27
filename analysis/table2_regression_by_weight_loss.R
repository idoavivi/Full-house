# ============================================================================
# Script: table2_regression_by_weight_loss.R
# Purpose: Create Table 2 - Regression analysis of weight loss vs step change
#          by GLP-1 status, for >5% and >10% weight loss cohorts
# Author: Claude AI Assistant
# Date: 2026-01-27
# ============================================================================

cat("========================================\n")
cat("Creating Table 2: Regression by Weight Loss and GLP-1 Status\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)
library(gtsummary)
library(gt)
library(broom)

# ============================================================================
# ENSURE OUTPUT DIRECTORIES EXIST
# ============================================================================

cat("Creating output directories...\n")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/datasets", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
cat("  - Output directories ready\n\n")

# ============================================================================
# STEP 1: LOAD FIGURE 2 DATA
# ============================================================================

cat("Step 1: Loading Figure 2 data...\n")

# Check if figure2_data exists from a previous run
if (file.exists("outputs/figures/figure2_data.csv")) {
  figure2_data <- read_csv("outputs/figures/figure2_data.csv", show_col_types = FALSE)
  cat("  - Loaded figure2_data.csv:", nrow(figure2_data), "persons\n\n")
} else {
  stop("Figure 2 data not found. Please run figure2_weight_loss_vs_steps.R first.")
}

# ============================================================================
# STEP 2: CREATE TWO COHORTS (>5% and >10% weight loss)
# ============================================================================

cat("Step 2: Creating weight loss cohorts...\n")

# Cohort 1: >5% weight loss (already filtered in figure2_data)
cohort_5pct <- figure2_data %>%
  filter(weight_change_pct <= -5)

cat("  - >5% weight loss cohort:", nrow(cohort_5pct), "persons\n")
cat("    - GLP-1 users:", sum(cohort_5pct$group == "GLP-1 User"), "\n")
cat("    - Non-GLP-1:", sum(cohort_5pct$group == "Non-GLP-1"), "\n\n")

# Cohort 2: >10% weight loss
cohort_10pct <- figure2_data %>%
  filter(weight_change_pct <= -10)

cat("  - >10% weight loss cohort:", nrow(cohort_10pct), "persons\n")
cat("    - GLP-1 users:", sum(cohort_10pct$group == "GLP-1 User"), "\n")
cat("    - Non-GLP-1:", sum(cohort_10pct$group == "Non-GLP-1"), "\n\n")

# ============================================================================
# STEP 3: FUNCTION TO RUN REGRESSIONS AND EXTRACT RESULTS
# ============================================================================

run_regression_analysis <- function(data, cohort_name) {
  cat("Running regression analysis for", cohort_name, "...\n")

  results <- list()

  # Overall regression
  model_overall <- lm(delta_steps ~ weight_change_pct, data = data)
  results$overall <- tidy(model_overall, conf.int = TRUE)
  results$overall_r2 <- glance(model_overall)$r.squared
  results$overall_n <- nrow(data)

  # GLP-1 users only
  glp1_data <- data %>% filter(group == "GLP-1 User")
  if (nrow(glp1_data) >= 10) {
    model_glp1 <- lm(delta_steps ~ weight_change_pct, data = glp1_data)
    results$glp1 <- tidy(model_glp1, conf.int = TRUE)
    results$glp1_r2 <- glance(model_glp1)$r.squared
    results$glp1_n <- nrow(glp1_data)
  } else {
    results$glp1 <- NULL
    results$glp1_r2 <- NA
    results$glp1_n <- nrow(glp1_data)
  }

  # Non-GLP-1 users only
  nonglp1_data <- data %>% filter(group == "Non-GLP-1")
  if (nrow(nonglp1_data) >= 10) {
    model_nonglp1 <- lm(delta_steps ~ weight_change_pct, data = nonglp1_data)
    results$nonglp1 <- tidy(model_nonglp1, conf.int = TRUE)
    results$nonglp1_r2 <- glance(model_nonglp1)$r.squared
    results$nonglp1_n <- nrow(nonglp1_data)
  } else {
    results$nonglp1 <- NULL
    results$nonglp1_r2 <- NA
    results$nonglp1_n <- nrow(nonglp1_data)
  }

  # Interaction model (tests if slopes differ between groups)
  if (nrow(glp1_data) >= 10 && nrow(nonglp1_data) >= 10) {
    model_interaction <- lm(delta_steps ~ weight_change_pct * group, data = data)
    results$interaction <- tidy(model_interaction, conf.int = TRUE)
    results$interaction_r2 <- glance(model_interaction)$r.squared
  } else {
    results$interaction <- NULL
    results$interaction_r2 <- NA
  }

  return(results)
}

# ============================================================================
# STEP 4: RUN REGRESSIONS FOR BOTH COHORTS
# ============================================================================

cat("\nStep 4: Running regressions...\n\n")

results_5pct <- run_regression_analysis(cohort_5pct, ">5% weight loss")
results_10pct <- run_regression_analysis(cohort_10pct, ">10% weight loss")

# ============================================================================
# STEP 5: CREATE SUMMARY TABLE
# ============================================================================

cat("\nStep 5: Creating summary tables...\n\n")

# Function to format regression results
format_regression_row <- function(results, group_name, cohort_name) {
  if (group_name == "Overall") {
    coef_data <- results$overall %>% filter(term == "weight_change_pct")
    r2 <- results$overall_r2
    n <- results$overall_n
  } else if (group_name == "GLP-1 User") {
    if (is.null(results$glp1)) return(NULL)
    coef_data <- results$glp1 %>% filter(term == "weight_change_pct")
    r2 <- results$glp1_r2
    n <- results$glp1_n
  } else if (group_name == "Non-GLP-1") {
    if (is.null(results$nonglp1)) return(NULL)
    coef_data <- results$nonglp1 %>% filter(term == "weight_change_pct")
    r2 <- results$nonglp1_r2
    n <- results$nonglp1_n
  }

  if (nrow(coef_data) == 0) return(NULL)

  tibble(
    Cohort = cohort_name,
    Group = group_name,
    N = n,
    Beta = round(coef_data$estimate, 1),
    SE = round(coef_data$std.error, 1),
    CI_lower = round(coef_data$conf.low, 1),
    CI_upper = round(coef_data$conf.high, 1),
    CI = paste0("(", round(coef_data$conf.low, 1), ", ", round(coef_data$conf.high, 1), ")"),
    t_value = round(coef_data$statistic, 2),
    p_value = coef_data$p.value,
    p_formatted = case_when(
      coef_data$p.value < 0.001 ~ "<0.001",
      coef_data$p.value < 0.01 ~ sprintf("%.3f", coef_data$p.value),
      TRUE ~ sprintf("%.3f", coef_data$p.value)
    ),
    R_squared = round(r2, 3)
  )
}

# Build summary table
table2_data <- bind_rows(
  format_regression_row(results_5pct, "Overall", ">5% weight loss"),
  format_regression_row(results_5pct, "GLP-1 User", ">5% weight loss"),
  format_regression_row(results_5pct, "Non-GLP-1", ">5% weight loss"),
  format_regression_row(results_10pct, "Overall", ">10% weight loss"),
  format_regression_row(results_10pct, "GLP-1 User", ">10% weight loss"),
  format_regression_row(results_10pct, "Non-GLP-1", ">10% weight loss")
)

cat("Regression Summary Table:\n")
print(table2_data %>% select(Cohort, Group, N, Beta, SE, CI, p_formatted, R_squared))
cat("\n")

# ============================================================================
# STEP 6: INTERACTION TESTS (DO SLOPES DIFFER?)
# ============================================================================

cat("Step 6: Testing if slopes differ between GLP-1 and Non-GLP-1...\n\n")

# Extract interaction term p-values
interaction_5pct <- results_5pct$interaction %>%
  filter(grepl("weight_change_pct:group", term))
interaction_10pct <- results_10pct$interaction %>%
  filter(grepl("weight_change_pct:group", term))

cat(">5% Weight Loss - Interaction (slope difference) test:\n")
if (!is.null(interaction_5pct) && nrow(interaction_5pct) > 0) {
  cat("  Beta (interaction):", round(interaction_5pct$estimate, 2), "\n")
  cat("  p-value:", format.pval(interaction_5pct$p.value, digits = 3), "\n")
  if (interaction_5pct$p.value < 0.05) {
    cat("  => Slopes significantly DIFFER between groups\n\n")
  } else {
    cat("  => Slopes do NOT significantly differ between groups\n\n")
  }
} else {
  cat("  Insufficient data for interaction test\n\n")
}

cat(">10% Weight Loss - Interaction (slope difference) test:\n")
if (!is.null(interaction_10pct) && nrow(interaction_10pct) > 0) {
  cat("  Beta (interaction):", round(interaction_10pct$estimate, 2), "\n")
  cat("  p-value:", format.pval(interaction_10pct$p.value, digits = 3), "\n")
  if (interaction_10pct$p.value < 0.05) {
    cat("  => Slopes significantly DIFFER between groups\n\n")
  } else {
    cat("  => Slopes do NOT significantly differ between groups\n\n")
  }
} else {
  cat("  Insufficient data for interaction test\n\n")
}

# Add interaction results to table
interaction_table <- bind_rows(
  tibble(
    Cohort = ">5% weight loss",
    Test = "GLP-1 x Non-GLP-1 slope difference",
    Beta_interaction = ifelse(!is.null(interaction_5pct) && nrow(interaction_5pct) > 0,
                              round(interaction_5pct$estimate, 2), NA),
    p_interaction = ifelse(!is.null(interaction_5pct) && nrow(interaction_5pct) > 0,
                          interaction_5pct$p.value, NA)
  ),
  tibble(
    Cohort = ">10% weight loss",
    Test = "GLP-1 x Non-GLP-1 slope difference",
    Beta_interaction = ifelse(!is.null(interaction_10pct) && nrow(interaction_10pct) > 0,
                              round(interaction_10pct$estimate, 2), NA),
    p_interaction = ifelse(!is.null(interaction_10pct) && nrow(interaction_10pct) > 0,
                          interaction_10pct$p.value, NA)
  )
)

# ============================================================================
# STEP 7: CREATE FORMATTED GT TABLE
# ============================================================================

cat("Step 7: Creating formatted table...\n\n")

table2_gt <- table2_data %>%
  select(Cohort, Group, N, Beta, SE, CI, p_formatted, R_squared) %>%
  rename(
    `Sample Size` = N,
    `β (steps/% weight)` = Beta,
    `Std. Error` = SE,
    `95% CI` = CI,
    `p-value` = p_formatted,
    `R²` = R_squared
  ) %>%
  gt(groupname_col = "Cohort") %>%
  tab_header(
    title = "Table 2. Association Between Weight Loss and Change in Daily Steps",
    subtitle = "Linear regression: ΔSteps = β × Weight Change (%)"
  ) %>%
  tab_footnote(
    footnote = "β represents change in daily steps per 1% weight change. Negative weight change indicates loss.",
    locations = cells_column_labels(columns = `β (steps/% weight)`)
  ) %>%
  tab_footnote(
    footnote = "GLP-1 users: nadir weight during treatment period. Non-GLP-1: weight losers without GLP-1 medications.",
    locations = cells_column_labels(columns = Group)
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Steps measured in 30-day windows around peak and nadir weight. ",
      "ΔSteps = steps at nadir - steps at peak."
    )
  ) %>%
  cols_align(align = "center", columns = c(`Sample Size`, `β (steps/% weight)`,
                                           `Std. Error`, `95% CI`, `p-value`, `R²`)) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `p-value`,
      rows = p_value < 0.05
    )
  )

print(table2_gt)

# ============================================================================
# STEP 8: SAVE TABLES
# ============================================================================

cat("\n\nStep 8: Saving tables...\n")

# Save as HTML
gtsave(table2_gt, filename = "outputs/tables/table2_regression_by_glp1.html")

# Save as CSV
write_csv(table2_data, "outputs/tables/table2_regression_data.csv")
write_csv(interaction_table, "outputs/tables/table2_interaction_tests.csv")

# Save as RDS
saveRDS(list(
  table2_data = table2_data,
  interaction_table = interaction_table,
  results_5pct = results_5pct,
  results_10pct = results_10pct
), file = "outputs/tables/table2_regression_results.rds")

cat("  - Table 2 saved to:\n")
cat("    - outputs/tables/table2_regression_by_glp1.html\n")
cat("    - outputs/tables/table2_regression_data.csv\n")
cat("    - outputs/tables/table2_interaction_tests.csv\n")
cat("    - outputs/tables/table2_regression_results.rds\n\n")

# ============================================================================
# STEP 9: PRINT DETAILED RESULTS
# ============================================================================

cat("========================================\n")
cat("DETAILED REGRESSION RESULTS\n")
cat("========================================\n\n")

cat("--- >5% WEIGHT LOSS COHORT ---\n\n")

cat("Overall (N =", results_5pct$overall_n, "):\n")
print(results_5pct$overall)
cat("\n")

if (!is.null(results_5pct$glp1)) {
  cat("GLP-1 Users (N =", results_5pct$glp1_n, "):\n")
  print(results_5pct$glp1)
  cat("\n")
}

if (!is.null(results_5pct$nonglp1)) {
  cat("Non-GLP-1 (N =", results_5pct$nonglp1_n, "):\n")
  print(results_5pct$nonglp1)
  cat("\n")
}

if (!is.null(results_5pct$interaction)) {
  cat("Interaction Model:\n")
  print(results_5pct$interaction)
  cat("\n")
}

cat("--- >10% WEIGHT LOSS COHORT ---\n\n")

cat("Overall (N =", results_10pct$overall_n, "):\n")
print(results_10pct$overall)
cat("\n")

if (!is.null(results_10pct$glp1)) {
  cat("GLP-1 Users (N =", results_10pct$glp1_n, "):\n")
  print(results_10pct$glp1)
  cat("\n")
}

if (!is.null(results_10pct$nonglp1)) {
  cat("Non-GLP-1 (N =", results_10pct$nonglp1_n, "):\n")
  print(results_10pct$nonglp1)
  cat("\n")
}

if (!is.null(results_10pct$interaction)) {
  cat("Interaction Model:\n")
  print(results_10pct$interaction)
  cat("\n")
}

cat("========================================\n")
cat("Table 2 creation complete!\n")
cat("========================================\n\n")
