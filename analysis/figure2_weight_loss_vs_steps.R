# ============================================================================
# Script: figure2_weight_loss_vs_steps.R
# Purpose: Create Figure 2 - Scatter plot of weight loss vs change in steps
# Author: Claude AI Assistant
# Date: 2026-01-27
# ============================================================================

cat("========================================\n")
cat("Creating Figure 2: Weight Loss vs Steps Change\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)
library(MatchIt)
library(cobalt)

# ============================================================================
# ENSURE OUTPUT DIRECTORIES EXIST
# ============================================================================

cat("Creating output directories...\n")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/datasets", recursive = TRUE, showWarnings = FALSE)
cat("  - Output directories ready\n\n")

# ============================================================================
# STEP 1: LOAD ALL REQUIRED DATA
# ============================================================================

cat("Step 1: Loading data...\n")

load("outputs/datasets/01_raw_data.RData")
load("outputs/datasets/02_weight_clean.RData")      # weight_final
load("outputs/datasets/03c_baseline_bmi.RData")     # baseline_bmi
load("outputs/datasets/04a_fitbit_valid.RData")     # fitbit_valid (daily valid Fitbit data)
load("outputs/datasets/05a_glp1_users.RData")       # glp1_users
load("outputs/datasets/05c_treatment_categories.RData")  # treatment_categories

cat("  - Data loaded successfully\n\n")

# ============================================================================
# STEP 2: IDENTIFY PEAK AND NADIR WEIGHTS
# ============================================================================

cat("Step 2: Identifying peak and nadir weights for each person...\n")

# For each person, find:
# - Peak weight (maximum weight)
# - Nadir weight (minimum weight at least 30 days AFTER peak)

weight_trajectory <- weight_final %>%
  arrange(person_id, measurement_date) %>%
  group_by(person_id) %>%
  mutate(
    # Find the peak (maximum) weight for this person
    peak_weight_kg = max(weight_kg, na.rm = TRUE)
  ) %>%
  ungroup()

# Get peak weight date (first occurrence of max weight)
peak_weights <- weight_trajectory %>%
  group_by(person_id) %>%
  filter(weight_kg == peak_weight_kg) %>%
  slice(1) %>%  # Take first occurrence if multiple
  ungroup() %>%
  select(person_id, peak_weight_kg, peak_date = measurement_date)

cat("  - Peak weights identified for", nrow(peak_weights), "persons\n")

# Find nadir weight (minimum weight at least 30 days after peak)
nadir_weights <- weight_trajectory %>%
  inner_join(peak_weights, by = "person_id") %>%
  filter(measurement_date >= peak_date + days(30)) %>%  # At least 30 days after peak
  group_by(person_id) %>%
  filter(weight_kg == min(weight_kg, na.rm = TRUE)) %>%
  slice(1) %>%  # Take first occurrence if multiple
  ungroup() %>%
  select(person_id, nadir_weight_kg = weight_kg, nadir_date = measurement_date)

cat("  - Nadir weights identified for", nrow(nadir_weights), "persons\n")

# Combine peak and nadir
weight_change <- peak_weights %>%
  inner_join(nadir_weights, by = "person_id") %>%
  mutate(
    weight_change_kg = nadir_weight_kg - peak_weight_kg,
    weight_change_pct = (nadir_weight_kg - peak_weight_kg) / peak_weight_kg * 100,
    days_to_nadir = as.numeric(nadir_date - peak_date)
  )

cat("  - Weight change calculated for", nrow(weight_change), "persons\n\n")

# ============================================================================
# STEP 3: FILTER TO WEIGHT LOSERS (>5% loss)
# ============================================================================

cat("Step 3: Filtering to weight losers (>5% loss)...\n")

weight_losers <- weight_change %>%
  filter(weight_change_pct <= -5)  # Lost more than 5% (negative value)

cat("  - Weight losers (>5%): ", nrow(weight_losers), "persons\n\n")

# ============================================================================
# STEP 4: MERGE GLP-1 STATUS AND VALIDATE TIMING
# ============================================================================

cat("Step 4: Merging GLP-1 status and validating timing...\n")

# Get GLP-1 start and end dates
glp1_dates <- glp1_users %>%
  select(person_id, glp1_first_date, glp1_last_date)

# Merge with weight losers
weight_losers_glp1 <- weight_losers %>%
  left_join(treatment_categories %>% select(person_id, glp1_user, treatment_category),
            by = "person_id") %>%
  left_join(glp1_dates, by = "person_id") %>%
  mutate(
    # For GLP-1 users, check if nadir is during GLP-1 use
    # Nadir should be between first GLP-1 date and last GLP-1 date (or after if still on it)
    glp1_valid = case_when(
      glp1_user == FALSE | is.na(glp1_user) ~ TRUE,  # Non-GLP1 users are always valid
      glp1_user == TRUE & nadir_date >= glp1_first_date ~ TRUE,  # Nadir during/after GLP1 start
      TRUE ~ FALSE
    ),
    # Create group label
    group = case_when(
      glp1_user == TRUE & glp1_valid ~ "GLP-1 User",
      glp1_user == FALSE | is.na(glp1_user) ~ "Non-GLP-1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group), glp1_valid)

cat("  - GLP-1 users with valid timing:", sum(weight_losers_glp1$group == "GLP-1 User"), "\n")
cat("  - Non-GLP-1 weight losers:", sum(weight_losers_glp1$group == "Non-GLP-1"), "\n\n")

# ============================================================================
# STEP 5: CALCULATE STEPS IN 30-DAY WINDOWS
# ============================================================================

cat("Step 5: Calculating average steps in 30-day windows around weight timepoints...\n")

# Function to get average steps in a window around a date
get_window_steps <- function(person_ids, dates, fitbit_data, window_days = 30) {
  # Create a dataframe with person_id and target_date
  targets <- tibble(
    person_id = person_ids,
    target_date = dates
  )

  # Join with fitbit data and filter to window
  result <- targets %>%
    left_join(fitbit_data, by = "person_id") %>%
    filter(
      activity_date >= target_date - days(window_days),
      activity_date <= target_date + days(window_days)
    ) %>%
    group_by(person_id) %>%
    summarize(
      mean_steps = mean(steps, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    )

  return(result)
}

# Get steps around peak weight date
cat("  - Calculating steps around peak weight...\n")
steps_at_peak <- get_window_steps(
  weight_losers_glp1$person_id,
  weight_losers_glp1$peak_date,
  fitbit_valid %>% select(person_id, activity_date, steps),
  window_days = 30
) %>%
  rename(steps_at_peak = mean_steps, n_days_peak = n_days)

# Get steps around nadir weight date
cat("  - Calculating steps around nadir weight...\n")
steps_at_nadir <- get_window_steps(
  weight_losers_glp1$person_id,
  weight_losers_glp1$nadir_date,
  fitbit_valid %>% select(person_id, activity_date, steps),
  window_days = 30
) %>%
  rename(steps_at_nadir = mean_steps, n_days_nadir = n_days)

# Merge steps with weight data
figure2_data <- weight_losers_glp1 %>%
  left_join(steps_at_peak, by = "person_id") %>%
  left_join(steps_at_nadir, by = "person_id") %>%
  mutate(
    delta_steps = steps_at_nadir - steps_at_peak,
    delta_steps_pct = (steps_at_nadir - steps_at_peak) / steps_at_peak * 100
  ) %>%
  # Filter to those with valid step data at both timepoints
  filter(
    !is.na(steps_at_peak),
    !is.na(steps_at_nadir),
    n_days_peak >= 3,   # At least 3 days of data
    n_days_nadir >= 3
  )

cat("  - Final cohort with step data:", nrow(figure2_data), "persons\n")
cat("    - GLP-1 users:", sum(figure2_data$group == "GLP-1 User"), "\n")
cat("    - Non-GLP-1:", sum(figure2_data$group == "Non-GLP-1"), "\n\n")

# ============================================================================
# STEP 6: SUMMARY STATISTICS
# ============================================================================

cat("Step 6: Summary statistics...\n\n")

summary_stats <- figure2_data %>%
  group_by(group) %>%
  summarize(
    n = n(),
    mean_weight_loss_pct = mean(weight_change_pct, na.rm = TRUE),
    sd_weight_loss_pct = sd(weight_change_pct, na.rm = TRUE),
    mean_delta_steps = mean(delta_steps, na.rm = TRUE),
    sd_delta_steps = sd(delta_steps, na.rm = TRUE),
    mean_days_to_nadir = mean(days_to_nadir, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)
cat("\n")

# ============================================================================
# STEP 7: CREATE FIGURE 2 - SCATTER PLOT
# ============================================================================

cat("Step 7: Creating Figure 2...\n\n")

# Apply outlier bounds from CLAUDE.md
figure2_plot_data <- figure2_data %>%
  filter(
    weight_change_pct >= -25,  # Min weight change
    weight_change_pct <= 10,   # Max weight change (should all be negative anyway)
    delta_steps >= -4000,      # Min steps change
    delta_steps <= 4000        # Max steps change
  )

cat("  - After outlier removal:", nrow(figure2_plot_data), "persons\n\n")

# Create scatter plot
fig2 <- ggplot(figure2_plot_data, aes(x = weight_change_pct, y = delta_steps, color = group)) +
  # Add scatter points
  geom_point(alpha = 0.5, size = 2) +

  # Add horizontal and vertical reference lines at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +

  # Add trend lines (LOESS for non-linear)
  geom_smooth(method = "loess", se = TRUE, size = 1.2, span = 0.75) +

  # Colors
  scale_color_manual(
    name = "Group",
    values = c("GLP-1 User" = "#d73027", "Non-GLP-1" = "#4575b4")
  ) +

  # Axis labels
  labs(
    x = "Weight Change (%)",
    y = "Change in Daily Steps (steps/day)",
    title = "Weight Loss vs Change in Physical Activity",
    subtitle = paste0(
      "N = ", comma(nrow(figure2_plot_data)), " participants with >5% weight loss\n",
      "Steps measured in 30-day windows around peak and nadir weight"
    )
  ) +

  # Formatting
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = comma) +

  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Print the plot
print(fig2)

# ============================================================================
# STEP 8: SAVE FIGURE 2
# ============================================================================

cat("\n\nStep 8: Saving Figure 2...\n")

# Save as PNG
ggsave(
  filename = "outputs/figures/figure2_weight_loss_vs_steps.png",
  plot = fig2,
  width = 10,
  height = 7,
  dpi = 300,
  create.dir = TRUE
)

# Save as PDF
ggsave(
  filename = "outputs/figures/figure2_weight_loss_vs_steps.pdf",
  plot = fig2,
  width = 10,
  height = 7,
  create.dir = TRUE
)

# Save plot object
saveRDS(fig2, file = "outputs/figures/figure2_weight_loss_vs_steps.rds")

# Save underlying data
write_csv(figure2_data, "outputs/figures/figure2_data.csv")

cat("  - Figure saved to:\n")
cat("    - outputs/figures/figure2_weight_loss_vs_steps.png\n")
cat("    - outputs/figures/figure2_weight_loss_vs_steps.pdf\n")
cat("    - outputs/figures/figure2_weight_loss_vs_steps.rds\n")
cat("    - outputs/figures/figure2_data.csv\n\n")

# ============================================================================
# STEP 9: STATISTICAL ANALYSIS
# ============================================================================

cat("Step 9: Statistical analysis...\n\n")

# Correlation: weight loss vs delta steps (overall)
cor_overall <- cor.test(figure2_data$weight_change_pct, figure2_data$delta_steps)
cat("Overall Correlation (Weight Loss % vs Delta Steps):\n")
cat("  r =", round(cor_overall$estimate, 3), "\n")
cat("  p =", format.pval(cor_overall$p.value, digits = 3), "\n\n")

# Correlation by group
cor_glp1 <- cor.test(
  figure2_data$weight_change_pct[figure2_data$group == "GLP-1 User"],
  figure2_data$delta_steps[figure2_data$group == "GLP-1 User"]
)
cat("GLP-1 Users Correlation:\n")
cat("  r =", round(cor_glp1$estimate, 3), "\n")
cat("  p =", format.pval(cor_glp1$p.value, digits = 3), "\n\n")

cor_nonglp1 <- cor.test(
  figure2_data$weight_change_pct[figure2_data$group == "Non-GLP-1"],
  figure2_data$delta_steps[figure2_data$group == "Non-GLP-1"]
)
cat("Non-GLP-1 Correlation:\n")
cat("  r =", round(cor_nonglp1$estimate, 3), "\n")
cat("  p =", format.pval(cor_nonglp1$p.value, digits = 3), "\n\n")

# Linear regression
model <- lm(delta_steps ~ weight_change_pct * group, data = figure2_data)
cat("Linear Regression: Delta Steps ~ Weight Change % * Group\n")
cat("----------------------------------------------------------\n")
print(summary(model))
cat("\n")

# Save regression results
regression_results <- list(
  correlation_overall = cor_overall,
  correlation_glp1 = cor_glp1,
  correlation_nonglp1 = cor_nonglp1,
  linear_model = model
)
saveRDS(regression_results, file = "outputs/figures/figure2_regression_results.rds")

cat("  - Regression results saved to:\n")
cat("    - outputs/figures/figure2_regression_results.rds\n\n")

# ============================================================================
# STEP 10: PROPENSITY SCORE MATCHING
# ============================================================================

cat("Step 10: Propensity score matching (GLP-1 vs Non-GLP-1)...\n\n")

# Prepare data for matching - need demographics
# Get age and sex from person data
person_demographics <- dataset_06597753_person_df %>%
  mutate(
    birth_date = as.Date(date_of_birth),
    age = as.numeric(difftime(Sys.Date(), birth_date, units = "days")) / 365.25,
    sex_female = case_when(
      sex_at_birth == "Female" ~ 1,
      sex_at_birth == "Male" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(person_id, age, sex_female)

# Merge demographics and baseline BMI with figure2 data
match_data <- figure2_data %>%
  left_join(person_demographics, by = "person_id") %>%
  left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  mutate(
    # Create binary treatment variable (1 = GLP-1, 0 = Non-GLP-1)
    glp1_treated = ifelse(group == "GLP-1 User", 1, 0)
  ) %>%
  # Remove rows with missing matching variables
  filter(
    !is.na(age),
    !is.na(sex_female),
    !is.na(baseline_bmi),
    !is.na(peak_weight_kg),
    !is.na(steps_at_peak)
  )

cat("  - Data prepared for matching:", nrow(match_data), "persons\n")
cat("    - GLP-1 users:", sum(match_data$glp1_treated == 1), "\n")
cat("    - Non-GLP-1:", sum(match_data$glp1_treated == 0), "\n\n")

# Perform propensity score matching (1:2 ratio if possible, otherwise 1:1)
# Match on: age, sex, baseline BMI, peak weight, steps at peak
n_glp1 <- sum(match_data$glp1_treated == 1)
n_nonglp1 <- sum(match_data$glp1_treated == 0)

# Determine matching ratio
if (n_nonglp1 >= 2 * n_glp1) {
  match_ratio <- 2
  cat("  - Using 1:2 matching ratio\n")
} else {
  match_ratio <- 1
  cat("  - Using 1:1 matching ratio (insufficient controls for 1:2)\n")
}

# Propensity score matching using nearest neighbor
match_formula <- glp1_treated ~ age + sex_female + baseline_bmi + peak_weight_kg + steps_at_peak

tryCatch({
  match_result <- matchit(
    match_formula,
    data = match_data,
    method = "nearest",
    ratio = match_ratio,
    caliper = 0.2,  # Maximum distance in propensity score
    replace = FALSE
  )

  cat("\n  - Matching complete!\n")
  print(summary(match_result))
  cat("\n")

  # Extract matched data
  matched_data <- match.data(match_result)

  cat("  - Matched cohort size:", nrow(matched_data), "persons\n")
  cat("    - GLP-1 users:", sum(matched_data$glp1_treated == 1), "\n")
  cat("    - Non-GLP-1:", sum(matched_data$glp1_treated == 0), "\n\n")

  # ============================================================================
  # STEP 11: COVARIATE BALANCE CHECK
  # ============================================================================

  cat("Step 11: Checking covariate balance after matching...\n\n")

  # Balance table
  bal_tab <- bal.tab(match_result, un = TRUE)
  print(bal_tab)
  cat("\n")

  # Save balance plot
  png("outputs/figures/figure2b_balance_plot.png", width = 800, height = 600)
  love.plot(match_result,
            stats = c("m", "v"),  # Mean and variance
            thresholds = c(m = 0.1),  # Threshold for standardized mean difference
            var.order = "unadjusted",
            abs = TRUE,
            title = "Covariate Balance: Before and After Matching")
  dev.off()
  cat("  - Balance plot saved to: outputs/figures/figure2b_balance_plot.png\n\n")

  # ============================================================================
  # STEP 12: CREATE FIGURE 2B - MATCHED COHORT
  # ============================================================================

  cat("Step 12: Creating Figure 2b (matched cohort)...\n\n")

  # Apply same outlier bounds
  matched_plot_data <- matched_data %>%
    filter(
      weight_change_pct >= -25,
      weight_change_pct <= 10,
      delta_steps >= -4000,
      delta_steps <= 4000
    )

  cat("  - After outlier removal:", nrow(matched_plot_data), "persons\n\n")

  # Create scatter plot for matched cohort
  fig2b <- ggplot(matched_plot_data, aes(x = weight_change_pct, y = delta_steps, color = group)) +
    # Add scatter points
    geom_point(alpha = 0.5, size = 2) +

    # Add horizontal and vertical reference lines at 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +

    # Add trend lines (LOESS for non-linear)
    geom_smooth(method = "loess", se = TRUE, size = 1.2, span = 0.75) +

    # Colors
    scale_color_manual(
      name = "Group",
      values = c("GLP-1 User" = "#d73027", "Non-GLP-1" = "#4575b4")
    ) +

    # Axis labels
    labs(
      x = "Weight Change (%)",
      y = "Change in Daily Steps (steps/day)",
      title = "Weight Loss vs Change in Physical Activity (Matched Cohort)",
      subtitle = paste0(
        "Propensity score matched (1:", match_ratio, ")\n",
        "N = ", comma(nrow(matched_plot_data)), " participants"
      )
    ) +

    # Formatting
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_continuous(labels = comma) +

    # Theme
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )

  # Print the plot
  print(fig2b)

  # ============================================================================
  # STEP 13: SAVE FIGURE 2B
  # ============================================================================

  cat("\n\nStep 13: Saving Figure 2b...\n")

  # Save as PNG
  ggsave(
    filename = "outputs/figures/figure2b_weight_loss_vs_steps_matched.png",
    plot = fig2b,
    width = 10,
    height = 7,
    dpi = 300,
    create.dir = TRUE
  )

  # Save as PDF
  ggsave(
    filename = "outputs/figures/figure2b_weight_loss_vs_steps_matched.pdf",
    plot = fig2b,
    width = 10,
    height = 7,
    create.dir = TRUE
  )

  # Save plot object
  saveRDS(fig2b, file = "outputs/figures/figure2b_weight_loss_vs_steps_matched.rds")

  # Save matched data
  write_csv(matched_data, "outputs/figures/figure2b_matched_data.csv")

  cat("  - Figure 2b saved to:\n")
  cat("    - outputs/figures/figure2b_weight_loss_vs_steps_matched.png\n")
  cat("    - outputs/figures/figure2b_weight_loss_vs_steps_matched.pdf\n")
  cat("    - outputs/figures/figure2b_weight_loss_vs_steps_matched.rds\n")
  cat("    - outputs/figures/figure2b_matched_data.csv\n\n")

  # ============================================================================
  # STEP 14: STATISTICAL ANALYSIS FOR MATCHED COHORT
  # ============================================================================

  cat("Step 14: Statistical analysis for matched cohort...\n\n")

  # Summary statistics
  summary_stats_matched <- matched_data %>%
    group_by(group) %>%
    summarize(
      n = n(),
      mean_weight_loss_pct = mean(weight_change_pct, na.rm = TRUE),
      sd_weight_loss_pct = sd(weight_change_pct, na.rm = TRUE),
      mean_delta_steps = mean(delta_steps, na.rm = TRUE),
      sd_delta_steps = sd(delta_steps, na.rm = TRUE),
      .groups = "drop"
    )

  cat("Summary Statistics (Matched Cohort):\n")
  print(summary_stats_matched)
  cat("\n")

  # Correlation: weight loss vs delta steps (matched)
  cor_matched <- cor.test(matched_data$weight_change_pct, matched_data$delta_steps)
  cat("Matched Cohort Correlation (Weight Loss % vs Delta Steps):\n")
  cat("  r =", round(cor_matched$estimate, 3), "\n")
  cat("  p =", format.pval(cor_matched$p.value, digits = 3), "\n\n")

  # Correlation by group (matched)
  cor_glp1_matched <- cor.test(
    matched_data$weight_change_pct[matched_data$group == "GLP-1 User"],
    matched_data$delta_steps[matched_data$group == "GLP-1 User"]
  )
  cat("GLP-1 Users Correlation (Matched):\n")
  cat("  r =", round(cor_glp1_matched$estimate, 3), "\n")
  cat("  p =", format.pval(cor_glp1_matched$p.value, digits = 3), "\n\n")

  cor_nonglp1_matched <- cor.test(
    matched_data$weight_change_pct[matched_data$group == "Non-GLP-1"],
    matched_data$delta_steps[matched_data$group == "Non-GLP-1"]
  )
  cat("Non-GLP-1 Correlation (Matched):\n")
  cat("  r =", round(cor_nonglp1_matched$estimate, 3), "\n")
  cat("  p =", format.pval(cor_nonglp1_matched$p.value, digits = 3), "\n\n")

  # T-test comparing delta steps between groups (matched)
  ttest_matched <- t.test(
    delta_steps ~ group,
    data = matched_data
  )
  cat("T-test: Delta Steps by Group (Matched):\n")
  cat("  t =", round(ttest_matched$statistic, 3), "\n")
  cat("  p =", format.pval(ttest_matched$p.value, digits = 3), "\n")
  cat("  Mean GLP-1:", round(ttest_matched$estimate[1], 1), "\n")
  cat("  Mean Non-GLP-1:", round(ttest_matched$estimate[2], 1), "\n\n")

  # Linear regression (matched)
  model_matched <- lm(delta_steps ~ weight_change_pct * group, data = matched_data)
  cat("Linear Regression (Matched): Delta Steps ~ Weight Change % * Group\n")
  cat("--------------------------------------------------------------------\n")
  print(summary(model_matched))
  cat("\n")

  # Save matched regression results
  regression_results_matched <- list(
    match_result = match_result,
    correlation_matched = cor_matched,
    correlation_glp1_matched = cor_glp1_matched,
    correlation_nonglp1_matched = cor_nonglp1_matched,
    ttest_matched = ttest_matched,
    linear_model_matched = model_matched
  )
  saveRDS(regression_results_matched, file = "outputs/figures/figure2b_regression_results.rds")

  cat("  - Matched regression results saved to:\n")
  cat("    - outputs/figures/figure2b_regression_results.rds\n\n")

}, error = function(e) {
  cat("  - ERROR in matching:", conditionMessage(e), "\n")
  cat("  - Skipping matched analysis\n\n")
})

cat("========================================\n")
cat("Figure 2 and 2b creation complete!\n")
cat("========================================\n\n")
