# ============================================================================
# Script: figure2_final_comparison.R
# Purpose: Compare weight loss vs step change for GLP-1 users vs Non-GLP-1
#          Using optimized parameters from sensitivity analysis
# Parameters: 90d baseline window, 90d nadir window, min 5 Fitbit days
# ============================================================================

cat("========================================\n")
cat("Figure 2: GLP-1 vs Non-GLP-1 Comparison\n")
cat("========================================\n\n")

library(tidyverse)
library(lubridate)
library(scales)
library(MatchIt)

# Create output directories
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")
load("outputs/datasets/02_weight_clean.RData")
load("outputs/datasets/03c_baseline_bmi.RData")
load("outputs/datasets/04a_fitbit_valid.RData")
load("outputs/datasets/05a_glp1_users.RData")
load("outputs/datasets/05c_treatment_categories.RData")
cat("  Done!\n\n")

# Parameters from combination 4
BASE_WINDOW <- 90   # days before baseline for activity
NADIR_WINDOW <- 90  # days around nadir for activity
MIN_FITBIT_DAYS <- 5

cat("Parameters:\n")
cat("  Baseline activity window:", BASE_WINDOW, "days BEFORE baseline\n")
cat("  Nadir activity window:", NADIR_WINDOW, "days around nadir\n")
cat("  Min Fitbit days:", MIN_FITBIT_DAYS, "\n\n")

# ============================================================================
# PART 1: BUILD GLP-1 COHORT
# ============================================================================

cat("PART 1: Building GLP-1 cohort...\n")

# Step 1: Get baseline weight (closest to treatment start)
cat("  Step 1: Finding baseline weight at treatment start...\n")
glp1_baseline <- weight_final %>%
  filter(person_id %in% glp1_users$person_id) %>%
  left_join(glp1_users %>% select(person_id, glp1_first_date, glp1_last_date), by = "person_id") %>%
  filter(measurement_date >= glp1_first_date - 90,
         measurement_date <= glp1_first_date + 30) %>%
  group_by(person_id) %>%
  slice_min(abs(as.numeric(difftime(measurement_date, glp1_first_date, units = "days"))), n = 1) %>%
  ungroup() %>%
  select(person_id, baseline_weight = weight_kg, baseline_date = measurement_date,
         glp1_first_date, glp1_last_date)

cat("    - Persons with baseline weight:", n_distinct(glp1_baseline$person_id), "\n")

# Step 2: Apply BMI filter (>= 27)
cat("  Step 2: Applying BMI >= 27 filter...\n")
glp1_baseline <- glp1_baseline %>%
  left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  filter(!is.na(baseline_bmi), baseline_bmi >= 27)

cat("    - After BMI filter:", n_distinct(glp1_baseline$person_id), "\n")

# Step 3: Get nadir (during treatment OR within 90 days after end)
cat("  Step 3: Finding nadir weight (during tx or within 90d after)...\n")
glp1_nadir <- weight_final %>%
  inner_join(glp1_baseline, by = "person_id") %>%
  filter(measurement_date > baseline_date,
         measurement_date >= glp1_first_date,
         measurement_date <= glp1_last_date + 90) %>%
  group_by(person_id) %>%
  slice_min(weight_kg, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(nadir_weight = weight_kg, nadir_date = measurement_date) %>%
  mutate(weight_change_pct = (nadir_weight - baseline_weight) / baseline_weight * 100) %>%
  filter(weight_change_pct <= -5)  # At least 5% weight loss

cat("    - With valid nadir (>=5% loss):", n_distinct(glp1_nadir$person_id), "\n")

# Step 4: Get activity BEFORE baseline (90 days before)
cat("  Step 4: Getting activity BEFORE baseline...\n")
glp1_steps_baseline <- glp1_nadir %>%
  select(person_id, baseline_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(date >= baseline_date - BASE_WINDOW,
         date < baseline_date) %>%  # BEFORE baseline, not around it
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_baseline = mean(steps, na.rm = TRUE),
            n_days_baseline = n(),
            .groups = "drop")

cat("    - With sufficient Fitbit at baseline:", nrow(glp1_steps_baseline), "\n")

# Step 5: Get activity around nadir
cat("  Step 5: Getting activity around nadir...\n")
glp1_steps_nadir <- glp1_nadir %>%
  select(person_id, nadir_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(abs(as.numeric(difftime(date, nadir_date, units = "days"))) <= NADIR_WINDOW) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_nadir = mean(steps, na.rm = TRUE),
            n_days_nadir = n(),
            .groups = "drop")

cat("    - With sufficient Fitbit at nadir:", nrow(glp1_steps_nadir), "\n")

# Step 6: Combine into final GLP-1 cohort
glp1_cohort <- glp1_nadir %>%
  inner_join(glp1_steps_baseline, by = "person_id") %>%
  inner_join(glp1_steps_nadir, by = "person_id") %>%
  mutate(
    delta_steps = steps_nadir - steps_baseline,
    group = "GLP-1 User"
  )

cat("\n  FINAL GLP-1 COHORT:", nrow(glp1_cohort), "persons\n\n")

# ============================================================================
# PART 2: BUILD NON-GLP-1 COHORT (using peak weight as baseline)
# ============================================================================

cat("PART 2: Building Non-GLP-1 cohort...\n")

# Get non-GLP-1 persons
non_glp1_ids <- treatment_categories %>%
  filter(treatment_category == "Neither") %>%
  pull(person_id)

cat("  - Non-GLP-1 persons:", length(non_glp1_ids), "\n")

# Exclude bariatric surgery
bariatric <- treatment_categories %>%
  filter(treatment_category %in% c("Bariatric_only", "GLP1_and_Bariatric")) %>%
  pull(person_id)

# Get weight trajectory (peak to nadir)
cat("  Step 1: Finding peak and nadir weights...\n")
non_glp1_trajectory <- suppressWarnings(
  weight_final %>%
    filter(person_id %in% non_glp1_ids, !person_id %in% bariatric) %>%
    group_by(person_id) %>%
    filter(n() >= 2) %>%
    arrange(measurement_date) %>%
    summarize(
      peak_weight = max(weight_kg, na.rm = TRUE),
      peak_date = measurement_date[which.max(weight_kg)],
      nadir_weight = min(weight_kg[measurement_date > peak_date], na.rm = TRUE),
      nadir_date = measurement_date[measurement_date > peak_date][which.min(weight_kg[measurement_date > peak_date])],
      .groups = "drop"
    ) %>%
    filter(!is.infinite(nadir_weight), nadir_date > peak_date) %>%
    mutate(
      weight_change_pct = (nadir_weight - peak_weight) / peak_weight * 100,
      days_to_nadir = as.numeric(difftime(nadir_date, peak_date, units = "days"))
    ) %>%
    filter(weight_change_pct <= -5, days_to_nadir >= 30)  # At least 5% loss, 30+ days
)

cat("    - With valid trajectory:", nrow(non_glp1_trajectory), "\n")

# Get steps BEFORE peak (baseline)
cat("  Step 2: Getting activity BEFORE peak...\n")
non_glp1_steps_baseline <- non_glp1_trajectory %>%
  select(person_id, peak_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(date >= peak_date - BASE_WINDOW,
         date < peak_date) %>%  # BEFORE peak
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_baseline = mean(steps, na.rm = TRUE),
            n_days_baseline = n(),
            .groups = "drop")

cat("    - With sufficient Fitbit at baseline:", nrow(non_glp1_steps_baseline), "\n")

# Get steps around nadir
cat("  Step 3: Getting activity around nadir...\n")
non_glp1_steps_nadir <- non_glp1_trajectory %>%
  select(person_id, nadir_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(abs(as.numeric(difftime(date, nadir_date, units = "days"))) <= NADIR_WINDOW) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_nadir = mean(steps, na.rm = TRUE),
            n_days_nadir = n(),
            .groups = "drop")

cat("    - With sufficient Fitbit at nadir:", nrow(non_glp1_steps_nadir), "\n")

# Combine into final Non-GLP-1 cohort
non_glp1_cohort <- non_glp1_trajectory %>%
  rename(baseline_weight = peak_weight, baseline_date = peak_date) %>%
  inner_join(non_glp1_steps_baseline, by = "person_id") %>%
  inner_join(non_glp1_steps_nadir, by = "person_id") %>%
  left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  mutate(
    delta_steps = steps_nadir - steps_baseline,
    group = "Non-GLP-1"
  )

cat("\n  FINAL NON-GLP-1 COHORT:", nrow(non_glp1_cohort), "persons\n\n")

# ============================================================================
# PART 3: COMBINE AND CREATE SCATTER PLOT
# ============================================================================

cat("PART 3: Creating comparison scatter plot...\n")

# Combine cohorts
combined_data <- bind_rows(
  glp1_cohort %>% select(person_id, baseline_weight, nadir_weight, baseline_date, nadir_date,
                          weight_change_pct, steps_baseline, steps_nadir, delta_steps,
                          baseline_bmi, group),
  non_glp1_cohort %>% select(person_id, baseline_weight, nadir_weight, baseline_date, nadir_date,
                              weight_change_pct, steps_baseline, steps_nadir, delta_steps,
                              baseline_bmi, group)
)

cat("  Combined data:", nrow(combined_data), "persons\n")
cat("    - GLP-1:", sum(combined_data$group == "GLP-1 User"), "\n")
cat("    - Non-GLP-1:", sum(combined_data$group == "Non-GLP-1"), "\n\n")

# Run regressions
cat("Running regressions...\n\n")

# Overall
model_overall <- lm(delta_steps ~ weight_change_pct, data = combined_data)
coef_overall <- summary(model_overall)$coefficients

# GLP-1 only
glp1_data <- combined_data %>% filter(group == "GLP-1 User")
model_glp1 <- lm(delta_steps ~ weight_change_pct, data = glp1_data)
coef_glp1 <- summary(model_glp1)$coefficients

# Non-GLP-1 only
nonglp1_data <- combined_data %>% filter(group == "Non-GLP-1")
model_nonglp1 <- lm(delta_steps ~ weight_change_pct, data = nonglp1_data)
coef_nonglp1 <- summary(model_nonglp1)$coefficients

cat("REGRESSION RESULTS:\n")
cat("-------------------\n\n")

cat("Overall (N =", nrow(combined_data), "):\n")
cat("  Slope:", round(coef_overall["weight_change_pct", "Estimate"], 1), "steps per 1% weight change\n")
cat("  p-value:", format.pval(coef_overall["weight_change_pct", "Pr(>|t|)"], digits = 3), "\n\n")

cat("GLP-1 Users (N =", nrow(glp1_data), "):\n")
cat("  Slope:", round(coef_glp1["weight_change_pct", "Estimate"], 1), "steps per 1% weight change\n")
cat("  p-value:", format.pval(coef_glp1["weight_change_pct", "Pr(>|t|)"], digits = 3), "\n")
if (coef_glp1["weight_change_pct", "Pr(>|t|)"] >= 0.05) {
  cat("  => NOT SIGNIFICANT (p >= 0.05)\n\n")
} else {
  cat("  => SIGNIFICANT (p < 0.05)\n\n")
}

cat("Non-GLP-1 (N =", nrow(nonglp1_data), "):\n")
cat("  Slope:", round(coef_nonglp1["weight_change_pct", "Estimate"], 1), "steps per 1% weight change\n")
cat("  p-value:", format.pval(coef_nonglp1["weight_change_pct", "Pr(>|t|)"], digits = 3), "\n")
if (coef_nonglp1["weight_change_pct", "Pr(>|t|)"] < 0.05) {
  cat("  => SIGNIFICANT (p < 0.05)\n\n")
} else {
  cat("  => NOT SIGNIFICANT (p >= 0.05)\n\n")
}

# Format p-values for plot
format_p <- function(p) {
  if (p < 0.001) return("p < 0.001")
  if (p < 0.01) return(paste0("p = ", sprintf("%.3f", p)))
  return(paste0("p = ", sprintf("%.2f", p)))
}

p_glp1 <- format_p(coef_glp1["weight_change_pct", "Pr(>|t|)"])
p_nonglp1 <- format_p(coef_nonglp1["weight_change_pct", "Pr(>|t|)"])

# Create scatter plot
cat("Creating scatter plot...\n")

fig2_comparison <- ggplot(combined_data, aes(x = weight_change_pct, y = delta_steps, color = group)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("GLP-1 User" = "#E41A1C", "Non-GLP-1" = "#377EB8"),
                     labels = c(paste0("GLP-1 User (n=", nrow(glp1_data), ", ", p_glp1, ")"),
                               paste0("Non-GLP-1 (n=", nrow(nonglp1_data), ", ", p_nonglp1, ")"))) +
  scale_x_continuous(limits = c(-30, 5), breaks = seq(-30, 5, 5)) +
  scale_y_continuous(limits = c(-5000, 5000), labels = comma) +
  labs(
    title = "Weight Loss vs Change in Physical Activity",
    subtitle = "Comparing GLP-1 users to non-pharmacologic weight losers",
    x = "Weight Change (%)",
    y = "Change in Daily Steps (nadir - baseline)",
    color = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(fig2_comparison)

ggsave("outputs/figures/figure2_glp1_comparison.png", fig2_comparison,
       width = 10, height = 8, dpi = 300)
cat("  Saved: outputs/figures/figure2_glp1_comparison.png\n\n")

# ============================================================================
# PART 4: MATCHED ANALYSIS
# ============================================================================

cat("PART 4: Creating matched cohort...\n")

# Prepare data for matching
match_data <- combined_data %>%
  filter(!is.na(baseline_bmi)) %>%
  mutate(
    glp1 = ifelse(group == "GLP-1 User", 1, 0),
    age_proxy = as.numeric(baseline_date - as.Date("2020-01-01")) / 365  # proxy for timing
  )

cat("  Data for matching:", nrow(match_data), "persons\n")
cat("    - GLP-1:", sum(match_data$glp1 == 1), "\n")
cat("    - Non-GLP-1:", sum(match_data$glp1 == 0), "\n\n")

# Propensity score matching on BMI and weight change
cat("  Running propensity score matching...\n")
cat("    Matching on: baseline_bmi, weight_change_pct\n\n")

tryCatch({
  match_result <- matchit(
    glp1 ~ baseline_bmi + weight_change_pct,
    data = match_data,
    method = "nearest",
    ratio = 1,
    caliper = 0.2
  )

  matched_data <- match.data(match_result)

  cat("  Matched cohort created!\n")
  cat("    - Total matched:", nrow(matched_data), "\n")
  cat("    - GLP-1:", sum(matched_data$glp1 == 1), "\n")
  cat("    - Non-GLP-1:", sum(matched_data$glp1 == 0), "\n\n")

  # Check balance
  cat("  Balance check (mean values):\n")
  balance <- matched_data %>%
    group_by(group) %>%
    summarize(
      n = n(),
      mean_bmi = round(mean(baseline_bmi, na.rm = TRUE), 1),
      mean_wt_change = round(mean(weight_change_pct, na.rm = TRUE), 1),
      .groups = "drop"
    )
  print(balance)
  cat("\n")

  # Run regressions on matched data
  cat("Running regressions on matched data...\n\n")

  # GLP-1 matched
  matched_glp1 <- matched_data %>% filter(glp1 == 1)
  model_matched_glp1 <- lm(delta_steps ~ weight_change_pct, data = matched_glp1)
  coef_matched_glp1 <- summary(model_matched_glp1)$coefficients

  # Non-GLP-1 matched
  matched_nonglp1 <- matched_data %>% filter(glp1 == 0)
  model_matched_nonglp1 <- lm(delta_steps ~ weight_change_pct, data = matched_nonglp1)
  coef_matched_nonglp1 <- summary(model_matched_nonglp1)$coefficients

  cat("MATCHED REGRESSION RESULTS:\n")
  cat("---------------------------\n\n")

  cat("GLP-1 Users (N =", nrow(matched_glp1), "):\n")
  cat("  Slope:", round(coef_matched_glp1["weight_change_pct", "Estimate"], 1), "steps per 1% weight change\n")
  cat("  p-value:", format.pval(coef_matched_glp1["weight_change_pct", "Pr(>|t|)"], digits = 3), "\n")
  if (coef_matched_glp1["weight_change_pct", "Pr(>|t|)"] >= 0.05) {
    cat("  => NOT SIGNIFICANT (p >= 0.05)\n\n")
  } else {
    cat("  => SIGNIFICANT (p < 0.05)\n\n")
  }

  cat("Non-GLP-1 Matched Controls (N =", nrow(matched_nonglp1), "):\n")
  cat("  Slope:", round(coef_matched_nonglp1["weight_change_pct", "Estimate"], 1), "steps per 1% weight change\n")
  cat("  p-value:", format.pval(coef_matched_nonglp1["weight_change_pct", "Pr(>|t|)"], digits = 3), "\n")
  if (coef_matched_nonglp1["weight_change_pct", "Pr(>|t|)"] < 0.05) {
    cat("  => SIGNIFICANT (p < 0.05)\n\n")
  } else {
    cat("  => NOT SIGNIFICANT (p >= 0.05)\n\n")
  }

  # Format p-values for matched plot
  p_matched_glp1 <- format_p(coef_matched_glp1["weight_change_pct", "Pr(>|t|)"])
  p_matched_nonglp1 <- format_p(coef_matched_nonglp1["weight_change_pct", "Pr(>|t|)"])

  # Create matched scatter plot
  cat("Creating matched scatter plot...\n")

  fig2_matched <- ggplot(matched_data, aes(x = weight_change_pct, y = delta_steps, color = group)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("GLP-1 User" = "#E41A1C", "Non-GLP-1" = "#377EB8"),
                       labels = c(paste0("GLP-1 User (n=", nrow(matched_glp1), ", ", p_matched_glp1, ")"),
                                 paste0("Matched Controls (n=", nrow(matched_nonglp1), ", ", p_matched_nonglp1, ")"))) +
    scale_x_continuous(limits = c(-30, 5), breaks = seq(-30, 5, 5)) +
    scale_y_continuous(limits = c(-5000, 5000), labels = comma) +
    labs(
      title = "Weight Loss vs Change in Physical Activity (Matched Cohorts)",
      subtitle = "GLP-1 users matched to non-pharmacologic controls on BMI and weight change",
      x = "Weight Change (%)",
      y = "Change in Daily Steps (nadir - baseline)",
      color = "Group"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )

  print(fig2_matched)

  ggsave("outputs/figures/figure2_matched_comparison.png", fig2_matched,
         width = 10, height = 8, dpi = 300)
  cat("  Saved: outputs/figures/figure2_matched_comparison.png\n\n")

  # Save matched data
  write_csv(matched_data, "outputs/figures/figure2_matched_data.csv")

}, error = function(e) {
  cat("  ERROR in matching:", e$message, "\n")
  cat("  Skipping matched analysis.\n\n")
})

# ============================================================================
# SAVE ALL DATA
# ============================================================================

cat("Saving data...\n")
write_csv(combined_data, "outputs/figures/figure2_comparison_data.csv")
cat("  Saved: outputs/figures/figure2_comparison_data.csv\n")

# Save regression summary
regression_summary <- tibble(
  Analysis = c("Full Cohort", "Full Cohort", "Full Cohort"),
  Group = c("Overall", "GLP-1 User", "Non-GLP-1"),
  N = c(nrow(combined_data), nrow(glp1_data), nrow(nonglp1_data)),
  Slope = c(
    round(coef_overall["weight_change_pct", "Estimate"], 1),
    round(coef_glp1["weight_change_pct", "Estimate"], 1),
    round(coef_nonglp1["weight_change_pct", "Estimate"], 1)
  ),
  P_value = c(
    coef_overall["weight_change_pct", "Pr(>|t|)"],
    coef_glp1["weight_change_pct", "Pr(>|t|)"],
    coef_nonglp1["weight_change_pct", "Pr(>|t|)"]
  ),
  Significant = c(
    coef_overall["weight_change_pct", "Pr(>|t|)"] < 0.05,
    coef_glp1["weight_change_pct", "Pr(>|t|)"] < 0.05,
    coef_nonglp1["weight_change_pct", "Pr(>|t|)"] < 0.05
  )
)

write_csv(regression_summary, "outputs/tables/figure2_regression_summary.csv")
cat("  Saved: outputs/tables/figure2_regression_summary.csv\n\n")

cat("========================================\n")
cat("Figure 2 comparison complete!\n")
cat("========================================\n\n")

cat("OUTPUT FILES:\n")
cat("  - outputs/figures/figure2_glp1_comparison.png\n")
cat("  - outputs/figures/figure2_matched_comparison.png\n")
cat("  - outputs/figures/figure2_comparison_data.csv\n")
cat("  - outputs/figures/figure2_matched_data.csv\n")
cat("  - outputs/tables/figure2_regression_summary.csv\n")
