# ============================================================================
# Script: figure2_sensitivity_lite.R
# Purpose: LIGHTWEIGHT sensitivity analysis - only 24 combinations
# ============================================================================

cat("========================================\n")
cat("Figure 2 Sensitivity Analysis (LITE)\n")
cat("Testing 24 key parameter combinations\n")
cat("========================================\n\n")

library(tidyverse)
library(lubridate)
library(scales)

# Load data (assuming already loaded, or load here)
cat("Loading data...\n")
load("outputs/datasets/02_weight_clean.RData")
load("outputs/datasets/03c_baseline_bmi.RData")
load("outputs/datasets/04a_fitbit_valid.RData")
load("outputs/datasets/05a_glp1_users.RData")
load("outputs/datasets/05c_treatment_categories.RData")
cat("  Done!\n\n")

# Prepare base data
glp1_info <- glp1_users %>%
  select(person_id, glp1_user, glp1_first_date, glp1_last_date)

# Simplified: only test key parameters
param_grid <- expand.grid(
  min_weight_loss_pct = c(-5, -10),
  activity_window_days = c(14, 30, 60),
  min_fitbit_days = c(3, 7),
  min_bmi_all = c(NA, 27),
  stringsAsFactors = FALSE
)

cat("Testing", nrow(param_grid), "combinations...\n\n")

# Simplified cohort builder
build_cohort_lite <- function(activity_window, min_fitbit_days, min_weight_loss, min_bmi = NULL) {

  # Get weight trajectory (peak and nadir)
  weight_traj <- suppressWarnings(
    weight_final %>%
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
      filter(weight_change_pct <= min_weight_loss, days_to_nadir >= 30)
  )

  # Add GLP-1 info
  weight_traj <- weight_traj %>%
    left_join(glp1_info, by = "person_id") %>%
    mutate(glp1_user = ifelse(is.na(glp1_user), FALSE, glp1_user))

  # Filter GLP-1 users: nadir during treatment
  weight_traj <- weight_traj %>%
    filter(
      glp1_user == FALSE |
      (nadir_date >= glp1_first_date & nadir_date <= glp1_last_date)
    )

  # Add BMI filter
  if (!is.null(min_bmi)) {
    weight_traj <- weight_traj %>%
      left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
      filter(!is.na(baseline_bmi), baseline_bmi >= min_bmi)
  }

  # Exclude bariatric
  bariatric <- treatment_categories %>%
    filter(treatment_category %in% c("Bariatric_only", "GLP1_and_Bariatric")) %>%
    pull(person_id)
  weight_traj <- weight_traj %>% filter(!person_id %in% bariatric)

  # Get steps at peak
  steps_peak <- weight_traj %>%
    select(person_id, peak_date) %>%
    left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
    filter(abs(as.numeric(difftime(date, peak_date, units = "days"))) <= activity_window) %>%
    group_by(person_id) %>%
    filter(n() >= min_fitbit_days) %>%
    summarize(steps_peak = mean(steps, na.rm = TRUE), .groups = "drop")

  # Get steps at nadir
  steps_nadir <- weight_traj %>%
    select(person_id, nadir_date) %>%
    left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
    filter(abs(as.numeric(difftime(date, nadir_date, units = "days"))) <= activity_window) %>%
    group_by(person_id) %>%
    filter(n() >= min_fitbit_days) %>%
    summarize(steps_nadir = mean(steps, na.rm = TRUE), .groups = "drop")

  # Combine
  final <- weight_traj %>%
    inner_join(steps_peak, by = "person_id") %>%
    inner_join(steps_nadir, by = "person_id") %>%
    mutate(
      delta_steps = steps_nadir - steps_peak,
      group = ifelse(glp1_user, "GLP-1 User", "Non-GLP-1")
    )

  return(final)
}

# Run analysis
results <- list()

for (i in 1:nrow(param_grid)) {
  p <- param_grid[i, ]
  cat("  Testing combo", i, "/", nrow(param_grid), "...\n")

  tryCatch({
    cohort <- build_cohort_lite(
      activity_window = p$activity_window_days,
      min_fitbit_days = p$min_fitbit_days,
      min_weight_loss = p$min_weight_loss_pct,
      min_bmi = if(is.na(p$min_bmi_all)) NULL else p$min_bmi_all
    )

    # Run regression
    if (nrow(cohort) >= 20) {
      model <- lm(delta_steps ~ weight_change_pct, data = cohort)
      coef <- summary(model)$coefficients

      results[[i]] <- tibble(
        combo = i,
        weight_loss = p$min_weight_loss_pct,
        window = p$activity_window_days,
        fitbit_days = p$min_fitbit_days,
        bmi_filter = ifelse(is.na(p$min_bmi_all), "None", p$min_bmi_all),
        n = nrow(cohort),
        n_glp1 = sum(cohort$group == "GLP-1 User"),
        slope = round(coef["weight_change_pct", "Estimate"], 1),
        p_value = coef["weight_change_pct", "Pr(>|t|)"],
        significant = coef["weight_change_pct", "Pr(>|t|)"] < 0.05
      )
    }
  }, error = function(e) NULL)
}

# Combine results
sensitivity_results <- bind_rows(results)

cat("\n========================================\n")
cat("RESULTS\n")
cat("========================================\n\n")

print(sensitivity_results %>% arrange(p_value))

# Show significant results
sig <- sensitivity_results %>% filter(significant == TRUE)
cat("\n\nSIGNIFICANT RESULTS (p < 0.05):\n")
if (nrow(sig) > 0) {
  print(sig)
} else {
  cat("  None found\n")
}

# Save results
write_csv(sensitivity_results, "outputs/tables/figure2_sensitivity_lite.csv")
cat("\nResults saved to outputs/tables/figure2_sensitivity_lite.csv\n")
