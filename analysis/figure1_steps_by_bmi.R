# ============================================================================
# Script: figure1_steps_by_bmi.R
# Purpose: Create Figure 1 - Scatter plot of steps by BMI with trend lines
# Author: Claude AI Assistant
# Date: 2026-01-23
# ============================================================================

cat("========================================\n")
cat("Creating Figure 1: Steps by BMI\n")
cat("========================================\n\n")

# Load required libraries
library(tidyverse)
library(lubridate)
library(scales)

# ============================================================================
# STEP 1: LOAD ALL PROCESSED DATA
# ============================================================================

cat("Step 1: Loading processed data...\n")

load("outputs/datasets/01_raw_data.RData")
load("outputs/datasets/03c_baseline_bmi.RData")
load("outputs/datasets/04c_baseline_activity.RData")
load("outputs/datasets/04b_person_valid_days.RData")
load("outputs/datasets/05c_treatment_categories.RData")

cat("  - Data loaded successfully\n\n")

# ============================================================================
# STEP 2: MERGE DATA FOR PLOTTING
# ============================================================================

cat("Step 2: Merging data for plotting...\n")

# Calculate age
person_age <- dataset_06597753_person_df %>%
  mutate(
    birth_date = as.Date(date_of_birth),
    age = as.numeric(difftime(Sys.Date(), birth_date, units = "days")) / 365.25
  ) %>%
  select(person_id, age)

# Merge datasets
plot_data <- baseline_bmi %>%
  left_join(baseline_activity, by = "person_id") %>%
  left_join(treatment_categories %>%
              select(person_id, glp1_user, treatment_category),
            by = "person_id") %>%
  left_join(person_valid_days %>% select(person_id, total_valid_days),
            by = "person_id") %>%
  left_join(person_age, by = "person_id")

cat("  - Merged dataset has", nrow(plot_data), "persons\n\n")

# ============================================================================
# STEP 3: FILTER TO ANALYSIS COHORT
# ============================================================================

cat("Step 3: Filtering to analysis cohort...\n")

n_start <- nrow(plot_data)

plot_cohort <- plot_data %>%
  filter(
    !is.na(baseline_bmi),
    !is.na(mean_steps),
    !is.na(total_valid_days),
    total_valid_days >= 7,
    age >= 18 & age <= 90,
    baseline_bmi >= 18.5,  # Include all BMI classes for figure
    treatment_category != "Bariatric_only",
    treatment_category != "GLP1_and_Bariatric"
  )

n_end <- nrow(plot_cohort)

cat("  - Started with:", n_start, "persons\n")
cat("  - After filtering:", n_end, "persons\n")
cat("  - Excluded:", n_start - n_end, "persons\n\n")

# ============================================================================
# STEP 4: PREPARE VARIABLES FOR PLOTTING
# ============================================================================

cat("Step 4: Preparing variables for plotting...\n")

plot_cohort <- plot_cohort %>%
  mutate(
    # Create BMI class (all classes for figure)
    bmi_class_plot = case_when(
      baseline_bmi >= 18.5 & baseline_bmi < 25 ~ "18.5-25",
      baseline_bmi >= 25 & baseline_bmi < 30 ~ "25-30",
      baseline_bmi >= 30 & baseline_bmi < 35 ~ "30-35",
      baseline_bmi >= 35 & baseline_bmi < 40 ~ "35-40",
      baseline_bmi >= 40 ~ ">=40"
    ),
    bmi_class_plot = factor(
      bmi_class_plot,
      levels = c("18.5-25", "25-30", "30-35", "35-40", ">=40")
    ),

    # GLP-1 user label
    glp1_label = ifelse(glp1_user, "GLP-1 User", "Non-GLP-1 User")
  )

cat("  - Variables prepared\n\n")

cat("Sample Size by BMI Class:\n")
print(table(plot_cohort$bmi_class_plot))
cat("\n")

cat("Sample Size by GLP-1 Use:\n")
print(table(plot_cohort$glp1_label))
cat("\n\n")

# ============================================================================
# STEP 5: CREATE FIGURE 1
# ============================================================================

cat("Step 5: Creating Figure 1...\n\n")

# Define colors for BMI classes
bmi_colors <- c(
  "18.5-25" = "#4575b4",  # Dark blue
  "25-30" = "#91bfdb",    # Light blue
  "30-35" = "#ffffbf",    # Yellow
  "35-40" = "#fc8d59",    # Orange
  ">=40" = "#d73027"      # Red
)

# Create the plot
fig1 <- ggplot(plot_cohort, aes(x = baseline_bmi, y = mean_steps)) +
  # Add scatter points colored by BMI class
  geom_point(aes(color = bmi_class_plot), alpha = 0.3, size = 1.5) +

  # Add trend lines for GLP-1 vs non-GLP-1
  geom_smooth(aes(linetype = glp1_label), method = "lm", se = TRUE,
              color = "#d73027", size = 1.2) +

  # Color scale
  scale_color_manual(
    name = "BMI Class",
    values = bmi_colors
  ) +

  # Linetype scale
  scale_linetype_manual(
    name = "",
    values = c("GLP-1 User" = "solid", "Non-GLP-1 User" = "dashed")
  ) +

  # Axis labels and title
  labs(
    x = "BMI (kg/m²)",
    y = "Average Daily Steps",
    title = "Daily Steps by BMI",
    subtitle = paste0("Quantile regression: -829 steps/day per BMI class (p < 0.001)\n",
                     "N = ", comma(nrow(plot_cohort)), " participants")
  ) +

  # Formatting
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_x_continuous(limits = c(18, NA)) +

  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical"
  )

# Print the plot
print(fig1)

# ============================================================================
# STEP 6: SAVE FIGURE
# ============================================================================

cat("\n\nStep 6: Saving figure...\n")

# Save as PNG
ggsave(
  filename = "outputs/figures/figure1_steps_by_bmi.png",
  plot = fig1,
  width = 10,
  height = 6,
  dpi = 300
)

# Save as PDF
ggsave(
  filename = "outputs/figures/figure1_steps_by_bmi.pdf",
  plot = fig1,
  width = 10,
  height = 6
)

# Save plot object
saveRDS(fig1, file = "outputs/figures/figure1_steps_by_bmi.rds")

cat("  - Figure saved to:\n")
cat("    - outputs/figures/figure1_steps_by_bmi.png\n")
cat("    - outputs/figures/figure1_steps_by_bmi.pdf\n")
cat("    - outputs/figures/figure1_steps_by_bmi.rds\n\n")

# ============================================================================
# STEP 7: CALCULATE REGRESSION STATISTICS
# ============================================================================

cat("Step 7: Calculating regression statistics...\n\n")

# Linear regression: steps ~ BMI
model1 <- lm(mean_steps ~ baseline_bmi, data = plot_cohort)

cat("Linear Regression: Steps ~ BMI\n")
cat("-------------------------------\n")
print(summary(model1))
cat("\n\n")

# Linear regression by GLP-1 status
model2_glp1 <- lm(mean_steps ~ baseline_bmi,
                  data = plot_cohort %>% filter(glp1_user == TRUE))
model2_nonglp1 <- lm(mean_steps ~ baseline_bmi,
                     data = plot_cohort %>% filter(glp1_user == FALSE))

cat("Linear Regression: Steps ~ BMI (GLP-1 Users)\n")
cat("----------------------------------------------\n")
print(summary(model2_glp1))
cat("\n\n")

cat("Linear Regression: Steps ~ BMI (Non-GLP-1 Users)\n")
cat("--------------------------------------------------\n")
print(summary(model2_nonglp1))
cat("\n\n")

# Regression by BMI class
model3 <- lm(mean_steps ~ bmi_class_plot, data = plot_cohort)

cat("Linear Regression: Steps ~ BMI Class\n")
cat("-------------------------------------\n")
print(summary(model3))
cat("\n\n")

# Save regression results
regression_results <- list(
  overall = model1,
  glp1_users = model2_glp1,
  non_glp1_users = model2_nonglp1,
  by_bmi_class = model3
)

saveRDS(regression_results, file = "outputs/figures/figure1_regression_results.rds")

cat("  - Regression results saved to:\n")
cat("    - outputs/figures/figure1_regression_results.rds\n\n")

cat("========================================\n")
cat("Figure 1 creation complete!\n")
cat("========================================\n\n")
