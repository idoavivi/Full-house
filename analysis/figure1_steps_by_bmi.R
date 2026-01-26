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
# ENSURE OUTPUT DIRECTORIES EXIST
# ============================================================================

cat("Creating output directories...\n")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/datasets", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
cat("  - Output directories ready\n\n")

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

# Create the plot with non-linear (LOESS) trend lines
fig1 <- ggplot(plot_cohort, aes(x = baseline_bmi, y = mean_steps)) +
  # Add scatter points colored by BMI class
  geom_point(aes(color = bmi_class_plot), alpha = 0.3, size = 1.5) +

  # Add non-linear trend lines for GLP-1 vs non-GLP-1 (LOESS)
  geom_smooth(aes(linetype = glp1_label), method = "loess", se = TRUE,
              color = "#d73027", size = 1.2, span = 0.75) +

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
    title = "Daily Steps by BMI (GLP-1 vs Non-GLP-1)",
    subtitle = paste0("Non-linear (LOESS) trend lines\n",
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
  dpi = 300,
  create.dir = TRUE
)

# Save as PDF
ggsave(
  filename = "outputs/figures/figure1_steps_by_bmi.pdf",
  plot = fig1,
  width = 10,
  height = 6,
  create.dir = TRUE
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

# ============================================================================
# STEP 8: CREATE FIGURE 1B - TREND LINES BY SEX
# ============================================================================

cat("Step 8: Creating Figure 1b (trend lines by sex)...\n\n")

# Get sex from person dataframe
person_sex <- dataset_06597753_person_df %>%
  select(person_id, sex_at_birth)

# Merge sex into plot cohort
plot_cohort_sex <- plot_cohort %>%
  left_join(person_sex, by = "person_id") %>%
  filter(!is.na(sex_at_birth)) %>%
  mutate(
    sex_label = case_when(
      tolower(sex_at_birth) %in% c("male", "m") ~ "Male",
      tolower(sex_at_birth) %in% c("female", "f") ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex_label))

cat("Sample Size by Sex:\n")
print(table(plot_cohort_sex$sex_label))
cat("\n")

# Create Figure 1b with sex trend lines (non-linear LOESS)
fig1b <- ggplot(plot_cohort_sex, aes(x = baseline_bmi, y = mean_steps)) +
  # Add scatter points colored by BMI class
  geom_point(aes(color = bmi_class_plot), alpha = 0.3, size = 1.5) +

  # Add non-linear trend lines for Male vs Female (LOESS)
  geom_smooth(aes(group = sex_label, linetype = sex_label),
              method = "loess", se = TRUE, color = "#2166ac", fill = "#2166ac",
              alpha = 0.2, size = 1.2, span = 0.75,
              data = plot_cohort_sex %>% filter(sex_label == "Male")) +
  geom_smooth(aes(group = sex_label, linetype = sex_label),
              method = "loess", se = TRUE, color = "#b2182b", fill = "#b2182b",
              alpha = 0.2, size = 1.2, span = 0.75,
              data = plot_cohort_sex %>% filter(sex_label == "Female")) +

  # Color scale for BMI class points
  scale_color_manual(
    name = "BMI Class",
    values = c(
      "18.5-25" = "#4575b4",
      "25-30" = "#91bfdb",
      "30-35" = "#ffffbf",
      "35-40" = "#fc8d59",
      ">=40" = "#d73027"
    )
  ) +

  # Linetype scale for sex trend lines
  scale_linetype_manual(
    name = "Sex",
    values = c("Male" = "solid", "Female" = "dashed")
  ) +

  # Axis labels and title
  labs(
    x = "BMI (kg/m²)",
    y = "Average Daily Steps",
    title = "Daily Steps by BMI (Male vs Female)",
    subtitle = paste0("Non-linear (LOESS) trend lines: Blue = Male, Red = Female\n",
                     "N = ", comma(nrow(plot_cohort_sex)), " participants")
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
print(fig1b)

# ============================================================================
# STEP 9: SAVE FIGURE 1B
# ============================================================================

cat("\n\nStep 9: Saving Figure 1b...\n")

# Save as PNG
ggsave(
  filename = "outputs/figures/figure1b_steps_by_bmi_sex.png",
  plot = fig1b,
  width = 10,
  height = 6,
  dpi = 300,
  create.dir = TRUE
)

# Save as PDF
ggsave(
  filename = "outputs/figures/figure1b_steps_by_bmi_sex.pdf",
  plot = fig1b,
  width = 10,
  height = 6,
  create.dir = TRUE
)

# Save plot object
saveRDS(fig1b, file = "outputs/figures/figure1b_steps_by_bmi_sex.rds")

cat("  - Figure 1b saved to:\n")
cat("    - outputs/figures/figure1b_steps_by_bmi_sex.png\n")
cat("    - outputs/figures/figure1b_steps_by_bmi_sex.pdf\n")
cat("    - outputs/figures/figure1b_steps_by_bmi_sex.rds\n\n")

# ============================================================================
# STEP 10: REGRESSION STATISTICS BY SEX
# ============================================================================

cat("Step 10: Calculating regression statistics by sex...\n\n")

# Linear regression by sex
model_male <- lm(mean_steps ~ baseline_bmi,
                 data = plot_cohort_sex %>% filter(sex_label == "Male"))
model_female <- lm(mean_steps ~ baseline_bmi,
                   data = plot_cohort_sex %>% filter(sex_label == "Female"))

cat("Linear Regression: Steps ~ BMI (Males)\n")
cat("---------------------------------------\n")
print(summary(model_male))
cat("\n\n")

cat("Linear Regression: Steps ~ BMI (Females)\n")
cat("-----------------------------------------\n")
print(summary(model_female))
cat("\n\n")

# Interaction model
model_sex_interaction <- lm(mean_steps ~ baseline_bmi * sex_label, data = plot_cohort_sex)

cat("Linear Regression: Steps ~ BMI * Sex (Interaction)\n")
cat("---------------------------------------------------\n")
print(summary(model_sex_interaction))
cat("\n\n")

# Save regression results
regression_results_sex <- list(
  males = model_male,
  females = model_female,
  interaction = model_sex_interaction
)

saveRDS(regression_results_sex, file = "outputs/figures/figure1b_regression_results.rds")

cat("  - Regression results saved to:\n")
cat("    - outputs/figures/figure1b_regression_results.rds\n\n")

# ============================================================================
# STEP 11: CREATE FIGURE 1C - BAR CHART BY BMI CLASS AND SEX
# ============================================================================

cat("Step 11: Creating Figure 1c (bar chart by BMI class and sex)...\n\n")

# Calculate summary statistics by BMI class and sex
bar_data <- plot_cohort_sex %>%
  group_by(bmi_class_plot, sex_label) %>%
  summarize(
    n = n(),
    mean_steps = mean(mean_steps, na.rm = TRUE),
    se_steps = sd(mean_steps, na.rm = TRUE) / sqrt(n()),
    mean_sedentary = mean(mean_sedentary_min, na.rm = TRUE),
    se_sedentary = sd(mean_sedentary_min, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(!is.na(bmi_class_plot), !is.na(sex_label))

cat("Summary by BMI Class and Sex:\n")
print(bar_data)
cat("\n")

# Calculate scaling factor for dual y-axis
# We want steps and sedentary minutes to be comparable on the plot
# Typical steps: 5000-10000, typical sedentary: 600-900 minutes
# Scale factor: sedentary * scale_factor ≈ steps range
scale_factor <- max(bar_data$mean_steps, na.rm = TRUE) / max(bar_data$mean_sedentary, na.rm = TRUE)

cat("Scale factor for dual y-axis:", round(scale_factor, 2), "\n\n")

# Create bar chart with dual y-axes
fig1c <- ggplot(bar_data, aes(x = bmi_class_plot)) +
  # Steps bars
  geom_bar(aes(y = mean_steps, fill = sex_label),
           stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, alpha = 0.9) +
  # Error bars for steps
  geom_errorbar(aes(y = mean_steps,
                    ymin = mean_steps - se_steps,
                    ymax = mean_steps + se_steps,
                    group = sex_label),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black") +
  # Sedentary points (scaled to right y-axis)
  geom_point(aes(y = mean_sedentary * scale_factor, color = sex_label),
             position = position_dodge(width = 0.8),
             size = 4, shape = 18) +
  # Connect sedentary points with lines
  geom_line(aes(y = mean_sedentary * scale_factor, color = sex_label, group = sex_label),
            position = position_dodge(width = 0.8),
            size = 1.2, linetype = "dashed") +

  # Colors
  scale_fill_manual(
    name = "Steps/Day",
    values = c("Male" = "#2166ac", "Female" = "#b2182b")
  ) +
  scale_color_manual(
    name = "Sedentary Min/Day",
    values = c("Male" = "#08519c", "Female" = "#67000d")
  ) +

  # Dual y-axis
  scale_y_continuous(
    name = "Average Daily Steps",
    labels = comma,
    limits = c(0, NA),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Sedentary Minutes/Day",
      labels = comma
    )
  ) +

  # Labels
  labs(
    x = "BMI Class (kg/m²)",
    title = "Steps and Sedentary Time by BMI Class and Sex",
    subtitle = paste0("Bars = Steps/day, Diamonds = Sedentary minutes/day\n",
                     "N = ", comma(nrow(plot_cohort_sex)), " participants")
  ) +

  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.y.right = element_text(color = "#67000d"),
    axis.text.y.right = element_text(color = "#67000d")
  )

# Print the plot
print(fig1c)

# ============================================================================
# STEP 12: SAVE FIGURE 1C
# ============================================================================

cat("\n\nStep 12: Saving Figure 1c...\n")

# Save as PNG
ggsave(
  filename = "outputs/figures/figure1c_bar_steps_sedentary_by_sex.png",
  plot = fig1c,
  width = 12,
  height = 7,
  dpi = 300,
  create.dir = TRUE
)

# Save as PDF
ggsave(
  filename = "outputs/figures/figure1c_bar_steps_sedentary_by_sex.pdf",
  plot = fig1c,
  width = 12,
  height = 7,
  create.dir = TRUE
)

# Save plot object
saveRDS(fig1c, file = "outputs/figures/figure1c_bar_steps_sedentary_by_sex.rds")

# Save bar data
write_csv(bar_data, "outputs/figures/figure1c_bar_data.csv")

cat("  - Figure 1c saved to:\n")
cat("    - outputs/figures/figure1c_bar_steps_sedentary_by_sex.png\n")
cat("    - outputs/figures/figure1c_bar_steps_sedentary_by_sex.pdf\n")
cat("    - outputs/figures/figure1c_bar_steps_sedentary_by_sex.rds\n")
cat("    - outputs/figures/figure1c_bar_data.csv\n\n")

cat("========================================\n")
cat("Figure 1, 1b, and 1c creation complete!\n")
cat("========================================\n\n")
