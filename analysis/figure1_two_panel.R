# ============================================================================
# Script: figure1_two_panel.R
# Purpose: Figure 1 for manuscript - Weight Change vs Activity Change
# Panel A: Scatter plot | Panel B: Box plots by weight loss category
# ============================================================================

cat("========================================\n")
cat("Figure 1: Weight Change vs Activity\n")
cat("========================================\n\n")

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)  # For combining panels

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

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

# Parameters
ACTIVITY_WINDOW <- 15  # days before/after weight measurement
MIN_ACTIVITY_DAYS <- 3  # RELAXED from 5 to 3 to increase sample size
MIN_DAYS_APART <- 30    # minimum days between max and min weight

# ============================================================================
# IDENTIFY BARIATRIC SURGERY (to exclude)
# ============================================================================

bariatric_ids <- treatment_categories %>%
  filter(treatment_category %in% c("Bariatric_only", "GLP1_and_Bariatric")) %>%
  pull(person_id)

# GLP-1 users with treatment dates (for checking if on treatment at min weight)
glp1_with_dates <- glp1_users %>%
  select(person_id, glp1_first_date, glp1_last_date)

# ============================================================================
# BUILD COHORT: Find max/min weights with activity data
# ============================================================================

cat("Building cohort...\n")

# Get all persons with Fitbit data
fitbit_persons <- fitbit_valid %>%
  select(person_id) %>%
  distinct()

# For each person, find max and min weight
cat("  Finding max/min weights per person...\n")

weight_extremes <- weight_final %>%
  filter(!person_id %in% bariatric_ids) %>%  # Exclude bariatric
  filter(person_id %in% fitbit_persons$person_id) %>%  # Must have Fitbit
  group_by(person_id) %>%
  filter(n() >= 2) %>%  # Need at least 2 measurements
  summarize(
    max_weight = max(weight_kg, na.rm = TRUE),
    max_weight_date = measurement_date[which.max(weight_kg)],
    min_weight = min(weight_kg, na.rm = TRUE),
    min_weight_date = measurement_date[which.min(weight_kg)],
    n_measurements = n(),
    .groups = "drop"
  ) %>%
  mutate(
    days_apart = abs(as.numeric(difftime(min_weight_date, max_weight_date, units = "days"))),
    delta_weight_kg = min_weight - max_weight,
    delta_weight_pct = (min_weight - max_weight) / max_weight * 100
  ) %>%
  filter(days_apart >= MIN_DAYS_APART)  # Ensure measurements are far enough apart

cat("    Persons with weight extremes >=", MIN_DAYS_APART, "days apart:", nrow(weight_extremes), "\n")

# ============================================================================
# GET ACTIVITY AT MAX WEIGHT (±15 days)
# ============================================================================

cat("  Getting activity at max weight...\n")

steps_at_max <- weight_extremes %>%
  select(person_id, max_weight_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(abs(as.numeric(difftime(date, max_weight_date, units = "days"))) <= ACTIVITY_WINDOW) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_ACTIVITY_DAYS) %>%
  summarize(
    steps_at_max = mean(steps, na.rm = TRUE),
    n_days_max = n(),
    .groups = "drop"
  )

cat("    With sufficient activity at max weight:", nrow(steps_at_max), "\n")

# ============================================================================
# GET ACTIVITY AT MIN WEIGHT (±15 days)
# ============================================================================

cat("  Getting activity at min weight...\n")

steps_at_min <- weight_extremes %>%
  select(person_id, min_weight_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(abs(as.numeric(difftime(date, min_weight_date, units = "days"))) <= ACTIVITY_WINDOW) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_ACTIVITY_DAYS) %>%
  summarize(
    steps_at_min = mean(steps, na.rm = TRUE),
    n_days_min = n(),
    .groups = "drop"
  )

cat("    With sufficient activity at min weight:", nrow(steps_at_min), "\n")

# ============================================================================
# COMBINE INTO FINAL COHORT
# ============================================================================

cat("  Combining into final cohort...\n")

cohort <- weight_extremes %>%
  inner_join(steps_at_max, by = "person_id") %>%
  inner_join(steps_at_min, by = "person_id") %>%
  # Join GLP-1 treatment dates to check if on treatment at min weight
  left_join(glp1_with_dates, by = "person_id") %>%
  mutate(
    delta_steps = steps_at_min - steps_at_max,
    # GLP-1 status: must be on active treatment at min weight date
    # (min_weight_date between glp1_first_date and glp1_last_date + 90 days grace period)
    glp1_user = ifelse(
      !is.na(glp1_first_date) &
      min_weight_date >= glp1_first_date &
      min_weight_date <= glp1_last_date + 90,
      "GLP-1", "Non-GLP-1"
    ),
    # Weight loss categories (for Panel B)
    weight_loss_cat = case_when(
      delta_weight_pct > 0 ~ "Weight Gain",
      delta_weight_pct >= -5 ~ "<5% Loss",
      delta_weight_pct >= -10 ~ "5-10% Loss",
      delta_weight_pct < -10 ~ "≥10% Loss"
    ),
    weight_loss_cat = factor(weight_loss_cat,
                              levels = c("≥10% Loss", "5-10% Loss", "<5% Loss", "Weight Gain"))
  ) %>%
  select(-glp1_first_date, -glp1_last_date)  # Clean up temp columns

cat("\nFINAL COHORT:", nrow(cohort), "persons\n")
cat("  GLP-1 users:", sum(cohort$glp1_user == "GLP-1"), "\n")
cat("  Non-GLP-1:", sum(cohort$glp1_user == "Non-GLP-1"), "\n\n")

# Summary by group
cat("Weight change summary:\n")
cohort %>%
  group_by(glp1_user) %>%
  summarize(
    n = n(),
    mean_wt_change_kg = round(mean(delta_weight_kg), 1),
    mean_wt_change_pct = round(mean(delta_weight_pct), 1),
    mean_step_change = round(mean(delta_steps)),
    .groups = "drop"
  ) %>%
  print()
cat("\n")

# ============================================================================
# CALCULATE CORRELATIONS
# ============================================================================

cat("Calculating correlations...\n")

# Overall
cor_overall <- cor.test(cohort$delta_weight_kg, cohort$delta_steps)

# By group
cor_glp1 <- cor.test(
  cohort$delta_weight_kg[cohort$glp1_user == "GLP-1"],
  cohort$delta_steps[cohort$glp1_user == "GLP-1"]
)

cor_nonglp1 <- cor.test(
  cohort$delta_weight_kg[cohort$glp1_user == "Non-GLP-1"],
  cohort$delta_steps[cohort$glp1_user == "Non-GLP-1"]
)

cat("  Overall: r =", round(cor_overall$estimate, 3), ", p =", format.pval(cor_overall$p.value, digits = 3), "\n")
cat("  GLP-1: r =", round(cor_glp1$estimate, 3), ", p =", format.pval(cor_glp1$p.value, digits = 3), "\n")
cat("  Non-GLP-1: r =", round(cor_nonglp1$estimate, 3), ", p =", format.pval(cor_nonglp1$p.value, digits = 3), "\n\n")

# Format for plot labels
format_cor <- function(r, p, n) {
  p_text <- ifelse(p < 0.001, "p < 0.001", paste0("p = ", sprintf("%.3f", p)))
  paste0("r = ", sprintf("%.2f", r), ", ", p_text, "\n(n = ", n, ")")
}

label_glp1 <- format_cor(cor_glp1$estimate, cor_glp1$p.value, sum(cohort$glp1_user == "GLP-1"))
label_nonglp1 <- format_cor(cor_nonglp1$estimate, cor_nonglp1$p.value, sum(cohort$glp1_user == "Non-GLP-1"))

# ============================================================================
# PANEL A: SCATTER PLOT
# ============================================================================

cat("Creating Panel A: Scatter plot...\n")

# Set axis limits (exclude extreme outliers for visualization)
x_limits <- c(-40, 20)  # Weight change in kg
y_limits <- c(-6000, 6000)  # Step change

panel_a <- ggplot(cohort, aes(x = delta_weight_kg, y = delta_steps, color = glp1_user)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  # Points
  geom_point(alpha = 0.4, size = 1.5) +
  # LOESS trend lines
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.5, span = 0.75) +
  # Colors
  scale_color_manual(values = c("GLP-1" = "#E41A1C", "Non-GLP-1" = "#377EB8")) +
  # Axis limits
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  scale_x_continuous(breaks = seq(-40, 20, 10)) +
  scale_y_continuous(labels = comma, breaks = seq(-6000, 6000, 2000)) +
  # Correlation labels
  annotate("label", x = -35, y = 5500, label = label_nonglp1,
           hjust = 0, size = 3.5, fill = "#377EB8", color = "white", fontface = "bold",
           label.padding = unit(0.4, "lines")) +
  annotate("label", x = -35, y = 3500, label = label_glp1,
           hjust = 0, size = 3.5, fill = "#E41A1C", color = "white", fontface = "bold",
           label.padding = unit(0.4, "lines")) +
  # Labels
  labs(
    title = "A",
    x = "Weight Change (kg)",
    y = "Change in Daily Steps",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ============================================================================
# PANEL B: BOX PLOTS BY WEIGHT LOSS CATEGORY
# ============================================================================

cat("Creating Panel B: Box plots by weight loss category...\n")

# Filter to weight loss only for Panel B
losers <- cohort %>%
  filter(delta_weight_pct < 0) %>%
  mutate(
    weight_loss_cat = case_when(
      delta_weight_pct >= -5 ~ "<5% Loss",
      delta_weight_pct >= -10 ~ "5-10% Loss",
      delta_weight_pct < -10 ~ "≥10% Loss"
    ),
    weight_loss_cat = factor(weight_loss_cat, levels = c("≥10% Loss", "5-10% Loss", "<5% Loss"))
  )

# Sample sizes for labels
n_by_cat <- losers %>%
  group_by(weight_loss_cat, glp1_user) %>%
  summarize(n = n(), .groups = "drop")

cat("\nSample sizes by category:\n")
print(n_by_cat %>% pivot_wider(names_from = glp1_user, values_from = n))
cat("\n")

# Calculate means for diamonds
means_by_cat <- losers %>%
  group_by(weight_loss_cat, glp1_user) %>%
  summarize(mean_steps = mean(delta_steps, na.rm = TRUE), .groups = "drop")

panel_b <- ggplot(losers, aes(x = weight_loss_cat, y = delta_steps, fill = glp1_user)) +
  # Reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  # Box plots
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 1,
               position = position_dodge(width = 0.8), width = 0.7) +
  # Means as diamonds
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#FFD700",
               position = position_dodge(width = 0.8)) +
  # Colors
  scale_fill_manual(values = c("GLP-1" = "#E41A1C", "Non-GLP-1" = "#377EB8")) +
  scale_y_continuous(labels = comma, limits = c(-2000, 3000)) +  # Smaller scale to show differences
  # Labels
  labs(
    title = "B",
    x = "Weight Loss Category",
    y = "Change in Daily Steps",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10)
  )

# ============================================================================
# COMBINE PANELS
# ============================================================================

cat("Combining panels...\n")

# Combine with patchwork
fig1_combined <- panel_a + panel_b +
  plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") +
  plot_annotation(
    title = "Figure 1: Weight Change and Physical Activity",
    subtitle = "Comparison of GLP-1 users vs non-pharmacologic weight changes",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray30")
    )
  ) &
  theme(legend.position = "bottom")

# Display
print(fig1_combined)

# Save
ggsave("outputs/figures/figure1_combined.png", fig1_combined,
       width = 14, height = 7, dpi = 300)
cat("  Saved: figure1_combined.png\n\n")

# Also save individual panels
ggsave("outputs/figures/figure1a_scatter.png", panel_a, width = 8, height = 7, dpi = 300)
ggsave("outputs/figures/figure1b_boxplots.png", panel_b, width = 8, height = 7, dpi = 300)
cat("  Saved individual panels\n\n")

# ============================================================================
# STATISTICAL TESTS FOR PANEL B
# ============================================================================

cat("========================================\n")
cat("STATISTICAL TESTS (Panel B)\n")
cat("========================================\n\n")

# Test: Is step change different from 0 within each category/group?
cat("1. Is step change significantly different from 0?\n")
for (cat_level in levels(losers$weight_loss_cat)) {
  cat("\n   ", cat_level, ":\n")
  for (g in c("Non-GLP-1", "GLP-1")) {
    test_data <- losers %>% filter(weight_loss_cat == cat_level, glp1_user == g)
    if (nrow(test_data) > 2) {
      t_test <- t.test(test_data$delta_steps, mu = 0)
      sig <- ifelse(t_test$p.value < 0.05, "*", "")
      cat("      ", g, ": mean =", round(mean(test_data$delta_steps)),
          ", p =", format.pval(t_test$p.value, digits = 3), sig, "\n")
    } else {
      cat("      ", g, ": insufficient data (n =", nrow(test_data), ")\n")
    }
  }
}

# Test: Is there a difference between GLP-1 and Non-GLP-1 within each category?
cat("\n2. GLP-1 vs Non-GLP-1 difference within each category:\n")
for (cat_level in levels(losers$weight_loss_cat)) {
  test_data <- losers %>% filter(weight_loss_cat == cat_level)
  n_glp1 <- sum(test_data$glp1_user == "GLP-1")
  n_nonglp1 <- sum(test_data$glp1_user == "Non-GLP-1")

  if (n_glp1 > 2 && n_nonglp1 > 2) {
    wilcox <- wilcox.test(delta_steps ~ glp1_user, data = test_data)
    sig <- ifelse(wilcox$p.value < 0.05, "*", "")
    cat("   ", cat_level, ": p =", format.pval(wilcox$p.value, digits = 3), sig, "\n")
  } else {
    cat("   ", cat_level, ": insufficient data\n")
  }
}

# ============================================================================
# SAVE DATA
# ============================================================================

write_csv(cohort, "outputs/figures/figure1_cohort_data.csv")

cat("\n========================================\n")
cat("Figure 1 complete!\n")
cat("========================================\n\n")

cat("OUTPUT FILES:\n")
cat("  - outputs/figures/figure1_combined.png\n")
cat("  - outputs/figures/figure1a_scatter.png\n")
cat("  - outputs/figures/figure1b_boxplots.png\n")
cat("  - outputs/figures/figure1_cohort_data.csv\n")
