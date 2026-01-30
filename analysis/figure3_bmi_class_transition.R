# ============================================================================
# Script: figure3_bmi_class_transition.R
# Purpose: Analyze if weight losers adapt their activity to their new BMI class
# Hypothesis: Non-GLP-1 users converge to new class activity, GLP-1 users don't
# ============================================================================

cat("========================================\n")
cat("Figure 3: BMI Class Transition Analysis\n")
cat("========================================\n\n")

library(tidyverse)
library(lubridate)
library(scales)

# Create output directories
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
BASE_WINDOW <- 90
NADIR_WINDOW <- 90
MIN_FITBIT_DAYS <- 5

# ============================================================================
# DEFINE BMI CLASS FUNCTION
# ============================================================================

get_bmi_class <- function(bmi) {
  case_when(
    bmi < 25 ~ "Normal/Overweight\n(<25)",
    bmi >= 25 & bmi < 30 ~ "Overweight\n(25-30)",
    bmi >= 30 & bmi < 35 ~ "Class I\n(30-35)",
    bmi >= 35 & bmi < 40 ~ "Class II\n(35-40)",
    bmi >= 40 ~ "Class III\n(≥40)",
    TRUE ~ NA_character_
  )
}

# Numeric version for ordering
get_bmi_class_num <- function(bmi) {
  case_when(
    bmi < 25 ~ 1,
    bmi >= 25 & bmi < 30 ~ 2,
    bmi >= 30 & bmi < 35 ~ 3,
    bmi >= 35 & bmi < 40 ~ 4,
    bmi >= 40 ~ 5,
    TRUE ~ NA_real_
  )
}

# ============================================================================
# PART 1: GET EXPECTED STEPS BY BMI CLASS (Cross-sectional reference)
# ============================================================================

cat("PART 1: Calculating expected steps by BMI class (cross-sectional)...\n")

# Get average steps per BMI class from all valid Fitbit users
cross_sectional_ref <- fitbit_valid %>%
  group_by(person_id) %>%
  summarize(avg_steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  filter(!is.na(baseline_bmi)) %>%
  mutate(bmi_class = get_bmi_class(baseline_bmi),
         bmi_class_num = get_bmi_class_num(baseline_bmi)) %>%
  filter(!is.na(bmi_class))

expected_by_class <- cross_sectional_ref %>%
  group_by(bmi_class, bmi_class_num) %>%
  summarize(
    n = n(),
    expected_steps = mean(avg_steps, na.rm = TRUE),
    sd_steps = sd(avg_steps, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(bmi_class_num)

cat("\nExpected steps by BMI class (cross-sectional reference):\n")
print(expected_by_class %>% select(bmi_class, n, expected_steps))
cat("\n")

# ============================================================================
# PART 2: BUILD GLP-1 COHORT WITH BMI TRAJECTORIES
# ============================================================================

cat("PART 2: Building GLP-1 cohort with BMI trajectories...\n")

# Get height for BMI calculation
heights <- weight_final %>%
  inner_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  select(person_id) %>%
  distinct()

# Baseline weight for GLP-1 users (at treatment start)
glp1_baseline <- weight_final %>%
  filter(person_id %in% glp1_users$person_id) %>%
  left_join(glp1_users %>% select(person_id, glp1_first_date, glp1_last_date), by = "person_id") %>%
  filter(measurement_date >= glp1_first_date - 90,
         measurement_date <= glp1_first_date + 30) %>%
  group_by(person_id) %>%
  slice_min(abs(as.numeric(difftime(measurement_date, glp1_first_date, units = "days"))), n = 1) %>%
  ungroup() %>%
  left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  filter(!is.na(baseline_bmi), baseline_bmi >= 27) %>%
  mutate(height_m = sqrt(weight_kg / baseline_bmi)) %>%  # derive height from BMI
  select(person_id, baseline_weight = weight_kg, baseline_date = measurement_date,
         glp1_first_date, glp1_last_date, height_m, baseline_bmi)

# Get nadir for GLP-1
glp1_nadir <- weight_final %>%
  inner_join(glp1_baseline, by = "person_id") %>%
  filter(measurement_date > baseline_date,
         measurement_date >= glp1_first_date,
         measurement_date <= glp1_last_date + 90) %>%
  group_by(person_id) %>%
  slice_min(weight_kg, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(nadir_weight = weight_kg, nadir_date = measurement_date) %>%
  mutate(
    weight_change_pct = (nadir_weight - baseline_weight) / baseline_weight * 100,
    nadir_bmi = nadir_weight / (height_m^2)
  ) %>%
  filter(weight_change_pct <= -5)

cat("  GLP-1 with valid weight trajectory:", nrow(glp1_nadir), "\n")

# Get steps before baseline and around nadir
glp1_steps_baseline <- glp1_nadir %>%
  select(person_id, baseline_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(date >= baseline_date - BASE_WINDOW, date < baseline_date) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_baseline = mean(steps, na.rm = TRUE), .groups = "drop")

glp1_steps_nadir <- glp1_nadir %>%
  select(person_id, nadir_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(abs(as.numeric(difftime(date, nadir_date, units = "days"))) <= NADIR_WINDOW) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_nadir = mean(steps, na.rm = TRUE), .groups = "drop")

glp1_cohort <- glp1_nadir %>%
  inner_join(glp1_steps_baseline, by = "person_id") %>%
  inner_join(glp1_steps_nadir, by = "person_id") %>%
  mutate(
    bmi_class_baseline = get_bmi_class(baseline_bmi),
    bmi_class_nadir = get_bmi_class(nadir_bmi),
    bmi_class_num_baseline = get_bmi_class_num(baseline_bmi),
    bmi_class_num_nadir = get_bmi_class_num(nadir_bmi),
    classes_dropped = bmi_class_num_baseline - bmi_class_num_nadir,
    delta_steps = steps_nadir - steps_baseline,
    group = "GLP-1"
  )

cat("  GLP-1 final cohort:", nrow(glp1_cohort), "\n\n")

# ============================================================================
# PART 3: BUILD NON-GLP-1 COHORT
# ============================================================================

cat("PART 3: Building Non-GLP-1 cohort...\n")

non_glp1_ids <- treatment_categories %>%
  filter(treatment_category == "Neither") %>%
  pull(person_id)

bariatric <- treatment_categories %>%
  filter(treatment_category %in% c("Bariatric_only", "GLP1_and_Bariatric")) %>%
  pull(person_id)

non_glp1_trajectory <- suppressWarnings(
  weight_final %>%
    filter(person_id %in% non_glp1_ids, !person_id %in% bariatric) %>%
    left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
    filter(!is.na(baseline_bmi)) %>%
    mutate(height_m = sqrt(weight_kg / baseline_bmi)) %>%
    group_by(person_id, height_m, baseline_bmi) %>%
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
      peak_bmi = peak_weight / (height_m^2),
      nadir_bmi = nadir_weight / (height_m^2)
    ) %>%
    filter(weight_change_pct <= -5,
           as.numeric(difftime(nadir_date, peak_date, units = "days")) >= 30)
)

cat("  Non-GLP-1 with valid weight trajectory:", nrow(non_glp1_trajectory), "\n")

# Get steps
non_glp1_steps_baseline <- non_glp1_trajectory %>%
  select(person_id, peak_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(date >= peak_date - BASE_WINDOW, date < peak_date) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_baseline = mean(steps, na.rm = TRUE), .groups = "drop")

non_glp1_steps_nadir <- non_glp1_trajectory %>%
  select(person_id, nadir_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(abs(as.numeric(difftime(date, nadir_date, units = "days"))) <= NADIR_WINDOW) %>%
  group_by(person_id) %>%
  filter(n() >= MIN_FITBIT_DAYS) %>%
  summarize(steps_nadir = mean(steps, na.rm = TRUE), .groups = "drop")

non_glp1_cohort <- non_glp1_trajectory %>%
  inner_join(non_glp1_steps_baseline, by = "person_id") %>%
  inner_join(non_glp1_steps_nadir, by = "person_id") %>%
  mutate(
    baseline_bmi = peak_bmi,
    bmi_class_baseline = get_bmi_class(peak_bmi),
    bmi_class_nadir = get_bmi_class(nadir_bmi),
    bmi_class_num_baseline = get_bmi_class_num(peak_bmi),
    bmi_class_num_nadir = get_bmi_class_num(nadir_bmi),
    classes_dropped = bmi_class_num_baseline - bmi_class_num_nadir,
    delta_steps = steps_nadir - steps_baseline,
    group = "Non-GLP-1"
  )

cat("  Non-GLP-1 final cohort:", nrow(non_glp1_cohort), "\n\n")

# ============================================================================
# PART 4: COMBINE AND ANALYZE BMI CLASS TRANSITIONS
# ============================================================================

cat("PART 4: Analyzing BMI class transitions...\n\n")

combined <- bind_rows(
  glp1_cohort %>% select(person_id, group, baseline_bmi, nadir_bmi,
                          bmi_class_baseline, bmi_class_nadir,
                          bmi_class_num_baseline, bmi_class_num_nadir,
                          classes_dropped, steps_baseline, steps_nadir, delta_steps,
                          weight_change_pct),
  non_glp1_cohort %>% select(person_id, group, baseline_bmi, nadir_bmi,
                              bmi_class_baseline, bmi_class_nadir,
                              bmi_class_num_baseline, bmi_class_num_nadir,
                              classes_dropped, steps_baseline, steps_nadir, delta_steps,
                              weight_change_pct)
)

# Add expected steps for baseline and nadir BMI classes
combined <- combined %>%
  left_join(expected_by_class %>% select(bmi_class_num, expected_baseline = expected_steps),
            by = c("bmi_class_num_baseline" = "bmi_class_num")) %>%
  left_join(expected_by_class %>% select(bmi_class_num, expected_nadir = expected_steps),
            by = c("bmi_class_num_nadir" = "bmi_class_num")) %>%
  mutate(
    expected_delta = expected_nadir - expected_baseline,
    # Activity gap: how far below expected for their class
    gap_baseline = steps_baseline - expected_baseline,
    gap_nadir = steps_nadir - expected_nadir,
    # Percent of expected change achieved
    pct_expected_achieved = ifelse(expected_delta != 0,
                                    (delta_steps / expected_delta) * 100,
                                    NA_real_)
  )

# Summary by group and transition type
cat("BMI Class Transitions Summary:\n")
transition_summary <- combined %>%
  mutate(transition_type = case_when(
    classes_dropped == 0 ~ "Same class",
    classes_dropped == 1 ~ "Dropped 1 class",
    classes_dropped >= 2 ~ "Dropped 2+ classes"
  )) %>%
  group_by(group, transition_type) %>%
  summarize(
    n = n(),
    mean_wt_change = round(mean(weight_change_pct), 1),
    mean_delta_steps = round(mean(delta_steps)),
    mean_expected_delta = round(mean(expected_delta, na.rm = TRUE)),
    .groups = "drop"
  )
print(transition_summary)
cat("\n")

# ============================================================================
# PART 5: CREATE VISUALIZATIONS
# ============================================================================

cat("PART 5: Creating visualizations...\n\n")

# --------------------------------------------------------------------------
# FIGURE 3A: Activity gap closure by group
# --------------------------------------------------------------------------

cat("Creating Figure 3A: Activity gap closure...\n")

# Focus on people who dropped at least 1 BMI class
movers <- combined %>%
  filter(classes_dropped >= 1)

cat("  Persons who dropped >=1 BMI class:\n")
cat("    GLP-1:", sum(movers$group == "GLP-1"), "\n")
cat("    Non-GLP-1:", sum(movers$group == "Non-GLP-1"), "\n\n")

# Calculate gap closure
gap_summary <- movers %>%
  group_by(group) %>%
  summarize(
    n = n(),
    mean_gap_baseline = mean(gap_baseline, na.rm = TRUE),
    mean_gap_nadir = mean(gap_nadir, na.rm = TRUE),
    mean_actual_change = mean(delta_steps, na.rm = TRUE),
    mean_expected_change = mean(expected_delta, na.rm = TRUE),
    pct_achieved = mean(pct_expected_achieved, na.rm = TRUE),
    .groups = "drop"
  )

cat("Gap analysis for BMI class movers:\n")
print(gap_summary)
cat("\n")

# Reshape for plotting
gap_long <- movers %>%
  select(person_id, group, gap_baseline, gap_nadir) %>%
  pivot_longer(cols = c(gap_baseline, gap_nadir),
               names_to = "timepoint",
               values_to = "gap") %>%
  mutate(timepoint = factor(timepoint,
                            levels = c("gap_baseline", "gap_nadir"),
                            labels = c("Baseline\n(higher BMI)", "After Weight Loss\n(lower BMI)")))

# Box plot of activity gap
fig3a <- ggplot(gap_long, aes(x = timepoint, y = gap, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("GLP-1" = "#E41A1C", "Non-GLP-1" = "#377EB8")) +
  scale_y_continuous(labels = comma, limits = c(-6000, 6000)) +
  labs(
    title = "Activity Gap Relative to BMI Class Norm",
    subtitle = "Among individuals who dropped ≥1 BMI class",
    x = "",
    y = "Steps relative to BMI class average\n(0 = matches class expectation)",
    fill = "Group",
    caption = "Positive = more active than class average; Negative = less active"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

print(fig3a)
ggsave("outputs/figures/figure3a_activity_gap.png", fig3a, width = 9, height = 7, dpi = 300)

# --------------------------------------------------------------------------
# FIGURE 3B: Actual vs Expected step change
# --------------------------------------------------------------------------

cat("Creating Figure 3B: Actual vs Expected change...\n")

# Bar plot comparing actual vs expected
comparison_data <- movers %>%
  group_by(group) %>%
  summarize(
    n = n(),
    Actual = mean(delta_steps, na.rm = TRUE),
    Expected = mean(expected_delta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Actual, Expected),
               names_to = "type",
               values_to = "steps")

fig3b <- ggplot(comparison_data, aes(x = group, y = steps, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, color = "gray30") +
  scale_fill_manual(values = c("Actual" = "#4DAF4A", "Expected" = "#984EA3"),
                    labels = c("Actual Change", "Expected Change\n(based on BMI class norms)")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Actual vs Expected Step Change After BMI Class Transition",
    subtitle = "Expected change based on cross-sectional BMI class activity norms",
    x = "",
    y = "Change in Daily Steps",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  ) +
  # Add sample size labels
  annotate("text", x = 1, y = -200, label = paste0("n=", sum(movers$group == "GLP-1")), size = 4) +
  annotate("text", x = 2, y = -200, label = paste0("n=", sum(movers$group == "Non-GLP-1")), size = 4)

print(fig3b)
ggsave("outputs/figures/figure3b_actual_vs_expected.png", fig3b, width = 9, height = 7, dpi = 300)

# --------------------------------------------------------------------------
# FIGURE 3C: Slope graph - Individual trajectories with class references
# --------------------------------------------------------------------------

cat("Creating Figure 3C: Slope graph with BMI class references...\n")

# Sample for clarity (too many lines = messy)
set.seed(42)
sample_glp1 <- movers %>% filter(group == "GLP-1") %>% sample_n(min(30, nrow(filter(movers, group == "GLP-1"))))
sample_nonglp1 <- movers %>% filter(group == "Non-GLP-1") %>% sample_n(min(50, nrow(filter(movers, group == "Non-GLP-1"))))
sample_data <- bind_rows(sample_glp1, sample_nonglp1)

# Reshape for slope graph
slope_data <- sample_data %>%
  select(person_id, group, steps_baseline, steps_nadir) %>%
  pivot_longer(cols = c(steps_baseline, steps_nadir),
               names_to = "timepoint",
               values_to = "steps") %>%
  mutate(timepoint = factor(timepoint,
                            levels = c("steps_baseline", "steps_nadir"),
                            labels = c("Baseline", "After Weight Loss")))

# Expected steps reference (horizontal lines for each class)
class_refs <- expected_by_class %>%
  mutate(label = paste0(bmi_class, "\n", comma(round(expected_steps)), " steps"))

fig3c <- ggplot() +
  # Reference lines for BMI class averages
  geom_hline(data = class_refs, aes(yintercept = expected_steps),
             linetype = "dotted", color = "gray60", linewidth = 0.5) +
  geom_text(data = class_refs, aes(x = 0.55, y = expected_steps, label = bmi_class),
            hjust = 0, vjust = -0.3, size = 3, color = "gray40") +
  # Individual trajectories
  geom_line(data = slope_data, aes(x = timepoint, y = steps, group = person_id, color = group),
            alpha = 0.4, linewidth = 0.5) +
  geom_point(data = slope_data, aes(x = timepoint, y = steps, color = group),
             alpha = 0.5, size = 2) +
  # Group means
  stat_summary(data = slope_data, aes(x = timepoint, y = steps, group = group, color = group),
               fun = mean, geom = "line", linewidth = 2) +
  stat_summary(data = slope_data, aes(x = timepoint, y = steps, group = group, color = group),
               fun = mean, geom = "point", size = 4) +
  scale_color_manual(values = c("GLP-1" = "#E41A1C", "Non-GLP-1" = "#377EB8")) +
  scale_y_continuous(labels = comma, limits = c(2000, 14000)) +
  labs(
    title = "Individual Activity Trajectories After Weight Loss",
    subtitle = "Dotted lines show average steps for each BMI class (cross-sectional)",
    x = "",
    y = "Daily Steps",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(fig3c)
ggsave("outputs/figures/figure3c_slope_trajectories.png", fig3c, width = 10, height = 8, dpi = 300)

# --------------------------------------------------------------------------
# FIGURE 3D: Percent of expected change achieved (striking bar chart)
# --------------------------------------------------------------------------

cat("Creating Figure 3D: Percent of expected change achieved...\n")

pct_achieved <- movers %>%
  filter(!is.na(pct_expected_achieved), !is.infinite(pct_expected_achieved)) %>%
  group_by(group) %>%
  summarize(
    n = n(),
    mean_pct = mean(pct_expected_achieved, na.rm = TRUE),
    se_pct = sd(pct_expected_achieved, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

cat("\nPercent of expected step change achieved:\n")
print(pct_achieved)
cat("\n")

fig3d <- ggplot(pct_achieved, aes(x = group, y = mean_pct, fill = group)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_pct - 1.96*se_pct, ymax = mean_pct + 1.96*se_pct),
                width = 0.2, linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray30", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(values = c("GLP-1" = "#E41A1C", "Non-GLP-1" = "#377EB8")) +
  scale_y_continuous(breaks = seq(-50, 150, 25)) +
  labs(
    title = "Activity Adaptation to New BMI Class",
    subtitle = "Percent of expected step increase achieved after dropping ≥1 BMI class",
    x = "",
    y = "% of Expected Step Change Achieved",
    caption = "100% = fully adapted to new BMI class activity level\nDashed line shows full adaptation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  annotate("text", x = 1, y = pct_achieved$mean_pct[pct_achieved$group == "GLP-1"] + 15,
           label = paste0(round(pct_achieved$mean_pct[pct_achieved$group == "GLP-1"]), "%\n(n=",
                         pct_achieved$n[pct_achieved$group == "GLP-1"], ")"),
           size = 5, fontface = "bold") +
  annotate("text", x = 2, y = pct_achieved$mean_pct[pct_achieved$group == "Non-GLP-1"] + 15,
           label = paste0(round(pct_achieved$mean_pct[pct_achieved$group == "Non-GLP-1"]), "%\n(n=",
                         pct_achieved$n[pct_achieved$group == "Non-GLP-1"], ")"),
           size = 5, fontface = "bold")

print(fig3d)
ggsave("outputs/figures/figure3d_pct_achieved.png", fig3d, width = 8, height = 7, dpi = 300)

# ============================================================================
# STATISTICAL TESTS
# ============================================================================

cat("\n========================================\n")
cat("STATISTICAL TESTS\n")
cat("========================================\n\n")

# Test: Is step change different from 0 within each group?
cat("1. Is actual step change significantly different from 0?\n")
for (g in c("GLP-1", "Non-GLP-1")) {
  test_data <- movers %>% filter(group == g)
  t_test <- t.test(test_data$delta_steps, mu = 0)
  cat("   ", g, ": mean =", round(mean(test_data$delta_steps)),
      ", t =", round(t_test$statistic, 2),
      ", p =", format.pval(t_test$p.value, digits = 3), "\n")
}

cat("\n2. Is percent achieved different between groups?\n")
test_pct <- movers %>% filter(!is.na(pct_expected_achieved), !is.infinite(pct_expected_achieved))
wilcox_test <- wilcox.test(pct_expected_achieved ~ group, data = test_pct)
cat("   Wilcoxon rank-sum test: p =", format.pval(wilcox_test$p.value, digits = 3), "\n")

cat("\n3. Is percent achieved different from 100% (full adaptation)?\n")
for (g in c("GLP-1", "Non-GLP-1")) {
  test_data <- test_pct %>% filter(group == g)
  t_test <- t.test(test_data$pct_expected_achieved, mu = 100)
  cat("   ", g, ": mean =", round(mean(test_data$pct_expected_achieved)), "%",
      ", t =", round(t_test$statistic, 2),
      ", p =", format.pval(t_test$p.value, digits = 3), "\n")
}

# ============================================================================
# SAVE DATA
# ============================================================================

cat("\nSaving data...\n")
write_csv(combined, "outputs/figures/figure3_bmi_transition_data.csv")
write_csv(expected_by_class, "outputs/figures/figure3_expected_by_class.csv")

cat("\n========================================\n")
cat("Figure 3 complete!\n")
cat("========================================\n\n")

cat("OUTPUT FILES:\n")
cat("  - outputs/figures/figure3a_activity_gap.png\n")
cat("  - outputs/figures/figure3b_actual_vs_expected.png\n")
cat("  - outputs/figures/figure3c_slope_trajectories.png\n")
cat("  - outputs/figures/figure3d_pct_achieved.png\n")
cat("  - outputs/figures/figure3_bmi_transition_data.csv\n")
cat("  - outputs/figures/figure3_expected_by_class.csv\n")
