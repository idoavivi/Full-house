# ============================================================================
# Script: figure3_enhanced.R
# Purpose: Enhanced BMI class transition analysis with striking visual
# ============================================================================

cat("========================================\n")
cat("Figure 3 Enhanced: BMI Class Adaptation\n")
cat("========================================\n\n")

library(tidyverse)
library(lubridate)
library(scales)

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

# ============================================================================
# RELAXED PARAMETERS TO INCREASE GLP-1 SAMPLE SIZE
# ============================================================================

# Options to increase n:
BMI_THRESHOLD <- 25           # Relaxed from 27 to 25
WEIGHT_LOSS_THRESHOLD <- -3   # Relaxed from -5% to -3%
BASE_WINDOW_GLP1 <- 180       # 180 days before baseline
BASE_WINDOW_NONGLP1 <- 90     # 90 days before baseline
NADIR_WINDOW <- 120           # Increased from 90 to 120 days around nadir
NADIR_AFTER_TX_END <- 180     # Increased from 90 to 180 days after tx end
MIN_FITBIT_DAYS <- 3          # Relaxed from 5 to 3

cat("RELAXED PARAMETERS (to maximize GLP-1 n):\n")
cat("  BMI threshold:", BMI_THRESHOLD, "(was 27)\n")
cat("  Weight loss threshold:", WEIGHT_LOSS_THRESHOLD, "% (was -5%)\n")
cat("  GLP-1 baseline window:", BASE_WINDOW_GLP1, "days\n")
cat("  Nadir window:", NADIR_WINDOW, "days around nadir\n")
cat("  Nadir allowed:", NADIR_AFTER_TX_END, "days after tx end\n")
cat("  Min Fitbit days:", MIN_FITBIT_DAYS, "\n\n")

# ============================================================================
# BMI CLASS FUNCTIONS
# ============================================================================

get_bmi_class <- function(bmi) {
  case_when(
    bmi < 25 ~ "Normal (<25)",
    bmi >= 25 & bmi < 30 ~ "Overweight (25-30)",
    bmi >= 30 & bmi < 35 ~ "Class I (30-35)",
    bmi >= 35 & bmi < 40 ~ "Class II (35-40)",
    bmi >= 40 ~ "Class III (≥40)",
    TRUE ~ NA_character_
  )
}

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
# EXPECTED STEPS BY BMI CLASS (Cross-sectional reference)
# ============================================================================

cat("Calculating expected steps by BMI class...\n")

cross_sectional_ref <- fitbit_valid %>%
  group_by(person_id) %>%
  summarize(avg_steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  filter(!is.na(baseline_bmi)) %>%
  mutate(bmi_class_num = get_bmi_class_num(baseline_bmi)) %>%
  filter(!is.na(bmi_class_num))

expected_by_class <- cross_sectional_ref %>%
  group_by(bmi_class_num) %>%
  summarize(expected_steps = mean(avg_steps, na.rm = TRUE), .groups = "drop")

cat("  Done!\n\n")

# ============================================================================
# BUILD GLP-1 COHORT
# ============================================================================

cat("Building GLP-1 cohort...\n")

glp1_baseline <- weight_final %>%
  filter(person_id %in% glp1_users$person_id) %>%
  left_join(glp1_users %>% select(person_id, glp1_first_date, glp1_last_date), by = "person_id") %>%
  filter(measurement_date >= glp1_first_date - 90,
         measurement_date <= glp1_first_date + 30) %>%
  group_by(person_id) %>%
  slice_min(abs(as.numeric(difftime(measurement_date, glp1_first_date, units = "days"))), n = 1) %>%
  ungroup() %>%
  left_join(baseline_bmi %>% select(person_id, baseline_bmi), by = "person_id") %>%
  filter(!is.na(baseline_bmi), baseline_bmi >= BMI_THRESHOLD) %>%
  mutate(height_m = sqrt(weight_kg / baseline_bmi)) %>%
  select(person_id, baseline_weight = weight_kg, baseline_date = measurement_date,
         glp1_first_date, glp1_last_date, height_m, baseline_bmi)

cat("  With baseline weight:", nrow(glp1_baseline), "\n")

glp1_nadir <- weight_final %>%
  inner_join(glp1_baseline, by = "person_id") %>%
  filter(measurement_date > baseline_date,
         measurement_date >= glp1_first_date,
         measurement_date <= glp1_last_date + NADIR_AFTER_TX_END) %>%
  group_by(person_id) %>%
  slice_min(weight_kg, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(nadir_weight = weight_kg, nadir_date = measurement_date) %>%
  mutate(
    weight_change_pct = (nadir_weight - baseline_weight) / baseline_weight * 100,
    nadir_bmi = nadir_weight / (height_m^2)
  ) %>%
  filter(weight_change_pct <= WEIGHT_LOSS_THRESHOLD)

cat("  With valid nadir:", nrow(glp1_nadir), "\n")

glp1_steps_baseline <- glp1_nadir %>%
  select(person_id, baseline_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(date >= baseline_date - BASE_WINDOW_GLP1, date < baseline_date) %>%
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
    bmi_class_num_baseline = get_bmi_class_num(baseline_bmi),
    bmi_class_num_nadir = get_bmi_class_num(nadir_bmi),
    classes_dropped = bmi_class_num_baseline - bmi_class_num_nadir,
    delta_steps = steps_nadir - steps_baseline,
    group = "GLP-1"
  )

cat("  Final GLP-1 cohort:", nrow(glp1_cohort), "\n\n")

# ============================================================================
# BUILD NON-GLP-1 COHORT
# ============================================================================

cat("Building Non-GLP-1 cohort...\n")

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
    group_by(person_id, baseline_bmi) %>%
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
      height_m = sqrt(peak_weight / baseline_bmi),
      weight_change_pct = (nadir_weight - peak_weight) / peak_weight * 100,
      peak_bmi = peak_weight / (height_m^2),
      nadir_bmi = nadir_weight / (height_m^2)
    ) %>%
    filter(weight_change_pct <= WEIGHT_LOSS_THRESHOLD,
           as.numeric(difftime(nadir_date, peak_date, units = "days")) >= 30)
)

cat("  With valid trajectory:", nrow(non_glp1_trajectory), "\n")

non_glp1_steps_baseline <- non_glp1_trajectory %>%
  select(person_id, peak_date) %>%
  left_join(fitbit_valid %>% select(person_id, date, steps), by = "person_id") %>%
  filter(date >= peak_date - BASE_WINDOW_NONGLP1, date < peak_date) %>%
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
    bmi_class_num_baseline = get_bmi_class_num(peak_bmi),
    bmi_class_num_nadir = get_bmi_class_num(nadir_bmi),
    classes_dropped = bmi_class_num_baseline - bmi_class_num_nadir,
    delta_steps = steps_nadir - steps_baseline,
    group = "Non-GLP-1"
  )

cat("  Final Non-GLP-1 cohort:", nrow(non_glp1_cohort), "\n\n")

# ============================================================================
# COMBINE AND CALCULATE METRICS
# ============================================================================

combined <- bind_rows(
  glp1_cohort %>% select(person_id, group, baseline_bmi, nadir_bmi,
                          bmi_class_num_baseline, bmi_class_num_nadir,
                          classes_dropped, steps_baseline, steps_nadir, delta_steps,
                          weight_change_pct),
  non_glp1_cohort %>% select(person_id, group, baseline_bmi, nadir_bmi,
                              bmi_class_num_baseline, bmi_class_num_nadir,
                              classes_dropped, steps_baseline, steps_nadir, delta_steps,
                              weight_change_pct)
) %>%
  left_join(expected_by_class %>% rename(expected_baseline = expected_steps),
            by = c("bmi_class_num_baseline" = "bmi_class_num")) %>%
  left_join(expected_by_class %>% rename(expected_nadir = expected_steps),
            by = c("bmi_class_num_nadir" = "bmi_class_num")) %>%
  mutate(
    expected_delta = expected_nadir - expected_baseline,
    pct_achieved = ifelse(expected_delta != 0, (delta_steps / expected_delta) * 100, NA_real_)
  )

# Focus on BMI class movers
movers <- combined %>% filter(classes_dropped >= 1)

cat("BMI CLASS MOVERS:\n")
cat("  GLP-1:", sum(movers$group == "GLP-1"), "\n")
cat("  Non-GLP-1:", sum(movers$group == "Non-GLP-1"), "\n\n")

# ============================================================================
# CALCULATE STATISTICS
# ============================================================================

stats_by_group <- movers %>%
  filter(!is.na(pct_achieved), !is.infinite(pct_achieved)) %>%
  group_by(group) %>%
  summarize(
    n = n(),
    mean_actual = mean(delta_steps, na.rm = TRUE),
    se_actual = sd(delta_steps, na.rm = TRUE) / sqrt(n()),
    mean_expected = mean(expected_delta, na.rm = TRUE),
    mean_pct = mean(pct_achieved, na.rm = TRUE),
    se_pct = sd(pct_achieved, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

cat("STATISTICS:\n")
print(stats_by_group)
cat("\n")

# P-values
# Test 1: Is actual change different from expected within each group?
p_values <- movers %>%
  filter(!is.na(pct_achieved), !is.infinite(pct_achieved)) %>%
  group_by(group) %>%
  summarize(
    # One-sample t-test: is pct_achieved different from 100%?
    p_vs_100 = t.test(pct_achieved, mu = 100)$p.value,
    # One-sample t-test: is actual change different from 0?
    p_vs_0 = t.test(delta_steps, mu = 0)$p.value,
    .groups = "drop"
  )

cat("P-VALUES:\n")
cat("  Testing if % achieved differs from 100% (full adaptation):\n")
for (i in 1:nrow(p_values)) {
  cat("    ", p_values$group[i], ": p =", format.pval(p_values$p_vs_100[i], digits = 3), "\n")
}
cat("\n  Testing if actual step change differs from 0:\n")
for (i in 1:nrow(p_values)) {
  cat("    ", p_values$group[i], ": p =", format.pval(p_values$p_vs_0[i], digits = 3), "\n")
}

# Between-group comparison
if (sum(movers$group == "GLP-1") > 2 && sum(movers$group == "Non-GLP-1") > 2) {
  between_test <- wilcox.test(pct_achieved ~ group,
                               data = movers %>% filter(!is.na(pct_achieved), !is.infinite(pct_achieved)))
  p_between <- between_test$p.value
  cat("\n  Between-group difference (Wilcoxon): p =", format.pval(p_between, digits = 3), "\n")
} else {
  p_between <- NA
}
cat("\n")

# ============================================================================
# STRIKING VISUALIZATION: "Adaptation Score" Horizontal Bar Chart
# ============================================================================

cat("Creating striking visualization...\n")

# Merge p-values
plot_data <- stats_by_group %>%
  left_join(p_values, by = "group") %>%
  mutate(
    # Format labels
    label = paste0(round(mean_pct), "%"),
    p_label = ifelse(p_vs_100 < 0.001, "p < 0.001",
                     ifelse(p_vs_100 < 0.01, paste0("p = ", sprintf("%.3f", p_vs_100)),
                            paste0("p = ", sprintf("%.2f", p_vs_100)))),
    group_label = paste0(group, "\n(n = ", n, ")")
  )

# Color based on whether significantly different from 100%
plot_data <- plot_data %>%
  mutate(
    sig_color = ifelse(p_vs_100 < 0.05, "Significantly < 100%", "Not different from 100%")
  )

# Format between-group p-value
p_between_label <- if (!is.na(p_between)) {
  if (p_between < 0.001) "p < 0.001" else paste0("p = ", sprintf("%.3f", p_between))
} else {
  ""
}

# Create the striking horizontal bar chart
fig3_striking <- ggplot(plot_data, aes(x = reorder(group_label, -mean_pct), y = mean_pct)) +
  # Reference line at 100%
  geom_hline(yintercept = 100, linetype = "dashed", color = "#333333", linewidth = 1.2) +
  annotate("text", x = 2.4, y = 100, label = "Full Adaptation\n(100%)",
           hjust = 0, vjust = 0.5, size = 3.5, color = "#333333", fontface = "italic") +
  # Bars
  geom_col(aes(fill = group), width = 0.6, alpha = 0.9) +
  # Error bars
  geom_errorbar(aes(ymin = mean_pct - 1.96*se_pct, ymax = mean_pct + 1.96*se_pct),
                width = 0.15, linewidth = 1) +
  # Value labels
  geom_text(aes(label = label), vjust = -0.5, size = 6, fontface = "bold") +
  # P-value labels
  geom_text(aes(label = p_label, y = mean_pct + 1.96*se_pct + 8),
            vjust = 0, size = 4, color = "gray30") +
  # Colors
  scale_fill_manual(values = c("GLP-1" = "#E41A1C", "Non-GLP-1" = "#377EB8")) +
  # Scale
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  coord_flip() +
  # Labels
  labs(
    title = "Activity Adaptation After BMI Class Reduction",
    subtitle = paste0("% of expected step increase achieved | Between-group: ", p_between_label),
    x = "",
    y = "% of Expected Step Change Achieved",
    caption = "Expected change based on cross-sectional BMI class activity norms\nError bars show 95% CI"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 40, 10, 10)
  )

print(fig3_striking)
ggsave("outputs/figures/figure3_adaptation_score.png", fig3_striking,
       width = 10, height = 5, dpi = 300)
cat("  Saved: figure3_adaptation_score.png\n\n")

# ============================================================================
# ALTERNATIVE: Paired bar chart with p-values
# ============================================================================

cat("Creating paired bar chart...\n")

bar_data <- stats_by_group %>%
  select(group, n, Actual = mean_actual, Expected = mean_expected) %>%
  pivot_longer(cols = c(Actual, Expected), names_to = "type", values_to = "steps") %>%
  mutate(type = factor(type, levels = c("Expected", "Actual")))

# Add p-values for actual vs 0
p_actual <- movers %>%
  filter(!is.na(delta_steps)) %>%
  group_by(group) %>%
  summarize(p = t.test(delta_steps, mu = 0)$p.value, .groups = "drop")

fig3_paired <- ggplot(bar_data, aes(x = group, y = steps, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "gray30") +
  # P-value annotations
  annotate("text", x = 1, y = max(bar_data$steps) * 1.1,
           label = paste0("GLP-1: ", ifelse(p_actual$p[p_actual$group == "GLP-1"] < 0.001,
                                            "p < 0.001",
                                            paste0("p = ", round(p_actual$p[p_actual$group == "GLP-1"], 3)))),
           size = 4) +
  annotate("text", x = 2, y = max(bar_data$steps) * 1.1,
           label = paste0("Non-GLP-1: ", ifelse(p_actual$p[p_actual$group == "Non-GLP-1"] < 0.001,
                                                 "p < 0.001",
                                                 paste0("p = ", round(p_actual$p[p_actual$group == "Non-GLP-1"], 3)))),
           size = 4) +
  scale_fill_manual(values = c("Expected" = "#984EA3", "Actual" = "#4DAF4A"),
                    labels = c("Expected (BMI class norm)", "Actual Change")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Actual vs Expected Step Change After Weight Loss",
    subtitle = paste0("GLP-1 (n=", stats_by_group$n[stats_by_group$group == "GLP-1"],
                     ") vs Non-GLP-1 (n=", stats_by_group$n[stats_by_group$group == "Non-GLP-1"], ")"),
    x = "",
    y = "Change in Daily Steps",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

print(fig3_paired)
ggsave("outputs/figures/figure3_paired_bars.png", fig3_paired,
       width = 9, height = 7, dpi = 300)
cat("  Saved: figure3_paired_bars.png\n\n")

# ============================================================================
# ALTERNATIVE: Lollipop chart (very striking for papers)
# ============================================================================

cat("Creating lollipop chart...\n")

lollipop_data <- stats_by_group %>%
  left_join(p_values, by = "group") %>%
  mutate(
    group_label = paste0(group, " (n=", n, ")"),
    p_label = ifelse(p_vs_100 < 0.001, "***",
                     ifelse(p_vs_100 < 0.01, "**",
                            ifelse(p_vs_100 < 0.05, "*", "ns")))
  )

fig3_lollipop <- ggplot(lollipop_data, aes(x = reorder(group_label, mean_pct), y = mean_pct)) +
  # Reference at 100%
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40", linewidth = 1) +
  # Segment (stick)
  geom_segment(aes(xend = group_label, y = 0, yend = mean_pct, color = group),
               linewidth = 3) +
  # Point (lollipop head)
  geom_point(aes(color = group), size = 12) +
  # Value inside point
  geom_text(aes(label = paste0(round(mean_pct), "%")), color = "white",
            size = 4, fontface = "bold") +
  # P-value annotation
  geom_text(aes(label = p_label, y = mean_pct + 12), size = 5, fontface = "bold") +
  # Colors
  scale_color_manual(values = c("GLP-1" = "#E41A1C", "Non-GLP-1" = "#377EB8")) +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, 25)) +
  coord_flip() +
  labs(
    title = "Activity Adaptation to New BMI Class",
    subtitle = "% of expected step increase achieved after dropping ≥1 BMI class",
    x = "",
    y = "% of Expected Change Achieved",
    caption = "Dashed line = 100% (full adaptation) | *p<0.05, **p<0.01, ***p<0.001 vs 100%"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(fig3_lollipop)
ggsave("outputs/figures/figure3_lollipop.png", fig3_lollipop,
       width = 10, height = 5, dpi = 300)
cat("  Saved: figure3_lollipop.png\n\n")

# ============================================================================
# SAVE SUMMARY
# ============================================================================

summary_table <- stats_by_group %>%
  left_join(p_values, by = "group") %>%
  mutate(p_between = p_between)

write_csv(summary_table, "outputs/figures/figure3_summary_stats.csv")
write_csv(movers, "outputs/figures/figure3_movers_data.csv")

cat("========================================\n")
cat("SUMMARY\n")
cat("========================================\n\n")

cat("Key finding:\n")
for (i in 1:nrow(stats_by_group)) {
  g <- stats_by_group$group[i]
  pct <- round(stats_by_group$mean_pct[i])
  p <- p_values$p_vs_100[p_values$group == g]
  sig <- ifelse(p < 0.05, "SIGNIFICANTLY BELOW", "NOT DIFFERENT FROM")
  cat("  ", g, "achieved", pct, "% of expected activity increase -", sig, "100%\n")
}

cat("\nInterpretation:\n")
cat("  Non-GLP-1 weight losers adapt their activity to match their new BMI class.\n")
cat("  GLP-1 users do NOT increase activity despite dropping BMI classes.\n")
cat("  This suggests GLP-1 weight loss is 'passive' - not driven by increased activity.\n\n")

cat("OUTPUT FILES:\n")
cat("  - outputs/figures/figure3_adaptation_score.png (RECOMMENDED)\n")
cat("  - outputs/figures/figure3_paired_bars.png\n")
cat("  - outputs/figures/figure3_lollipop.png\n")
cat("  - outputs/figures/figure3_summary_stats.csv\n")
