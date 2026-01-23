# CLAUDE.md - AI Assistant Guide for Full House Research Project

## Project Overview

This repository contains R code for analyzing bidirectional relationships between physical activity and weight loss using data from the All of Us Research Program.

### Research Questions
1. Does weight loss increase physical activity? (mechanical unloading hypothesis)
2. Does physical activity increase weight loss? (behavioral hypothesis)
3. How do GLP-1 medication users compare to non-pharmacologic weight losers?

### Dataset Information
- **Dataset ID**: 06597753
- **Environment**: All of Us Secure Workbench (RStudio)
- **Data Security**: All data lives in the secure workbench and CANNOT be exported
- **Workflow**: Code is generated locally, then copy-pasted to RStudio in All of Us

### Data Export Paths (GCS)
These are the Google Cloud Storage paths for the exported data (exported 2026-01-23):

| Data Type | GCS Path |
|-----------|----------|
| Person | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/person_06597753/person_06597753_*.csv` |
| Fitbit Activity | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/fitbit_activity_06597753/fitbit_activity_06597753_*.csv` |
| Fitbit Intraday Steps | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/fitbit_intraday_steps_06597753/fitbit_intraday_steps_06597753_*.csv` |
| Measurement | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/measurement_06597753/measurement_06597753_*.csv` |
| Procedure | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/procedure_06597753/procedure_06597753_*.csv` |
| Condition | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/condition_06597753/condition_06597753_*.csv` |
| Observation | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/observation_06597753/observation_06597753_*.csv` |
| Drug | `gs://fc-secure-6de4ba39-0bb8-4e01-98e9-92988c2e5ddc/bq_exports/idoavivi@researchallofus.org/20260123/drug_06597753/drug_06597753_*.csv` |

---

## Environment Setup

### Platform
- **IDE**: RStudio on All of Us Workbench
- **User Level**: Beginner to RStudio - provide step-by-step guidance

### Required R Packages
```r
library(tidyverse)
library(lubridate)
library(gtsummary)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(MatchIt)
library(cobalt)
library(bigrquery)
```

### Coding Conventions
- Load previous .RData file when continuing work
- Print progress messages using `cat()`
- Save outputs to .RData files
- Use simple syntax - avoid complex pipe chains that might break
- Provide clear comments for beginners

---

## Data Structure

### Available Dataframes
After running All of Us export code, these dataframes exist:

| Dataframe Name | Content | Size |
|----------------|---------|------|
| `dataset_06597753_person_df` | Demographics (age, sex, race, ethnicity) | 58,053 rows |
| `dataset_06597753_fitbit_activity_df` | Daily activity summaries | 43,466,989 rows |
| `dataset_06597753_fitbit_intraday_steps_df` | Intraday steps data | 40,947,166 rows |
| `dataset_06597753_measurement_df` | Weight, height, BMI measurements | 6,301,949 rows |
| `dataset_06597753_condition_df` | Medical conditions (diabetes, obesity, etc.) | 1,086,391 rows |
| `dataset_06597753_drug_df` | Medication prescriptions (GLP-1 drugs) | 10,462 rows |
| `dataset_06597753_procedure_df` | Procedures (bariatric surgery) | 1,088 rows |
| `dataset_06597753_observation_df` | Other observations | 15,402 rows |

### Naming Convention
All dataframes follow the pattern: `dataset_06597753_[table]_df`

---

## Data Filtering and Processing Rules

### 1. Weight Data

**Inclusion Patterns:**
- "body weight", "weight measured"

**Exclusion Patterns:**
- "birth", "fetal", "ideal", "dry", "dosing", "percentile"

**Unit Conversion:**
```r
# lbs → kg: multiply by 0.453592
# If unknown unit and value > 200: assume lbs
# If unknown unit and value <= 200: assume kg
```

**Quality Control:**
- Plausibility bounds: 30 to 300 kg
- Same-day duplicates: keep first measurement only

### 2. Height Data

**Inclusion Patterns:**
- "body height", "height", "stature"

**Exclusion Patterns:**
- "percentile", "z-score", "fundal", "sitting"

**Unit Conversion:**
```r
# inches → m: multiply by 0.0254
# cm → m: divide by 100
# Unknown 100-250: assume cm
# Unknown 40-100: assume inches
# Unknown 1-3: assume meters
```

**Quality Control:**
- Plausibility bounds: 1.20 to 2.20 m
- Multiple measurements: use MEAN per person

### 3. BMI Data

**Calculation:**
```r
bmi = weight_kg / (height_m^2)
```

**Quality Control:**
- Plausibility bounds: 12 to 80 kg/m²
- Prefer computed BMI over direct BMI measurements if both available

**BMI Classes:**
- Normal: 18.5-25 kg/m²
- Overweight: 25-30 kg/m²
- Class I Obesity: 30-35 kg/m²
- Class II Obesity: 35-40 kg/m²
- Class III Obesity: >=40 kg/m²

### 4. Fitbit Data

**Validation Thresholds:**
```r
STEPS_MIN = 100
STEPS_MAX = 50000
SEDENTARY_MIN = 180 # minutes
WEAR_TIME_MIN = 600 # minutes (sedentary + all active)
MIN_DAYS_PER_PERIOD = 3
```

**Derived Variables:**
```r
wear_time = sedentary_minutes + lightly_active_minutes +
            fairly_active_minutes + very_active_minutes
mvpa = fairly_active_minutes + very_active_minutes
```

**Valid Day Definition:**
A day is valid if ALL conditions are met:
- steps >= 100 AND steps <= 50,000
- sedentary_minutes >= 180
- wear_time >= 600

### 5. GLP-1 Medications

**Search Patterns:**
- semaglutide, ozempic, wegovy
- tirzepatide, mounjaro, zepbound

**Variables to Extract (per person):**
```r
glp1_first_date     # first prescription date (becomes index_date)
glp1_last_date      # last prescription date
glp1_n_fills        # count of prescriptions
glp1_duration_days  # last_date - first_date
first_glp1_type     # which GLP-1 was first (Semaglutide/Tirzepatide)
glp1_long_term      # TRUE if duration > 90 days OR n_fills >= 3
```

### 6. Bariatric Surgery

**Search Patterns:**
- bariatric, gastric bypass, sleeve, roux-en-y, gastric band, duodenal switch

**CRITICAL:** EXCLUDE all patients with ANY bariatric surgery from analysis

### 7. Treatment Categories

| Category | Definition | Action |
|----------|-----------|--------|
| GLP1_only | Has GLP-1, no bariatric | INCLUDE |
| Neither | No GLP-1, no bariatric | INCLUDE (if weight loser) |
| Bariatric_only | Has bariatric, no GLP-1 | EXCLUDE |
| GLP1_and_Bariatric | Has both | EXCLUDE |

---

## Index Date Definition

**For GLP-1 users:**
- Index date = First GLP-1 prescription date

**For Neither group:**
- Index date = Peak weight date (date of maximum weight)

---

## Weight Outcome Definitions

### Baseline Weight
- **GLP-1 users**: Closest weight within 180 days BEFORE index_date
- **Neither group**: Peak weight (maximum weight in trajectory)

### Nadir Weight
- **CRITICAL**: Nadir must be AFTER baseline_weight_date
- Nadir = Minimum weight after baseline_weight_date

### Calculations
```r
weight_change_kg = nadir_weight_kg - baseline_weight_kg
weight_change_pct = (nadir - baseline) / baseline * 100
days_to_nadir = nadir_date - index_date
```

### Weight Loss Categories
```r
weight_loss_category = case_when(
  weight_change_pct > 0 ~ "Weight gain",
  weight_change_pct >= -5 ~ "<5% loss",
  weight_change_pct >= -10 ~ "5-10% loss",
  weight_change_pct >= -15 ~ "10-15% loss",
  weight_change_pct < -15 ~ ">=15% loss"
)
```

### Neither Group Qualification
To be included as a "weight loser" in the Neither group:
- Must have >=2 weight measurements
- Weight change <= -5%
- Nadir date must be AFTER peak date

---

## Post-Nadir Weight Persistence

For patients with weight measurements after nadir:
```r
regain_kg = latest_weight_after_nadir - nadir_weight
weight_lost = baseline_weight - nadir_weight
regain_pct_of_lost = regain_kg / weight_lost * 100
weight_maintained = TRUE if regain_pct_of_lost < 50
```

---

## Analysis Cohort Criteria

Include patient if ALL of the following are true:
- Has Fitbit data with >= 7 valid days
- Has baseline BMI
- Baseline BMI >= 27 (overweight/obese)
- Age 18-90 years
- Has valid weight outcome data (valid nadir after baseline)
- NO bariatric surgery

---

## Time Periods for Activity Analysis

All time periods are relative to index_date:

| Period | Days | Minimum Valid Days |
|--------|------|-------------------|
| Baseline | -30 to -1 | 3 |
| Period 1 | 1 to 30 | 3 |
| Period 2 | 31 to 90 | 3 |
| Period 3 | 91 to 180 | 3 |
| Period 4 | 181 to 365 | 3 |

---

## Activity Response Categories

Based on step change from baseline to Period 1 (1-30 days):
```r
activity_response = case_when(
  step_change < -500 ~ "Decreaser",
  step_change >= -500 & step_change <= 500 ~ "Maintainer",
  step_change > 500 ~ "Increaser"
)
```

---

## Outlier Bounds

For visualization purposes (Figure 2):
```r
# Weight change outlier bounds
weight_change_min = -25%  # minimum weight change
weight_change_max = +10%  # maximum weight change

# Steps change outlier bounds
steps_change_min = -4000  # minimum steps/day change
steps_change_max = +4000  # maximum steps/day change
```

---

## Current Analysis Tasks

### Task 1: Table 1 - Demographics by BMI Class

**Rows:** BMI classes (as defined above)
**Columns:**
- n (sample size)
- Age (mean ± SD)
- Sex (% distribution)
- GLP-1 use (% yes)
- BMI (mean ± SD)
- Sedentary minutes (mean ± SD)
- Lightly active minutes (mean ± SD)
- Fairly active minutes (mean ± SD)
- Very active minutes (mean ± SD)
- Steps (mean ± SD)
- Calories (mean ± SD)

**Statistical Tests:**
- Add tests to show differences between BMI class groups
- Use appropriate tests (ANOVA/Kruskal-Wallis for continuous, chi-square for categorical)

**Example Code Structure:**
```r
# Create BMI categories
data <- data %>%
  mutate(bmi_class = case_when(
    bmi >= 18.5 & bmi < 25 ~ "18.5-25",
    bmi >= 25 & bmi < 30 ~ "25-30",
    bmi >= 30 & bmi < 35 ~ "30-35",
    bmi >= 35 & bmi < 40 ~ "35-40",
    bmi >= 40 ~ ">=40"
  ))

# Use gtsummary for table
library(gtsummary)
table1 <- data %>%
  select(bmi_class, age, sex, glp1_user, bmi,
         sedentary_minutes, lightly_active_minutes,
         fairly_active_minutes, very_active_minutes,
         steps, calories_out) %>%
  tbl_summary(
    by = bmi_class,
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    missing = "no"
  ) %>%
  add_p()
```

### Task 2: Figure 1 - Steps by BMI Scatter Plot

**Design:**
- X-axis: BMI (continuous)
- Y-axis: Average daily steps
- Color: BMI class (5 colors for 5 classes)
- Trend lines: Separate for GLP-1 users vs non-users

**Reference Image Provided:** User has shared a reference image showing the desired visualization style

**Example Code Structure:**
```r
library(ggplot2)

# Create the scatter plot
fig1 <- ggplot(data, aes(x = bmi, y = steps, color = bmi_class)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(aes(group = glp1_user, linetype = glp1_user),
              method = "lm", se = TRUE, color = "red") +
  scale_color_manual(values = c("18.5-25" = "#blue_shade1",
                                "25-30" = "#blue_shade2",
                                "30-35" = "#yellow",
                                "35-40" = "#orange",
                                ">=40" = "#red")) +
  labs(x = "BMI",
       y = "Average Daily Steps",
       title = "Daily Steps by BMI",
       color = "BMI Class",
       linetype = "GLP-1 User") +
  theme_minimal()
```

---

## Development Workflow

### Step 1: Data Preparation
1. Load the exported dataframes
2. Apply filtering rules (weight, height, BMI, Fitbit)
3. Identify GLP-1 users and bariatric surgery patients
4. Create treatment categories
5. Define index dates

### Step 2: Cohort Selection
1. Apply cohort inclusion criteria
2. Document exclusions at each step
3. Create flowchart of cohort selection

### Step 3: Outcome Calculation
1. Calculate baseline and nadir weights
2. Compute weight change metrics
3. Define weight loss categories
4. Calculate activity metrics by time period

### Step 4: Statistical Analysis
1. Create descriptive tables (Table 1)
2. Generate visualizations (Figure 1, etc.)
3. Run regression models
4. Perform sensitivity analyses

### Step 5: Output and Saving
1. Save all intermediate datasets to .RData
2. Export tables and figures
3. Document all decisions and assumptions

---

## Code Quality Guidelines

### For AI Assistants Working on This Project

1. **Beginner-Friendly**
   - Assume the user is new to RStudio
   - Provide step-by-step instructions
   - Explain what each code block does
   - Use simple, clear variable names

2. **Defensive Programming**
   - Check data types before operations
   - Validate assumptions (e.g., date formats)
   - Handle missing values explicitly
   - Add progress messages with `cat()`

3. **Documentation**
   - Comment complex operations
   - Document data transformations
   - Explain filtering decisions
   - Note any deviations from the protocol

4. **Reproducibility**
   - Set random seeds where applicable
   - Save intermediate results
   - Document package versions
   - Include session info

5. **Data Security**
   - Remember: data CANNOT be exported
   - All analysis must happen in the workbench
   - Only export aggregated results/figures

---

## Common Pitfalls to Avoid

1. **Date/Time Handling**
   - All dates should be converted to Date or POSIXct
   - Be careful with time zones
   - Check for missing or invalid dates

2. **Unit Conversions**
   - Double-check weight (lbs vs kg)
   - Double-check height (inches vs cm vs m)
   - Verify BMI calculations

3. **Fitbit Data**
   - Not all days are valid (apply validation rules)
   - Need minimum number of valid days per period
   - Watch for outliers in steps

4. **GLP-1 Identification**
   - Use case-insensitive string matching
   - Check both generic and brand names
   - Verify first prescription date

5. **Exclusion Criteria**
   - Bariatric surgery patients MUST be excluded
   - Apply exclusions before any analysis
   - Document sample sizes after each exclusion

---

## File Organization

```
Full-house/
├── CLAUDE.md                    # This file
├── data_preparation/
│   ├── 01_load_data.R          # Load exported dataframes
│   ├── 02_filter_weight.R      # Weight data filtering
│   ├── 03_filter_height_bmi.R  # Height and BMI filtering
│   ├── 04_filter_fitbit.R      # Fitbit validation
│   └── 05_identify_glp1.R      # GLP-1 and bariatric identification
├── cohort_selection/
│   ├── 06_define_index_date.R  # Index date definition
│   ├── 07_calculate_outcomes.R # Weight outcomes
│   └── 08_apply_inclusion.R    # Cohort inclusion criteria
├── analysis/
│   ├── 09_table1.R             # Table 1 generation
│   ├── 10_figure1.R            # Figure 1 generation
│   └── future_analyses.R       # Placeholder for future work
├── outputs/
│   ├── tables/                 # Generated tables
│   ├── figures/                # Generated figures
│   └── datasets/               # Saved .RData files
└── README.md                   # Project overview for humans
```

---

## Version History

- **2026-01-23**: Initial CLAUDE.md created
  - Documented data structure and filtering rules
  - Defined analysis tasks (Table 1, Figure 1)
  - Established coding conventions

---

## Contact and Support

For questions about the All of Us Research Program:
- https://www.researchallofus.org/

For questions about this specific analysis:
- Review this CLAUDE.md file
- Check the research protocol in system messages
- Consult with the research team

---

*This file is designed to help AI assistants understand and contribute to this research project. It should be updated as the project evolves and new conventions are established.*
