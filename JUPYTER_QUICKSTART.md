# Jupyter Notebook Quickstart Guide

## For All of Us Workbench Jupyter Environment

This guide is for running the analysis in **Jupyter notebooks** in the All of Us Workbench.

---

## Setup

### 1. Create a New R Notebook

In All of Us Workbench:
1. Click "New" → "Notebook"
2. Select "R" as the kernel
3. Name it something like "Full_House_Analysis"

### 2. Install Required Packages

Run this in your first cell:

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "gtsummary",
  "scales"
))
```

### 3. Create Output Directories

Run this in a new cell:

```r
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/datasets", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)
```

---

## Running the Analysis

### Option 1: Copy and Paste Code Cells (Recommended)

Open the Jupyter-compatible scripts in the `notebooks/` folder and copy each cell into your Jupyter notebook:

1. **01_load_data_jupyter.R** - Copy cells 1-13 to load all data
2. **02_filter_weight_jupyter.R** - Copy cells to filter weight data
3. **03_filter_height_bmi_jupyter.R** - Copy cells to process height and BMI
4. And so on...

### Option 2: Source the Entire File

You can also source the entire file in one cell:

```r
source("notebooks/01_load_data_jupyter.R")
```

---

## File Organization

### Jupyter-Compatible Scripts (notebooks/ folder)
- `01_load_data_jupyter.R` - Load data from Google Cloud Storage
- `02_filter_weight_jupyter.R` - Filter and clean weight data
- `03_filter_height_bmi_jupyter.R` - Process height and BMI
- `04_filter_fitbit_jupyter.R` - Validate Fitbit data
- `05_identify_glp1_bariatric_jupyter.R` - Identify treatments
- `table1_jupyter.R` - Create Table 1
- `figure1_jupyter.R` - Create Figure 1

### Regular R Scripts (data_preparation/ and analysis/ folders)
- Use these if you're working in **RStudio** instead of Jupyter
- Same functionality, just structured differently

---

## Key Differences from RStudio Scripts

### Jupyter Version:
- ✅ Each section is clearly marked as a separate cell
- ✅ Loads data from Google Cloud Storage paths (`gs://...`)
- ✅ Displays output inline in the notebook
- ✅ Can run cells individually or all at once
- ✅ Better for interactive exploration

### RStudio Version:
- Uses `source()` to run entire scripts
- Uses `cat()` for console output
- Designed for batch processing

---

## Step-by-Step Workflow

### Step 1: Load Data (5-10 minutes)

Open a new cell and run:

```r
# Load the data loading script
source("notebooks/01_load_data_jupyter.R")
```

Or copy cells 1-13 from `01_load_data_jupyter.R` into separate cells in your notebook.

**Expected Output:**
- Progress messages as each dataset loads
- Summary table showing row counts for each dataset
- Data saved to `outputs/datasets/01_raw_data.RData`

### Step 2: Filter Weight Data (2-3 minutes)

```r
source("notebooks/02_filter_weight_jupyter.R")
```

**Expected Output:**
- Weight measurements before/after filtering
- Unit conversion summary
- Cleaned weight data saved

### Step 3: Process Height and BMI (2-3 minutes)

```r
source("notebooks/03_filter_height_bmi_jupyter.R")
```

**Expected Output:**
- Height measurements processed
- BMI computed from weight and height
- BMI classes created

### Step 4: Validate Fitbit Data (5-10 minutes)

```r
source("notebooks/04_filter_fitbit_jupyter.R")
```

**Expected Output:**
- Valid Fitbit days identified
- Baseline activity metrics calculated
- Person-level activity summaries

### Step 5: Identify Treatments (1-2 minutes)

```r
source("notebooks/05_identify_glp1_bariatric_jupyter.R")
```

**Expected Output:**
- GLP-1 users identified
- Bariatric surgery patients identified
- Treatment categories assigned

### Step 6: Create Table 1 (1 minute)

```r
source("notebooks/table1_jupyter.R")
```

**Expected Output:**
- Table 1 displayed in notebook
- Saved to `outputs/tables/`

### Step 7: Create Figure 1 (1 minute)

```r
source("notebooks/figure1_jupyter.R")
```

**Expected Output:**
- Scatter plot displayed in notebook
- Saved to `outputs/figures/`

---

## Tips for Jupyter

### 1. Run Cells in Order
The scripts depend on each other, so run them in numerical order (01, 02, 03, etc.)

### 2. Check for Errors
If a cell produces an error:
- Read the error message carefully
- Check that previous cells ran successfully
- Verify data paths are correct

### 3. Save Intermediate Results
Each script saves its output to `.RData` files. If you need to restart:

```r
# Load previous results instead of re-running everything
load("outputs/datasets/01_raw_data.RData")
load("outputs/datasets/02_weight_clean.RData")
# etc.
```

### 4. View Data
To inspect data at any point:

```r
# View first few rows
head(dataset_06597753_person_df)

# View structure
str(dataset_06597753_person_df)

# View summary statistics
summary(baseline_bmi)
```

### 5. Memory Management
For very large datasets (like Fitbit intraday steps), you may want to remove them after processing:

```r
# After you're done with intraday steps
rm(dataset_06597753_fitbit_intraday_steps_df)
gc()  # Garbage collection to free memory
```

---

## Troubleshooting

### "File not found" errors
Make sure you've run the data loading script first and the paths are correct.

### "Object not found" errors
Run previous cells in order - later cells depend on objects created in earlier cells.

### Memory errors
Try running notebooks in smaller chunks and removing large objects you no longer need.

### Data type errors
The data is loaded as characters initially. The processing scripts convert to appropriate types (numeric, dates, etc.)

---

## Next Steps

After running all scripts successfully:

1. **Explore the data** - Use `head()`, `summary()`, `table()` to understand your data
2. **Customize analyses** - Modify the scripts to answer specific questions
3. **Create additional figures** - Use the pattern from Figure 1 as a template
4. **Run statistical models** - Add new cells with regression models, etc.

---

## Questions?

- Check **CLAUDE.md** for detailed data specifications
- Check **README.md** for general project overview
- Review the scripts - they have extensive comments explaining each step
