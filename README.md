# Connecticut Property Assessment Accuracy Analysis

An analysis of Connecticut residential property sales (2006-2024) showing how assessed values fell out of line with sale prices after the 2008 crash, how long they took to realign, and a first estimate of the tax impact using town mill rates.

## The Problem

Property assessments determine how much homeowners pay in property taxes, which fund schools, infrastructure, and public services. When assessments drift from market values, the tax burden can become unfair. I wanted to measure how much the 2008 crash disrupted assessment accuracy in Connecticut, how long it took to correct, and what that might mean for homeowners' tax bills.

## Approach

For each sale I calculated an **assessment accuracy ratio**: the implied fair market value (assessed value / 0.7, per Connecticut's 70% assessment standard) divided by the sale price. This is the state's `Sales Ratio` column divided by 0.7. A ratio of 1.0 means the assessment matched the sale price; above 1.0 means overassessment, below means underassessment.

To turn the ratio into dollars, I joined town-level mill rates from two Connecticut sources covering grand list years 2005-2024. For each overassessed sale, the excess assessed value × (mill rate / 1000) gives a rough annual tax impact.

Cleaning steps: residential properties only, sales ratios between 0.25 and 2.0 (to drop family transfers and other non-market sales), years 2006 and later (earlier records lack residential type labels), and normalizing two mill rate datasets with different layouts.

## Key Findings

- **Pre-crash (2006-2008):** homes were *under*assessed, with a median accuracy ratio around 0.85 - assessments lagged behind rising prices.
- **Post-crash peak (2010-2011):** the median ratio reached ~1.18, about **18% overassessment**. Prices fell but assessed values hadn't caught up.
- **Duration:** the median ratio stayed above 1.0 from 2009 to 2016 before returning to accuracy.
- **Why the lag:** Connecticut towns revalue every five years and assessments stay fixed in between, so they trail the market by design. A sharp price drop shows up as overassessment until each town's next revaluation.
- **Geographic variation:** every county moved from underassessed to overassessed after the crash. Tolland and Middlesex had the largest swings (~0.45 change in median ratio between 2007 and 2011); Fairfield and New London the smallest.
- **Tax impact (rough upper bound):** during 2009-2016, 65% of sales with mill rate data (227K sales, 84% coverage) were overassessed. Multiplying the excess assessment by the town mill rate gives a median of $841/year for those sales. See the limitations below before reading this as money overpaid.

## Limitations

- **Mill rates adjust.** Towns set the mill rate to raise a fixed budget, so if every home in a town is overassessed by the same amount, the mill rate is lower and total taxes don't change. Only *relative* misassessment - a home assessed high compared with others in its town - shifts taxes between owners. The dollar figures above ignore this and are an upper bound, not an overpayment. A within-town version of this analysis is in progress.
- **One-sided count.** The tax figure only counts overassessed sales; the 35% of sales that were underassessed (and paid less) aren't netted out.
- **Sales, not all homes.** The data covers homes that sold, which may differ from the full housing stock, especially during the crash when distressed sales were common.
- **Ratio filter.** The 0.25-2.0 window filters on the same ratio being measured. The dataset's `Non Use Code` column would be a cleaner way to drop non-market sales.
- **Regression.** An OLS interaction model (`accuracy ~ (year - 2008) * overassessment_period`) is included, but the 2009-2016 window was chosen by looking at the trend, so the model describes the pattern rather than testing for it.

## Methodology

| Step | Detail |
|------|--------|
| **Accuracy metric** | `(Assessed Value / 0.7) / Sale Price` - ratio of implied FMV to actual sale price |
| **Filtering** | Residential only, sales ratios 0.25-2.0, years 2006-2024 (pre-2006 lacks residential type labels) |
| **Aggregation** | Median accuracy by year, robust to outliers from distressed sales and family transfers |
| **Geographic** | Town-to-county mapping via CT Data Collaborative FIPS crosswalk, median accuracy by county per year |
| **Mill rate join** | Two CT mill rate datasets (grand list years 2005-2011 and 2012-2024) normalized and joined by town + list year |
| **Tax impact calc** | `max(0, (assessed - 0.7 × sale price) × mill_rate / 1000)` per sale |
| **Regression** | OLS with interaction: `accuracy ~ (year - 2008) * overassessment_indicator` (descriptive) |

> **Why median over mean?** Sale price distributions are heavily right-skewed, and outlier transactions would distort a mean-based metric. Confirmed via skewness check (e1071).

## Poster

This project was completed as a semester-long final project for a Data Visualization course. The final deliverable was a printed poster presented at a poster session open to faculty, students, and employers. The poster was designed in Adobe Illustrator: [`poster/IllustratorPosterRevised.pdf`](poster/IllustratorPosterRevised.pdf).

## Visualizations

All plots are produced by `Connecticut_cleaned_analysis.R`:

1. **Assessment Accuracy Over Time** *(main plot)* - median accuracy ratio 2006-2024 with the 2008 crash marked and the 2009-2016 overassessment window shaded.
2. **County Map Timeline** - five small-multiple county maps (2007, 2009, 2011, 2016, 2020) with a diverging blue-white-red palette.
3. **Total Tax Impact by Year** - bar chart of the summed tax-impact estimate, 2009-2016.
4. **Median Per-Home Tax Impact** - line + area chart of the median annual estimate per overassessed sale.
5. **County Accuracy Shift (2007 -> 2011)** - counties ranked by how far they moved toward overassessment.
6. **Median Home Sale Price Over Time** - the price drop and recovery for context.
7. **Residential Sales Volume by Year** - transaction counts across pre-crash, crash, and recovery periods.

## Tech Stack

- **R** - tidyverse (dplyr, ggplot2), lubridate, readxl, scales
- **Spatial:** maps
- **Text:** tidytext, wordcloud (assessor remark exploration)
- **Stats:** e1071 (skewness), base R `lm()`
- **Poster layout:** Adobe Illustrator

## Data

**Primary:** [Real Estate Sales 2001-2023 GL](https://data.ct.gov/Housing-and-Development/Real-Estate-Sales-2001-2023-GL/5mzw-sjtu) - Connecticut Open Data Portal. 14 columns including assessed value, sale amount, sales ratio, town, date, property type, and assessor remarks.

**Mill rates (newer):** [Mill Rates for FY 2014-2026](https://data.ct.gov/Local-Government/Mill-Rates-for-FY-2014-2026/emyx-j53e) (grand list years 2012-2024) - CT Open Data Portal

**Mill rates (older):** [Grand List Years 2012-1991 Mill Rates](https://portal.ct.gov/-/media/OPM/IGPP-Data-Grants-Mgmt/Grand-List-Years-2012---1991-Mill-Rates.xlsx) - CT OPM

**Crosswalk:** [CT Town-County FIPS List](https://github.com/CT-Data-Collaborative/ct-town-county-fips-list) - maps 169 towns to 8 counties.

> The sales CSV is too large for this repo. Download it from the link above and save it as `data/ConnecticutRealEstateSales.csv`. The mill rate files and crosswalk are already in `data/`.

## Running Locally

```bash
git clone https://github.com/dprindfu/Connecticut-RealEstate-Analysis.git
cd Connecticut-RealEstate-Analysis
```

Install R dependencies:

```r
install.packages(c(
  "tidyverse", "tidytext", "textdata", "wordcloud", "scales",
  "reshape2", "maps", "lubridate", "readxl", "gridExtra",
  "tigris", "e1071"
))
```

Then run `Connecticut_cleaned_analysis.R` from the repo root (in RStudio, or `Rscript Connecticut_cleaned_analysis.R`). File paths in the script are relative to the repo root.

## Project Structure

```
├── README.md
├── .gitignore
├── Connecticut_cleaned_analysis.R      # Main analysis + all plots
├── data/
│   ├── ConnecticutRealEstateSales.csv  # Download separately (gitignored)
│   ├── Mill_Rates_for_FY_2014-2026_20251125.csv
│   ├── Grand List Years 2012 - 1991 Mill Rates.xlsx
│   └── ct-town-county-fips-list.csv
└── poster/
    └── IllustratorPosterRevised.pdf
```

## Future Work

- Measure **relative** misassessment within each town (each sale's ratio vs. its town-year median) to estimate the actual tax shift between homeowners
- Add the standard assessment-quality measures (COD for uniformity, PRD for regressivity) by town and year
- Use `Non Use Code` instead of the ratio window to remove non-market sales
- Link each town's revaluation years to test whether revaluation timing explains how fast towns corrected
