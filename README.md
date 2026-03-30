# Connecticut Property Assessment Accuracy Analysis

An analysis of ~800K+ residential property transactions in Connecticut revealing how the 2008 market crash caused an 8-year period of systematic overassessment, and estimating the dollar cost to homeowners using mill rate data.

## The Problem

Property assessments directly determine how much homeowners pay in property taxes, which fund schools, infrastructure, and public services. When assessments drift from actual market values, the tax burden becomes unfair. I wanted to quantify how badly the 2008 crash disrupted assessment accuracy in Connecticut, how long it took to correct, and what it actually cost homeowners in excess taxes.

## Approach

I calculated an **assessment accuracy ratio** for each transaction by deriving the implied fair market value (assessed value / 0.7, per Connecticut's 70% assessment standard) and comparing it to the actual sale price. A ratio of 1.0 means the assessment perfectly predicted market value; above 1.0 means overassessment, below means underassessment.

To translate overassessment into dollar impact, I joined town-level mill rate data from two Connecticut sources covering 2005-2026. For each overassessed property, the excess assessed value x (mill rate / 1000) gives the annual tax overpayment.

The dataset required significant cleaning: filtering to residential properties only, removing transactions with unrealistic assessment-to-sale ratios (outside 0.25-2.0), handling pre-2006 records that lacked residential type classifications, and normalizing two separate mill rate datasets with different column layouts.

## Key Findings

- **Pre-crash (2006-2008):** Homes were slightly *under*assessed, with the median accuracy ratio sitting around 0.85, meaning assessments lagged behind rapidly appreciating prices.
- **Post-crash peak (2010-2011):** The ratio spiked to ~1.18, an **18% overassessment**. Home prices dropped but assessed values hadn't caught up, so homeowners were taxed on ghost equity.
- **Duration:** It took roughly **8 years (2009-2016)** before median assessments returned to accuracy.
- **Dollar impact:** During the overassessment period, 65% of properties were overassessed. The median homeowner overpaid **$841/year** in property taxes, with statewide overpayment totaling **$184M** across 227K transactions with mill rate data (84% coverage). Over the full 8-year window, that's roughly **$6,700 per home** at the median.
- **Geographic variation:** Every county shifted from underassessed to overassessed post-crash. Tolland and Middlesex counties saw the largest swings (~0.45+ change in accuracy ratio between 2007 and 2011); Fairfield and New London the smallest.
- **Regression:** An OLS interaction model (`accuracy ~ year_centered * overassessment_period`) confirms the structural break. The slope of accuracy over time differs significantly during 2009-2016.

## Methodology

| Step | Detail |
|------|--------|
| **Accuracy metric** | `(Assessed Value / 0.7) / Sale Price` - ratio of implied FMV to actual sale price |
| **Filtering** | Residential only, sale ratios 0.25-2.0, years 2006-2024 (pre-2006 lacks residential type flags) |
| **Aggregation** | Median accuracy by year, robust to outliers from distressed sales, family transfers |
| **Geographic** | Town-to-county mapping via CT Data Collaborative FIPS crosswalk, median accuracy by county per year |
| **Mill rate join** | Two CT mill rate datasets (2005-2012, 2014-2026) normalized and joined by town + year |
| **Overpayment calc** | `max(0, (actual_assessed - fair_assessed) × mill_rate / 1000)` per transaction |
| **Regression** | OLS with interaction: `accuracy ~ (year - 2008) * overassessment_indicator` |

> **Why median over mean?** Sale price distributions are heavily right-skewed, and outlier transactions would distort a mean-based metric. Confirmed via skewness check (e1071).

## Poster

This project was completed as a semester-long final project for a graduate Data Visualization course. The final deliverable was a printed poster presented at a poster session open to faculty, students, and employers. The poster was designed in Adobe Illustrator and is available in [`poster/IllustratorPosterRevised.pdf`](poster/IllustratorPosterRevised.pdf).

## Visualizations

1. **Assessment Accuracy Over Time** *(main plot)* -- Line chart showing median accuracy ratio 2006-2024 with the 2008 crash marked and the 8-year overassessment window shaded in red.
2. **County Map Timeline** *(supporting)* -- Five small-multiple choropleth maps at key years (2007, 2009, 2011, 2016, 2020) showing the geographic spread using a diverging blue-white-red palette.
3. **Total Statewide Overpayment by Year** -- Bar chart of aggregate dollar overpayment 2009-2016 with labeled values.
4. **Median Per-Home Overpayment** -- Line + area chart of median annual tax overpayment per property.
5. **County Accuracy Shift (2007 -> 2011)** -- Horizontal bar chart ranking counties by magnitude of shift toward overassessment.
6. **Median Home Sale Price Over Time** *(descriptive)* -- Shows the price drop and recovery arc for context.
7. **Residential Sales Volume by Year** *(descriptive)* -- Bar chart of transaction counts across pre-crash, crash, and recovery periods.

## Tech Stack

- **R** — tidyverse, dplyr, ggplot2, lubridate, scales
- **Spatial:** maps, tigris
- **Text:** tidytext, wordcloud (assessor remark exploration)
- **Stats:** e1071 (skewness), base R `lm()` for regression
- **Poster layout:** Adobe Illustrator

## Data

**Primary:** [Real Estate Sales 2001-2023 GL](https://data.ct.gov/Housing-and-Development/Real-Estate-Sales-2001-2023-GL/5mzw-sjtu) - Connecticut Open Data Portal
~800K+ residential transactions, 14 columns including assessed value, sale amount, sale ratio, town, date, property type, and assessor remarks.

**Mill rates (newer):** [Mill Rates for FY 2014-2026](https://data.ct.gov/Local-Government/Mill-Rates-for-FY-2014-2026/emyx-j53e) - CT Open Data Portal

**Mill rates (older):** [Grand List Years 2012-1991 Mill Rates](https://portal.ct.gov/-/media/OPM/IGPP-Data-Grants-Mgmt/Grand-List-Years-2012---1991-Mill-Rates.xlsx) - CT OPM (direct .xlsx download)

**Crosswalk:** [CT Town-County FIPS List](https://github.com/CT-Data-Collaborative/ct-town-county-fips-list) - maps 169 towns to 8 counties.

> The large sales CSV is not included in this repo due to size. Download from the link above and place in `data/`. The mill rate files and crosswalk CSV are included.

## Running Locally

```bash
git clone https://github.com/yourusername/ct-assessment-accuracy.git
cd ct-assessment-accuracy
```

Install R dependencies:

```r
install.packages(c(
  "tidyverse", "tidytext", "textdata", "wordcloud",
  "scales", "reshape2", "maps", "lubridate",
  "gridExtra", "tigris", "e1071"
))
```

Place data files in the project root (or update the file paths at the top of the script), then run `Connecticut_RScript.R` in RStudio or from the command line.

## Project Structure

```
├── README.md
├── .gitignore
├── Connecticut_cleaned_analysis.R  # Main analysis + all plots
├── ct-town-county-fips-list.csv   # Town-to-county crosswalk
├── plots/                         # Exported plot PNGs
├── poster/
│   └── final_poster.pdf
└── poster/
    └── final_poster.ai            # Illustrator source
```

## Future Work

- Fit a linear mixed-effects model (lme4) with random intercepts by town to test whether reassessment frequency explains correction speed
- Two-way ANOVA: time period × residential type interaction on accuracy
- Incorporate property-level reassessment dates (if available) to separate "stale assessment" lag from market-level effects