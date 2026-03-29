library(tidytext) 
library(tidyverse)
library(textdata)
library(wordcloud) 
library(scales)
library(reshape2)
library(dplyr)
library(maps)
library(lubridate)
library(readxl)
library(ggplot2)
library(e1071)
library(gridExtra)
library(tigris)


df_data <- read.csv("data/ConnecticutRealEstateSales.csv")

# CT uses a 70% assessment ratio by statute — assessed / 0.7 gives implied FMV
# https://www.darienct.gov/Faq.aspx?QID=87
ASSESSMENT_RATIO <- 0.7

#Sales.Ratio to numeric
df_data$Sales.Ratio <- gsub("[$,]", "", df_data$Sales.Ratio)
df_data$Sales.Ratio <- as.numeric(df_data$Sales.Ratio)

#fix dates
df_data$Date.Recorded <- as.Date(df_data$Date.Recorded, format = "%m/%d/%Y")

df_data <- df_data %>%
  filter(Date.Recorded >= as.Date("2001-01-01") & 
           Date.Recorded <= as.Date("2024-12-31"))

df_data[df_data == ""] <- NA
df_data[df_data == 0] <- NA

options(scipen = 999)

summary(df_data)

df_data['fmv'] = round(df_data$Assessed.Value / ASSESSMENT_RATIO, 0)
df_data['accuracy_of_assessment'] <- round(df_data['fmv'] / df_data['Sale.Amount'], 2)


# OPM remarks like "INCORRECT SALE PRICE" and "DUPLICATE ENTRY" explain some
# extreme ratios — useful context but I filter on ratio range instead
opm_ag <- df_data %>%
  group_by(OPM.remarks) %>%
  summarise(n = n())

#look at density of sales ratios
ggplot(df_data, aes(x = Sales.Ratio)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_classic() +
  xlim(0,5)


#Serial.Number useless for my purposes
df_data <- subset(df_data, select = -Serial.Number)


# 181,057 assessor remarks — quick word freq to understand what gets flagged
tb_data <- df_data %>%
  filter(!is.na(df_data$Assessor.Remarks)) %>%
  unnest_tokens(word, Assessor.Remarks) %>%
  anti_join(stop_words)

tb_wf_data <- tb_data %>%
  group_by(word) %>%
  summarise(n = n())


#Remove commercial and vacant properties by filtering Residential.Type
#Removes ~402,000 rows
cleaned_ratio <- df_data %>%
  filter(!is.na(Residential.Type))

rounded_data <- cleaned_ratio
rounded_data$Sales.Ratio <- round(rounded_data$Sales.Ratio, 2)

# Ratio window [0.25, 2.0] — cuts family transfers and foreclosure fire sales
# while keeping real arm's-length transactions. Spike at 70% visible in density.
condensed <- rounded_data[rounded_data['Sales.Ratio'] >= .25 & rounded_data['Sales.Ratio'] <= 2, ]
ggplot(condensed, aes(x = Sales.Ratio)) +
  geom_density()+
  theme_minimal()


condensed %>%
  mutate(Year = year(Date.Recorded)) %>%
  group_by(Year) %>%
  summarise(
    Median_fmv = mean(fmv, na.rm = TRUE),
    Median_sale = median(Sale.Amount, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Median_fmv, color = "Median_fmv"), color = "#1E3A8A", linewidth = 1.2) +
  geom_line(aes(y = Median_sale, color = "Median_sale"), color = "#16A34A", linewidth = 1.2) +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 2008, color = 'red') +
  labs(
    title = "Median Sale prices vs FMV over time for residential ASR (.25 , 2)",
    x = "Year",
    y = "Amount ($)",
    color = "Measure"
  ) +
  theme_minimal()

# 1 is accurate assessment, >1 is over-assessment, <1 is under-assessment
condensed %>%
  mutate(Year = year(Date.Recorded)) %>%
  group_by(Year) %>%
  summarise(median_accuracy = median(accuracy_of_assessment, na.rm = TRUE)) %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = median_accuracy), color = "#1E3A8A", linewidth = 1.2) +
  geom_vline(xintercept = 2008, color = 'red') +
  geom_hline(yintercept = 1, linetype = 'dashed')+
  labs(
    title = "Home Assessment Accuracy",
    x = "Year",
    y = "Assessment Accuracy"
  ) +
  theme_minimal()


# ── County mapping ───────────────────────────────────────────────────────────
# https://github.com/CT-Data-Collaborative/ct-town-county-fips-list
town_to_county <- read_csv("data/ct-town-county-fips-list.csv")

df_tc <- condensed %>%
  left_join(town_to_county, by = "Town")


# Found that before 2006, data was not being flagged as type residential
# so our residential filter silently drops almost everything pre-2006
df_data %>%
  filter(Date.Recorded >= as.Date("2001-01-01") & 
           Date.Recorded <= as.Date("2005-12-31")) %>%
  mutate(Year = year(Date.Recorded)) %>%
  group_by(Year) %>%
  summarise(
    total_records = n(),
    has_residential_type = sum(!is.na(Residential.Type)),
    within_ratio_range = sum(Sales.Ratio >= 0.25 & Sales.Ratio <= 2, na.rm = TRUE)
  )


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN PLOT — assessment accuracy over time
# ═══════════════════════════════════════════════════════════════════════════════

accuracy_plot_data <- condensed %>%
  mutate(Year = year(Date.Recorded)) %>%
  filter(Year >= 2006) %>%
  group_by(Year) %>%
  summarise(median_accuracy = median(accuracy_of_assessment, na.rm = TRUE)) %>%
  ungroup()

ggplot(accuracy_plot_data, aes(x = Year, y = median_accuracy)) +
  annotate("rect", xmin = 2009, xmax = 2016, ymin = -Inf, ymax = Inf, 
           alpha = 0.15, fill = "red") +
  geom_line(color = "#1E3A8A", linewidth = 1.5) +
  geom_point(color = "#1E3A8A", size = 3) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = "black", linewidth = 1) +
  geom_vline(xintercept = 2008, color = 'red', linewidth = 1.2, linetype = "solid") +
  annotate("text", x = 2012.5, y = 1.22, 
           label = "8-Year Overassessment Period\n(2009-2016)", 
           color = "#8B0000", fontface = "bold", size = 4) +
  annotate("text", x = 2008, y = 0.72, 
           label = "2008\nMarket\nCrash", 
           color = "red", fontface = "bold", size = 3.5, vjust = 1) +
  annotate("text", x = 2010.5, y = 1.15, 
           label = "Peak: 18% overassessed", 
           color = "#8B0000", size = 3, fontface = "italic") +
  annotate("text", x = 2020, y = 1.05, 
           label = "Overassessed:\nHomeowners pay taxes\non inflated values", 
           color = "grey30", size = 3, hjust = 0) +
  annotate("text", x = 2020, y = 0.85, 
           label = "Underassessed:\nTax base eroded", 
           color = "grey30", size = 3, hjust = 0) +
  scale_y_continuous(breaks = seq(0.7, 1.2, 0.1),
                     labels = number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = seq(2006, 2024, 2)) +
  labs(
    title = "Connecticut Homeowners Paid Inflated Property Taxes for 8 Years After Market Crash",
    subtitle = "Median assessment accuracy by year (1.0 = perfect accuracy)",
    x = "Year",
    y = "Assessment Accuracy Ratio",
    caption = "Data: Connecticut Open Data Portal | Residential properties with assessment-to-sale ratios 0.25-2.0"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )


# ═══════════════════════════════════════════════════════════════════════════════
# COUNTY MAPS — pre/post crash comparison and 5-year timeline
# ═══════════════════════════════════════════════════════════════════════════════

ct_map <- map("county", "connecticut", plot = FALSE, fill = TRUE)

# Diverging palette used across all county maps
breaks <- c(-Inf, 0.85, 0.95, 1.05, 1.15, Inf)
labels <- c(
  "< 0.85 (Severe Underassessment)",
  "0.85-0.95 (Underassessment)",
  "0.95-1.05 (Accurate)",
  "1.05-1.15 (Overassessment)",
  "> 1.15 (Severe Overassessment)"
)

pal_cat <- c(
  "< 0.85 (Severe Underassessment)" = "#08519C",
  "0.85-0.95 (Underassessment)" = "#6BAED6",
  "0.95-1.05 (Accurate)" = "#F0F0F0",
  "1.05-1.15 (Overassessment)" = "#FB6A4A",
  "> 1.15 (Severe Overassessment)" = "#A50F15"
)

# Helper — avoids repeating the same group_by/summarize/mutate for each year
get_county_stats <- function(target_year) {
  df_tc %>%
    filter(year(Date.Recorded) == target_year) %>%
    group_by(County) %>%
    summarize(stat = median(accuracy_of_assessment, na.rm = TRUE), .groups = 'drop') %>%
    mutate(
      County_clean = tolower(trimws(County)),
      region = paste0("connecticut,", County_clean)
    )
}

# ── Side-by-side pre/post crash ──────────────────────────────────────────────

county_stats_pre <- df_tc %>%
  filter(year(Date.Recorded) >= 2006 & year(Date.Recorded) <= 2007) %>%
  group_by(County) %>%
  summarize(stat = median(accuracy_of_assessment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(County_clean = tolower(trimws(County)),
         region = paste0("connecticut,", County_clean))

county_stats_crisis <- df_tc %>%
  filter(year(Date.Recorded) >= 2010 & year(Date.Recorded) <= 2012) %>%
  group_by(County) %>%
  summarize(stat = median(accuracy_of_assessment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(County_clean = tolower(trimws(County)),
         region = paste0("connecticut,", County_clean))

dev.new(width = 12, height = 5)
par(mfrow = c(1, 2), mar = c(0.5, 0.5, 2, 0.5))

vals_pre <- county_stats_pre$stat[match(ct_map$names, county_stats_pre$region)]
cat_vals_pre <- cut(vals_pre, breaks = breaks, labels = labels)
cols_pre <- pal_cat[as.character(cat_vals_pre)]
cols_pre[is.na(cols_pre)] <- "white"

map("county", "connecticut", fill = TRUE, col = cols_pre, lwd = 0.5)
title("Pre-Crash Period (2006-2007)", cex.main = 1, font.main = 2)

vals_crisis <- county_stats_crisis$stat[match(ct_map$names, county_stats_crisis$region)]
cat_vals_crisis <- cut(vals_crisis, breaks = breaks, labels = labels)
cols_crisis <- pal_cat[as.character(cat_vals_crisis)]
cols_crisis[is.na(cols_crisis)] <- "white"

map("county", "connecticut", fill = TRUE, col = cols_crisis, lwd = 0.5)
title("Peak Overassessment (2010-2012)", cex.main = 1, font.main = 2)


# ── 5-panel timeline — this ended up on the poster ──────────────────────────

years_to_map <- list(
  list(year = 2007, label = "2007: Pre-Crash\nUnderassessment"),
  list(year = 2009, label = "2009: Post-Crash\nShift"),
  list(year = 2011, label = "2011: Peak\nOverassessment"),
  list(year = 2016, label = "2016: Return to\nAccuracy"),
  list(year = 2020, label = "2020: Cycle\nRepeats")
)

dev.new(width = 18, height = 4)
par(mfrow = c(1, 5), mar = c(0.5, 0.5, 3, 0.5))

for (year_info in years_to_map) {
  county_stats <- get_county_stats(year_info$year)
  
  vals <- county_stats$stat[match(ct_map$names, county_stats$region)]
  cat_vals <- cut(vals, breaks = breaks, labels = labels)
  cols <- pal_cat[as.character(cat_vals)]
  cols[is.na(cols)] <- "white"
  
  map("county", "connecticut", fill = TRUE, col = cols, lwd = 0.5)
  title(year_info$label, cex.main = 0.85, font.main = 2)
}

# Separate legend — stitched into poster in Illustrator
plot.new()
legend("center", 
       legend = labels,
       fill = pal_cat[labels],
       border = "grey20",
       title = "Assessment Accuracy",
       cex = 0.8,
       bty = "n")

par(mfrow = c(1, 1))


# ═══════════════════════════════════════════════════════════════════════════════
# SUPPORTING PLOTS
# ═══════════════════════════════════════════════════════════════════════════════

# ── Sales volume by year ─────────────────────────────────────────────────────

sales_data <- condensed %>%
  mutate(Year = year(Date.Recorded)) %>%
  filter(Year >= 2007) %>%
  group_by(Year) %>%
  summarise(n_sales = n()) %>%
  mutate(period = case_when(
    Year <= 2007 ~ "Pre-Crash",
    Year >= 2008 & Year <= 2012 ~ "Crash Period",
    TRUE ~ "Recovery"
  ))

sales_volume_colored <- ggplot(sales_data, aes(x = Year, y = n_sales, fill = period)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(
    values = c("Pre-Crash" = "#2166AC", 
               "Crash Period" = "#D6604D",
               "Recovery" = "#4292C6"),
    name = "Period"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007, 2024, 2)) +
  labs(
    title = "Residential Sales Volume by Year",
    subtitle = "Transaction activity patterns before, during, and after market crash",
    x = "Year",
    y = "Number of Sales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

sales_volume_colored

# ── Median sale price over time ──────────────────────────────────────────────

median_price_plot <- condensed %>%
  mutate(Year = year(Date.Recorded)) %>%
  filter(Year >= 2007) %>%
  group_by(Year) %>%
  summarise(median_price = median(Sale.Amount, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = median_price)) +
  geom_line(color = "#1E3A8A", linewidth = 1.5) +
  geom_point(color = "#1E3A8A", size = 3) +
  geom_vline(xintercept = 2008, color = 'red', linetype = "dashed", linewidth = 1) +
  annotate("text", x = 2008.2, y = 400000,
           label = "2008 Crash", color = "red", hjust = 0, size = 3.5) +
  scale_y_continuous(labels = scales::dollar_format(), 
                     limits = c(200000, 400000),
                     breaks = seq(200000, 400000, 25000)) +
  scale_x_continuous(breaks = seq(2007, 2024, 2)) +
  labs(
    title = "Median Home Sale Price Over Time",
    subtitle = "Prices dropped after 2008, then steadily recovered",
    x = "Year",
    y = "Median Sale Price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

median_price_plot

# ── County accuracy shift 2007 → 2011 ───────────────────────────────────────

county_change <- df_tc %>%
  filter(year(Date.Recorded) %in% c(2007, 2011)) %>%
  mutate(Year = year(Date.Recorded)) %>%
  group_by(County, Year) %>%
  summarise(median_accuracy = median(accuracy_of_assessment, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Year, values_from = median_accuracy, names_prefix = "Y") %>%
  mutate(change = Y2011 - Y2007) %>%
  arrange(desc(change)) %>%
  ggplot(aes(x = reorder(County, change), y = change)) +
  geom_col(fill = "#A50F15", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  coord_flip() +
  labs(
    title = "Change in Assessment Accuracy: 2007 -> 2011",
    subtitle = "All counties shifted from underassessed to overassessed",
    x = "County",
    y = "Change in Accuracy (positive = shift toward overassessment)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )
county_change


# ═══════════════════════════════════════════════════════════════════════════════
# MILL RATE JOIN & OVERPAYMENT ANALYSIS
# ═══════════════════════════════════════════════════════════════════════════════
# Mill rate = tax per $1,000 of assessed value. Joining this lets me turn
# the accuracy ratio into actual dollar overpayment per home.

millratenewer  <- read_csv("data/Mill_Rates_for_FY_2014-2026_20251125.csv")
millrateolder  <- read_excel("data/Grand List Years 2012 - 1991 Mill Rates.xlsx")

millrateolder <- millrateolder[-1, ]

millrateolder <- millrateolder %>%
  rename(
    `TownWorking` = `...2`,
    `2011` = `...4`,
    `2010` = `...5`,
    `2009` = `...6`,
    `2008` = `...7`,
    `2007` = `...8`,
    `2006` = `...9`,
    `2005` = `...10`
  )

millrateolder <- millrateolder %>%
  pivot_longer(
    cols = c(`2011`, `2010`,`2009`, `2008`, `2007`, `2006`, `2005`),  
    names_to = "year",
    values_to = "mill_rate"
  )

millratenewer <- millratenewer %>%
  rename(
    'town' = 'Municipality/District',
    'mill_rate' = 'Mill Rate',
    'year' = 'Grand List Year'
  )

millrateolder <- millrateolder %>%
  rename(
    'town' = 'TownWorking'
  )

millrateolder$year <- as.numeric(millrateolder$year)
millrateolder$mill_rate <- as.numeric(millrateolder$mill_rate)
millratenewer$mill_rate <- as.numeric(millratenewer$mill_rate)

millratenewer <- millratenewer %>%
  select(town, mill_rate, year)

millrateolder <- millrateolder %>%
  select(town, mill_rate, year)

combined <- bind_rows(millrateolder, millratenewer) %>%
  arrange(town, year) %>%
  select(town, year, mill_rate)


# Join mill rates to condensed by town + year
condensed <- condensed %>%
  rename(
    'town' = 'Town',
    'year' = 'List.Year')

condensed <- condensed %>%
  left_join(combined, by = c("town", "year"))

sum(is.na(condensed$mill_rate))

# Check coverage — 84% of 2009-2016 records have mill rates, and the subset
# has the same median accuracy as the full set, so it's representative
condensed %>%
  mutate(Year = year(Date.Recorded)) %>%
  filter(Year >= 2009 & Year <= 2016) %>%
  summarise(
    total_records = n(),
    with_mill_rate = sum(!is.na(mill_rate)),
    pct_coverage = round(with_mill_rate / total_records * 100, 1),
    median_accuracy_all = median(accuracy_of_assessment, na.rm = TRUE),
    median_accuracy_with_mill = median(accuracy_of_assessment[!is.na(mill_rate)], na.rm = TRUE)
  )


# ── Overpayment calculation ─────────────────────────────────────────────────
# For each transaction: what SHOULD the assessed value have been (70% of sale)
# vs what it actually was? Multiply excess by mill rate / 1000.

overpayment_analysis <- condensed %>%
  filter(!is.na(mill_rate)) %>%
  mutate(
    Year = year(Date.Recorded),
    fair_assessed = Sale.Amount * ASSESSMENT_RATIO,
    actual_assessed = Assessed.Value,
    assessment_excess = actual_assessed - fair_assessed,
    annual_tax_overpayment = pmax(0, assessment_excess * (mill_rate / 1000))
  ) %>%
  filter(Year >= 2009 & Year <= 2016)

overpayment_summary <- overpayment_analysis %>%
  summarise(
    total_properties = n(),
    properties_overassessed = sum(assessment_excess > 0),
    pct_overassessed = round(properties_overassessed / total_properties * 100, 1),
    median_annual_overpayment = median(annual_tax_overpayment[annual_tax_overpayment > 0], na.rm = TRUE),
    mean_annual_overpayment = mean(annual_tax_overpayment[annual_tax_overpayment > 0], na.rm = TRUE),
    total_annual_overpayment = sum(annual_tax_overpayment, na.rm = TRUE),
    estimated_per_home_8year = median_annual_overpayment * 8
  )
print(overpayment_summary)

# ── Overpayment by year ─────────────────────────────────────────────────────

overpayment_by_year <- overpayment_analysis %>%
  group_by(Year) %>%
  summarise(
    n_properties = n(),
    median_overpayment = median(annual_tax_overpayment[annual_tax_overpayment > 0], na.rm = TRUE),
    mean_overpayment = mean(annual_tax_overpayment[annual_tax_overpayment > 0], na.rm = TRUE),
    total_overpayment = sum(annual_tax_overpayment, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(overpayment_by_year, aes(x = Year, y = median_overpayment)) +
  geom_line(color = "#A50F15", linewidth = 1.5) +
  geom_point(size = 3, color = "#A50F15") +
  geom_area(alpha = 0.2, fill = "#A50F15") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = 2009:2016, limits = c(2009,2016)) +
  labs(
    title = "Median Annual Property Tax Overpayment per Home",
    subtitle = "During 8-year overassessment period (2009-2016)",
    x = "Year",
    y = "Median Annual Overpayment",
    caption = "Based on residential sales with mill rate data (84% coverage)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ── Total statewide overpayment by year ──────────────────────────────────────

total_overpayment_by_year <- overpayment_analysis %>%
  filter(Year >= 2009 & Year <= 2016) %>%
  group_by(Year) %>%
  summarise(
    total_overpayment = sum(annual_tax_overpayment, na.rm = TRUE),
    n_properties = n(),
    .groups = 'drop'
  )

ggplot(total_overpayment_by_year, aes(x = Year, y = total_overpayment)) +
  geom_col(fill = "#A50F15", width = 0.7) +
  geom_text(aes(label = scales::dollar(total_overpayment, scale = 1e-6, suffix = "M", accuracy = 0.1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_x_continuous(breaks = 2009:2016) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Total Property Tax Overpayment Across Connecticut",
    subtitle = "Annual overpayment during 8-year overassessment period",
    x = "Year",
    y = "Total Overpayment",
    caption = "Based on residential sales with mill rate data (84% coverage)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


# ═══════════════════════════════════════════════════════════════════════════════
# REGRESSION — accuracy ~ time with structural break at 2008
# ═══════════════════════════════════════════════════════════════════════════════

regression_data <- condensed %>%
  mutate(Year = year(Date.Recorded)) %>%
  filter(Year >= 2006) %>%
  mutate(
    pre_crash = ifelse(Year <= 2008, 1, 0),
    overassessment_period = ifelse(Year >= 2009 & Year <= 2016, 1, 0),
    post_recovery = ifelse(Year >= 2017, 1, 0),
    year_centered = Year - 2008
  )

annual_summary <- regression_data %>%
  group_by(Year) %>%
  summarise(
    median_accuracy = median(accuracy_of_assessment, na.rm = TRUE),
    mean_accuracy = mean(accuracy_of_assessment, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

# Interaction model: does the slope of accuracy over time differ during
# the overassessment window?
model <- lm(accuracy_of_assessment ~ year_centered * overassessment_period, 
            data = regression_data)

summary(model)