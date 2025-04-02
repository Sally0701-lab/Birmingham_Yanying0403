## ===============================================================
## Project: Pre-Interview Task for Lancet Countdown Data Science Research Fellow Position
## Author: Yanying Wang
## Purpose: Prepare a clean country-year panel dataset for analysis: (1) Clean the CCLW data 
## (2) Merge it with air pollution data, governance indicators, health, and socio-economic indicators from WDI

## ===============================================================

# =======================
# Load Required Libraries
# =======================

library(dplyr)
library(readr)
library(lubridate)
library(slider)
library(readxl)
library(tidyr)
library(fixest)
library(ggplot2)
library(purrr)
library(skimr)
library(naniar)


# =======================
# Step 1. Read and Clean Raw CCLW Data
# =======================

# Set working directory at the beginning
setwd("C:/Users/wyy98/Desktop/birmingham")

df <- read_csv("data_raw/Document_Data_Download-2025-03-17.csv")

df <- df %>%
  mutate(Year = year(ymd(`Last event in timeline`))) %>%
  filter(!is.na(Geographies), !is.na(`Document ID`), !is.na(Year))

# =======================
# Step 2. Create Basic Panel Dataset
# =======================

panel_data <- df %>%
  group_by(Geographies, Year) %>%
  summarise(Document_Count = n_distinct(`Document ID`), .groups = "drop")

panel_data <- panel_data %>%
  arrange(Geographies, Year) %>%
  group_by(Geographies) %>%
  mutate(
    Cumulative_Document_Count_ToDate = cumsum(Document_Count),
    Document_Count_Last3Years = slide_dbl(Document_Count, .before = 2, .complete = FALSE, .f = sum)
  ) %>%
  ungroup()

# =======================
# Step 3. Create Document Type Dummies
# =======================

df_dummies <- df %>%
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = c(Geographies, Year, `Document ID`),
    names_from = `Document Type`,
    values_from = value,
    values_fill = 0
  )

document_dummies_summary <- df_dummies %>%
  group_by(Geographies, Year) %>%
  summarise(across(-`Document ID`, sum), .groups = "drop")

# =======================
# Step 4. Merge Dummies into Panel Data
# =======================

panel_data_with_dummies <- panel_data %>%
  left_join(document_dummies_summary, by = c("Geographies", "Year"))

# Clean variable names
names(panel_data_with_dummies) <- gsub(" ", "_", names(panel_data_with_dummies))

# =======================
# Step 5. Compute Cumulative and Rolling Sums for Policy Types
# =======================

vars_to_process <- c(
  "Law", "Plan", "Strategy", "Decree", "Policy",
  "Submission_To_The_Global_Stocktake", "Act",
  "Nationally_Determined_Contribution", "Regulation", "National_Communication"
)

panel_data_with_dummies <- panel_data_with_dummies %>%
  arrange(Geographies, Year) %>%
  group_by(Geographies)

for (var in vars_to_process) {
  panel_data_with_dummies <- panel_data_with_dummies %>%
    mutate(
      !!paste0("Cumulative_", var) := cumsum(.data[[var]]),
      !!paste0("Rolling3_", var) := slide_dbl(.data[[var]], .before = 2, .complete = FALSE, .f = sum)
    )
}

panel_data_with_dummies <- panel_data_with_dummies %>% ungroup()

# =======================
# Step 6. Merge Region Data
# =======================

region_data <- read_excel("data_raw/region and geographies.xlsx")

# Standardize country names
name_map <- c(
  "Hong Kong SAR, China" = "Hong Kong", "Macao SAR, China" = "Macau",
  "Taiwan, China" = "Taiwan", "Korea, Rep." = "South Korea",
  "Korea, Dem. People's Rep." = "Korea, North", "Lao PDR" = "Lao People's Democratic Republic",
  "Micronesia, Fed. Sts." = "Micronesia", "Congo, Dem. Rep." = "Democratic Republic of Congo",
  "Congo, Rep" = "Congo", "Gambia, The" = "Gambia", "Venezuela, RB" = "Venezuela",
  "Egypt, Arab Rep." = "Egypt", "Iran, Islamic Rep." = "Iran", "Yemen, Rep." = "Yemen",
  "Slovak Republic" = "Slovakia", "Russian Federation" = "Russia", "Türkiye" = "Turkey",
  "São Tomé and Principe" = "Sao Tome and Principe", "St. Lucia" = "Saint Lucia",
  "St. Kitts and Nevis" = "Saint Kitts and Nevis", "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
  "United States" = "United States of America", "North Macedonia" = "North Macedonia (Republic of North Macedonia)"
)

region_data <- region_data %>%
  mutate(geo_std = ifelse(Geographies %in% names(name_map), name_map[Geographies], Geographies))

panel_data_region <- panel_data_with_dummies %>%
  left_join(region_data %>% select(geo_std, Region), by = c("Geographies" = "geo_std"))

# =======================
# Step 7. Merge Governance Indicators from WGI
# =======================

wdi_data <- read_excel("data_raw/wgidataset.xlsx")

wdi_filtered <- wdi_data %>%
  filter(indicator %in% c("rq", "ge")) %>%
  select(countryname, year, indicator, estimate) %>%
  pivot_wider(names_from = indicator, values_from = estimate) %>%
  rename(
    Geographies = countryname,
    Year = year,
    rq_estimate = rq,
    ge_estimate = ge
  ) %>%
  mutate(
    ge_estimate = as.numeric(ge_estimate),
    rq_estimate = as.numeric(rq_estimate)
  )

panel_data_final <- panel_data_region %>%
  left_join(wdi_filtered, by = c("Geographies", "Year"))

# =======================
# Step 8. Merge PM2.5 Data
# =======================

pm_data <- read_excel("data_raw/PM.xlsx") %>%
  select(country, year, PM25) %>%
  rename(Geographies = country, Year = year)

panel_data_final <- panel_data_final %>%
  left_join(pm_data, by = c("Geographies", "Year"))

# =======================
# Step 9. Merge WDI Health & Socio-Economic Indicators
# =======================

wdi_raw <- read_excel("data_raw/WDIEXCEL.xlsx")

target_indicators <- c(
  "GDP per capita (constant 2015 US$)",
  "Current health expenditure per capita (current US$)",
  "Population ages 65 and above (% of total population)",
  "GDP growth (annual %)",
  "Mortality rate, infant (per 1,000 live births)",
  "Life expectancy at birth, total (years)",
  "Mortality rate, adult, female (per 1,000 female adults)",
  "Mortality rate, adult, male (per 1,000 male adults)",
  "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)",
  "Current health expenditure (% of GDP)",
  "Survival to age 65, female (% of cohort)",
  "Survival to age 65, male (% of cohort)",
  "Forest area (sq. km)",
  "Mortality rate, under-5 (per 1,000 live births)",
  "Mortality rate attributed to household and ambient air pollution, age-standardized (per 100,000 population)",
  "Suicide mortality rate (per 100,000 population)",
  "Mortality rate, neonatal (per 1,000 live births)",
  "Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population)"
)

wdi_filtered <- wdi_raw %>%
  filter(`Indicator Name` %in% target_indicators) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(Year)) %>%
  select(`Country Name`, Year, `Indicator Name`, Value) %>%
  pivot_wider(names_from = `Indicator Name`, values_from = Value)

panel_data_final <- panel_data_final %>%
  left_join(wdi_filtered, by = c("Geographies" = "Country Name", "Year" = "Year"))

# =======================
# Step 10. Rename Key Columns for Clarity
# =======================

panel_data_final <- panel_data_final %>%
  rename(
    GDP_per_capita = `GDP per capita (constant 2015 US$)`,
    Health_exp_per_capita = `Current health expenditure per capita (current US$)`,
    Pop_65plus_percent = `Population ages 65 and above (% of total population)`,
    GDP_growth_rate = `GDP growth (annual %)`,
    Infant_mortality = `Mortality rate, infant (per 1,000 live births)`,
    Life_expectancy = `Life expectancy at birth, total (years)`,
    Mortality_female = `Mortality rate, adult, female (per 1,000 female adults)`,
    Mortality_male = `Mortality rate, adult, male (per 1,000 male adults)`,
    Mortality_NCD = `Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)`,
    Health_exp_percent_GDP = `Current health expenditure (% of GDP)`,
    Survival_to_65_female = `Survival to age 65, female (% of cohort)`,
    Survival_to_65_male = `Survival to age 65, male (% of cohort)`,
    Forest_area = `Forest area (sq. km)`,
    Under5_mortality = `Mortality rate, under-5 (per 1,000 live births)`,
    Air_pollution_mortality = `Mortality rate attributed to household and ambient air pollution, age-standardized (per 100,000 population)`,
    Suicide_mortality = `Suicide mortality rate (per 100,000 population)`,
    Neonatal_mortality = `Mortality rate, neonatal (per 1,000 live births)`,
    Unsafewater_mortality = `Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population)`
  )




# =====================================================
# Step 11. Life expectancy plot 
# =====================================================



#  Filter data from 1990 onwards and remove missing values
plot_data <- panel_data_final %>%
  filter(Year >= 1990, !is.na(Life_expectancy), !is.na(Region)) %>%
  group_by(Region, Year) %>%
  summarise(mean_life_exp = mean(Life_expectancy, na.rm = TRUE), .groups = "drop")

#  Plot smoothed line chart
p1 <-ggplot(plot_data, aes(x = Year, y = mean_life_exp, color = Region)) +
  geom_smooth(se = FALSE, size = 1.2) +   
  labs(
    title = "Smoothed Trends of Life Expectancy by Region (Since 1990)",
    x = "Year",
    y = "Average Life Expectancy",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

ggsave("Life Expectancy by Region (Since 1990) .png", plot = p1, width = 8, height = 6, dpi = 600, bg = "white")


