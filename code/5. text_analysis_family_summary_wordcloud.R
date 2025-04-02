## ===============================================================
## Project: Pre-Interview Task for Lancet Countdown Data Science Research Fellow Position
## Author: Yanying Wang
## Purpose: Conduct exploratory text analysis on policy Family Summary
## Notes:
##   - Due to limited time, only Family Summary is analyzed.
##   - Further text analysis can be extended to full text (see python script).
## ===============================================================


# =======================
# Load Required Libraries
# =======================

library(tm)
library(wordcloud)
library(dplyr)
library(readxl)

# =========================
# Step 1. Merge Region Information
# =========================

region_data <- read_excel("data_raw/region and geographies.xlsx")

# Define name mapping
name_map <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Macao SAR, China" = "Macau",
  "Taiwan, China" = "Taiwan",
  "Korea, Rep." = "South Korea",
  "Korea, Dem. People's Rep." = "Korea, North",
  "Lao PDR" = "Lao People's Democratic Republic",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Congo, Dem. Rep." = "Democratic Republic of Congo",
  "Congo, Rep" = "Congo",
  "Gambia, The" = "Gambia",
  "Venezuela, RB" = "Venezuela",
  "Egypt, Arab Rep." = "Egypt",
  "Iran, Islamic Rep." = "Iran",
  "Yemen, Rep." = "Yemen",
  "Slovak Republic" = "Slovakia",
  "Russian Federation" = "Russia",
  "Türkiye" = "Turkey",
  "São Tomé and Principe" = "Sao Tome and Principe",
  "St. Lucia" = "Saint Lucia",
  "St. Kitts and Nevis" = "Saint Kitts and Nevis",
  "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines"
)

# Standardize country names
region_data <- region_data %>%
  mutate(geo_std = ifelse(Geographies %in% names(name_map), name_map[Geographies], Geographies))

# Merge region info into main data
data <- df %>%
  left_join(region_data %>% select(geo_std, Region), by = c("Geographies" = "geo_std"))

# =========================
# Step 2. Clean Family Summary Text
# =========================

data <- data %>%
  rename(family_summary = `Family Summary`) %>%
  filter(!is.na(family_summary)) %>%
  filter(!is.na(Region))  # Remove observations without Region

# Preprocess text
data$family_summary <- data$family_summary %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  removeWords(stopwords("en"))

# =========================
# Step 3. Generate Region-specific Wordclouds
# =========================

regions <- unique(data$Region)  # List of regions

for (region in regions) {
  
  region_data <- data %>%
    filter(Region == region)  # Subset data
  
  text_data <- paste(region_data$family_summary, collapse = " ")  # Combine all text
  
  # Define output file name
  file_name <- paste0("wordcloud_", region, ".png")
  
  # Save wordcloud as PNG
  png(file_name, width = 800, height = 700)
  
  wordcloud(
    text_data,
    scale = c(6, 1),        # Bigger words
    min.freq = 5,           # Minimum frequency
    max.words = 100,        # Maximum words
    random.order = FALSE,   # Order by frequency
    rot.per = 0.25,         # 25% rotated
    colors = brewer.pal(8, "Dark2")
  )
  
  title(paste("Word Cloud for Region:", region), cex.main = 2.5)  # Title
  
  dev.off()  # Close the device
}

