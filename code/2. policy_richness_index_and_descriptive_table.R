## ===============================================================
## Project: Pre-Interview Task for Lancet Countdown Data Science Research Fellow Position
## Author: Yanying Wang
## Purpose: Generate the policy richness index for regression and genereate descriptive table

## ================================================================

# =======================
# Load Required Libraries
# =======================

library(dplyr)
library(purrr)
library(tidyr)
library(slider)

# =======================
# Step 1. Define Category Lists
# =======================

topic_categories <- c("Mitigation", "Adaptation", "Loss And Damage", "Disaster Risk Management")

sector_categories <- c(
  "Adaptation", "Agriculture", "Buildings", "Coastal zones", "Cross cutting zones",
  "Disaster risk management", "Economy-wide", "Energy", "Environment", "Finance",
  "Health", "Industry", "LULUCF", "Public sector", "Rural", "Social development",
  "Tourism", "Transport", "Transportation", "Urban", "Waste", "Water", "Other"
)

instrument_categories <- c(
  "Direct investment", "Economic", "Governance",
  "Information", "Regulation"
)

# =======================
# Step 2. Prepare Cleaned Dataset
# =======================

df_cleaned <- df %>%
  select(Geographies, Year, `Document ID`, `Document Type`, Category, 
         `Topic/Response`, Sector, Instrument)

# =======================
# Step 3. Define Text Processing Function
# =======================

split_semicolon <- function(x) {
  unique(unlist(strsplit(x, ";\\s*")))
}

# =======================
# Step 4. Compute Basic Richness Indicators
# =======================

richness_panel <- df_cleaned %>%
  group_by(Geographies, Year) %>%
  summarise(
    Document_Count = n_distinct(`Document ID`),
    Topics = list(split_semicolon(paste(`Topic/Response`, collapse = ";"))),
    Sectors = list(split_semicolon(paste(Sector, collapse = ";"))),
    Instruments = list(paste(Instrument, collapse = " ; ")),
    health = ifelse(any(grepl("health", Sector, ignore.case = TRUE)), 1, 0),
    .groups = "drop"
  ) %>%
  mutate(
    Topic_Richness = sapply(Topics, function(x) sum(x %in% topic_categories)),
    Sector_Richness = sapply(Sectors, function(x) sum(x %in% sector_categories)),
    Instrument_Richness = sapply(Instruments, function(instr_string) {
      sum(sapply(instrument_categories, function(keyword) grepl(keyword, instr_string, ignore.case = TRUE)))
    })
  )

# =======================
# Step 5. Compute Cumulative and Rolling Richness
# =======================

richness_panel <- richness_panel %>%
  filter(!is.na(Year)) %>%
  arrange(Geographies, Year) %>%
  group_by(Geographies) %>%
  mutate(
    # Topic
    Cumulative_Topic_Richness = accumulate(Topics, ~ union(.x, .y)) %>%
      map_int(~ sum(.x %in% topic_categories)),
    Recent_Topic_Richness = slide_index(
      Topics, Year, ~ sum(unique(unlist(.x)) %in% topic_categories),
      .before = 2, .complete = FALSE
    ),
    
    # Sector
    Cumulative_Sector_Richness = accumulate(Sectors, ~ union(.x, .y)) %>%
      map_int(~ sum(.x %in% sector_categories)),
    Recent_Sector_Richness = slide_index(
      Sectors, Year, ~ sum(unique(unlist(.x)) %in% sector_categories),
      .before = 2, .complete = FALSE
    ),
    
    # Instrument (fuzzy matching)
    Cumulative_Instrument_Richness = accumulate(Instruments, ~ paste(.x, .y, sep = " ; ")) %>%
      map_int(~ sum(sapply(instrument_categories, function(keyword) grepl(keyword, .x, ignore.case = TRUE)))),
    Recent_Instrument_Richness = slide_index(
      Instruments, Year, ~ sum(sapply(instrument_categories, function(keyword)
        grepl(keyword, paste(.x, collapse = " ; "), ignore.case = TRUE))),
      .before = 2, .complete = FALSE
    ),
    
    # Health
    Cumulative_Health_Count = cumsum(health),
    Recent_Health_Count = slide_index(
      health, Year, ~ sum(.x), 
      .before = 2, .complete = FALSE
    )
  ) %>%
  ungroup()

# =======================
# Step 6. Merge to Main Panel
# =======================

panel_data_final <- panel_data_final %>%
  left_join(richness_panel, by = c("Geographies", "Year"))

# =======================
# Step 7. Clean-up
# =======================

panel_data_final <- panel_data_final %>%
  mutate(across(c(Recent_Health_Count, Recent_Topic_Richness, Recent_Sector_Richness, Recent_Instrument_Richness), unlist))

# ------------------------------
# Step 8. Descriptive Statistics Table
# ------------------------------


# Select key variables for the table
vars <- c(
  "Document_Count.x",
  "Cumulative_Document_Count_ToDate",
  "Document_Count_Last3Years",
  "Topic_Richness",
  "Sector_Richness",
  "Instrument_Richness",
  "Cumulative_Topic_Richness",
  "Recent_Topic_Richness",
  "Cumulative_Sector_Richness",
  "Recent_Sector_Richness",
  "Cumulative_Instrument_Richness",
  "Recent_Instrument_Richness",
  "Cumulative_Health_Count",
  "Recent_Health_Count",
  "GDP_per_capita",
  "Health_exp_per_capita",
  "Pop_65plus_percent",
  "GDP_growth_rate",
  "Infant_mortality",
  "Life_expectancy",
  "Mortality_female",
  "Mortality_male",
  "Mortality_NCD",
  "Health_exp_percent_GDP",
  "Survival_to_65_female",
  "Survival_to_65_male",
  "Forest_area",
  "Under5_mortality",
  "Air_pollution_mortality",
  "Suicide_mortality",
  "Neonatal_mortality",
  "Unsafewater_mortality"
)

# generate Descriptive Statistics Table
desc_table <- data.frame(
  Variable = vars,
  N = sapply(panel_data_final[vars], function(x) sum(!is.na(x))),
  Missing = sapply(panel_data_final[vars], function(x) sum(is.na(x))),
  Mean = sapply(panel_data_final[vars], function(x) mean(x, na.rm = TRUE)),
  Min = sapply(panel_data_final[vars], function(x) min(x, na.rm = TRUE)),
  Max = sapply(panel_data_final[vars], function(x) max(x, na.rm = TRUE))
)

# print table
print(desc_table)

# write CSV
write.csv(desc_table, "output/summary_descriptive.csv", row.names = FALSE)




# =====================================================
# Step 11. policy number plot & policy richness plot
# =====================================================


#  Prepare data for Document Count.x
doc_plot_data <- panel_data_final %>%
  filter(Year >= 1990, !is.na(Document_Count.x), !is.na(Region)) %>%
  group_by(Region, Year) %>%
  summarise(mean_doc_count = mean(Document_Count.x, na.rm = TRUE), .groups = "drop")

# Plot smoothed line chart for Document Count
p2 <-  ggplot(doc_plot_data, aes(x = Year, y = mean_doc_count, color = Region)) +
  geom_smooth(se = FALSE, size = 1.2) +   # Smoothed curve without confidence interval
  labs(
    title = "Smoothed Trends of Document Count by Region (Since 1990)",
    x = "Year",
    y = "Average Document Count",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

ggsave("Document Count by Region (Since 1990) .png", plot = p2, width = 8, height = 6, dpi = 600, bg = "white")


#  Prepare data for Instrument Richness
instrument_plot_data <- panel_data_final %>%
  filter(Year >= 1990, !is.na(Instrument_Richness), !is.na(Region)) %>%
  group_by(Region, Year) %>%
  summarise(mean_instrument_richness = mean(Instrument_Richness, na.rm = TRUE), .groups = "drop")

#  Plot smoothed line chart
p3 <- ggplot(instrument_plot_data, aes(x = Year, y = mean_instrument_richness, color = Region)) +
  geom_smooth(se = FALSE, size = 1.2) +   # Smoothed curve without confidence interval
  labs(
    title = "Smoothed Trends of Instrument Richness by Region (Since 1990)",
    x = "Year",
    y = "Average Instrument Richness",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
ggsave("Instrument Richness by Region (Since 1990) .png", plot = p3, width = 8, height = 6, dpi = 600, bg = "white")


#  Prepare data for Topic Richness
Topic_plot_data <- panel_data_final %>%
  filter(Year >= 1990, !is.na(Topic_Richness), !is.na(Region)) %>%
  group_by(Region, Year) %>%
  summarise(mean_Topic_richness = mean(Topic_Richness, na.rm = TRUE), .groups = "drop")

#  Plot smoothed line chart
p4 <- ggplot(Topic_plot_data, aes(x = Year, y = mean_Topic_richness, color = Region)) +
  geom_smooth(se = FALSE, size = 1.2) +   # Smoothed curve without confidence interval
  labs(
    title = "Smoothed Trends of Topic Richness by Region (Since 1990)",
    x = "Year",
    y = "Average Topic Richness",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
ggsave("Topic Richness by Region (Since 1990) .png", plot = p4, width = 8, height = 6, dpi = 600, bg = "white")

#  Prepare data for Sector Richness
Sector_plot_data <- panel_data_final %>%
  filter(Year >= 1990, !is.na(Sector_Richness), !is.na(Region)) %>%
  group_by(Region, Year) %>%
  summarise(mean_Sector_richness = mean(Sector_Richness, na.rm = TRUE), .groups = "drop")

#  Plot smoothed line chart
p5 <- ggplot(Sector_plot_data, aes(x = Year, y = mean_Sector_richness, color = Region)) +
  geom_smooth(se = FALSE, size = 1.2) +   # Smoothed curve without confidence interval
  labs(
    title = "Smoothed Trends of Sector Richness by Region (Since 1990)",
    x = "Year",
    y = "Average Sector Richness",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
ggsave("Sector Richness by Region (Since 1990) .png", plot = p5, width = 8, height = 6, dpi = 600, bg = "white")

saveRDS(panel_data_final, "data_processed/panel_data_final.rds")