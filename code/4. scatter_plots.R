## ===============================================================
## Project: Pre-Interview Task for Lancet Countdown Data Science Research Fellow Position
## Author: Yanying Wang
## Purpose:  Visualize the relationship between document counts, sector richness, and health outcomes
## ================================================================


# =======================
# Load Required Libraries
# =======================

library(ggplot2)
library(dplyr)
library(RColorBrewer)

# =========================
# Step 1. Prepare Data (document count and Life_expectancy in 2022)
# =========================

summary_df_2022 <- panel_data_final %>%
  filter(Year == 2022) %>%
  select(Geographies, Region,Document_Count.x, Document_Count_Last3Years,Life_expectancy,
         Cumulative_Sector_Richness, Cumulative_Document_Count_ToDate) %>%
  mutate(
    Documentcount =  Cumulative_Document_Count_ToDate,
    Life_expectancy =Life_expectancy,
  ) %>%
  filter(!is.na(Region))



# =========================
# Step 2. Prepare Data (document count and document richness in 2023)
# =========================

summary_df_2023 <- panel_data_final %>%
  filter(Year == 2023) %>%
  select(Geographies, Region, Document_Count_Last3Years, Mortality_NCD,
         Cumulative_Sector_Richness, Cumulative_Document_Count_ToDate) %>%
  mutate(
    Documentcount = Cumulative_Document_Count_ToDate,
    richness = Cumulative_Sector_Richness,
  ) %>%
  filter(!is.na(Region))


# =========================
# Step 3. Plot 1 - Document Count vs. Sector Richness (2023)
# =========================
region_colors <- brewer.pal(n = max(3, length(unique(summary_df_2023$Region))), name = "Set3")
names(region_colors) <- unique(summary_df_2023$Region)

p1 <- 
  ggplot(summary_df_2023, aes(x = Documentcount, y = richness, color = Region)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(
    data = summary_df_2023,
    mapping = aes(x = Documentcount, y = richness, label = Geographies),
    size = 2, hjust = 0, vjust = 1.2, check_overlap = TRUE,
    inherit.aes = FALSE  
  ) +
  scale_color_manual(values = region_colors) +
  labs(
    title = " Document Count vs. Instrument richness ",
    x = " Document Count (-2023)",
    y = " Sector richness(-2023)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.6),
    legend.justification = c("right", "top"),
    legend.box.background = element_rect(color = "gray80", fill = "white"),
    legend.background = element_blank(),
    panel.grid = element_blank(),  
    axis.line = element_line(color = "black", size = 0.5) 
  )
ggsave("Document Count vs Sector Richness (2023).png", plot = p1, width = 8, height = 6, dpi = 600, bg = "white")

# =========================
# Step 4. Plot 2 - Document Count vs. Life_expectancy (2023)
# =========================
region_colors <- brewer.pal(n = max(3, length(unique(summary_df_2022$Region))), name = "Set3")
names(region_colors) <- unique(summary_df_2022$Region)

p2 <- ggplot(summary_df_2022, aes(x = Life_expectancy, y = Documentcount, color = Region)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(
    data = summary_df_2022,
    mapping = aes(x = Life_expectancy, y = Documentcount, label = Geographies),
    size = 2, hjust = 0, vjust = 1.2, check_overlap = TRUE,
    inherit.aes = FALSE  
  ) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "2022  Life_expectancy vs.Climate Policy Engagement ",
    x = "2022  Life_expectancy",
    y = "2022  Climate Policy Engagement",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.3, 0.95),
    legend.justification = c("right", "top"),
    legend.box.background = element_rect(color = "gray80", fill = "white"),
    legend.background = element_blank(),
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black", size = 0.5)  
  )
ggsave("Life_expectancy vs Climate Policy Engagement .png", plot = p2, width = 8, height = 6, dpi = 600, bg = "white")

