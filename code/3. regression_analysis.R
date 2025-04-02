## ===============================================================
## Project: Pre-Interview Task for Lancet Countdown Data Science Research Fellow Position
## Author: Yanying Wang
## Purpose: (1) Conduct fixed-effects panel regressions on policy participation indicators
##          (2) Estimate the effects of health outcomes on policy participation (richness, document counts)
##          (3) Output regression tables and coefficient plots
## ===============================================================

# =======================
# Load Required Libraries
# =======================
library(fixest)
library(clubSandwich)
library(modelsummary)
library(officer)
library(flextable)
library(broom)
library(ggplot2)
library(dplyr)

# =======================
# Step 1. Define Dependent and Independent Variables
# =======================

# Policy participation indicators (dependent variables)
y_list <- c(
  "Cumulative_Document_Count_ToDate",
  "Document_Count_Last3Years",
  "Recent_Health_Count",
  "Recent_Sector_Richness",
  "Recent_Instrument_Richness",
  "Recent_Topic_Richness"
)

# Variables that need to be lagged
lagged_y <- c(
  "Recent_Health_Count",
  "Recent_Sector_Richness",
  "Recent_Instrument_Richness",
  "Recent_Topic_Richness"
)

# Independent variables (health outcomes + controls)
x_controls <- c(
  "Life_expectancy",
  "PM25", "GDP_per_capita", "GDP_growth_rate", "ge_estimate", "rq_estimate"
)

# =======================
# Step 2. Run Fixed Effects Regressions (Mixed Lag Specification)
# =======================

model_list <- list()

for (y in y_list) {
  
  # Apply lag(3) only if y in lagged_y
  y_term <- ifelse(y %in% lagged_y, paste0("l(", y, ",3)"), y)
  
  formula <- as.formula(
    paste0(y_term, " ~ ", paste(x_controls, collapse = " + "), " | Geographies + Year")
  )
  
  model_list[[paste0(ifelse(y %in% lagged_y, "lag3_", ""), y)]] <- feols(
    formula,
    data = panel_data_final,
    panel.id = ~ Geographies + Year
  )
  
  cat("✅ Finished model for:", y, "\n")
}

# =======================
# Step 3. Export Regression Table
# =======================

table_flex <- msummary(
  model_list,
  stars = TRUE,
  statistic = "({std.error})",   # Clustered standard errors in parentheses
  vcov = ~Geographies,            # Cluster by Geographies
  output = "flextable"
)

# Save regression table
save_as_docx(
  "Regression Results" = table_flex,
  path = "output/health_to_policy_regression.docx"
)

# =======================
# Step 4. Coefficient Plot for Selected Health Effects
# =======================

# Extract all coefficients
coef_df_all <- map_dfr(
  names(model_list),
  ~ tidy(model_list[[.x]], conf.int = TRUE) %>% mutate(model = .x),
  .id = NULL
) %>%
  mutate(Dependent_Variable = model)

# Select key health indicators to plot
health_vars_to_plot <- c(
"Life_expectancy"
)

# Plot each health indicator's effect on all policy variables
for (var in health_vars_to_plot) {
  
  coef_df_var <- coef_df_all %>%
    filter(term == var)
  
  p <- ggplot(coef_df_var, aes(x = estimate, y = Dependent_Variable)) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Effect of", var, "on Policy Participation Indicators"),
      x = "Coefficient (95% CI)", y = "Policy Participation Indicator"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(filename = paste0("output/figure_", var, ".png"), plot = p, width = 8, height = 6, dpi = 600, bg = "white")
}




# =======================
# Step 5. Effect of Life Expectancy on Document Type Indicators
# =======================

# Define additional dependent variables
richness_y_list <- c(
  "Rolling3_Law", "Rolling3_Plan", "Rolling3_Strategy", "Rolling3_Decree", "Rolling3_Policy",
  "Rolling3_Submission_To_The_Global_Stocktake", "Rolling3_Act",
  "Rolling3_Nationally_Determined_Contribution", "Rolling3_Regulation", "Rolling3_National_Communication"
)

# Store models
richness_model_list <- list()

# Regression loop
for (y in richness_y_list) {
  
  formula_rich <- as.formula(
    paste0(y, " ~ Life_expectancy + PM25 + GDP_per_capita + GDP_growth_rate + ge_estimate + rq_estimate | Geographies + Year")
  )
  
  richness_model_list[[y]] <- feols(
    formula_rich,
    data = panel_data_final,
    panel.id = ~ Geographies + Year
  )
  
  cat("✅ Finished rodel for:", y, "\n")
}

# =======================
# Step 6. Export Type Regression Table
# =======================

richness_table <- msummary(
  richness_model_list,
  stars = TRUE,
  statistic = "({std.error})",  
  vcov = ~Geographies,           
  output = "flextable"
)

save_as_docx(
  "Richness Regression Results" = richness_table,
  path = "output/health_to_policy_type_regression.docx"
)

# =======================
# Step 7. Plot Effect of Life Expectancy on type Indicators
# =======================

# Extract coefficients
coef_df_richness <- map_dfr(
  names(richness_model_list),
  ~ tidy(richness_model_list[[.x]], conf.int = TRUE) %>% mutate(model = .x),
  .id = NULL
) %>%
  filter(term == "Life_expectancy")

# Plot
p_richness <- ggplot(coef_df_richness, aes(x = estimate, y = model)) +
  geom_point(size = 2.5, color = "darkgreen") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "darkgreen") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Life Expectancy on different document types",
    x = "Coefficient (95% CI)",
    y = "Richness Indicator"
  ) +
  theme_minimal()

ggsave("output/figure_lifeexpectancy_on_different_document_types.png", 
       plot = p_richness, width = 8, height = 6, dpi = 600, bg = "white")