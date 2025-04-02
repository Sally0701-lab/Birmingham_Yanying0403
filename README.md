# Birmingham_Yanying0403
# Pre-Interview Task  
**Lancet Countdown – Data Science Research Fellow Application**  
Author: Yanying Wang  

---

## Purpose

This project explores how health outcomes influence national climate policy engagement. It constructs panel indicators for policy participation and evaluates their association with various health indicators using two-way fixed-effects panel regressions and exploratory text analysis.

---

## Project Structure

### `data_cleaning.R`
- Merges multi-source data:
    - Climate Change Laws of the World (CCLW)
    - World Development Indicators (WDI)
    - PM2.5 air pollution data
    - Worldwide Governance Indicators (WGI)
- Builds a panel dataset at the country-year level.
- Prepares all variables for further analysis.

### `policy_richness_index_and_descriptive_table.R`
- Constructs:
    - Cumulative and rolling document counts.
    - Policy richness indicators (topic, sector, instrument dimensions).
- Generates a publication-style descriptive statistics table.

### `regression_analysis.R`
- Runs two-way fixed effects panel regressions.
- Analyzes the relationship between health outcomes and:
    - Climate policy document counts.
    - Policy diversity (topic, sector, and instrument richness).
- Produces:
    - Regression tables.
    - Coefficient plots.

### `scatter_plots.R`
- Produces:
    - Global and regional trend plots of life expectancy and policy engagement.
    - Scatter plots of policy richness vs. document counts.

### `text_analysis_family_summary_wordcloud.R`
- Conducts exploratory text analysis of policy "family summary" fields.
- Generates region-specific word clouds illustrating thematic patterns.

---

## Notes
- Health indicators are introduced separately in regressions to mitigate multicollinearity.
- All models adopt country and year fixed effects.
- Standard errors are clustered at the country level.
- Given more time and resources, I would extend this study by incorporating text analysis.
- a python code "CCLW Policy Documents PDF Downloader.py" is prepared to download the documents in CCLW.

---

## Author
Yanying Wang  
PhD Candidate, Peking University  
Application for Lancet Countdown – Data Science Research Fellow  
Email: yanying.wang@stu.pku.edu.cn
