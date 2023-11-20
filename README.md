# DataEnrichment_Qualtrics2Tableau

#### DATA ENRICHMENT #####

# 1. Data Cleaning
# --> eliminating discontinued metrics 
# ---> Changing character to numeric data types
# 2. Aggregating data 
# ---> creating aggregated metrics of the retained metrics
# ---> retained excluded metrics after commenting out for posterity
# ----> Overall index as an average of the people, planet and governance indices.
# 3. Merging with automated financial data

Cleaning data in R
- Variable names are cleaned
- Variable types are defined (conversion to string, integer etc)
- 
Merging data in R
- Qualtrics survey data and automated data are merged to create a single data file
- 
Aggregating data in R
- Sub-metrics are merged to create metrics/indices 
E.g., TCFD metric combines 8 sub-metrics related to TCFD
Metrics are aggregated to create pillar level scores and a single overall score



