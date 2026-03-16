# Check if Shapley decomposition would add value
# This decomposes R² into contributions from each predictor

library(tidyverse)
library(relaimpo)  # For relative importance / Shapley values

setwd("C:/Users/tomas/viz-playground")

# Load data (simplified version of main script)
aei <- read_csv("data/anthropic/aei_geo.csv", show_col_types = FALSE)

read_wb <- function(pattern, var_name) {
  file <- list.files("data/worldbank", pattern = pattern, full.names = TRUE)[1]
  if (is.na(file)) return(NULL)
  df <- read_csv(file, skip = 4, show_col_types = FALSE)
  df %>%
    select(`Country Code`, starts_with("20")) %>%
    pivot_longer(-`Country Code`, names_to = "year", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(`Country Code`) %>%
    slice_max(year, n = 1) %>%
    ungroup() %>%
    select(geo_id = `Country Code`, !!var_name := value)
}

ai_usage <- aei %>%
  filter(geography == "country", facet == "country",
         variable %in% c("usage_per_capita", "gdp_per_working_age_capita")) %>%
  select(geo_id, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(usage_per_capita) & usage_per_capita > 0) %>%
  mutate(log_usage = log10(usage_per_capita),
         log_gdp = log10(gdp_per_working_age_capita))

# Add key predictors
vars <- list(
  read_wb("API_RQ.EST", "regulatory_quality"),
  read_wb("API_SP.DYN.TFRT", "fertility"),
  read_wb("API_SP.POP.65UP", "aging"),
  read_wb("API_GB.XPD.RSDV", "rd_spending")
)

for (v in vars) {
  if (!is.null(v)) ai_usage <- left_join(ai_usage, v, by = "geo_id")
}

model_data <- ai_usage %>%
  filter(complete.cases(log_usage, log_gdp, regulatory_quality, fertility, aging, rd_spending))

cat("Sample size for Shapley decomposition:", nrow(model_data), "\n\n")

# Fit model
m <- lm(log_usage ~ log_gdp + regulatory_quality + fertility + aging + rd_spending, data = model_data)

cat("OLS Summary:\n")
print(summary(m))

# Shapley value decomposition (LMG method)
cat("\n\n=== SHAPLEY VALUE DECOMPOSITION (R² attribution) ===\n")
cat("This shows how much each predictor contributes to explaining variance\n\n")

relimp <- calc.relimp(m, type = "lmg", rela = TRUE)
print(relimp)

cat("\n\nInterpretation:\n")
cat("- 'lmg' = Lindeman-Merenda-Gold method (equivalent to Shapley values)\n")
cat("- Values sum to 100% of explained variance\n")
cat("- Accounts for correlations between predictors\n")
