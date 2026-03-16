library(tidyverse)
aei <- read_csv('aei_geo.csv', show_col_types = FALSE)

# Load trade openness
trade <- read_csv(list.files(pattern='API_NE.TRD.GNFS')[1], skip=4, show_col_types=FALSE) %>%
  select(`Country Code`, starts_with('20')) %>%
  pivot_longer(-`Country Code`, names_to='year', values_to='value') %>%
  filter(!is.na(value)) %>%
  group_by(`Country Code`) %>%
  slice_max(year, n=1) %>%
  ungroup() %>%
  select(geo_id=`Country Code`, trade_openness=value)

cat("Trade data rows:", nrow(trade), "\n")

# Get automation data
auto_aug <- aei %>%
  filter(variable == 'automation_pct', geography == 'country') %>%
  select(geo_id, automation_pct = value)

cat("Automation data rows:", nrow(auto_aug), "\n")

# Get GDP and usage count
gdp <- aei %>%
  filter(variable == 'gdp_per_working_age_capita', geography == 'country', facet == 'country') %>%
  select(geo_id, gdp = value) %>%
  mutate(log_gdp = log10(gdp))

usage <- aei %>%
  filter(variable == 'usage_count', geography == 'country', facet == 'country') %>%
  select(geo_id, usage_count = value)

# Merge
merged <- auto_aug %>%
  left_join(gdp, by='geo_id') %>%
  left_join(trade, by='geo_id') %>%
  left_join(usage, by='geo_id') %>%
  filter(!is.na(log_gdp), !is.na(automation_pct), usage_count > 500)

cat("\nMerged (before trade filter):", nrow(merged), "\n")
cat("With trade_openness:", sum(!is.na(merged$trade_openness)), "\n")

merged_clean <- merged %>% filter(!is.na(trade_openness))
cat("Final sample:", nrow(merged_clean), "\n")
cat("Trade openness range:", min(merged_clean$trade_openness), "-", max(merged_clean$trade_openness), "\n\n")

# Run regression
m <- lm(automation_pct ~ log_gdp + trade_openness, data = merged_clean)
print(summary(m))
