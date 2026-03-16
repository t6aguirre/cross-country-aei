library(tidyverse)

# Load data
aei <- read_csv("data/anthropic/aei_geo.csv", show_col_types = FALSE)

usage <- aei %>%
  filter(geography == "country", facet == "country", variable == "usage_per_capita") %>%
  select(geo_id, usage = value) %>%
  filter(!is.na(usage) & usage > 0) %>%
  mutate(log_usage = log10(usage))

auto_data <- aei %>%
  filter(variable == "automation_pct", geography == "country") %>%
  select(geo_id, automation_pct = value)

aug_data <- aei %>%
  filter(variable == "augmentation_pct", geography == "country") %>%
  select(geo_id, augmentation_pct = value)

gdp_pc <- aei %>%
  filter(geography == "country", facet == "country", variable == "gdp_per_working_age_capita") %>%
  select(geo_id, gdp_pc = value) %>%
  mutate(log_gdp = log10(gdp_pc))

# Define BPO/routine services exporter countries
# Based on AT Kearney Global Services Location Index, industry reports
bpo_countries <- c(
  # Major BPO hubs
  "IND",  # India - largest BPO exporter
  "PHL",  # Philippines - 2nd largest, call centers
  # Significant BPO exporters
  "KEN",  # Kenya - emerging African BPO hub
  "BGD",  # Bangladesh - growing IT services
  "VNM",  # Vietnam - IT outsourcing
  "MAR",  # Morocco - French-speaking BPO
  "EGY",  # Egypt - Arabic/English BPO
  "ZAF",  # South Africa - English BPO
  "POL",  # Poland - European nearshore
  "ROU",  # Romania - European nearshore
  # Nearshore Americas
  "CRI",  # Costa Rica - US nearshore
  "MEX",  # Mexico - US nearshore
  "COL",  # Colombia - Spanish BPO
  "GTM",  # Guatemala - emerging
  "JAM",  # Jamaica - English Caribbean
  "PAN"   # Panama - Americas hub
)

# Merge all
combined <- usage %>%
  left_join(auto_data, by = "geo_id") %>%
  left_join(aug_data, by = "geo_id") %>%
  left_join(gdp_pc, by = "geo_id") %>%
  mutate(is_bpo = geo_id %in% bpo_countries)

# Summary stats
cat("=== BPO Countries vs Others ===\n\n")

bpo_summary <- combined %>%
  group_by(is_bpo) %>%
  summarize(
    n = n(),
    mean_log_usage = mean(log_usage, na.rm = TRUE),
    mean_automation = mean(automation_pct, na.rm = TRUE),
    mean_augmentation = mean(augmentation_pct, na.rm = TRUE),
    mean_log_gdp = mean(log_gdp, na.rm = TRUE),
    .groups = "drop"
  )

print(bpo_summary)

# T-test for usage
cat("\n=== T-test: Log Usage ===\n")
t_usage <- t.test(log_usage ~ is_bpo, data = combined)
print(t_usage)

# T-test for automation (conditional on having data)
cat("\n=== T-test: Automation % ===\n")
auto_subset <- combined %>% filter(!is.na(automation_pct))
t_auto <- t.test(automation_pct ~ is_bpo, data = auto_subset)
print(t_auto)

# Regression controlling for GDP
cat("\n=== Regression: Usage controlling for GDP ===\n")
m1 <- lm(log_usage ~ is_bpo + log_gdp, data = combined)
print(summary(m1)$coefficients)

cat("\n=== Regression: Automation % controlling for GDP ===\n")
m2 <- lm(automation_pct ~ is_bpo + log_gdp, data = auto_subset)
print(summary(m2)$coefficients)

# Individual BPO country details
cat("\n=== BPO Country Details ===\n")
bpo_details <- combined %>%
  filter(is_bpo) %>%
  arrange(desc(log_usage)) %>%
  select(geo_id, log_usage, automation_pct, augmentation_pct, log_gdp)
print(bpo_details, n = 20)

# Compare to non-BPO countries at similar GDP levels
cat("\n=== Comparison: BPO vs non-BPO at similar GDP ===\n")
# Get median GDP of BPO countries
bpo_median_gdp <- median(combined$log_gdp[combined$is_bpo], na.rm = TRUE)
cat("Median log GDP of BPO countries:", round(bpo_median_gdp, 2), "\n")

# Compare BPO to non-BPO in similar GDP range
similar_gdp <- combined %>%
  filter(log_gdp > bpo_median_gdp - 0.5, log_gdp < bpo_median_gdp + 0.5)

cat("\nCountries in similar GDP range (", round(bpo_median_gdp - 0.5, 2), " to ",
    round(bpo_median_gdp + 0.5, 2), "):\n")
similar_summary <- similar_gdp %>%
  group_by(is_bpo) %>%
  summarize(
    n = n(),
    mean_log_usage = mean(log_usage, na.rm = TRUE),
    mean_automation = mean(automation_pct, na.rm = TRUE),
    .groups = "drop"
  )
print(similar_summary)

# Residual analysis - do BPO countries over/underperform?
cat("\n=== Residual Analysis ===\n")
combined$resid <- residuals(lm(log_usage ~ log_gdp, data = combined))

bpo_resid <- combined %>%
  filter(is_bpo) %>%
  select(geo_id, log_gdp, log_usage, resid, automation_pct) %>%
  arrange(desc(resid))

cat("BPO countries - residuals from GDP-only model:\n")
print(bpo_resid, n = 20)

cat("\nMean residual for BPO countries:", round(mean(bpo_resid$resid), 3), "\n")
cat("Mean residual for non-BPO countries:",
    round(mean(combined$resid[!combined$is_bpo], na.rm = TRUE), 3), "\n")
