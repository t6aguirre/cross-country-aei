library(tidyverse)

# Load AEI data
aei <- read_csv("data/anthropic/aei_geo.csv", show_col_types = FALSE)

usage <- aei %>%
  filter(geography == "country", facet == "country", variable == "usage_per_capita") %>%
  select(geo_id, usage = value) %>%
  filter(!is.na(usage) & usage > 0) %>%
  mutate(log_usage = log10(usage))

auto_data <- aei %>%
  filter(variable == "automation_pct", geography == "country") %>%
  select(geo_id, automation_pct = value)

gdp_pc <- aei %>%
  filter(geography == "country", facet == "country", variable == "gdp_per_working_age_capita") %>%
  select(geo_id, gdp_pc = value) %>%
  mutate(log_gdp = log10(gdp_pc))

# Load ICT services exports data
ict_raw <- read_csv("data/worldbank/API_BX.GSR.CCIS.ZS_DS2_en_csv_v2_116950.csv",
                    skip = 4, show_col_types = FALSE)

# Get most recent value for each country
ict_data <- ict_raw %>%
  select(`Country Code`, starts_with("20")) %>%
  pivot_longer(-`Country Code`, names_to = "year", values_to = "ict_exports") %>%
  filter(!is.na(ict_exports)) %>%
  group_by(`Country Code`) %>%
  slice_max(year, n = 1) %>%
  ungroup() %>%
  select(geo_id = `Country Code`, ict_exports, ict_year = year)

# Define regions
southeast_asia <- c("VNM", "THA", "PHL", "IDN", "MYS", "SGP", "MMR", "KHM", "LAO", "BRN")
south_asia <- c("IND", "BGD", "PAK", "LKA", "NPL")
eastern_europe <- c("POL", "ROU", "UKR", "BGR", "HUN", "CZE", "SVK", "HRV", "SRB")
latam <- c("MEX", "BRA", "ARG", "COL", "CHL", "PER", "CRI", "PAN", "GTM", "JAM")
africa <- c("ZAF", "KEN", "NGA", "EGY", "MAR", "GHA", "TZA", "UGA", "RWA", "ETH")

# Merge all
combined <- usage %>%
  left_join(auto_data, by = "geo_id") %>%
  left_join(gdp_pc, by = "geo_id") %>%
  left_join(ict_data, by = "geo_id") %>%
  mutate(
    region = case_when(
      geo_id %in% southeast_asia ~ "Southeast Asia",
      geo_id %in% south_asia ~ "South Asia",
      geo_id %in% eastern_europe ~ "Eastern Europe",
      geo_id %in% latam ~ "Latin America",
      geo_id %in% africa ~ "Africa",
      TRUE ~ "Other"
    )
  )

# Summary
cat("=== ICT Service Exports Data ===\n")
cat("Countries with ICT exports data:", sum(!is.na(combined$ict_exports)), "\n")
cat("Range:", round(min(combined$ict_exports, na.rm=T), 1), "to",
    round(max(combined$ict_exports, na.rm=T), 1), "%\n\n")

# Top ICT exporters
cat("Top 15 ICT service exporters (% of service exports):\n")
combined %>%
  filter(!is.na(ict_exports)) %>%
  arrange(desc(ict_exports)) %>%
  head(15) %>%
  select(geo_id, ict_exports, log_usage, automation_pct, log_gdp) %>%
  print()

# Correlations
cat("\n=== Correlations with ICT exports ===\n")
ict_subset <- combined %>% filter(!is.na(ict_exports), !is.na(automation_pct))
cat("N:", nrow(ict_subset), "\n")
cat("ICT exports vs log_usage:", round(cor(ict_subset$ict_exports, ict_subset$log_usage), 3), "\n")
cat("ICT exports vs automation_pct:", round(cor(ict_subset$ict_exports, ict_subset$automation_pct), 3), "\n")
cat("ICT exports vs log_gdp:", round(cor(ict_subset$ict_exports, ict_subset$log_gdp), 3), "\n")

# Regressions
cat("\n=== Regression: Usage ~ ICT exports + GDP ===\n")
m1 <- lm(log_usage ~ ict_exports + log_gdp, data = ict_subset)
print(summary(m1)$coefficients)

cat("\n=== Regression: Automation ~ ICT exports + GDP ===\n")
m2 <- lm(automation_pct ~ ict_exports + log_gdp, data = ict_subset)
print(summary(m2)$coefficients)

# Regional analysis
cat("\n=== Regional Patterns ===\n")
regional_summary <- combined %>%
  filter(!is.na(ict_exports), !is.na(automation_pct)) %>%
  group_by(region) %>%
  summarize(
    n = n(),
    mean_ict = mean(ict_exports, na.rm = TRUE),
    mean_automation = mean(automation_pct, na.rm = TRUE),
    mean_log_usage = mean(log_usage, na.rm = TRUE),
    mean_log_gdp = mean(log_gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ict))

print(regional_summary)

# Southeast Asia detail
cat("\n=== Southeast Asia Detail ===\n")
sea_detail <- combined %>%
  filter(geo_id %in% southeast_asia) %>%
  arrange(desc(log_usage)) %>%
  select(geo_id, ict_exports, log_usage, automation_pct, log_gdp)
print(sea_detail)

# South Asia detail
cat("\n=== South Asia Detail ===\n")
sa_detail <- combined %>%
  filter(geo_id %in% south_asia) %>%
  arrange(desc(log_usage)) %>%
  select(geo_id, ict_exports, log_usage, automation_pct, log_gdp)
print(sa_detail)

# Eastern Europe detail
cat("\n=== Eastern Europe Detail ===\n")
ee_detail <- combined %>%
  filter(geo_id %in% eastern_europe) %>%
  arrange(desc(log_usage)) %>%
  select(geo_id, ict_exports, log_usage, automation_pct, log_gdp)
print(ee_detail)

# Test regional interaction
cat("\n=== Regional Interaction: Does ICT effect vary by region? ===\n")
# Focus on key regions
key_regions <- combined %>%
  filter(region %in% c("Southeast Asia", "South Asia", "Eastern Europe", "Latin America")) %>%
  filter(!is.na(ict_exports), !is.na(automation_pct))

if(nrow(key_regions) > 20) {
  m3 <- lm(automation_pct ~ ict_exports * region + log_gdp, data = key_regions)
  cat("Automation ~ ICT * region + GDP:\n")
  print(summary(m3)$coefficients)
}

# High ICT exporters vs Low (median split)
cat("\n=== High vs Low ICT Exporters ===\n")
ict_median <- median(ict_subset$ict_exports)
cat("Median ICT exports:", round(ict_median, 1), "%\n")

ict_subset <- ict_subset %>%
  mutate(high_ict = ict_exports > ict_median)

high_low <- ict_subset %>%
  group_by(high_ict) %>%
  summarize(
    n = n(),
    mean_automation = mean(automation_pct),
    mean_log_usage = mean(log_usage),
    mean_log_gdp = mean(log_gdp),
    .groups = "drop"
  )
print(high_low)

# T-test controlling for GDP (via regression)
cat("\nHigh ICT coefficient on automation (controlling for GDP):\n")
m4 <- lm(automation_pct ~ high_ict + log_gdp, data = ict_subset)
print(summary(m4)$coefficients)
