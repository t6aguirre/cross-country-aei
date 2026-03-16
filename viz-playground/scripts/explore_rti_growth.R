library(tidyverse)

# Load data
aei <- read_csv("data/anthropic/aei_geo.csv", show_col_types = FALSE)
usage <- aei %>%
  filter(geography == "country", facet == "country", variable == "usage_per_capita") %>%
  select(geo_id, usage = value) %>%
  filter(!is.na(usage) & usage > 0) %>%
  mutate(log_usage = log10(usage))

# RTI data
rti_raw <- read_csv("data/rti_country_specific_survey_predicted.csv", show_col_types = FALSE)
rti_data <- rti_raw %>%
  select(geo_id = Country_code, starts_with("rti_isco1d")) %>%
  pivot_longer(-geo_id, names_to = "occupation", values_to = "rti_val") %>%
  group_by(geo_id) %>%
  summarize(rti = mean(rti_val, na.rm = TRUE), .groups = "drop")

# GDP growth
gdp_growth_file <- list.files("data/worldbank", pattern = "API_NY.GDP.MKTP.KD.ZG", full.names = TRUE)[1]
gdp_growth_raw <- read_csv(gdp_growth_file, skip = 4, show_col_types = FALSE)
gdp_growth <- gdp_growth_raw %>%
  select(`Country Code`, `2023`) %>%
  rename(geo_id = `Country Code`, gdp_growth = `2023`) %>%
  filter(!is.na(gdp_growth))

# GDP level
gdp_pc <- aei %>%
  filter(geography == "country", facet == "country", variable == "gdp_per_working_age_capita") %>%
  select(geo_id, gdp_pc = value) %>%
  mutate(log_gdp = log10(gdp_pc))

# Merge all
combined <- usage %>%
  left_join(rti_data, by = "geo_id") %>%
  left_join(gdp_growth, by = "geo_id") %>%
  left_join(gdp_pc, by = "geo_id")

cat("=== RTI (Routine Task Intensity) ===\n")
rti_subset <- combined %>% filter(!is.na(rti))
cat("N with RTI:", nrow(rti_subset), "\n")
cat("Correlation RTI vs log_usage:", round(cor(rti_subset$rti, rti_subset$log_usage), 3), "\n")
cat("Correlation RTI vs log_usage (controlling for GDP):\n")
m <- lm(log_usage ~ rti + log_gdp, data = rti_subset)
print(summary(m)$coefficients)

cat("\n=== GDP Growth ===\n")
growth_subset <- combined %>% filter(!is.na(gdp_growth))
cat("N with GDP growth:", nrow(growth_subset), "\n")
cat("Correlation GDP growth vs log_usage:", round(cor(growth_subset$gdp_growth, growth_subset$log_usage), 3), "\n")
cat("Correlation GDP growth vs log_usage (controlling for GDP level):\n")
m2 <- lm(log_usage ~ gdp_growth + log_gdp, data = growth_subset)
print(summary(m2)$coefficients)

cat("\n=== Top/Bottom by RTI ===\n")
cat("High RTI countries (more routine jobs):\n")
rti_subset %>% arrange(desc(rti)) %>% head(10) %>% select(geo_id, rti, log_usage) %>% print()
cat("\nLow RTI countries (less routine jobs):\n")
rti_subset %>% arrange(rti) %>% head(10) %>% select(geo_id, rti, log_usage) %>% print()

cat("\n=== Top/Bottom by GDP Growth ===\n")
cat("Fastest growing:\n")
growth_subset %>% arrange(desc(gdp_growth)) %>% head(10) %>% select(geo_id, gdp_growth, log_usage) %>% print()
cat("\nSlowest growing:\n")
growth_subset %>% arrange(gdp_growth) %>% head(10) %>% select(geo_id, gdp_growth, log_usage) %>% print()

# Also check automation correlation with RTI
auto_data <- aei %>%
  filter(variable == "automation_pct", geography == "country") %>%
  select(geo_id, automation_pct = value)

auto_rti <- combined %>%
  left_join(auto_data, by = "geo_id") %>%
  filter(!is.na(automation_pct), !is.na(rti))

cat("\n=== RTI vs Automation Rate ===\n")
cat("N:", nrow(auto_rti), "\n")
cat("Correlation RTI vs automation_pct:", round(cor(auto_rti$rti, auto_rti$automation_pct), 3), "\n")
cat("Controlling for GDP:\n")
m3 <- lm(automation_pct ~ rti + log_gdp, data = auto_rti)
print(summary(m3)$coefficients)
