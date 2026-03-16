library(tidyverse)

aei <- read_csv('aei_geo.csv', show_col_types = FALSE)

ai_usage <- aei %>%
  filter(geography == 'country', facet == 'country',
         variable %in% c('usage_per_capita', 'gdp_per_working_age_capita')) %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(usage_per_capita) & usage_per_capita > 0,
         !is.na(gdp_per_working_age_capita)) %>%
  mutate(log_usage = log10(usage_per_capita), log_gdp = log10(gdp_per_working_age_capita))

ex_soviet <- c('ARM', 'AZE', 'BLR', 'EST', 'GEO', 'KAZ', 'KGZ', 'LVA', 'LTU',
               'MDA', 'RUS', 'TJK', 'TKM', 'UKR', 'UZB')
anglophone <- c('USA', 'GBR', 'CAN', 'AUS', 'NZL', 'IRL', 'ZAF', 'SGP', 'IND',
                'PHL', 'NGA', 'KEN', 'GHA', 'ZWE', 'JAM', 'TTO', 'MLT', 'MYS')
small_open <- c('SGP', 'HKG', 'LUX', 'IRL', 'CHE', 'BEL', 'NLD', 'ISR', 'DNK',
                'NOR', 'AUT', 'SWE', 'FIN', 'NZL', 'EST', 'LVA', 'LTU', 'SVN',
                'CYP', 'MLT', 'ISL')

ai_usage <- ai_usage %>%
  mutate(
    is_anglophone = as.integer(geo_id %in% anglophone),
    is_ex_soviet = as.integer(geo_id %in% ex_soviet),
    is_small_open = as.integer(geo_id %in% small_open)
  )

m1 <- lm(log_usage ~ log_gdp, data = ai_usage)
ai_usage$resid_gdp <- residuals(m1)

cat("=== RESIDUAL REGRESSION ===\n")
resid_reg <- lm(resid_gdp ~ is_anglophone + is_ex_soviet + is_small_open, data = ai_usage)
print(summary(resid_reg))

cat("\n=== EX-SOVIET COUNTRIES ===\n")
ai_usage %>%
  filter(is_ex_soviet == 1) %>%
  select(geo_name, log_gdp, log_usage, resid_gdp) %>%
  arrange(desc(resid_gdp)) %>%
  print(n = 20)

cat("\n=== MEAN RESIDUALS BY GROUP ===\n")
cat("Anglophone:", round(mean(ai_usage$resid_gdp[ai_usage$is_anglophone == 1]), 3), "\n")
cat("Ex-Soviet:", round(mean(ai_usage$resid_gdp[ai_usage$is_ex_soviet == 1]), 3), "\n")
cat("Small open:", round(mean(ai_usage$resid_gdp[ai_usage$is_small_open == 1]), 3), "\n")
cat("Other:", round(mean(ai_usage$resid_gdp[ai_usage$is_anglophone == 0 &
                                             ai_usage$is_ex_soviet == 0 &
                                             ai_usage$is_small_open == 0]), 3), "\n")
