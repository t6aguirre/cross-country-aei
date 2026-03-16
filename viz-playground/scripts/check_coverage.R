library(tidyverse)

setwd("C:/Users/tomas/viz-playground")

# Check AEI coverage
aei <- read_csv('data/anthropic/aei_geo.csv', show_col_types=FALSE)
aei_countries <- aei %>% filter(geography=='country', variable=='usage_per_capita') %>%
  filter(!is.na(value), value > 0) %>% nrow()
cat('AEI countries with usage data:', aei_countries, '\n')

# Check World Bank coverage for key variables
read_wb_count <- function(pattern) {
  file <- list.files("data/worldbank", pattern = pattern, full.names = TRUE)[1]
  if (is.na(file)) return(0)
  df <- read_csv(file, skip = 4, show_col_types = FALSE)
  df %>%
    select(`Country Code`, `2020`, `2021`, `2022`, `2023`) %>%
    pivot_longer(-`Country Code`, names_to = "year", values_to = "value") %>%
    filter(!is.na(value)) %>%
    distinct(`Country Code`) %>%
    nrow()
}

cat('\nWorld Bank coverage:\n')
cat('  GDP growth:', read_wb_count("API_NY.GDP.MKTP.KD.ZG"), '\n')
cat('  Internet:', read_wb_count("API_IT.NET.USER"), '\n')
cat('  Fertility:', read_wb_count("API_SP.DYN.TFRT"), '\n')
cat('  Regulatory quality:', read_wb_count("API_RQ.EST"), '\n')
cat('  R&D spending:', read_wb_count("API_GB.XPD.RSDV"), '\n')
cat('  Trade openness:', read_wb_count("API_NE.TRD.GNFS"), '\n')

# Survey coverage
survey <- read_csv('data/surveys/ai_usage_survey.csv', show_col_types=FALSE)
cat('\nBCG Survey countries:', nrow(survey), '\n')

perception <- read_csv('data/surveys/ai_perception.csv', show_col_types=FALSE)
cat('Ipsos Perception countries:', nrow(perception), '\n')

# Count hardcoded data
cat('\nHardcoded data in script:\n')
cat('  EF EPI: ~93 countries\n')
cat('  HDI: ~63 countries\n')
cat('  RTI: ~30 countries (OECD only)\n')

# Count EF EPI from script
ef_count <- 93  # Manually counted from tribble

cat('\n--- NOTES ON COVERAGE ---\n')
cat('AEI has 166 countries total but only', aei_countries, 'have usage_per_capita > 0\n')
cat('World Bank has good coverage (200+ for most indicators)\n')
cat('EF EPI covers 116 countries but we only hardcoded ~93\n')
cat('Surveys are limited: BCG=21, Ipsos=31 countries\n')
cat('RTI is OECD-only (~30 countries)\n')
