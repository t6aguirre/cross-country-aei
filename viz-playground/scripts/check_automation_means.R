library(tidyverse)
aei <- read_csv('data/anthropic/aei_geo.csv', show_col_types = FALSE)
auto <- aei %>% filter(variable == 'automation_pct', geography == 'country') %>% select(geo_id, auto = value)

cat('Overall mean automation:', round(mean(auto$auto, na.rm=T), 1), '%\n')
cat('Overall median:', round(median(auto$auto, na.rm=T), 1), '%\n')
cat('Overall SD:', round(sd(auto$auto, na.rm=T), 1), '%\n\n')

# Regional
south_asia <- c('IND', 'BGD', 'PAK', 'LKA', 'NPL')
eastern_eu <- c('POL', 'ROU', 'UKR', 'BGR', 'HUN', 'CZE', 'SVK', 'HRV', 'SRB')

sa <- auto %>% filter(geo_id %in% south_asia)
ee <- auto %>% filter(geo_id %in% eastern_eu)
overall_mean <- mean(auto$auto, na.rm=T)

cat('South Asia mean:', round(mean(sa$auto), 1), '% (n=', nrow(sa), ')\n')
cat('Eastern Europe mean:', round(mean(ee$auto), 1), '% (n=', nrow(ee), ')\n')
cat('\nDifference from overall mean (', round(overall_mean, 1), '%):\n')
cat('  South Asia: +', round(mean(sa$auto) - overall_mean, 1), 'pp\n')
cat('  Eastern Europe:', round(mean(ee$auto) - overall_mean, 1), 'pp\n')

cat('\nSo the story is:\n')
cat('- South Asia is barely above average (+', round(mean(sa$auto) - overall_mean, 1), 'pp)\n')
cat('- Eastern Europe is well BELOW average (', round(mean(ee$auto) - overall_mean, 1), 'pp)\n')
cat('- The real finding is Eastern Europe augments more, not that South Asia automates more\n')
