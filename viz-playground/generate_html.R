# Generate HTML report directly

library(tidyverse)
library(glmnet)
library(broom)
library(showtext)
library(relaimpo)  # For Shapley decomposition

# Fix MASS::select masking dplyr::select
select <- dplyr::select

font_add_google("Libre Baskerville", "cm")
showtext_auto()
showtext_opts(dpi = 300)

theme_academic <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "cm") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2), margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.85), color = "#555555", margin = margin(b = 12)),
      plot.caption = element_text(size = rel(0.65), color = "#888888", hjust = 1, margin = margin(t = 12)),
      axis.title = element_text(size = rel(0.9)),
      axis.text = element_text(size = rel(0.8)),
      panel.grid.major = element_line(color = "#eeeeee", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 25, 15, 20)
    )
}

# Helper function to make nice HTML table
html_table <- function(df, caption = "", highlight_col = NULL) {
  header <- paste0("<th>", names(df), "</th>", collapse = "")
  rows <- apply(df, 1, function(row) {
    cells <- sapply(seq_along(row), function(i) {
      val <- row[i]
      # Highlight significant results
      if (!is.null(highlight_col) && i == highlight_col && grepl("\\*", val)) {
        paste0("<td style='font-weight:600;'>", val, "</td>")
      } else {
        paste0("<td>", val, "</td>")
      }
    })
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })
  paste0(
    "<table class='reg-table'>",
    if (caption != "") paste0("<caption>", caption, "</caption>") else "",
    "<thead><tr>", header, "</tr></thead>",
    "<tbody>", paste(rows, collapse = "\n"), "</tbody>",
    "</table>"
  )
}

# Load data - paths relative to project root
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

aei <- read_csv("data/anthropic/aei_geo.csv", show_col_types = FALSE)
ai_usage <- aei %>%
  filter(geography == "country", facet == "country",
         variable %in% c("usage_per_capita", "gdp_per_working_age_capita")) %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(usage_per_capita) & usage_per_capita > 0)

vars_list <- list(
  internet = read_wb("API_IT.NET.USER", "internet"),
  rule_of_law = read_wb("API_RL.EST", "rule_of_law"),
  corruption = read_wb("API_CC.EST", "corruption"),
  gov_effect = read_wb("API_GE.EST", "gov_effectiveness"),
  regulatory = read_wb("API_RQ.EST", "regulatory_quality"),
  tertiary = read_wb("API_SE.TER.ENRR", "tertiary_edu"),
  rd = read_wb("API_GB.XPD.RSDV", "rd_spending"),
  fertility = read_wb("API_SP.DYN.TFRT", "fertility"),
  aging = read_wb("API_SP.POP.65UP", "aging"),
  gini = read_wb("API_SI.POV.GINI", "gini"),
  trade = read_wb("API_NE.TRD.GNFS", "trade_openness"),
  hightech = read_wb("API_TX.VAL.TECH", "hightech_exports"),
  unemployment = read_wb("API_SL.UEM.TOTL", "unemployment"),
  urban = read_wb("API_SP.URB.TOTL.IN.ZS", "urban"),
  life_exp = read_wb("API_SP.DYN.LE00", "life_expectancy"),
  mobile = read_wb("API_IT.CEL.SETS.P2", "mobile"),
  patents = read_wb("API_IP.PAT.RESD", "patents"),
  female_labor = read_wb("API_SL.TLF.CACT.FE", "female_labor"),
  gdp_growth = read_wb("API_NY.GDP.MKTP.KD.ZG", "gdp_growth")
)

# Read GDP growth for volatility and acceleration calculation
gdp_growth_file <- list.files("data/worldbank", pattern = "API_NY.GDP.MKTP.KD.ZG", full.names = TRUE)[1]
if (!is.na(gdp_growth_file)) {
  gdp_growth_raw <- read_csv(gdp_growth_file, skip = 4, show_col_types = FALSE)

  # Volatility: 5-year std dev
  gdp_volatility <- gdp_growth_raw %>%
    select(`Country Code`, `2019`, `2020`, `2021`, `2022`, `2023`) %>%
    pivot_longer(-`Country Code`, names_to = "year", values_to = "growth") %>%
    group_by(`Country Code`) %>%
    summarize(gdp_volatility = sd(growth, na.rm = TRUE), .groups = "drop") %>%
    rename(geo_id = `Country Code`) %>%
    filter(!is.na(gdp_volatility))

  # Acceleration: change in growth rate (recent - earlier period)
  # Compare 2021-2023 avg to 2018-2020 avg (captures post-COVID trajectory)
  gdp_accel <- gdp_growth_raw %>%
    select(`Country Code`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`) %>%
    pivot_longer(-`Country Code`, names_to = "year", values_to = "growth") %>%
    mutate(period = ifelse(year %in% c("2018", "2019", "2020"), "early", "late")) %>%
    group_by(`Country Code`, period) %>%
    summarize(avg_growth = mean(growth, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = avg_growth) %>%
    mutate(gdp_accel = late - early) %>%  # Positive = accelerating growth
    select(geo_id = `Country Code`, gdp_accel) %>%
    filter(!is.na(gdp_accel))
}

# Routine Task Intensity (RTI) index - Lewandowski et al. (2022) World Bank Economic Review
# 102 countries, survey-based + predicted. Higher = more routine jobs.
# Average across 1-digit ISCO occupations for country-level measure
rti_raw <- read_csv("data/rti_country_specific_survey_predicted.csv", show_col_types = FALSE)
rti_data <- rti_raw %>%
  select(geo_id = Country_code, starts_with("rti_isco1d")) %>%
  pivot_longer(-geo_id, names_to = "occupation", values_to = "rti_val") %>%
  group_by(geo_id) %>%
  summarize(rti = mean(rti_val, na.rm = TRUE), .groups = "drop")

# EF English Proficiency Index 2024 (L1+L2 ability, not just native speakers)
# Source: EF EPI 2024, Ethnologue methodology caveats apply
ef_epi <- tribble(
  ~geo_id, ~english_prof,
  "NLD", 636, "NOR", 610, "SGP", 609, "SWE", 608, "HRV", 607,
  "PRT", 605, "DNK", 603, "GRC", 602, "AUT", 600, "DEU", 598,
  "ZAF", 594, "ROU", 593, "BEL", 592, "FIN", 590, "POL", 588,
  "BGR", 586, "HUN", 585, "SVK", 584, "KEN", 581, "EST", 578,
  "LUX", 576, "PHL", 570, "LTU", 569, "SRB", 568, "CZE", 567,
  "MYS", 566, "ARG", 562, "CYP", 558, "NGA", 557, "CHE", 550,
  "HKG", 549, "HND", 545, "GEO", 543, "BLR", 539, "ESP", 538,
  "URY", 538, "ARM", 537, "MDA", 536, "UKR", 535, "GHA", 534,
  "CRI", 534, "ALB", 533, "RUS", 532, "PRY", 531, "ITA", 528,
  "CHL", 525, "BOL", 525, "FRA", 524, "KOR", 523, "ISR", 522,
  "NPL", 520, "CUB", 520, "PER", 519, "UGA", 518, "SLV", 513,
  "VEN", 510, "GTM", 507, "DOM", 503, "NIC", 502, "BGD", 500,
  "IRN", 499, "ETH", 498, "VNM", 498, "TUR", 497, "MOZ", 496,
  "TUN", 496, "PAK", 493, "LBN", 492, "IND", 490, "ARE", 489,
  "PAN", 488, "TZA", 487, "LKA", 486, "COL", 485, "QAT", 480,
  "MAR", 479, "SYR", 473, "DZA", 471, "IDN", 468, "BRA", 466,
  "EGY", 465, "ECU", 465, "MNG", 464, "MDG", 463, "AZE", 462,
  "MEX", 459, "KGZ", 457, "KWT", 456, "CHN", 455, "JPN", 454,
  "MMR", 449, "AFG", 447, "MWI", 447, "CMR", 445, "UZB", 439,
  "SDN", 432, "HTI", 432, "JOR", 431, "SEN", 429, "KAZ", 427,
  "OMN", 421, "SAU", 417, "THA", 415, "IRQ", 414, "BEN", 413,
  "TJK", 412, "AGO", 409, "KHM", 408, "LBY", 405, "RWA", 401,
  "CIV", 399, "SOM", 399, "YEM", 394
)

# HDI data (UNDP 2024) - hardcoded for key countries
hdi_data <- tribble(
  ~geo_id, ~hdi,
  "NOR", 0.966, "CHE", 0.967, "ISL", 0.959, "HKG", 0.956, "AUS", 0.951,
  "DNK", 0.952, "SWE", 0.947, "IRL", 0.950, "DEU", 0.942, "NLD", 0.946,
  "FIN", 0.942, "SGP", 0.949, "GBR", 0.940, "BEL", 0.942, "NZL", 0.939,
  "CAN", 0.935, "USA", 0.927, "AUT", 0.926, "ISR", 0.915, "JPN", 0.920,
  "KOR", 0.929, "SVN", 0.926, "LUX", 0.930, "ESP", 0.911, "FRA", 0.910,
  "CZE", 0.895, "ITA", 0.906, "MLT", 0.918, "EST", 0.899, "CYP", 0.907,
  "GRC", 0.893, "POL", 0.881, "LTU", 0.879, "ARE", 0.937, "SAU", 0.875,
  "PRT", 0.874, "LVA", 0.879, "HRV", 0.878, "CHL", 0.860, "ARG", 0.849,
  "TUR", 0.855, "URY", 0.830, "ROU", 0.827, "MYS", 0.807, "RUS", 0.822,
  "BLR", 0.808, "BGR", 0.799, "SRB", 0.805, "GEO", 0.814, "MUS", 0.796,
  "THA", 0.803, "ARM", 0.786, "ALB", 0.796, "CHN", 0.788, "UKR", 0.734,
  "MEX", 0.781, "BRA", 0.760, "COL", 0.758, "PER", 0.762, "ECU", 0.765,
  "IRN", 0.780, "PHL", 0.710, "ZAF", 0.713, "EGY", 0.728, "IDN", 0.713,
  "VNM", 0.726, "IND", 0.644, "MAR", 0.698, "BGD", 0.670, "PAK", 0.540,
  "KEN", 0.601, "NGA", 0.539, "NPL", 0.601, "LKA", 0.782, "MDA", 0.767
)

# Ex-Soviet states
ex_soviet <- c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU",
               "MDA", "RUS", "TJK", "TKM", "UKR", "UZB")

# Anglophone countries (English as primary/official language)
anglophone <- c("USA", "GBR", "CAN", "AUS", "NZL", "IRL", "ZAF", "SGP", "IND",
                "PHL", "NGA", "KEN", "GHA", "ZWE", "JAM", "TTO", "MLT", "MYS")

# Small open economies (population < 10M, trade/GDP > 100%)
small_open <- c("SGP", "HKG", "LUX", "IRL", "CHE", "BEL", "NLD", "ISR", "DNK",
                "NOR", "AUT", "SWE", "FIN", "NZL", "EST", "LVA", "LTU", "SVN",
                "CYP", "MLT", "ISL")

full_data <- ai_usage
for (v in vars_list) {
  if (!is.null(v)) full_data <- left_join(full_data, v, by = "geo_id")
}

# Merge HDI, RTI, EF EPI, volatility, acceleration and create boolean indicators
full_data <- full_data %>%
  left_join(hdi_data, by = "geo_id") %>%
  left_join(rti_data, by = "geo_id") %>%
  left_join(ef_epi, by = "geo_id") %>%
  left_join(gdp_volatility, by = "geo_id") %>%
  left_join(gdp_accel, by = "geo_id") %>%
  mutate(
    log_usage = log10(usage_per_capita),
    log_gdp = log10(gdp_per_working_age_capita),
    is_anglophone = as.integer(geo_id %in% anglophone),
    is_ex_soviet = as.integer(geo_id %in% ex_soviet),
    is_small_open = as.integer(geo_id %in% small_open)
  )

# Only include predictors that exist in full_data
available_preds <- c("log_gdp", "internet", "rule_of_law", "corruption",
                     "gov_effectiveness", "regulatory_quality", "tertiary_edu",
                     "rd_spending", "fertility", "aging", "gini",
                     "trade_openness", "hightech_exports", "unemployment",
                     "urban", "life_expectancy", "mobile", "patents", "female_labor",
                     "hdi", "gdp_growth", "gdp_volatility", "gdp_accel", "rti")
predictor_names <- intersect(available_preds, names(full_data))

model_data <- full_data %>%
  select(geo_id, geo_name, log_usage, is_anglophone, is_ex_soviet, is_small_open, all_of(predictor_names), english_prof) %>%
  filter(complete.cases(across(c(log_usage, log_gdp, internet, regulatory_quality, fertility, aging, rd_spending))))

# Models - build up intuitively: economic baseline → infrastructure → governance → demographics → human capital
# Column (1): GDP alone - baseline economic capacity
m1 <- lm(log_usage ~ log_gdp, data = model_data)
# Column (2): Add internet - prerequisite infrastructure (need internet to use Claude)
m2 <- lm(log_usage ~ log_gdp + internet, data = model_data)
# Column (3): Add governance - regulatory quality enables technology adoption
m3 <- lm(log_usage ~ log_gdp + internet + regulatory_quality, data = model_data)
# Column (4): Add demographics - fertility as labor scarcity proxy
m4 <- lm(log_usage ~ log_gdp + internet + regulatory_quality + fertility, data = model_data)
# Column (5): Add aging - complements fertility story
m5 <- lm(log_usage ~ log_gdp + internet + regulatory_quality + fertility + aging, data = model_data)
# Column (6): Add R&D - innovation capacity
m6 <- lm(log_usage ~ log_gdp + internet + regulatory_quality + fertility + aging + rd_spending, data = model_data)
# Columns (7)-(8): English proficiency on subset with EF EPI data
model_data_eng <- model_data %>% filter(!is.na(english_prof))
m7 <- lm(log_usage ~ log_gdp + internet + regulatory_quality + fertility + aging + rd_spending + english_prof, data = model_data_eng)
# Column (8): Full model with mobile subscriptions
m8 <- lm(log_usage ~ log_gdp + internet + regulatory_quality + fertility + aging + rd_spending + english_prof + mobile, data = model_data_eng)

# AER-style coefficient table
make_aer_row <- function(varname, models, digits = 3) {
  coefs <- sapply(models, function(m) {
    if (varname %in% names(coef(m))) {
      s <- summary(m)
      est <- coef(m)[varname]
      se <- s$coefficients[varname, "Std. Error"]
      pval <- s$coefficients[varname, "Pr(>|t|)"]
      stars <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
      paste0(round(est, digits), stars)
    } else { "" }
  })
  ses <- sapply(models, function(m) {
    if (varname %in% names(coef(m))) {
      s <- summary(m)
      se <- s$coefficients[varname, "Std. Error"]
      paste0("(", round(se, digits), ")")
    } else { "" }
  })
  list(coefs = coefs, ses = ses)
}

models <- list(m1, m2, m3, m4, m5, m6, m7, m8)
vars_to_show <- c("log_gdp", "internet", "regulatory_quality", "fertility", "aging", "rd_spending", "english_prof", "mobile")
var_labels <- c("Log GDP per capita", "Internet users (%)", "Regulatory quality", "Fertility rate", "Population 65+", "R&D spending (% GDP)", "English proficiency (EF EPI)", "Mobile subscriptions")

# Build AER table
aer_rows <- list()
for (i in seq_along(vars_to_show)) {
  row <- make_aer_row(vars_to_show[i], models)
  aer_rows[[length(aer_rows) + 1]] <- c(var_labels[i], row$coefs)
  aer_rows[[length(aer_rows) + 1]] <- c("", row$ses)
}

# Add stats rows
n_row <- c("N", sapply(models, nobs))
r2_row <- c("R²", sapply(models, function(m) round(summary(m)$r.squared, 3)))
adjr2_row <- c("Adj. R²", sapply(models, function(m) round(summary(m)$adj.r.squared, 3)))

aer_df <- as.data.frame(do.call(rbind, aer_rows))
names(aer_df) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")
aer_stats <- as.data.frame(rbind(n_row, r2_row, adjr2_row))
names(aer_stats) <- names(aer_df)

# Old simple comparison for backup
model_comp <- tibble(
  Model = paste0("(", 1:length(models), ")"),
  N = sapply(models, nobs),
  `R²` = round(sapply(models, function(m) summary(m)$r.squared), 3),
  `Adj R²` = round(sapply(models, function(m) summary(m)$adj.r.squared), 3)
)

# Core model coefficients (for detailed view)
coef_table <- tidy(m5) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 2),
    p.value = round(p.value, 4),
    sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", p.value < 0.1 ~ "†", TRUE ~ "")
  ) %>%
  select(Variable = term, Coefficient = estimate, `Std. Error` = std.error, `t-stat` = statistic, ` ` = sig)

# LASSO - use core predictors with good coverage
core_preds <- c("log_gdp", "internet", "rule_of_law", "corruption",
                "gov_effectiveness", "regulatory_quality", "tertiary_edu",
                "rd_spending", "fertility", "aging", "gini",
                "trade_openness", "hightech_exports", "unemployment",
                "urban", "life_expectancy", "mobile", "patents", "female_labor",
                "english_prof")
core_preds <- intersect(core_preds, names(full_data))

lasso_data <- full_data %>%
  select(log_usage, all_of(core_preds)) %>%
  filter(complete.cases(.))

X <- as.matrix(lasso_data[, core_preds])
y <- lasso_data$log_usage
X_scaled <- scale(X)
set.seed(42)
cv_lasso <- cv.glmnet(X_scaled, y, alpha = 1, nfolds = 10)
lasso_coef <- coef(cv_lasso, s = "lambda.min")

# Nice labels for LASSO output
lasso_labels <- c(
  log_gdp = "Log GDP per capita", internet = "Internet users (%)",
  rule_of_law = "Rule of law", corruption = "Corruption control",
  gov_effectiveness = "Govt effectiveness", regulatory_quality = "Regulatory quality",
  tertiary_edu = "Tertiary enrollment", rd_spending = "R&D spending (% GDP)",
  fertility = "Fertility rate", aging = "Population 65+ (%)",
  gini = "Gini coefficient", trade_openness = "Trade openness",
  hightech_exports = "High-tech exports", unemployment = "Unemployment",
  urban = "Urban population (%)", life_expectancy = "Life expectancy",
  mobile = "Mobile subscriptions", patents = "Patent applications",
  female_labor = "Female labor force", english_prof = "English proficiency (EF EPI)"
)

lasso_df <- data.frame(
  Variable = rownames(lasso_coef)[-1],
  Coefficient = round(as.vector(lasso_coef)[-1], 4)
) %>%
  filter(Coefficient != 0) %>%
  mutate(Variable = ifelse(Variable %in% names(lasso_labels),
                           lasso_labels[Variable], Variable)) %>%
  arrange(desc(abs(Coefficient)))

lasso_r2 <- round(cor(y, predict(cv_lasso, X_scaled, s = "lambda.min"))^2 * 100, 1)

# Generate LASSO importance plot - matching theme_academic style
lasso_plot_df <- lasso_df %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))

p_lasso <- ggplot(lasso_plot_df, aes(x = Coefficient, y = Variable)) +
  geom_col(aes(fill = Coefficient > 0), width = 0.7) +
  geom_vline(xintercept = 0, color = "#333333") +
  geom_text(aes(label = sprintf("%.2f", Coefficient),
                hjust = ifelse(Coefficient > 0, -0.15, 1.15)),
            size = 4, family = "cm", color = "#333333") +
  scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#ef4444"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.1))) +
  labs(
    title = "Lower fertility is the strongest predictor of Claude usage",
    subtitle = paste0("Standardized LASSO coefficients | Model R² = ", lasso_r2, "%"),
    x = "Standardized Coefficient",
    y = NULL,
    caption = "LASSO with 10-fold CV automatically selects predictors via L1 regularization"
  ) +
  theme_academic() +
  theme(
    panel.grid.major.y = element_blank()
  )

ggsave("figures/lasso_variable_importance.png", p_lasso, width = 12, height = 8, dpi = 300, bg = "white")

# Shapley decomposition of R² for model m6
# Uses Lindeman-Merenda-Gold (lmg) method - equivalent to Shapley values
shapley_result <- calc.relimp(m6, type = "lmg", rela = TRUE)
shapley_df <- data.frame(
  Variable = c("Log GDP per capita", "Internet users", "Regulatory quality",
               "Fertility rate", "Population 65+", "R&D spending"),
  Share = round(shapley_result$lmg * 100, 1)
) %>%
  arrange(desc(Share)) %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))

# Shapley plot
p_shapley <- ggplot(shapley_df, aes(x = Share, y = Variable)) +
  geom_col(fill = "#3b82f6", width = 0.7) +
  geom_text(aes(label = paste0(Share, "%")), hjust = -0.15,
            size = 4, family = "cm", color = "#333333") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, NA)) +
  labs(
    title = "Shapley decomposition: What explains Claude usage?",
    subtitle = paste0("Share of R² = ", round(summary(m6)$r.squared * 100, 1), "% attributable to each predictor"),
    x = "% of Explained Variance",
    y = NULL,
    caption = "Lindeman-Merenda-Gold method (Shapley values for regression)"
  ) +
  theme_academic() +
  theme(panel.grid.major.y = element_blank())

ggsave("figures/shapley_decomposition.png", p_shapley, width = 12, height = 6, dpi = 300, bg = "white")

# Residual analysis - use full model not just GDP
full_model <- m6  # The 6-variable model (GDP + internet + governance + demographics + R&D)
model_data$resid_gdp <- round(residuals(m1), 3)
model_data$resid_full <- round(residuals(full_model), 3)
model_data$fitted_full <- round(fitted(full_model), 3)

# Formal residual regression with boolean predictors
resid_reg <- lm(resid_full ~ is_anglophone + is_ex_soviet + is_small_open, data = model_data)
resid_coef <- tidy(resid_reg) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 3),
    sig = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ "")
  ) %>%
  select(Variable = term, Coefficient = estimate, `Std. Error` = std.error, `p-value` = p.value, ` ` = sig)

resid_r2 <- round(summary(resid_reg)$r.squared, 3)
resid_n <- nrow(model_data)

# Overperformers from full model
over <- model_data %>%
  arrange(desc(resid_full)) %>%
  head(10) %>%
  mutate(
    log_gdp = round(log_gdp, 2),
    log_usage = round(log_usage, 2),
    Anglo = ifelse(is_anglophone == 1, "Yes", ""),
    ExSov = ifelse(is_ex_soviet == 1, "Yes", ""),
    Small = ifelse(is_small_open == 1, "Yes", "")
  ) %>%
  select(Country = geo_name, Residual = resid_full, Anglo, ExSov, Small)

under <- model_data %>%
  arrange(resid_full) %>%
  head(10) %>%
  mutate(
    log_gdp = round(log_gdp, 2),
    log_usage = round(log_usage, 2),
    Anglo = ifelse(is_anglophone == 1, "Yes", ""),
    ExSov = ifelse(is_ex_soviet == 1, "Yes", ""),
    Small = ifelse(is_small_open == 1, "Yes", "")
  ) %>%
  select(Country = geo_name, Residual = resid_full, Anglo, ExSov, Small)

# Extract ex-Soviet residuals for narrative text
exsov_positive <- model_data %>%
  filter(is_ex_soviet == 1, resid_full > 0) %>%
  arrange(desc(resid_full))
exsov_worst <- model_data %>%
  filter(is_ex_soviet == 1) %>%
  arrange(resid_full) %>%
  slice(1)

# Build dynamic ex-Soviet overperformer text
if (nrow(exsov_positive) >= 1) {
  exsov_text <- paste0(
    paste(sapply(seq_len(min(nrow(exsov_positive), 3)), function(i) {
      paste0(exsov_positive$geo_name[i], " (+", sprintf("%.2f", exsov_positive$resid_full[i]), ")")
    }), collapse = ", "),
    if (nrow(exsov_positive) > 1) " are among the biggest overperformers globally" else " is among the biggest overperformers globally",
    "—", if (nrow(exsov_positive) > 1) "countries" else "a country",
    " with strong Soviet-era technical education and active tech diasporas"
  )
} else {
  exsov_text <- "some ex-Soviet states overperform—countries with strong Soviet-era technical education"
}
exsov_worst_text <- paste0(exsov_worst$geo_name, " (", sprintf("%.2f", exsov_worst$resid_full), ")")

# Survey data
survey <- read_csv("data/surveys/ai_usage_survey.csv", show_col_types = FALSE)
perception <- read_csv("data/surveys/ai_perception.csv", show_col_types = FALSE)
usage <- aei %>% filter(variable == "usage_per_capita", geography == "country", facet == "country") %>%
  select(geo_id, claude_usage = value) %>% filter(!is.na(claude_usage) & claude_usage > 0)
combined <- survey %>% inner_join(usage, by = "geo_id") %>%
  left_join(perception %>% select(geo_id, excited, nervous), by = "geo_id") %>%
  mutate(log_claude = log10(claude_usage))
cor_survey <- round(cor(combined$chatgpt_adoption_pct, combined$log_claude), 2)
cor_excited <- round(cor(combined$excited, combined$log_claude, use = "complete.obs"), 2)

# RTI-Claude usage correlation and controlled regression
rti_usage <- full_data %>% filter(!is.na(rti), !is.na(log_usage))
cor_rti <- round(cor(rti_usage$rti, rti_usage$log_usage), 2)
n_rti_countries <- nrow(rti_usage)
rti_ctrl <- lm(log_usage ~ log_gdp + rti, data = rti_usage)
p_rti_ctrl <- round(summary(rti_ctrl)$coefficients["rti", "Pr(>|t|)"], 2)

# GDP growth-Claude usage correlation and controlled regression
growth_usage <- full_data %>% filter(!is.na(gdp_growth), !is.na(log_usage))
cor_growth <- round(cor(growth_usage$gdp_growth, growth_usage$log_usage), 2)
growth_ctrl <- lm(log_usage ~ log_gdp + gdp_growth, data = growth_usage)
p_growth_ctrl <- round(summary(growth_ctrl)$coefficients["gdp_growth", "Pr(>|t|)"], 2)

# Automation analysis
auto_aug <- aei %>%
  filter(variable %in% c("automation_pct", "augmentation_pct"), geography == "country") %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(automation_pct))
usage_count <- aei %>% filter(variable == "usage_count", geography == "country", facet == "country") %>%
  select(geo_id, usage_count = value)
gdp_d <- full_data %>% select(geo_id, log_gdp, trade_openness, rti, gdp_accel)
auto_aug <- auto_aug %>%
  left_join(usage_count, by = "geo_id") %>%
  left_join(gdp_d, by = "geo_id") %>%
  filter(usage_count > 500, !is.na(log_gdp))
cor_auto <- round(cor(auto_aug$log_gdp, auto_aug$automation_pct), 2)

# Regional automation averages
south_asia <- c("IND", "PAK", "BGD", "LKA", "NPL")
east_europe <- c("UKR", "ROU", "POL", "CZE", "HUN", "BGR", "SVK", "HRV", "SRB", "LTU", "LVA", "EST")
sa_auto <- auto_aug %>% filter(geo_id %in% south_asia)
ee_auto <- auto_aug %>% filter(geo_id %in% east_europe)
sa_auto_pct <- round(mean(sa_auto$automation_pct, na.rm = TRUE), 0)
ee_auto_pct <- round(mean(ee_auto$automation_pct, na.rm = TRUE), 0)

# Decomposition regressions: test each theory
# Model 1: GDP only (labor cost story baseline)
auto_m1 <- lm(automation_pct ~ log_gdp, data = auto_aug)
# Model 2: Add aging (labor scarcity proxy)
auto_aug_aging <- auto_aug %>% left_join(full_data %>% select(geo_id, aging), by = "geo_id")
auto_m2 <- lm(automation_pct ~ log_gdp + aging, data = auto_aug_aging %>% filter(!is.na(aging)))
# Model 3: Add RTI (routine task intensity - task composition story)
auto_m3 <- lm(automation_pct ~ log_gdp + rti, data = auto_aug %>% filter(!is.na(rti)))
# Model 4: Full model
auto_m4 <- lm(automation_pct ~ log_gdp + aging + rti, data = auto_aug_aging %>% filter(!is.na(rti), !is.na(aging)))

# Build automation regression table
auto_models <- list(auto_m1, auto_m2, auto_m3, auto_m4)
auto_vars <- c("log_gdp", "aging", "rti")
auto_labels <- c("Log GDP per capita", "Population 65+ (%)", "Routine task intensity")

make_auto_row <- function(varname, models) {
  sapply(models, function(m) {
    if (varname %in% names(coef(m))) {
      s <- summary(m)
      est <- coef(m)[varname]
      pval <- s$coefficients[varname, "Pr(>|t|)"]
      stars <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
      paste0(round(est, 2), stars)
    } else { "" }
  })
}

auto_rows <- list()
for (i in seq_along(auto_vars)) {
  auto_rows[[i]] <- c(auto_labels[i], make_auto_row(auto_vars[i], auto_models))
}
auto_n <- c("N", sapply(auto_models, nobs))
auto_r2 <- c("R²", sapply(auto_models, function(m) round(summary(m)$r.squared, 3)))

auto_reg_df <- as.data.frame(do.call(rbind, auto_rows))
names(auto_reg_df) <- c("", "(1)", "(2)", "(3)", "(4)")
auto_stats_df <- as.data.frame(rbind(auto_n, auto_r2))
names(auto_stats_df) <- names(auto_reg_df)

# Quadrant classification for plot
median_gdp <- median(auto_aug$log_gdp)
median_auto <- median(auto_aug$automation_pct)
auto_aug <- auto_aug %>%
  mutate(
    quadrant = case_when(
      log_gdp >= median_gdp & automation_pct >= median_auto ~ "High GDP + High Auto",
      log_gdp >= median_gdp & automation_pct < median_auto ~ "High GDP + Low Auto (Augmenters)",
      log_gdp < median_gdp & automation_pct >= median_auto ~ "Low GDP + High Auto (Substituters)",
      TRUE ~ "Low GDP + Low Auto"
    )
  )

high_auto <- auto_aug %>% arrange(desc(automation_pct)) %>% head(5) %>%
  mutate(automation_pct = round(automation_pct, 1), augmentation_pct = round(augmentation_pct, 1)) %>%
  select(Country = geo_name, `Automation %` = automation_pct, `Augmentation %` = augmentation_pct)
high_aug <- auto_aug %>% arrange(desc(augmentation_pct)) %>% head(5) %>%
  mutate(automation_pct = round(automation_pct, 1), augmentation_pct = round(augmentation_pct, 1)) %>%
  select(Country = geo_name, `Automation %` = automation_pct, `Augmentation %` = augmentation_pct)

# Computed values for HTML (avoid hardcoding)
top_automator <- auto_aug %>% arrange(desc(automation_pct)) %>% slice(1)
top_augmenter <- auto_aug %>% arrange(desc(augmentation_pct)) %>% slice(1)
top_auto_name <- top_automator$geo_name
top_auto_pct <- round(top_automator$automation_pct, 0)
top_aug_name <- top_augmenter$geo_name
top_aug_pct <- round(top_augmenter$augmentation_pct, 0)

# Model stats
n_core <- nobs(m1)  # Core sample size (127)
n_english <- nobs(m7)  # English proficiency sample (90)
r2_main <- round(summary(m6)$r.squared * 100, 0)  # R² as percentage

# Survey examples
top_survey <- combined %>% arrange(desc(chatgpt_adoption_pct)) %>% slice(1)
top_survey_country <- top_survey$country
top_survey_pct <- round(top_survey$chatgpt_adoption_pct, 0)

# Perception examples
top_excited <- combined %>% filter(!is.na(excited)) %>% arrange(desc(excited)) %>% slice(1)
top_skeptic <- combined %>% filter(!is.na(nervous)) %>% arrange(desc(nervous), excited) %>%
  filter(log_claude > median(combined$log_claude, na.rm = TRUE)) %>% slice(1)
excited_country <- top_excited$country
excited_pct <- round(top_excited$excited, 0)
skeptic_country <- top_skeptic$country
skeptic_excited_pct <- round(top_skeptic$excited, 0)
skeptic_nervous_pct <- round(top_skeptic$nervous, 0)

# Quadrant summary table
quadrant_summary <- auto_aug %>%
  group_by(quadrant) %>%
  summarize(
    Countries = n(),
    `Avg Auto %` = round(mean(automation_pct), 1),
    `Avg GDP` = paste0("$", round(10^mean(log_gdp)/1000, 0), "k"),
    .groups = "drop"
  ) %>%
  rename(Quadrant = quadrant)

# Generate HTML
html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>What Predicts AI Usage Across Countries?</title>
  <style>
    body { font-family: "Palatino Linotype", Georgia, serif; max-width: 820px; margin: 50px auto; padding: 0 25px; line-height: 1.75; color: #2a2a2a; }
    h1 { font-size: 2.1em; margin-bottom: 0.3em; font-weight: 600; }
    h2 { font-size: 1.35em; margin-top: 2.2em; border-bottom: 2px solid #e0e0e0; padding-bottom: 0.4em; color: #1a1a1a; }
    h3 { font-size: 1.05em; margin-top: 1.5em; color: #444; font-weight: 600; }
    p { margin: 1em 0; }
    .subtitle { font-size: 1.1em; color: #666; margin-bottom: 2em; font-style: italic; }

    /* Nice regression tables */
    .reg-table {
      border-collapse: collapse;
      margin: 1.5em 0;
      font-size: 0.88em;
      font-family: "SF Mono", Consolas, monospace;
      width: 100%;
      background: #fff;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    }
    .reg-table th {
      background: linear-gradient(180deg, #f8f9fa 0%, #f1f3f4 100%);
      padding: 12px 14px;
      text-align: left;
      font-weight: 600;
      border-bottom: 2px solid #dee2e6;
      font-size: 0.9em;
      text-transform: uppercase;
      letter-spacing: 0.03em;
      color: #495057;
    }
    .reg-table td {
      padding: 10px 14px;
      border-bottom: 1px solid #e9ecef;
    }
    .reg-table tr:hover { background: #f8f9fa; }
    .reg-table tr:last-child td { border-bottom: none; }
    caption {
      font-weight: 600;
      margin-bottom: 0.6em;
      text-align: left;
      font-size: 0.95em;
      color: #333;
    }

    img { max-width: 100%; margin: 1.8em 0; border-radius: 4px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
    .stat {
      background: linear-gradient(135deg, #f0f7ff 0%, #e8f4fd 100%);
      padding: 16px 20px;
      border-left: 4px solid #3b82f6;
      margin: 1.5em 0;
      border-radius: 0 6px 6px 0;
      font-size: 0.95em;
    }
    .stat strong { color: #1e40af; }
    code { background: #f1f3f4; padding: 2px 7px; font-size: 0.88em; border-radius: 3px; }
    .footnote { font-size: 0.82em; color: #666; margin-top: 3em; padding-top: 1.5em; border-top: 1px solid #ddd; }
    .sig-note { font-size: 0.78em; color: #888; margin-top: -0.5em; font-style: italic; }
    hr { border: none; border-top: 1px solid #e0e0e0; margin: 2.5em 0; }
    .references { font-size: 0.85em; line-height: 1.6; }
    .references p { margin: 0.6em 0; padding-left: 2em; text-indent: -2em; }

    /* Stylized facts box */
    .stylized-facts {
      background: #fafafa;
      border: 1px solid #e5e5e5;
      border-radius: 8px;
      padding: 24px 28px;
      margin: 2em 0;
    }
    .stylized-facts h2 {
      margin-top: 0;
      border-bottom: none;
      font-size: 1.1em;
      color: #333;
      letter-spacing: 0.05em;
      text-transform: uppercase;
    }
    .stylized-facts ol {
      margin: 0;
      padding-left: 1.2em;
    }
    .stylized-facts li {
      margin: 0.8em 0;
      padding-left: 0.3em;
    }
    .stylized-facts .fact-stat {
      font-family: "SF Mono", Consolas, monospace;
      background: #fff;
      padding: 2px 8px;
      border-radius: 4px;
      font-size: 0.9em;
      color: #1e40af;
      border: 1px solid #e0e7ff;
    }
  </style>
</head>
<body>

<h1>What Predicts AI Usage Across Countries?</h1>
<p class="subtitle">Patterns in the Anthropic Economic Index</p>
<p style="background: #fff3cd; border: 1px solid #ffc107; border-radius: 6px; padding: 12px 16px; font-size: 0.9em; color: #664d03; margin-bottom: 2em;"><strong>Status:</strong> This is preliminary — mostly Claude-generated, vibe-coded and vibe-blogged over a weekend. Haven\'t shared with anyone yet. Not sure what I\'m going to do with it.</p>

<p>Anthropic recently released their Economic Index—one of the first public datasets on actual AI assistant usage at the country level. I spent some time with it. Some patterns emerged that seemed worth writing up.</p>

<p>A caveat upfront: this is exploratory work—pattern-finding rather than hypothesis testing. The sample is small (127-150 countries depending on specification), the data source is one product from one company, and cross-country regressions have well-known limitations. I write up these patterns because they seem interesting and because comparable public data on AI usage is scarce, not because I\'m confident in any particular coefficient.</p>

<p>One more thing to keep in mind: this is Claude data, not all AI. ChatGPT dominates the consumer market (~60% share vs Claude\'s ~3-4%). But the skew may be informative: <a href="https://empiricrafting.substack.com/p/one-llm-to-rule-them-all">Fradkin (2025)</a> finds Claude is disproportionately a work-focused productivity tool, so this data likely captures the professional/productivity slice of AI adoption better than recreational use.</p>

<div class="stylized-facts">
<h2>Summary of Patterns</h2>
<ol>
<li><strong>', r2_main, '% of cross-country variance is explained</strong> by six predictors: GDP, internet access, regulatory quality, fertility, aging, and R&D spending. Standard development economics appears to apply.</li>
<li><strong>But how countries use AI differs by income.</strong> Poorer countries tend to automate; richer countries tend to augment. GDP correlates with automation rate at <span class="fact-stat">r = ', cor_auto, '</span>. ', top_auto_name, ' uses Claude for task replacement (', top_auto_pct, '% automation); ', top_aug_name, ' uses it for task enhancement (', top_aug_pct, '% augmentation).</li>
<li><strong>Survey enthusiasm inversely correlates with actual usage.</strong> Self-reported AI adoption correlates with Claude usage at <span class="fact-stat">r = ', cor_survey, '</span>. Countries reporting highest AI enthusiasm don\'t show highest usage.</li>
<li><strong>After controlling for everything, demographics survive.</strong> Fertility rate appears to be the strongest marginal predictor—negative coefficient survives LASSO regularization even controlling for GDP, governance, R&D, and 20 other variables.</li>
</ol>
</div>

<h2>The standard correlates</h2>

<p>The report notes Claude usage tracks GDP, internet penetration, and governance quality. This is consistent with the technology diffusion literature (Comin & Hobijn, 2010): richer countries with better infrastructure adopt new technologies faster.</p>

<p>The more interesting question: which of these actually do independent work? They\'re all correlated with each other—GDP predicts governance, governance predicts R&D, R&D predicts education. The usual development cluster. Separating signal from multicollinearity takes some regularization.</p>

<h2>Regression results</h2>

<p>I tested 23 indicators against Claude usage per capita: World Bank development variables, HDI, GDP volatility, and the routine task intensity index from Lewandowski et al. (2022). Table 1 shows the OLS results, with predictors added in an intuitive order: economic baseline (GDP), then infrastructure (internet access—the prerequisite for using Claude at all), then governance, then demographics.</p>

<h3>Table 1: OLS Estimates of Log Claude Usage per Capita</h3>
', html_table(aer_df), '
', html_table(aer_stats), '
<p class="sig-note">Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01</p>

<p>Column (1) shows the baseline GDP relationship. Column (2) adds internet access—which matters: you need internet to use Claude. But GDP remains significant after controlling for internet, suggesting the relationship isn\'t purely about infrastructure access. This matters for interpretation: the remaining GDP effect could reflect affordability (Claude Pro costs ~$20/month), skill composition of the workforce, or something else beyond mere connectivity.</p>

<p>Columns (3)-(6) add governance, demographics, and R&D. Fertility and aging add substantial explanatory power—the demographic variables do real work beyond the development cluster. The core model in columns (1)-(6) uses ', n_core, ' countries; columns (7)-(8) restrict to the ', n_english, ' countries with EF English Proficiency Index coverage.</p>

<h3>LASSO Variable Selection</h3>

<img src="figures/lasso_variable_importance.png" alt="LASSO variable importance">

<p>The notable result: <strong>fertility rate appears to be the strongest predictor</strong> in this sample after regularization. Negative coefficient—lower fertility, higher Claude usage. This pattern holds after controlling for GDP, governance, and R&D.</p>

<p>What to make of this? Low fertility correlates with aging populations and labor scarcity, which might increase demand for productivity tools—Acemoglu & Restrepo (2022) document exactly this pattern for industrial robots. It could also proxy for something cultural about time orientation and technology adoption. Or it could be noise—', n_core, ' countries isn\'t that many observations. I lean toward the labor scarcity story, but wouldn\'t bet heavily on it.</p>

<p><strong>English proficiency</strong> (measured by the EF English Proficiency Index) also survives regularization with a positive coefficient. Countries where more people speak English use Claude more, which makes sense given the model\'s training but is worth quantifying. This variable captures L1 and L2 speakers combined—not just Anglophone countries—so the effect is broader than just native English speakers.</p>

<p>Variables zeroed out by LASSO: internet access, tertiary education, corruption control. They\'re correlated with the survivors but don\'t add independent signal.</p>

<h3>Shapley Decomposition</h3>

<p>LASSO tells us which variables survive regularization, but not how much of the explained variance each contributes. For that, we need Shapley values—a game-theoretic method that fairly allocates R² to each predictor by averaging over all possible orderings. This handles multicollinearity properly.</p>

<img src="figures/shapley_decomposition.png" alt="Shapley decomposition">

<p>GDP alone accounts for the largest share of explained variance, but fertility and regulatory quality together contribute nearly as much. The demographic variables (fertility + aging) jointly explain about as much as governance. This suggests fertility isn\'t just noise—it appears to be doing real explanatory work independent of the development cluster.</p>

<h2>What this analysis cannot tell us</h2>

<p>Before going further, it\'s worth being explicit about the limitations:</p>

<p><strong>Correlation, not causation.</strong> These are cross-sectional associations. Countries with low fertility use Claude more, but we can\'t say low fertility <em>causes</em> AI adoption. The relationship could run the other way, or both could be driven by something unmeasured.</p>

<p><strong>One product, one moment.</strong> This is Claude data from one time period. ChatGPT has ~60% market share; Claude has ~3-4%. Patterns here may not generalize to other AI tools, and may not persist as the market evolves.</p>

<p><strong>Small sample, many predictors.</strong> With 127 observations and 8+ predictors in the full specification, individual coefficients are noisy. The LASSO helps with variable selection, but standard errors should be interpreted cautiously.</p>

<p><strong>Ecological fallacy.</strong> Country-level patterns may not apply at the individual level. A country with high automation rates may still have many users doing augmentation work—we\'re seeing averages, not distributions.</p>

<p><strong>Time series would be more informative.</strong> A single cross-section can\'t distinguish between countries that adopted early vs. those that will catch up. Repeated measurements would help separate persistent gaps from timing effects.</p>

<p><strong>Intensive vs. extensive margin.</strong> The data show usage per capita, but we can\'t separate the <em>extensive margin</em> (how many people use Claude) from the <em>intensive margin</em> (how much each user uses it). Claude Pro costs ~$20/month—a meaningful expense in many countries. If rich countries show higher usage, is that because more people can afford subscriptions, or because subscribers use it more intensively? Or is it skill composition—more knowledge workers whose tasks are Claude-relevant? The data can\'t disentangle these stories, though the surviving GDP coefficient after controlling for internet access suggests it\'s not purely about connectivity.</p>

<p>With those caveats in mind, some patterns are still worth noting.</p>

<h2>Residual analysis</h2>

<p>Table 2 shows countries that deviate most from the fitted values in Column (6)—i.e., after controlling for GDP, internet access, governance, demographics, and R&D. This is more informative than simple GDP residuals, since we\'re asking: who outperforms <em>conditional on observable fundamentals</em>?</p>

<h3>Table 2: Overperformers (Positive Residuals from Column 6)</h3>
', html_table(over), '

<h3>Table 3: Underperformers (Negative Residuals from Column 6)</h3>
', html_table(under), '

<p>The overperformer/underperformer lists hint at patterns. To formalize this, I coded three binary variables: <strong>Anglophone</strong> (English as primary/official language), <strong>Ex-Soviet</strong> (post-Soviet states), and <strong>Small open economy</strong> (population &lt;10M, high trade/GDP). Then regressed the residuals from Column (6) on these dummies:</p>

<h3>Table 4: What Predicts Overperformance?</h3>
', html_table(resid_coef), '
<p class="sig-note">Dependent variable: residuals from Column (6) model. N = ', resid_n, ', R² = ', resid_r2, '</p>

<p>The Anglophone coefficient is positive and borderline significant (p ≈ 0.08). English-speaking countries use Claude more than fundamentals predict—unsurprising given Claude\'s English-first training.</p>

<p>The ex-Soviet coefficient is essentially zero, which hides interesting within-group variation. ', exsov_text, '. But ', exsov_worst_text, ' is the biggest underperformer among ex-Soviet states, dragging down the group average. That\'s mechanical though: you can\'t use Claude if you can\'t access it. When you exclude restricted-internet countries, the ex-Soviet STEM education story might show up more clearly.</p>

<img src="figures/gdp_residuals.png" alt="GDP residuals">

<h2>Survey vs. revealed preference</h2>

<p>This is where it gets interesting. BCG surveyed ', nrow(combined), ' countries about ChatGPT usage. I matched this to the Claude data.</p>

<p class="stat">Correlation between self-reported AI use and actual Claude usage: <strong>r = ', cor_survey, '</strong></p>

<p>Negative. ', top_survey_country, ' reports the highest ChatGPT adoption (', top_survey_pct, '%) but has low Claude usage per capita. The pattern holds across emerging markets.</p>

<p>The obvious objection: ChatGPT ≠ Claude. Maybe there\'s product substitution—emerging markets use ChatGPT, not Claude. That\'s plausible. But the magnitude of divergence is still striking. There may also be survey response effects—Johnson & Van de Vijver (2003) document systematic cross-cultural differences in socially desirable responding.</p>

<p>The data sources aren\'t measuring the same thing, so I wouldn\'t push this too hard. But it\'s a pattern worth flagging for anyone forecasting AI adoption from survey data.</p>

<img src="figures/survey_vs_actual_usage.png" alt="Survey vs actual">

<h2>The attitude paradox</h2>

<p>Ipsos tracks whether people feel "excited" or "nervous" about AI. Natural hypothesis: excitement predicts adoption.</p>

<p class="stat">Correlation between AI excitement and Claude usage: <strong>r = ', cor_excited, '</strong></p>

<p>Also negative. ', excited_country, ' is ', excited_pct, '% excited, low usage. ', skeptic_country, ' is ', skeptic_excited_pct, '% excited/', skeptic_nervous_pct, '% nervous, high usage.</p>

<p>This is likely confounded by development level. Richer countries tend to be both more skeptical of technology hype and better positioned to use the technology. The skeptics with infrastructure outadopt the enthusiasts without it. Microsoft\'s AI Diffusion report (2025) documents a similar pattern: AI use in the Global North is roughly double that in the Global South, despite higher enthusiasm in developing economies.</p>

<img src="figures/perception_quadrant.png" alt="Perception quadrant">

<h2>Automation vs. augmentation</h2>

<p>This might be the most interesting finding. The AEI data classifies each conversation as "automation" (Claude does the task) vs "augmentation" (Claude helps with the task). I assumed this would be roughly constant across countries. It\'s not.</p>

<p class="stat">Correlation between GDP and automation rate: <strong>r = ', cor_auto, '</strong></p>

<p>This is a strong correlation for cross-country data—though as with the other findings, I wouldn\'t push the causal interpretation too hard. The pattern suggests poorer countries tend to use Claude to replace tasks; richer countries tend to use it to enhance them.</p>

<h3>Highest Automation</h3>
', html_table(high_auto), '

<h3>Highest Augmentation</h3>
', html_table(high_aug), '

<h3>The Quadrants</h3>
', html_table(quadrant_summary), '

<p>The "Low GDP + High Automation" quadrant is the labor substitution story: Claude as a force multiplier for scarce skilled labor. The "High GDP + Low Automation" quadrant is the productivity enhancement story: Claude as a collaborator for already-productive workers.</p>

<img src="figures/map_automation.png" alt="Global map of automation vs augmentation">

<p>The map shows the pattern geographically. Rich countries (North America, Western Europe, Australia) cluster blue—using Claude for augmentation. Parts of Africa and South Asia show higher automation rates. Eastern Europe is notably augmentation-heavy despite being a major services exporter.</p>

<p>This connects to a recurring debate in AI economics. Acemoglu and others emphasize automation and displacement; Brynjolfsson and others emphasize augmentation and productivity gains. The pattern here suggests both perspectives may have empirical support—but in different contexts. Automation-focused analyses may better describe AI\'s role in emerging markets; augmentation-focused analyses may better describe high-income contexts. The answer to "will AI replace or enhance workers?" might be "both, depending on where you look."</p>

<img src="figures/gdp_vs_automation.png" alt="GDP vs automation">

<h3>Testing the theories</h3>

<p>A few candidate explanations, not mutually exclusive. Can we separate them empirically?</p>

<p><strong>Labor costs (GDP).</strong> Baseline story: in high-wage economies, the constraint is productivity per worker. In lower-wage economies, the constraint is capacity. GDP is the proxy.</p>

<p><strong>Labor scarcity (aging).</strong> Countries with older populations have tighter labor markets, which might push toward augmentation (making existing workers more productive) rather than automation. Population 65+ is the proxy.</p>

<p><strong>Task composition (RTI).</strong> Users in emerging markets may disproportionately be doing tasks that are more automatable. We can test this with the IMF\'s Routine Task Intensity index—countries with more routine jobs should automate more.</p>

<h3>Table: Decomposing Automation Rate</h3>
', html_table(auto_reg_df), '
', html_table(auto_stats_df), '
<p class="sig-note">Dependent variable: automation %. * p < 0.1, ** p < 0.05, *** p < 0.01. Columns (3)-(4) restricted to countries with RTI data (Lewandowski et al. 2022).</p>

<p>GDP explains most of the variance—R² ≈ 0.68 in Column (1), which is high for cross-country regressions. Adding aging improves fit modestly; RTI (routine task intensity) adds little beyond GDP. This suggests wage levels may matter more than task composition, though the sample is small.</p>

<p>One interpretation: "how AI affects work" depends on development level. The job-replacement frame may be more accurate in emerging markets; the productivity-enhancement frame may be more accurate in high-income contexts. Both can be true simultaneously—which has implications for how we think about AI labor market effects globally.</p>

<h3>Task routineness, growth, and transformative AI</h3>

<p><a href="https://www.nber.org/papers/w34256">Brynjolfsson, Korinek & Agrawal (2025)</a> identify "transition dynamics" as one of nine grand challenges for transformative AI research—how does AI adoption spread across countries and sectors? The patterns here offer early data points.</p>

<p>One might expect countries with more routine-intensive jobs to show higher AI adoption—the "AI automates routine work" hypothesis. I tested this using the Routine Task Intensity index from Lewandowski et al. (2022), which covers ', n_rti_countries, ' countries. The raw correlation between RTI and Claude usage is <strong>r = ', cor_rti, '</strong>—countries with more routine jobs use Claude <em>less</em>. But this is entirely explained by GDP. After controlling for income level, RTI adds no predictive power (p = ', p_rti_ctrl, ').</p>

<p>Similarly, GDP growth shows a weak negative correlation with Claude usage (<strong>r = ', cor_growth, '</strong>). Fast-growing countries—often emerging markets in catch-up mode—haven\'t yet adopted Claude at high rates. This remains marginally significant (p ≈ ', p_growth_ctrl, ') even after controlling for GDP level.</p>

<p>What to make of this? If AI were already highly transformative—capable of automating routine work cost-effectively—we might expect high-RTI countries and fast-growing emerging markets to show stronger adoption. Instead, adoption tracks resources and development level. This doesn\'t mean AI won\'t become transformative, but current diffusion patterns look more like standard technology adoption than a discontinuous shift.</p>

<p><strong>A case study: services exporters.</strong> Countries that have built large industries around routine cognitive work—call centers, data processing, back-office operations—are particularly relevant for transformative AI scenarios. These workers do exactly the tasks AI is supposed to automate. If current AI were already disrupting this work, these countries should show distinctive patterns.</p>

<p>Using World Bank data on ICT service exports, I find the overall relationship doesn\'t survive GDP controls—high ICT exporters don\'t show systematically different automation rates once you account for income (p ≈ 0.57). But one regional pattern stands out:</p>

<ul>
<li><strong>South Asia</strong> (major ICT service exporters): Automation at ', sa_auto_pct, '%—close to the global average. Nothing unusual.</li>
<li><strong>Eastern Europe</strong> (also significant ICT exporters): Automation at ', ee_auto_pct, '%—well <em>below</em> the global mean. Much more augmentation.</li>
</ul>

<p>The story isn\'t "South Asia automates more"—they\'re average. The story is <strong>Eastern Europe augments more</strong> despite being major ICT exporters. One interpretation: Eastern European IT workers—closer to Western clients, competing on quality not just cost—are using AI as a productivity tool to move up the value chain rather than having tasks replaced. This is speculative, and the regional interactions aren\'t statistically significant. But it suggests that "services exporter" isn\'t a monolithic category, and that the transformative AI transition—if it comes—may not look like uniform job displacement.</p>

<h2>Implications for policy and research</h2>

<p>If these patterns hold up, a few implications seem worth noting:</p>

<p><strong>For forecasting AI adoption:</strong> Survey data on AI attitudes and self-reported usage may not track revealed preference. Countries that report high AI enthusiasm don\'t necessarily show high actual usage. If you\'re forecasting AI diffusion from attitude surveys, you might want to discount heavily—or weight by GDP and infrastructure measures.</p>

<p><strong>For thinking about distribution:</strong> The automation/augmentation split suggests AI\'s labor market effects may differ substantially by development level. Analyses focused on high-income countries may miss the more automation-heavy pattern in emerging markets, and vice versa. Global AI policy discussions should probably account for this heterogeneity rather than assuming uniform effects.</p>

<p><strong>For AI governance:</strong> The fact that standard development indicators (GDP, governance quality, infrastructure) predict AI adoption so well is actually somewhat reassuring. It suggests AI adoption follows familiar technology diffusion patterns and may not require entirely novel governance frameworks—though the automation/augmentation split might warrant attention.</p>

<p><strong>For measurement:</strong> More public data like the Anthropic Economic Index would be valuable. Right now, most AI usage data is proprietary. Public datasets on actual usage patterns—across platforms, not just one—would help researchers and policymakers move beyond speculation.</p>

<h2>Takeaways</h2>

<p>Some patterns that seem worth tracking, with appropriate uncertainty:</p>

<p><strong>Demographics appear predictive.</strong> The fertility/aging relationship is consistent across specifications in this sample, though the mechanism is unclear. One interpretation: labor-scarce societies may adopt AI tools faster. This would be worth testing with other data sources.</p>

<p><strong>Stated preferences diverge from revealed preferences.</strong> Country-level surveys on AI enthusiasm don\'t track well with actual usage in this data. If you\'re forecasting AI adoption from attitude surveys, caution seems warranted.</p>

<p><strong>How AI is used varies more than whether.</strong> The automation/augmentation split is arguably more interesting than adoption levels. "AI adoption" isn\'t one thing—the same tool appears to be used quite differently depending on economic context.</p>

<p><strong>Standard frameworks seem to apply.</strong> Most cross-country variation is explained by familiar predictors: income, infrastructure, institutions. AI adoption appears to follow existing technology diffusion patterns, which is somewhat reassuring for forecasting and governance.</p>

<p>These patterns should be treated as hypotheses for future work rather than established findings. Replication with ChatGPT data, or with multiple time periods, would be valuable.</p>

<hr>

<p>The Anthropic dataset is valuable—there\'s not much comparable public data on actual AI usage patterns. These findings are preliminary, and I\'d want more time periods and better controls before betting on any specific coefficient. But the broad patterns seem real.</p>

<p>If anyone has ideas for extending this or spots errors, I\'m reachable at <a href="mailto:t6aguirre@gmail.com">t6aguirre@gmail.com</a> or <a href="https://twitter.com/t6aguirre">@t6aguirre</a>.</p>

<hr>

<p style="font-size: 0.9em; color: #555;"><strong>Suggested citation:</strong> Aguirre, T. (2025). "What Predicts AI Usage Across Countries? Patterns in the Anthropic Economic Index." Working paper. Available at: [this URL]</p>

<h2>References</h2>

<div class="references">
<p>Acemoglu, D., & Restrepo, P. (2022). Demographics and Automation. <em>The Review of Economic Studies</em>, 89(1), 1-44. https://doi.org/10.1093/restud/rdab031</p>

<p>Anthropic. (2025). <em>The Anthropic Economic Index: September 2025 Report</em>. https://www.anthropic.com/research/anthropic-economic-index-september-2025-report</p>

<p>BCG. (2023). <em>Global Consumer Sentiment Survey: AI Adoption</em>. Boston Consulting Group.</p>

<p>Brynjolfsson, E., Korinek, A., & Agrawal, A. (2025). A Research Agenda for the Economics of Transformative AI. NBER Working Paper 34256. https://www.nber.org/papers/w34256</p>

<p>Comin, D., & Hobijn, B. (2010). An Exploration of Technology Diffusion. <em>American Economic Review</em>, 100(5), 2031-2059. https://doi.org/10.1257/aer.100.5.2031</p>

<p>Fradkin, A. (2025). <em>Demand for LLMs: Descriptive Evidence on Substitution, Market Expansion, and Multihoming</em>. Working paper, Boston University. https://empiricrafting.substack.com/p/one-llm-to-rule-them-all</p>

<p>Johnson, T. P., & Van de Vijver, F. J. R. (2003). Social Desirability in Cross-Cultural Research. In J. A. Harkness, F. J. R. Van de Vijver, & P. P. Mohler (Eds.), <em>Cross-Cultural Survey Methods</em> (pp. 195-204). Wiley.</p>

<p>Microsoft Research. (2025). <em>AI Diffusion: Mapping Global AI Adoption and Innovation</em>. https://www.microsoft.com/en-us/research/group/aiei/ai-diffusion/</p>

<p>ILO. (2025). <em>Generative AI and Jobs: A Refined Global Index of Occupational Exposure</em>. Working Paper No. 140. https://www.ilo.org/publications/generative-ai-and-jobs-refined-global-index-occupational-exposure</p>

<p>Ipsos. (2024). <em>Ipsos AI Monitor 2024: Changing Attitudes and Feelings About AI</em>. https://www.ipsos.com/en/ipsos-ai-monitor-2024</p>

<p>Lewandowski, P., Park, A., Hardy, W., Du, Y., & Wu, S. (2022). Technology, Skills, and Globalization: Explaining International Differences in Routine and Nonroutine Work Using Survey Data. <em>The World Bank Economic Review</em>, 36(3), 687-708. https://doi.org/10.1093/wber/lhac005</p>

<p>UNDP. (2024). <em>Human Development Report 2024</em>. New York: United Nations Development Programme. https://hdr.undp.org/</p>

<p>EF Education First. (2024). <em>EF English Proficiency Index 2024</em>. https://www.ef.com/epi/</p>

<p>World Bank. (2024). <em>World Development Indicators</em>. Washington, DC: World Bank Group. https://databank.worldbank.org/source/world-development-indicators</p>
</div>

<p class="footnote"><em>Methods: OLS regression with incremental model building and LASSO with 10-fold cross-validation for variable selection. Sample sizes range from n = ', nrow(model_data), ' (core model) to n = ', n_english, ' (with English proficiency). LASSO uses lambda.min from cross-validation; coefficients are standardized for comparison. Standard errors are conventional OLS and don\'t account for spatial correlation or model uncertainty from variable selection. Predictors include World Bank WDI indicators (2019-2023), UNDP HDI (2024), Lewandowski et al. (2022) Routine Task Intensity index (102 countries), and EF English Proficiency Index (2024). The automation/augmentation classification is from Anthropic\'s methodology using their CLIO privacy-preserving framework. Replication code available at <a href="https://github.com/t6aguirre">github.com/t6aguirre</a>.</em></p>

</body>
</html>')

writeLines(html, "blog_post.html")
cat("Saved: blog_post.html\n")
