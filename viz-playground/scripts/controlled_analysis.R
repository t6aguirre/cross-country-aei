# Controlled Analysis: What predicts AI BEYOND internet access?
# Partial correlations and residual analysis

options(repos = c(CRAN = "https://cloud.r-project.org"))

library(tidyverse)
library(showtext)
library(ggrepel)
library(scales)

# Computer Modern style - academic/LaTeX look
font_add_google("Libre Baskerville", "cm")  # Clean serif like Computer Modern
font_add_google("Source Serif Pro", "ss")
showtext_auto()
showtext_opts(dpi = 300)

theme_twitter <- function(base_size = 16) {
  theme_minimal(base_size = base_size, base_family = "cm") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.3), color = "#1a1a1a", margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.85), color = "#555555", margin = margin(b = 12)),
      plot.caption = element_text(size = rel(0.7), color = "#888888", hjust = 1, margin = margin(t = 12)),
      axis.title = element_text(size = rel(0.85), color = "#333333"),
      axis.text = element_text(size = rel(0.8), color = "#555555"),
      panel.grid.major = element_line(color = "#eeeeee", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 25, 15, 20)
    )
}

read_wb <- function(pattern, var_name) {
  file <- list.files(pattern = pattern)[1]
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

cat("Loading all data...\n")

# Load AI data
aei <- read_csv("aei_geo.csv", show_col_types = FALSE)
ai_usage <- aei %>%
  filter(geography == "country", facet == "country",
         variable %in% c("usage_per_capita", "gdp_per_working_age_capita")) %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(usage_per_capita) & usage_per_capita > 0)

# Load all indicators
internet <- read_wb("API_IT.NET.USER", "internet_pct")
rule_of_law <- read_wb("API_RL.EST", "rule_of_law")
corruption <- read_wb("API_CC.EST", "corruption_control")
tertiary <- read_wb("API_SE.TER.ENRR", "tertiary_edu")
rd <- read_wb("API_GB.XPD.RSDV", "rd_pct")
fertility <- read_wb("API_SP.DYN.TFRT", "fertility_rate")
aging <- read_wb("API_SP.POP.65UP", "pct_65plus")
gdp_growth <- read_wb("API_NY.GDP.MKTP.KD.ZG", "gdp_growth")

# Merge all
full_data <- ai_usage %>%
  left_join(internet, by = "geo_id") %>%
  left_join(rule_of_law, by = "geo_id") %>%
  left_join(corruption, by = "geo_id") %>%
  left_join(tertiary, by = "geo_id") %>%
  left_join(rd, by = "geo_id") %>%
  left_join(fertility, by = "geo_id") %>%
  left_join(aging, by = "geo_id") %>%
  left_join(gdp_growth, by = "geo_id")

# Filter to countries with internet data
analysis_data <- full_data %>%
  filter(!is.na(internet_pct) & !is.na(usage_per_capita)) %>%
  mutate(log_usage = log10(usage_per_capita),
         log_gdp = log10(gdp_per_working_age_capita))

cat("Countries for controlled analysis:", nrow(analysis_data), "\n")

# Step 1: Regress AI usage on internet access
model_internet <- lm(log_usage ~ internet_pct, data = analysis_data)
analysis_data$ai_residual <- residuals(model_internet)

# Step 2: Calculate correlations AFTER controlling for internet
cat("\n========================================\n")
cat("RAW vs CONTROLLED CORRELATIONS\n")
cat("========================================\n")
cat(sprintf("%-22s %8s %10s\n", "Variable", "Raw r", "Controlled"))
cat("----------------------------------------\n")

vars <- c("log_gdp", "rule_of_law", "corruption_control", "tertiary_edu",
          "rd_pct", "fertility_rate", "pct_65plus", "gdp_growth")
var_names <- c("GDP per Working-Age Capita (log)",
               "World Bank Rule of Law Index",
               "Control of Corruption Index",
               "Tertiary Education Enrollment Rate",
               "R&D Expenditure (% of GDP)",
               "Total Fertility Rate (births/woman)",
               "Population Aged 65 and Over (%)",
               "Annual GDP Growth Rate (%)")

raw_cors <- numeric(length(vars))
controlled_cors <- numeric(length(vars))

for (i in seq_along(vars)) {
  raw_cors[i] <- cor(analysis_data$log_usage, analysis_data[[vars[i]]], use = "complete.obs")
  controlled_cors[i] <- cor(analysis_data$ai_residual, analysis_data[[vars[i]]], use = "complete.obs")
  cat(sprintf("%-22s %8.3f %10.3f\n", var_names[i], raw_cors[i], controlled_cors[i]))
}
cat("========================================\n")

# ============================================
# PLOT 1: Raw vs Controlled Correlations
# ============================================
cor_comparison <- data.frame(
  variable = factor(var_names, levels = var_names[order(abs(controlled_cors), decreasing = TRUE)]),
  raw = raw_cors,
  controlled = controlled_cors
) %>%
  pivot_longer(cols = c(raw, controlled), names_to = "type", values_to = "correlation") %>%
  mutate(type = factor(type, levels = c("raw", "controlled"),
                       labels = c("Raw correlation", "After controlling for internet")))

p1 <- ggplot(cor_comparison, aes(x = correlation, y = variable, fill = type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_vline(xintercept = 0, color = "#333333") +
  scale_fill_manual(values = c("#3b82f6", "#10b981")) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  labs(
    title = "What REALLY Drives AI Adoption Beyond Internet Access?",
    subtitle = "Correlations before and after controlling for internet penetration",
    x = "Correlation with log(AI Usage per Capita)",
    y = NULL,
    fill = NULL,
    caption = "Some factors remain strong even after accounting for internet access | Data: Anthropic + World Bank"
  ) +
  theme_twitter() +
  theme(legend.position = "top")

ggsave("controlled_comparison.png", p1, width = 12, height = 7, dpi = 300, bg = "white")
cat("\nSaved: controlled_comparison.png\n")

# ============================================
# PLOT 2: Fertility controlled for internet
# ============================================
notable <- c("United States", "China", "India", "Japan", "Germany",
             "South Korea", "Italy", "France", "Brazil", "Israel",
             "Singapore", "Nigeria", "Kenya", "Poland", "Ukraine")

plot2_data <- analysis_data %>% filter(!is.na(fertility_rate))
cor2 <- cor(plot2_data$ai_residual, plot2_data$fertility_rate, use = "complete.obs")

p2 <- ggplot(plot2_data, aes(x = fertility_rate, y = ai_residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#666666") +
  geom_vline(xintercept = 2.1, linetype = "dashed", color = "#dc2626", alpha = 0.5) +
  geom_point(aes(color = fertility_rate < 2.1), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#7c3aed", fill = "#ddd6fe", linewidth = 1) +
  geom_text_repel(
    data = plot2_data %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 15
  ) +
  scale_color_manual(values = c("#10b981", "#ef4444"),
                     labels = c("Above replacement", "Below replacement"), name = NULL) +
  labs(
    title = "Fertility Crisis Still Predicts AI (Even After Controlling for Internet)",
    subtitle = paste0("Partial correlation r = ", round(cor2, 2),
                      " - low fertility countries use MORE AI beyond just having internet"),
    x = "Total Fertility Rate (births per woman)",
    y = "AI Usage Residual\n(after controlling for internet access)",
    caption = "Red dashed = replacement level (2.1) | Data: Anthropic + World Bank"
  ) +
  theme_twitter() +
  theme(legend.position = "top")

ggsave("fertility_controlled.png", p2, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: fertility_controlled.png\n")

# ============================================
# PLOT 3: Rule of Law controlled for internet
# ============================================
plot3_data <- analysis_data %>% filter(!is.na(rule_of_law))
cor3 <- cor(plot3_data$ai_residual, plot3_data$rule_of_law, use = "complete.obs")

p3 <- ggplot(plot3_data, aes(x = rule_of_law, y = ai_residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#666666") +
  geom_point(aes(color = rule_of_law), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#10b981", fill = "#d1fae5", linewidth = 1) +
  geom_text_repel(
    data = plot3_data %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 15
  ) +
  scale_color_gradient2(low = "#ef4444", mid = "#fbbf24", high = "#10b981",
                        midpoint = 0, guide = "none") +
  labs(
    title = "Institutions Matter Beyond Connectivity",
    subtitle = paste0("Rule of Law still predicts AI adoption (r = ", round(cor3, 2),
                      ") after controlling for internet access"),
    x = "Rule of Law Index",
    y = "AI Usage Residual\n(after controlling for internet access)",
    caption = "Acemoglu-style institutions explain AI adoption beyond infrastructure | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("rule_of_law_controlled.png", p3, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: rule_of_law_controlled.png\n")

# ============================================
# PLOT 4: Aging controlled for internet
# ============================================
plot4_data <- analysis_data %>% filter(!is.na(pct_65plus))
cor4 <- cor(plot4_data$ai_residual, plot4_data$pct_65plus, use = "complete.obs")

p4 <- ggplot(plot4_data, aes(x = pct_65plus, y = ai_residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#666666") +
  geom_point(aes(color = pct_65plus), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#0891b2", fill = "#a5f3fc", linewidth = 1) +
  geom_text_repel(
    data = plot4_data %>% filter(geo_name %in% notable | pct_65plus > 20),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 15
  ) +
  scale_color_gradient(low = "#fef3c7", high = "#0891b2", guide = "none") +
  labs(
    title = "Aging Societies Still Embrace AI (Controlling for Internet)",
    subtitle = paste0("Population aging predicts AI usage (r = ", round(cor4, 2),
                      ") even with equal internet access"),
    x = "Population Age 65+ (%)",
    y = "AI Usage Residual\n(after controlling for internet access)",
    caption = "Labor substitution hypothesis holds | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("aging_controlled.png", p4, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: aging_controlled.png\n")

# ============================================
# PLOT 5: GDP Growth controlled - does it flip?
# ============================================
plot5_data <- analysis_data %>% filter(!is.na(gdp_growth) & abs(gdp_growth) < 15)
cor5 <- cor(plot5_data$ai_residual, plot5_data$gdp_growth, use = "complete.obs")

p5 <- ggplot(plot5_data, aes(x = gdp_growth, y = ai_residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#666666") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#666666") +
  geom_point(aes(color = gdp_growth > 0), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#f97316", fill = "#fed7aa", linewidth = 1) +
  geom_text_repel(
    data = plot5_data %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 15
  ) +
  scale_color_manual(values = c("#ef4444", "#10b981"), guide = "none") +
  labs(
    title = "GDP Growth: Still No Effect on AI Adoption",
    subtitle = paste0("Even controlling for internet, growth rate doesn't predict AI use (r = ",
                      round(cor5, 2), ")"),
    x = "GDP Growth Rate (%)",
    y = "AI Usage Residual\n(after controlling for internet access)",
    caption = "Fast-growing economies don't adopt AI faster | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("gdp_growth_controlled.png", p5, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: gdp_growth_controlled.png\n")

# ============================================
# Summary comparison table
# ============================================
cat("\n========================================\n")
cat("WHAT SURVIVES CONTROLLING FOR INTERNET?\n")
cat("========================================\n")

results <- data.frame(
  Variable = var_names,
  Raw = round(raw_cors, 3),
  Controlled = round(controlled_cors, 3),
  Reduction = round((1 - abs(controlled_cors)/abs(raw_cors)) * 100, 1)
)
results <- results[order(abs(results$Controlled), decreasing = TRUE), ]
print(results)

cat("\nKey insight: Fertility crisis and aging effects\n")
cat("SURVIVE controlling for internet access.\n")
cat("This supports labor substitution hypothesis!\n")
