# Creative AI Correlations - Acemoglu-style Institutional Analysis
# Beyond GDP: What really predicts AI adoption?

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install/load packages
packages <- c("tidyverse", "showtext", "ggrepel", "scales", "patchwork", "corrplot")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(tidyverse)
library(showtext)
library(ggrepel)
library(scales)
library(patchwork)
library(corrplot)

# Modern fonts for Twitter
font_add_google("Inter", "inter")
font_add_google("IBM Plex Sans", "ibm")
showtext_auto()
showtext_opts(dpi = 300)

# Twitter theme
theme_twitter <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "inter") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.3), color = "#1a1a1a", margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.85), color = "#555555", margin = margin(b = 12)),
      plot.caption = element_text(size = rel(0.7), color = "#888888", hjust = 1, margin = margin(t = 12)),
      axis.title = element_text(size = rel(0.85), color = "#333333"),
      axis.text = element_text(size = rel(0.8), color = "#555555"),
      panel.grid.major = element_line(color = "#eeeeee", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "top",
      plot.margin = margin(20, 25, 15, 20)
    )
}

# Helper function to read World Bank data
read_wb_data <- function(pattern, var_name) {
  file <- list.files(pattern = pattern)[1]
  if (is.na(file)) return(NULL)

  df <- read_csv(file, skip = 4, show_col_types = FALSE)

  # Get most recent non-NA value for each country
  df %>%
    select(`Country Code`, starts_with("20")) %>%
    pivot_longer(-`Country Code`, names_to = "year", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(`Country Code`) %>%
    slice_max(year, n = 1) %>%
    ungroup() %>%
    select(geo_id = `Country Code`, !!var_name := value)
}

cat("Loading data...\n")

# Load AI usage data
aei_data <- read_csv("aei_geo.csv", show_col_types = FALSE)

# Get country-level AI metrics
ai_usage <- aei_data %>%
  filter(geography == "country", facet == "country",
         variable %in% c("usage_per_capita", "usage_per_capita_index", "gdp_per_working_age_capita")) %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(usage_per_capita) & usage_per_capita > 0)

# Load World Bank indicators
gdp_growth <- read_wb_data("API_NY.GDP.MKTP.KD.ZG", "gdp_growth")
rule_of_law <- read_wb_data("API_RL.EST", "rule_of_law")
corruption <- read_wb_data("API_CC.EST", "corruption_control")
internet <- read_wb_data("API_IT.NET.USER.ZS", "internet_pct")
tertiary_edu <- read_wb_data("API_SE.TER.ENRR", "tertiary_enrollment")
rd_spending <- read_wb_data("API_GB.XPD.RSDV.GD.ZS", "rd_pct_gdp")
urban <- read_wb_data("API_SP.URB.TOTL.IN.ZS", "urban_pct")

# Merge all data
full_data <- ai_usage %>%
  left_join(gdp_growth, by = "geo_id") %>%
  left_join(rule_of_law, by = "geo_id") %>%
  left_join(corruption, by = "geo_id") %>%
  left_join(internet, by = "geo_id") %>%
  left_join(tertiary_edu, by = "geo_id") %>%
  left_join(rd_spending, by = "geo_id") %>%
  left_join(urban, by = "geo_id")

cat("Countries with complete data:", sum(complete.cases(full_data)), "\n")

# Calculate correlations
cor_vars <- c("usage_per_capita", "gdp_per_working_age_capita", "gdp_growth",
              "rule_of_law", "corruption_control", "internet_pct",
              "tertiary_enrollment", "rd_pct_gdp", "urban_pct")

cor_data <- full_data %>%
  select(all_of(cor_vars)) %>%
  filter(complete.cases(.)) %>%
  mutate(log_usage = log10(usage_per_capita),
         log_gdp = log10(gdp_per_working_age_capita))

# Print correlation matrix
cat("\n=== Correlation Matrix with log(AI Usage) ===\n")
correlations <- cor_data %>%
  select(log_usage, gdp_growth, rule_of_law, corruption_control,
         internet_pct, tertiary_enrollment, rd_pct_gdp, urban_pct) %>%
  cor(use = "pairwise.complete.obs")
print(round(correlations[1,], 3))

# Countries to label
notable <- c("United States", "China", "India", "Germany", "Japan",
             "United Kingdom", "France", "Brazil", "Israel", "Singapore",
             "South Korea", "Rwanda", "Estonia", "Georgia", "Ukraine",
             "Vietnam", "Nigeria", "Kenya", "Chile", "Poland")

# ============================================
# PLOT 1: Rule of Law (Acemoglu-style)
# ============================================
plot_data1 <- full_data %>% filter(!is.na(rule_of_law))
cor1 <- cor(log10(plot_data1$usage_per_capita), plot_data1$rule_of_law, use = "complete.obs")

p1 <- ggplot(plot_data1, aes(x = rule_of_law, y = usage_per_capita)) +
  geom_point(aes(color = rule_of_law), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#dc2626", fill = "#fecaca", linewidth = 1) +
  geom_text_repel(
    data = plot_data1 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 12
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient2(low = "#ef4444", mid = "#fbbf24", high = "#10b981",
                        midpoint = 0, guide = "none") +
  labs(
    title = "Institutions Matter: Rule of Law Predicts AI Adoption",
    subtitle = paste0("Acemoglu was right - institutional quality (r = ", round(cor1, 2),
                      ") rivals GDP in explaining AI usage"),
    x = "Rule of Law Index (World Bank WGI)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Data: Anthropic Economic Index + World Bank WGI 2023"
  ) +
  theme_twitter()

ggsave("rule_of_law_ai.png", p1, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: rule_of_law_ai.png\n")

# ============================================
# PLOT 2: Control of Corruption
# ============================================
plot_data2 <- full_data %>% filter(!is.na(corruption_control))
cor2 <- cor(log10(plot_data2$usage_per_capita), plot_data2$corruption_control, use = "complete.obs")

p2 <- ggplot(plot_data2, aes(x = corruption_control, y = usage_per_capita)) +
  geom_point(aes(color = corruption_control), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#7c3aed", fill = "#ddd6fe", linewidth = 1) +
  geom_text_repel(
    data = plot_data2 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 12
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient2(low = "#ef4444", mid = "#fbbf24", high = "#10b981",
                        midpoint = 0, guide = "none") +
  labs(
    title = "Less Corruption = More AI",
    subtitle = paste0("Control of corruption strongly predicts AI adoption (r = ", round(cor2, 2), ")"),
    x = "Control of Corruption Index",
    y = "Claude AI Usage per Capita (log)",
    caption = "Clean governance enables technology adoption | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("corruption_ai.png", p2, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: corruption_ai.png\n")

# ============================================
# PLOT 3: GDP Growth (Surprising finding?)
# ============================================
plot_data3 <- full_data %>% filter(!is.na(gdp_growth) & abs(gdp_growth) < 15)
cor3 <- cor(log10(plot_data3$usage_per_capita), plot_data3$gdp_growth, use = "complete.obs")

p3 <- ggplot(plot_data3, aes(x = gdp_growth, y = usage_per_capita)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#999999") +
  geom_point(aes(color = gdp_growth > 0), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#0ea5e9", fill = "#bae6fd", linewidth = 1) +
  geom_text_repel(
    data = plot_data3 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 12
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_manual(values = c("#ef4444", "#10b981"), guide = "none") +
  labs(
    title = "Surprising: GDP Growth Weakly Predicts AI Usage",
    subtitle = paste0("Fast-growing economies don't necessarily adopt AI more (r = ", round(cor3, 2), ")"),
    x = "GDP Growth Rate (Annual %)",
    y = "Claude AI Usage per Capita (log)",
    caption = "AI adoption is about level of development, not growth rate | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("gdp_growth_ai.png", p3, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: gdp_growth_ai.png\n")

# ============================================
# PLOT 4: Internet Access
# ============================================
plot_data4 <- full_data %>% filter(!is.na(internet_pct))
cor4 <- cor(log10(plot_data4$usage_per_capita), plot_data4$internet_pct, use = "complete.obs")

p4 <- ggplot(plot_data4, aes(x = internet_pct, y = usage_per_capita)) +
  geom_point(aes(color = internet_pct), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#0891b2", fill = "#a5f3fc", linewidth = 1) +
  geom_text_repel(
    data = plot_data4 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 12
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient(low = "#fde68a", high = "#0891b2", guide = "none") +
  labs(
    title = "Digital Infrastructure Enables AI Adoption",
    subtitle = paste0("Internet penetration is a strong predictor of AI usage (r = ", round(cor4, 2), ")"),
    x = "Internet Users (% of population)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Data: Anthropic Economic Index + World Bank"
  ) +
  theme_twitter()

ggsave("internet_ai.png", p4, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: internet_ai.png\n")

# ============================================
# PLOT 5: R&D Spending
# ============================================
plot_data5 <- full_data %>% filter(!is.na(rd_pct_gdp) & rd_pct_gdp > 0)
cor5 <- cor(log10(plot_data5$usage_per_capita), plot_data5$rd_pct_gdp, use = "complete.obs")

p5 <- ggplot(plot_data5, aes(x = rd_pct_gdp, y = usage_per_capita)) +
  geom_point(aes(color = rd_pct_gdp), size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#8b5cf6", fill = "#ddd6fe", linewidth = 1) +
  geom_text_repel(
    data = plot_data5 %>% filter(geo_name %in% notable | rd_pct_gdp > 3),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient(low = "#fef3c7", high = "#8b5cf6", guide = "none") +
  labs(
    title = "Innovation Breeds AI Adoption",
    subtitle = paste0("R&D spending (% of GDP) strongly correlates with AI usage (r = ", round(cor5, 2), ")"),
    x = "R&D Expenditure (% of GDP)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Countries investing in innovation adopt AI faster | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("rd_spending_ai.png", p5, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: rd_spending_ai.png\n")

# ============================================
# PLOT 6: Tertiary Education
# ============================================
plot_data6 <- full_data %>% filter(!is.na(tertiary_enrollment))
cor6 <- cor(log10(plot_data6$usage_per_capita), plot_data6$tertiary_enrollment, use = "complete.obs")

p6 <- ggplot(plot_data6, aes(x = tertiary_enrollment, y = usage_per_capita)) +
  geom_point(aes(color = tertiary_enrollment), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#f97316", fill = "#fed7aa", linewidth = 1) +
  geom_text_repel(
    data = plot_data6 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 12
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient(low = "#fef3c7", high = "#f97316", guide = "none") +
  labs(
    title = "Education Drives AI Usage",
    subtitle = paste0("Tertiary education enrollment predicts AI adoption (r = ", round(cor6, 2), ")"),
    x = "Tertiary Education Enrollment (%)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Educated populations use AI more | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("education_ai.png", p6, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: education_ai.png\n")

# ============================================
# PLOT 7: Multi-panel correlation summary
# ============================================
cor_summary <- data.frame(
  variable = c("GDP per Capita", "Rule of Law", "Corruption Control",
               "Internet Access", "R&D Spending", "Tertiary Education",
               "Urbanization", "GDP Growth"),
  correlation = c(
    cor(log10(full_data$usage_per_capita), log10(full_data$gdp_per_working_age_capita), use = "complete.obs"),
    cor(log10(full_data$usage_per_capita), full_data$rule_of_law, use = "complete.obs"),
    cor(log10(full_data$usage_per_capita), full_data$corruption_control, use = "complete.obs"),
    cor(log10(full_data$usage_per_capita), full_data$internet_pct, use = "complete.obs"),
    cor(log10(full_data$usage_per_capita), full_data$rd_pct_gdp, use = "complete.obs"),
    cor(log10(full_data$usage_per_capita), full_data$tertiary_enrollment, use = "complete.obs"),
    cor(log10(full_data$usage_per_capita), full_data$urban_pct, use = "complete.obs"),
    cor(log10(full_data$usage_per_capita), full_data$gdp_growth, use = "complete.obs")
  ),
  category = c("Economic", "Institutional", "Institutional",
               "Infrastructure", "Innovation", "Human Capital",
               "Demographic", "Economic")
) %>%
  mutate(variable = fct_reorder(variable, correlation))

p7 <- ggplot(cor_summary, aes(x = correlation, y = variable, fill = category)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, color = "#333333") +
  geom_text(aes(label = round(correlation, 2),
                hjust = ifelse(correlation > 0, -0.2, 1.2)),
            family = "inter", fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Economic" = "#3b82f6",
    "Institutional" = "#10b981",
    "Infrastructure" = "#0891b2",
    "Innovation" = "#8b5cf6",
    "Human Capital" = "#f97316",
    "Demographic" = "#6b7280"
  )) +
  scale_x_continuous(limits = c(-0.2, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "What Predicts AI Adoption? A Multi-Factor Analysis",
    subtitle = "Institutions rival pure GDP in explaining cross-country AI usage",
    x = "Correlation with log(AI Usage per Capita)",
    y = NULL,
    fill = "Category",
    caption = "Acemoglu was right: Institutions matter | Data: Anthropic Economic Index + World Bank"
  ) +
  theme_twitter() +
  theme(legend.position = "right")

ggsave("correlation_summary.png", p7, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: correlation_summary.png\n")

# ============================================
# PLOT 8: Residual analysis - Beyond GDP
# ============================================
# What explains AI adoption AFTER controlling for GDP?
residual_data <- full_data %>%
  filter(!is.na(gdp_per_working_age_capita) & !is.na(usage_per_capita))

model_gdp <- lm(log10(usage_per_capita) ~ log10(gdp_per_working_age_capita), data = residual_data)
residual_data$gdp_residual <- residuals(model_gdp)

# Now correlate residuals with other factors
residual_cors <- data.frame(
  variable = c("Rule of Law", "Corruption Control", "Internet Access",
               "R&D Spending", "Tertiary Education", "GDP Growth"),
  correlation = c(
    cor(residual_data$gdp_residual, residual_data$rule_of_law, use = "complete.obs"),
    cor(residual_data$gdp_residual, residual_data$corruption_control, use = "complete.obs"),
    cor(residual_data$gdp_residual, residual_data$internet_pct, use = "complete.obs"),
    cor(residual_data$gdp_residual, residual_data$rd_pct_gdp, use = "complete.obs"),
    cor(residual_data$gdp_residual, residual_data$tertiary_enrollment, use = "complete.obs"),
    cor(residual_data$gdp_residual, residual_data$gdp_growth, use = "complete.obs")
  )
) %>%
  mutate(variable = fct_reorder(variable, correlation))

p8 <- ggplot(residual_cors, aes(x = correlation, y = variable)) +
  geom_col(aes(fill = correlation > 0), width = 0.7) +
  geom_vline(xintercept = 0, color = "#333333") +
  geom_text(aes(label = round(correlation, 2),
                hjust = ifelse(correlation > 0, -0.2, 1.2)),
            family = "inter", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("#ef4444", "#10b981"), guide = "none") +
  scale_x_continuous(limits = c(-0.3, 0.4)) +
  labs(
    title = "Beyond GDP: What Else Drives AI Adoption?",
    subtitle = "Correlations with AI usage residuals after controlling for GDP per capita",
    x = "Correlation with AI Usage Residual",
    y = NULL,
    caption = "Even after accounting for wealth, R&D and education boost AI adoption | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("beyond_gdp.png", p8, width = 12, height = 6, dpi = 300, bg = "white")
cat("Saved: beyond_gdp.png\n")

# ============================================
# Summary Statistics
# ============================================
cat("\n========================================\n")
cat("SUMMARY: Correlations with log(AI Usage)\n")
cat("========================================\n")
cat(sprintf("%-25s %8s\n", "Variable", "r"))
cat("----------------------------------------\n")
cat(sprintf("%-25s %8.3f\n", "GDP per Capita (log)",
    cor(log10(full_data$usage_per_capita), log10(full_data$gdp_per_working_age_capita), use = "complete.obs")))
cat(sprintf("%-25s %8.3f\n", "Rule of Law",
    cor(log10(full_data$usage_per_capita), full_data$rule_of_law, use = "complete.obs")))
cat(sprintf("%-25s %8.3f\n", "Corruption Control",
    cor(log10(full_data$usage_per_capita), full_data$corruption_control, use = "complete.obs")))
cat(sprintf("%-25s %8.3f\n", "Internet Access",
    cor(log10(full_data$usage_per_capita), full_data$internet_pct, use = "complete.obs")))
cat(sprintf("%-25s %8.3f\n", "R&D Spending",
    cor(log10(full_data$usage_per_capita), full_data$rd_pct_gdp, use = "complete.obs")))
cat(sprintf("%-25s %8.3f\n", "Tertiary Education",
    cor(log10(full_data$usage_per_capita), full_data$tertiary_enrollment, use = "complete.obs")))
cat(sprintf("%-25s %8.3f\n", "Urbanization",
    cor(log10(full_data$usage_per_capita), full_data$urban_pct, use = "complete.obs")))
cat(sprintf("%-25s %8.3f\n", "GDP Growth",
    cor(log10(full_data$usage_per_capita), full_data$gdp_growth, use = "complete.obs")))
cat("========================================\n")

cat("\nAll visualizations complete!\n")
cat("Files created:\n")
cat("  1. rule_of_law_ai.png - Acemoglu-style institutional analysis\n")
cat("  2. corruption_ai.png - Control of corruption\n")
cat("  3. gdp_growth_ai.png - GDP growth (surprising weak correlation)\n")
cat("  4. internet_ai.png - Digital infrastructure\n")
cat("  5. rd_spending_ai.png - R&D investment\n")
cat("  6. education_ai.png - Tertiary education\n")
cat("  7. correlation_summary.png - Multi-factor comparison\n")
cat("  8. beyond_gdp.png - Residual analysis after controlling for GDP\n")
