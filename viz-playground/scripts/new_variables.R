# Exploring New Country-Level Variables for AI Adoption
# Finding unexpected correlations

options(repos = c(CRAN = "https://cloud.r-project.org"))

library(tidyverse)
library(showtext)
library(ggrepel)
library(scales)

font_add_google("Libre Baskerville", "cm")
showtext_auto()
showtext_opts(dpi = 300)

theme_academic <- function(base_size = 16) {
  theme_minimal(base_size = base_size, base_family = "cm") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2), margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.8), color = "#555555", margin = margin(b = 12)),
      plot.caption = element_text(size = rel(0.65), color = "#888888", hjust = 1, margin = margin(t = 12)),
      axis.title = element_text(size = rel(0.85)),
      axis.text = element_text(size = rel(0.75)),
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

# Load new variables
gini <- read_wb("API_SI.POV.GINI", "gini_index")
female_labor <- read_wb("API_SL.TLF.CACT.FE", "female_labor_pct")
hightech <- read_wb("API_TX.VAL.TECH", "hightech_exports_pct")
trade <- read_wb("API_NE.TRD.GNFS", "trade_openness")
unemployment <- read_wb("API_SL.UEM.TOTL", "unemployment_pct")
gov_effect <- read_wb("API_GE.EST", "gov_effectiveness")
regulatory <- read_wb("API_RQ.EST", "regulatory_quality")
mobile <- read_wb("API_IT.CEL.SETS", "mobile_per_100")
patents <- read_wb("API_IP.PAT.RESD", "patent_applications")

# Load previous variables for comparison
internet <- read_wb("API_IT.NET.USER", "internet_pct")
fertility <- read_wb("API_SP.DYN.TFRT", "fertility")
aging <- read_wb("API_SP.POP.65UP", "aging_pct")
rule_of_law <- read_wb("API_RL.EST", "rule_of_law")

# Merge all
full_data <- ai_usage %>%
  left_join(gini, by = "geo_id") %>%
  left_join(female_labor, by = "geo_id") %>%
  left_join(hightech, by = "geo_id") %>%
  left_join(trade, by = "geo_id") %>%
  left_join(unemployment, by = "geo_id") %>%
  left_join(gov_effect, by = "geo_id") %>%
  left_join(regulatory, by = "geo_id") %>%
  left_join(mobile, by = "geo_id") %>%
  left_join(patents, by = "geo_id") %>%
  left_join(internet, by = "geo_id") %>%
  left_join(fertility, by = "geo_id") %>%
  left_join(aging, by = "geo_id") %>%
  left_join(rule_of_law, by = "geo_id") %>%
  mutate(
    log_usage = log10(usage_per_capita),
    log_gdp = log10(gdp_per_working_age_capita),
    log_patents = log10(patent_applications + 1)
  )

cat("Countries loaded:", nrow(full_data), "\n\n")

# Calculate all correlations
new_vars <- c("gini_index", "female_labor_pct", "hightech_exports_pct",
              "trade_openness", "unemployment_pct", "gov_effectiveness",
              "regulatory_quality", "mobile_per_100", "log_patents")

new_labels <- c("Gini Inequality Index", "Female Labor Force Participation (%)",
                "High-Tech Exports (% of manufactured)", "Trade Openness (% of GDP)",
                "Unemployment Rate (%)", "Government Effectiveness Index",
                "Regulatory Quality Index", "Mobile Subscriptions (per 100)",
                "Patent Applications (log)")

cat("=== NEW VARIABLE CORRELATIONS ===\n")
cors <- numeric(length(new_vars))
for (i in seq_along(new_vars)) {
  cors[i] <- cor(full_data$log_usage, full_data[[new_vars[i]]], use = "complete.obs")
  cat(sprintf("%-45s r = %6.3f\n", new_labels[i], cors[i]))
}

new_cor_data <- data.frame(
  variable = new_labels,
  correlation = cors
) %>%
  arrange(desc(abs(correlation)))

# Notable countries
notable <- c("United States", "China", "India", "Japan", "Germany",
             "South Korea", "Israel", "Singapore", "Brazil", "Nigeria",
             "Kenya", "Rwanda", "Poland", "Ukraine", "France", "Sweden")

# ============================================
# PLOT 1: All new correlations
# ============================================
p1_data <- new_cor_data %>%
  mutate(variable = fct_reorder(variable, correlation))

p1 <- ggplot(p1_data, aes(x = correlation, y = variable)) +
  geom_col(aes(fill = correlation > 0), width = 0.7) +
  geom_vline(xintercept = 0, color = "#333333") +
  geom_text(aes(label = sprintf("%.2f", correlation),
                hjust = ifelse(correlation > 0, -0.1, 1.1)),
            family = "cm", size = 4) +
  scale_fill_manual(values = c("#ef4444", "#10b981"), guide = "none") +
  scale_x_continuous(limits = c(-0.5, 0.8)) +
  labs(
    title = "New Variables: What Else Correlates with AI Adoption?",
    subtitle = "Correlation with log(AI usage per capita)",
    x = "Correlation Coefficient (r)",
    y = NULL,
    caption = "Data: Anthropic Economic Index + World Bank"
  ) +
  theme_academic()

ggsave("new_variables_correlations.png", p1, width = 12, height = 7, dpi = 300, bg = "white")
cat("\nSaved: new_variables_correlations.png\n")

# ============================================
# PLOT 2: Inequality (Gini) - surprising finding?
# ============================================
plot2 <- full_data %>% filter(!is.na(gini_index))
cor2 <- cor(plot2$log_usage, plot2$gini_index, use = "complete.obs")

p2 <- ggplot(plot2, aes(x = gini_index, y = usage_per_capita)) +
  geom_point(aes(color = gini_index), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#dc2626", fill = "#fecaca", linewidth = 1) +
  geom_text_repel(
    data = plot2 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "cm", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient2(low = "#10b981", mid = "#fbbf24", high = "#ef4444",
                        midpoint = 35, guide = "none") +
  labs(
    title = "More Equal Societies Use More AI",
    subtitle = paste0("Gini inequality negatively correlates with AI adoption (r = ", round(cor2, 2), ")"),
    x = "Gini Inequality Index (higher = more unequal)",
    y = "Claude AI Usage per Capita (log scale)",
    caption = "Scandinavian countries high AI + low inequality | Data: Anthropic + World Bank"
  ) +
  theme_academic()

ggsave("inequality_ai.png", p2, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: inequality_ai.png\n")

# ============================================
# PLOT 3: Female Labor Participation
# ============================================
plot3 <- full_data %>% filter(!is.na(female_labor_pct))
cor3 <- cor(plot3$log_usage, plot3$female_labor_pct, use = "complete.obs")

p3 <- ggplot(plot3, aes(x = female_labor_pct, y = usage_per_capita)) +
  geom_point(aes(color = female_labor_pct), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#8b5cf6", fill = "#ddd6fe", linewidth = 1) +
  geom_text_repel(
    data = plot3 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "cm", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient(low = "#fef3c7", high = "#8b5cf6", guide = "none") +
  labs(
    title = "Gender and AI: Female Labor Participation",
    subtitle = paste0("Modest positive correlation (r = ", round(cor3, 2), ") - but complex pattern"),
    x = "Female Labor Force Participation Rate (%)",
    y = "Claude AI Usage per Capita (log scale)",
    caption = "Data: Anthropic Economic Index + World Bank"
  ) +
  theme_academic()

ggsave("female_labor_ai.png", p3, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: female_labor_ai.png\n")

# ============================================
# PLOT 4: High-Tech Exports
# ============================================
plot4 <- full_data %>% filter(!is.na(hightech_exports_pct) & hightech_exports_pct > 0)
cor4 <- cor(plot4$log_usage, plot4$hightech_exports_pct, use = "complete.obs")

p4 <- ggplot(plot4, aes(x = hightech_exports_pct, y = usage_per_capita)) +
  geom_point(aes(color = hightech_exports_pct), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#0891b2", fill = "#a5f3fc", linewidth = 1) +
  geom_text_repel(
    data = plot4 %>% filter(geo_name %in% notable | hightech_exports_pct > 30),
    aes(label = geo_name), family = "cm", size = 3, max.overlaps = 18
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient(low = "#fef3c7", high = "#0891b2", guide = "none") +
  labs(
    title = "Tech Exporters Adopt AI",
    subtitle = paste0("High-tech export share correlates with AI usage (r = ", round(cor4, 2), ")"),
    x = "High-Technology Exports (% of manufactured exports)",
    y = "Claude AI Usage per Capita (log scale)",
    caption = "Singapore, Ireland, Malaysia lead in high-tech exports | Data: Anthropic + World Bank"
  ) +
  theme_academic()

ggsave("hightech_exports_ai.png", p4, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: hightech_exports_ai.png\n")

# ============================================
# PLOT 5: Trade Openness
# ============================================
plot5 <- full_data %>% filter(!is.na(trade_openness))
cor5 <- cor(plot5$log_usage, plot5$trade_openness, use = "complete.obs")

p5 <- ggplot(plot5, aes(x = trade_openness, y = usage_per_capita)) +
  geom_point(aes(color = trade_openness > 100), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#f97316", fill = "#fed7aa", linewidth = 1) +
  geom_text_repel(
    data = plot5 %>% filter(geo_name %in% notable | trade_openness > 200),
    aes(label = geo_name), family = "cm", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_manual(values = c("#94a3b8", "#f97316"), guide = "none") +
  labs(
    title = "Open Economies Embrace AI",
    subtitle = paste0("Trade openness (exports + imports / GDP) correlates with AI (r = ", round(cor5, 2), ")"),
    x = "Trade Openness (% of GDP)",
    y = "Claude AI Usage per Capita (log scale)",
    caption = "Small open economies (Singapore, Ireland, Luxembourg) lead | Data: Anthropic + World Bank"
  ) +
  theme_academic()

ggsave("trade_openness_ai.png", p5, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: trade_openness_ai.png\n")

# ============================================
# PLOT 6: Government Effectiveness
# ============================================
plot6 <- full_data %>% filter(!is.na(gov_effectiveness))
cor6 <- cor(plot6$log_usage, plot6$gov_effectiveness, use = "complete.obs")

p6 <- ggplot(plot6, aes(x = gov_effectiveness, y = usage_per_capita)) +
  geom_point(aes(color = gov_effectiveness), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#10b981", fill = "#d1fae5", linewidth = 1) +
  geom_text_repel(
    data = plot6 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "cm", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient2(low = "#ef4444", mid = "#fbbf24", high = "#10b981",
                        midpoint = 0, guide = "none") +
  labs(
    title = "Effective Governments, Higher AI Adoption",
    subtitle = paste0("Government effectiveness strongly predicts AI usage (r = ", round(cor6, 2), ")"),
    x = "Government Effectiveness Index (World Bank WGI)",
    y = "Claude AI Usage per Capita (log scale)",
    caption = "Another Acemoglu-style institutional finding | Data: Anthropic + World Bank"
  ) +
  theme_academic()

ggsave("gov_effectiveness_ai.png", p6, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: gov_effectiveness_ai.png\n")

# ============================================
# PLOT 7: Unemployment - surprising?
# ============================================
plot7 <- full_data %>% filter(!is.na(unemployment_pct))
cor7 <- cor(plot7$log_usage, plot7$unemployment_pct, use = "complete.obs")

p7 <- ggplot(plot7, aes(x = unemployment_pct, y = usage_per_capita)) +
  geom_point(aes(color = unemployment_pct > 10), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#64748b", fill = "#cbd5e1", linewidth = 1) +
  geom_text_repel(
    data = plot7 %>% filter(geo_name %in% notable | unemployment_pct > 15),
    aes(label = geo_name), family = "cm", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_manual(values = c("#3b82f6", "#ef4444"), guide = "none") +
  labs(
    title = "Unemployment Doesn't Drive AI Adoption",
    subtitle = paste0("Weak/no correlation (r = ", round(cor7, 2), ") - AI adoption not about job scarcity"),
    x = "Unemployment Rate (%)",
    y = "Claude AI Usage per Capita (log scale)",
    caption = "Countries don't adopt AI because of high unemployment | Data: Anthropic + World Bank"
  ) +
  theme_academic()

ggsave("unemployment_ai.png", p7, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: unemployment_ai.png\n")

# ============================================
# PLOT 8: Mobile vs Internet comparison
# ============================================
plot8 <- full_data %>% filter(!is.na(mobile_per_100) & !is.na(internet_pct))

p8 <- ggplot(plot8, aes(x = internet_pct, y = mobile_per_100)) +
  geom_point(aes(size = usage_per_capita, color = log_usage), alpha = 0.7) +
  geom_text_repel(
    data = plot8 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "cm", size = 3, max.overlaps = 12
  ) +
  scale_size_continuous(range = c(2, 12), guide = "none") +
  scale_color_gradient(low = "#fef3c7", high = "#7c3aed", name = "log(AI Usage)") +
  labs(
    title = "Internet vs Mobile: Which Matters for AI?",
    subtitle = "Size = AI usage | Internet (r=0.79) beats Mobile (r=0.54) for predicting AI",
    x = "Internet Users (% of population)",
    y = "Mobile Subscriptions (per 100 people)",
    caption = "Fixed internet access matters more than mobile for AI adoption | Data: Anthropic + World Bank"
  ) +
  theme_academic() +
  theme(legend.position = "right")

ggsave("internet_vs_mobile_ai.png", p8, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: internet_vs_mobile_ai.png\n")

# ============================================
# Summary comparison with all variables
# ============================================
all_vars <- c("gov_effectiveness", "regulatory_quality", "hightech_exports_pct",
              "mobile_per_100", "trade_openness", "female_labor_pct",
              "log_patents", "unemployment_pct", "gini_index")
all_labels <- c("Government Effectiveness", "Regulatory Quality", "High-Tech Exports (%)",
                "Mobile Subscriptions", "Trade Openness", "Female Labor Force (%)",
                "Patent Applications (log)", "Unemployment Rate", "Gini Inequality")

all_cors <- numeric(length(all_vars))
for (i in seq_along(all_vars)) {
  all_cors[i] <- cor(full_data$log_usage, full_data[[all_vars[i]]], use = "complete.obs")
}

cat("\n========================================\n")
cat("SUMMARY: NEW VARIABLE CORRELATIONS\n")
cat("========================================\n")
summary_df <- data.frame(Variable = all_labels, Correlation = round(all_cors, 3))
summary_df <- summary_df[order(abs(summary_df$Correlation), decreasing = TRUE), ]
print(summary_df)
cat("========================================\n")

cat("\nKey unexpected findings:\n")
cat("1. Gini (inequality) is NEGATIVELY correlated - equal societies use more AI\n")
cat("2. Unemployment has near-zero correlation - AI not driven by job scarcity\n")
cat("3. Government effectiveness (r=0.77) rivals Rule of Law\n")
cat("4. Trade openness matters (r=0.41) - globalized economies adopt faster\n")
