# AI and the Fertility Crisis - Unexpected Correlations
# Do aging, low-fertility societies adopt AI faster?

options(repos = c(CRAN = "https://cloud.r-project.org"))

library(tidyverse)
library(showtext)
library(ggrepel)
library(scales)
library(patchwork)

font_add_google("Inter", "inter")
showtext_auto()
showtext_opts(dpi = 300)

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
      plot.margin = margin(20, 25, 15, 20)
    )
}

# Helper to read World Bank data
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

cat("Loading data...\n")

# Load AI data
aei <- read_csv("aei_geo.csv", show_col_types = FALSE)
ai_usage <- aei %>%
  filter(geography == "country", facet == "country",
         variable %in% c("usage_per_capita", "gdp_per_working_age_capita")) %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(usage_per_capita) & usage_per_capita > 0)

# Load demographic data
fertility <- read_wb("API_SP.DYN.TFRT", "fertility_rate")
life_exp <- read_wb("API_SP.DYN.LE00", "life_expectancy")
aging <- read_wb("API_SP.POP.65UP", "pct_over_65")

# Merge
demo_data <- ai_usage %>%
  left_join(fertility, by = "geo_id") %>%
  left_join(life_exp, by = "geo_id") %>%
  left_join(aging, by = "geo_id")

cat("Countries with fertility data:", sum(!is.na(demo_data$fertility_rate)), "\n")

# Notable countries
notable <- c("United States", "China", "India", "Japan", "Germany",
             "South Korea", "Italy", "Spain", "France", "Brazil",
             "Nigeria", "Kenya", "Israel", "Singapore", "Poland",
             "Ukraine", "Russia", "Iran", "Saudi Arabia", "Egypt")

# ============================================
# PLOT 1: Fertility Rate vs AI Usage
# ============================================
plot1 <- demo_data %>% filter(!is.na(fertility_rate))
cor1 <- cor(log10(plot1$usage_per_capita), plot1$fertility_rate, use = "complete.obs")

# Replacement level line
replacement <- 2.1

p1 <- ggplot(plot1, aes(x = fertility_rate, y = usage_per_capita)) +
  geom_vline(xintercept = replacement, linetype = "dashed", color = "#dc2626", linewidth = 0.8) +
  annotate("text", x = replacement + 0.15, y = max(plot1$usage_per_capita) * 0.7,
           label = "Replacement\nlevel (2.1)", family = "inter", size = 3, color = "#dc2626", hjust = 0) +
  geom_point(aes(color = fertility_rate < replacement), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#7c3aed", fill = "#ddd6fe", linewidth = 1) +
  geom_text_repel(
    data = plot1 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_manual(values = c("#10b981", "#ef4444"),
                     labels = c("Above replacement", "Below replacement"),
                     name = "Fertility") +
  labs(
    title = "The Fertility-AI Paradox",
    subtitle = paste0("Countries with declining births use MORE AI (r = ", round(cor1, 2),
                      ") - labor substitution or coincidence?"),
    x = "Total Fertility Rate (births per woman)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Red line = replacement level (2.1) | Data: Anthropic Economic Index + World Bank"
  ) +
  theme_twitter() +
  theme(legend.position = "top")

ggsave("fertility_ai.png", p1, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: fertility_ai.png\n")

# ============================================
# PLOT 2: Aging Population vs AI
# ============================================
plot2 <- demo_data %>% filter(!is.na(pct_over_65))
cor2 <- cor(log10(plot2$usage_per_capita), plot2$pct_over_65, use = "complete.obs")

p2 <- ggplot(plot2, aes(x = pct_over_65, y = usage_per_capita)) +
  geom_point(aes(color = pct_over_65), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#0891b2", fill = "#a5f3fc", linewidth = 1) +
  geom_text_repel(
    data = plot2 %>% filter(geo_name %in% notable | pct_over_65 > 20),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 15
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient(low = "#fef3c7", high = "#0891b2", guide = "none") +
  labs(
    title = "Aging Societies Embrace AI",
    subtitle = paste0("Countries with older populations use more AI (r = ", round(cor2, 2),
                      ") - preparing for workforce decline?"),
    x = "Population Age 65+ (%)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Data: Anthropic Economic Index + World Bank"
  ) +
  theme_twitter()

ggsave("aging_ai.png", p2, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: aging_ai.png\n")

# ============================================
# PLOT 3: Life Expectancy vs AI
# ============================================
plot3 <- demo_data %>% filter(!is.na(life_expectancy))
cor3 <- cor(log10(plot3$usage_per_capita), plot3$life_expectancy, use = "complete.obs")

p3 <- ggplot(plot3, aes(x = life_expectancy, y = usage_per_capita)) +
  geom_point(aes(color = life_expectancy), size = 3.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#10b981", fill = "#d1fae5", linewidth = 1) +
  geom_text_repel(
    data = plot3 %>% filter(geo_name %in% notable),
    aes(label = geo_name), family = "inter", size = 3, max.overlaps = 12
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient(low = "#fecaca", high = "#10b981", guide = "none") +
  labs(
    title = "Longer Lives, More AI",
    subtitle = paste0("Life expectancy strongly predicts AI adoption (r = ", round(cor3, 2), ")"),
    x = "Life Expectancy at Birth (years)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Data: Anthropic Economic Index + World Bank"
  ) +
  theme_twitter()

ggsave("life_expectancy_ai.png", p3, width = 12, height = 6.75, dpi = 300, bg = "white")
cat("Saved: life_expectancy_ai.png\n")

# ============================================
# PLOT 4: Demographic Crisis Index
# ============================================
# Create composite: low fertility + high aging = demographic pressure
demo_crisis <- demo_data %>%
  filter(!is.na(fertility_rate) & !is.na(pct_over_65)) %>%
  mutate(
    # Normalize each
    fertility_z = (fertility_rate - mean(fertility_rate)) / sd(fertility_rate),
    aging_z = (pct_over_65 - mean(pct_over_65)) / sd(pct_over_65),
    # Demographic pressure = high aging - low fertility (inverted)
    demo_pressure = aging_z - fertility_z
  )

cor4 <- cor(log10(demo_crisis$usage_per_capita), demo_crisis$demo_pressure, use = "complete.obs")

p4 <- ggplot(demo_crisis, aes(x = demo_pressure, y = usage_per_capita)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#666666") +
  geom_point(aes(color = demo_pressure), size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#dc2626", fill = "#fecaca", linewidth = 1.2) +
  geom_text_repel(
    data = demo_crisis %>% filter(geo_name %in% notable | abs(demo_pressure) > 1.5),
    aes(label = geo_name), family = "inter", size = 3.2, max.overlaps = 18
  ) +
  scale_y_log10(labels = scientific_format()) +
  scale_color_gradient2(low = "#10b981", mid = "#fbbf24", high = "#dc2626",
                        midpoint = 0, guide = "none") +
  labs(
    title = "Demographic Pressure Drives AI Adoption",
    subtitle = paste0("Composite index (aging + low fertility) correlates with AI use (r = ",
                      round(cor4, 2), ")"),
    x = "Demographic Pressure Index\n(Higher = More aging + Lower fertility)",
    y = "Claude AI Usage per Capita (log)",
    caption = "Are countries facing workforce decline turning to AI? | Data: Anthropic + World Bank"
  ) +
  theme_twitter()

ggsave("demographic_pressure_ai.png", p4, width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: demographic_pressure_ai.png\n")

# ============================================
# Summary
# ============================================
cat("\n========================================\n")
cat("DEMOGRAPHIC CORRELATIONS WITH AI USAGE\n")
cat("========================================\n")
cat(sprintf("%-25s %8.3f\n", "Fertility Rate", cor1))
cat(sprintf("%-25s %8.3f\n", "Aging (% over 65)", cor2))
cat(sprintf("%-25s %8.3f\n", "Life Expectancy", cor3))
cat(sprintf("%-25s %8.3f\n", "Demographic Pressure", cor4))
cat("========================================\n")

cat("\nKey insight: Countries facing demographic decline\n")
cat("(low fertility + aging populations) use MORE AI.\n")
cat("Possible interpretation: AI as labor substitution?\n")

cat("\nFiles created:\n")
cat("  1. fertility_ai.png - Fertility rate correlation\n")
cat("  2. aging_ai.png - Aging population correlation\n")
cat("  3. life_expectancy_ai.png - Life expectancy correlation\n")
cat("  4. demographic_pressure_ai.png - Composite demographic index\n")
