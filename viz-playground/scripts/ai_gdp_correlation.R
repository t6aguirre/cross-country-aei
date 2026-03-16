# AI Usage vs GDP Correlation Analysis
# Using Anthropic Economic Index Data
# Twitter-optimized visualization with modern fonts

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install packages if needed
packages <- c("tidyverse", "showtext", "ggrepel", "scales", "patchwork", "ggtext")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tidyverse)
library(showtext)
library(ggrepel)
library(scales)
library(patchwork)
library(ggtext)

# Add Google Fonts - modern, clean fonts for Twitter
font_add_google("Inter", "inter")
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()
showtext_opts(dpi = 300)

# Twitter-optimized theme
theme_twitter <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "inter") +
    theme(
      plot.title = element_text(
        family = "inter",
        face = "bold",
        size = rel(1.4),
        margin = margin(b = 10),
        color = "#1a1a1a"
      ),
      plot.subtitle = element_text(
        family = "inter",
        size = rel(0.9),
        color = "#666666",
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        family = "inter",
        size = rel(0.7),
        color = "#999999",
        hjust = 1,
        margin = margin(t = 15)
      ),
      axis.title = element_text(
        family = "inter",
        size = rel(0.85),
        color = "#333333"
      ),
      axis.text = element_text(
        family = "inter",
        size = rel(0.8),
        color = "#555555"
      ),
      legend.title = element_text(
        family = "inter",
        face = "bold",
        size = rel(0.85)
      ),
      legend.text = element_text(
        family = "inter",
        size = rel(0.8)
      ),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 25, 20, 20)
    )
}

# Load the Anthropic Economic Index data
aei_data <- read_csv("aei_geo.csv")

# Filter for country-level usage data with GDP
country_gdp_usage <- aei_data %>%
  filter(
    geography == "country",
    facet == "country",
    variable %in% c("gdp_per_working_age_capita", "usage_per_capita", "usage_per_capita_index")
  ) %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(gdp_per_working_age_capita) & !is.na(usage_per_capita)) %>%
  filter(usage_per_capita > 0)  # Only countries with actual usage

# Calculate correlation
correlation <- cor(
  log10(country_gdp_usage$gdp_per_working_age_capita),
  log10(country_gdp_usage$usage_per_capita),
  use = "complete.obs"
)

cat("Correlation between log(GDP per capita) and log(AI usage per capita):", round(correlation, 3), "\n")

# Identify key countries to label
top_gdp <- country_gdp_usage %>%
  slice_max(gdp_per_working_age_capita, n = 5) %>%
  pull(geo_name)

top_usage <- country_gdp_usage %>%
  slice_max(usage_per_capita, n = 5) %>%
  pull(geo_name)

notable_countries <- c("United States", "United Kingdom", "Germany", "France",
                       "Japan", "China", "India", "Brazil", "Canada",
                       "Australia", "South Korea", "Singapore", "Switzerland",
                       "Netherlands", "Sweden", "Israel", "Ireland")

countries_to_label <- country_gdp_usage %>%
  filter(geo_name %in% c(top_gdp, top_usage, notable_countries))

# Plot 1: AI Usage vs GDP scatter plot
p1 <- ggplot(country_gdp_usage, aes(x = gdp_per_working_age_capita, y = usage_per_capita)) +
  geom_point(
    aes(size = usage_per_capita_index),
    alpha = 0.6,
    color = "#7c3aed"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#dc2626",
    fill = "#fecaca",
    linewidth = 1.2
  ) +
  geom_text_repel(
    data = countries_to_label,
    aes(label = geo_name),
    family = "inter",
    size = 3.5,
    max.overlaps = 15,
    segment.color = "#999999",
    segment.size = 0.3,
    box.padding = 0.5
  ) +
  scale_x_log10(
    labels = dollar_format(scale = 0.001, suffix = "K"),
    breaks = c(1000, 5000, 10000, 25000, 50000, 100000)
  ) +
  scale_y_log10(
    labels = scientific_format()
  ) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  labs(
    title = "Wealthier Countries Use More AI",
    subtitle = paste0("Claude AI usage per capita strongly correlates with GDP (r = ",
                      round(correlation, 2), ")"),
    x = "GDP per Working-Age Capita (log scale)",
    y = "Claude AI Usage per Capita (log scale)",
    caption = "Data: Anthropic Economic Index (Aug 2025) | @your_handle"
  ) +
  theme_twitter() +
  annotate(
    "text",
    x = 2000, y = max(country_gdp_usage$usage_per_capita) * 0.5,
    label = paste0("r = ", round(correlation, 2)),
    family = "inter",
    fontface = "bold",
    size = 6,
    color = "#dc2626"
  )

# Save Plot 1
ggsave(
  "ai_gdp_correlation.png",
  p1,
  width = 12,
  height = 6.75,  # Twitter optimal ratio 16:9
  dpi = 300,
  bg = "white"
)

cat("Saved: ai_gdp_correlation.png\n")

# Load task percentage data for automation analysis
task_data <- read_csv("task_pct.csv")
automation_data <- read_csv("automation_augmentation.csv")

# Merge task data with automation patterns
task_analysis <- task_data %>%
  inner_join(automation_data, by = "task_name") %>%
  mutate(
    automation_score = directive + filtered,
    augmentation_score = feedback_loop + task_iteration + validation + learning
  )

# Plot 2: Top tasks by usage with automation breakdown
top_tasks <- task_analysis %>%
  slice_max(pct, n = 20) %>%
  mutate(
    task_short = str_trunc(task_name, 60),
    task_short = fct_reorder(task_short, pct)
  )

p2 <- ggplot(top_tasks, aes(x = pct, y = task_short)) +
  geom_col(
    aes(fill = automation_score),
    width = 0.7
  ) +
  scale_fill_gradient2(
    low = "#10b981",
    mid = "#fbbf24",
    high = "#ef4444",
    midpoint = 0.5,
    name = "Automation\nScore",
    limits = c(0, 1)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = "What Tasks Do People Use AI For?",
    subtitle = "Top 20 O*NET tasks by Claude usage share | Color = automation vs augmentation",
    x = "% of Total AI Usage",
    y = NULL,
    caption = "Green = Augmentation (human-AI collaboration) | Red = Automation | Data: Anthropic Economic Index"
  ) +
  theme_twitter() +
  theme(
    axis.text.y = element_text(size = rel(0.7)),
    legend.position = c(0.85, 0.3)
  )

ggsave(
  "ai_tasks_breakdown.png",
  p2,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("Saved: ai_tasks_breakdown.png\n")

# Plot 3: Unexpected correlation - GDP growth potential
# Countries with high AI adoption relative to GDP might show more growth potential
country_gdp_usage <- country_gdp_usage %>%
  mutate(
    ai_adoption_residual = residuals(lm(log10(usage_per_capita) ~ log10(gdp_per_working_age_capita)))
  )

outliers <- country_gdp_usage %>%
  filter(abs(ai_adoption_residual) > 0.5)

p3 <- ggplot(country_gdp_usage, aes(x = gdp_per_working_age_capita, y = ai_adoption_residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999") +
  geom_point(
    aes(color = ai_adoption_residual > 0),
    size = 4,
    alpha = 0.7
  ) +
  geom_text_repel(
    data = outliers,
    aes(label = geo_name),
    family = "inter",
    size = 3.5,
    max.overlaps = 20
  ) +
  scale_color_manual(
    values = c("#ef4444", "#10b981"),
    labels = c("Below expected", "Above expected"),
    name = "AI Adoption"
  ) +
  scale_x_log10(labels = dollar_format(scale = 0.001, suffix = "K")) +
  labs(
    title = "Which Countries Punch Above Their Weight in AI Adoption?",
    subtitle = "Residuals from GDP-usage regression: positive = higher AI adoption than GDP predicts",
    x = "GDP per Working-Age Capita (log scale)",
    y = "AI Adoption Residual\n(Higher = More AI adoption than expected)",
    caption = "Data: Anthropic Economic Index (Aug 2025) | Analysis reveals unexpected AI leaders"
  ) +
  theme_twitter() +
  theme(legend.position = "top")

ggsave(
  "ai_adoption_outliers.png",
  p3,
  width = 12,
  height = 6.75,
  dpi = 300,
  bg = "white"
)

cat("Saved: ai_adoption_outliers.png\n")

# Summary statistics
cat("\n=== Summary Statistics ===\n")
cat("Total countries analyzed:", nrow(country_gdp_usage), "\n")
cat("GDP-AI Usage Correlation:", round(correlation, 3), "\n")

cat("\nTop 10 Countries by AI Usage per Capita:\n")
country_gdp_usage %>%
  slice_max(usage_per_capita, n = 10) %>%
  select(geo_name, gdp_per_working_age_capita, usage_per_capita) %>%
  print()

cat("\nCountries with highest AI adoption relative to GDP (positive outliers):\n")
country_gdp_usage %>%
  slice_max(ai_adoption_residual, n = 10) %>%
  select(geo_name, gdp_per_working_age_capita, ai_adoption_residual) %>%
  print()

cat("\n=== Visualizations Complete ===\n")
cat("Files saved:\n")
cat("  1. ai_gdp_correlation.png - GDP vs AI usage scatter\n")
cat("  2. ai_tasks_breakdown.png - Top tasks by usage\n")
cat("  3. ai_adoption_outliers.png - Unexpected AI adopters\n")
