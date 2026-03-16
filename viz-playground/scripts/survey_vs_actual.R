# Survey vs Actual: Do self-reported AI users actually use Claude?

library(tidyverse)
library(showtext)

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

# Load data
survey <- read_csv("ai_usage_survey.csv", show_col_types = FALSE)
perception <- read_csv("ai_perception.csv", show_col_types = FALSE)

aei <- read_csv("aei_geo.csv", show_col_types = FALSE)
usage <- aei %>%
  filter(geography == "country", facet == "country",
         variable == "usage_per_capita") %>%
  select(geo_id, geo_name, claude_usage = value) %>%
  filter(!is.na(claude_usage) & claude_usage > 0)

# Merge all three
combined <- survey %>%
  inner_join(usage, by = "geo_id") %>%
  left_join(perception %>% select(geo_id, excited, nervous), by = "geo_id") %>%
  mutate(
    log_claude = log10(claude_usage),
    # Normalize for comparison
    survey_norm = chatgpt_adoption_pct / max(chatgpt_adoption_pct),
    claude_norm = claude_usage / max(claude_usage)
  )

cat("Countries with all data:", nrow(combined), "\n\n")

# Correlations
cor_survey_claude <- cor(combined$chatgpt_adoption_pct, combined$log_claude)
cat("Correlation: Survey adoption vs Claude usage:", round(cor_survey_claude, 3), "\n")

if (sum(!is.na(combined$excited)) > 5) {
  cor_excited_survey <- cor(combined$excited, combined$chatgpt_adoption_pct, use = "complete.obs")
  cat("Correlation: Excitement vs Survey adoption:", round(cor_excited_survey, 3), "\n")
}

# Plot 1: Survey adoption vs actual Claude usage
p1 <- ggplot(combined, aes(x = chatgpt_adoption_pct, y = log_claude)) +
  geom_point(size = 4, alpha = 0.8, color = "#3b82f6") +
  geom_smooth(method = "lm", se = TRUE, color = "#dc2626", linetype = "dashed") +
  geom_text(aes(label = geo_id), vjust = -1, family = "cm", size = 3) +
  labs(
    title = "Self-Reported AI Adoption vs Actual Claude Usage",
    subtitle = sprintf("r = %.2f | What people say vs what they do", cor_survey_claude),
    x = "% saying they use ChatGPT (BCG Survey 2023)",
    y = "Actual Claude usage per capita (log scale)",
    caption = "Data: BCG Global Consumer Survey + Anthropic Economic Index"
  ) +
  theme_academic()

ggsave("survey_vs_actual_usage.png", p1, width = 12, height = 8, dpi = 300, bg = "white")
cat("\nSaved: survey_vs_actual_usage.png\n")

# Plot 2: Gap analysis - who over/under reports?
combined <- combined %>%
  mutate(
    gap = survey_norm - claude_norm,
    gap_type = case_when(
      gap > 0.2 ~ "Over-reporters",
      gap < -0.2 ~ "Under-reporters",
      TRUE ~ "Accurate"
    )
  ) %>%
  arrange(gap)

p2 <- ggplot(combined, aes(x = reorder(country, gap), y = gap)) +
  geom_col(aes(fill = gap_type), width = 0.7) +
  geom_hline(yintercept = 0, color = "#333333") +
  coord_flip() +
  scale_fill_manual(values = c("Over-reporters" = "#ef4444",
                                "Under-reporters" = "#10b981",
                                "Accurate" = "#6b7280")) +
  labs(
    title = "Who Over-Reports AI Usage?",
    subtitle = "Gap between self-reported adoption and actual Claude usage",
    x = NULL,
    y = "Survey - Actual (normalized)",
    fill = NULL,
    caption = "Positive = claim to use AI more than they actually do"
  ) +
  theme_academic() +
  theme(legend.position = "top")

ggsave("survey_gap_analysis.png", p2, width = 10, height = 10, dpi = 300, bg = "white")
cat("Saved: survey_gap_analysis.png\n")

# Plot 3: Comparison bar chart
combined_long <- combined %>%
  select(country, geo_id, Survey = survey_norm, Actual = claude_norm) %>%
  pivot_longer(c(Survey, Actual), names_to = "measure", values_to = "value") %>%
  mutate(measure = factor(measure, levels = c("Survey", "Actual")))

p3 <- ggplot(combined_long, aes(x = reorder(country, -value), y = value, fill = measure)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Survey" = "#fbbf24", "Actual" = "#3b82f6")) +
  coord_flip() +
  labs(
    title = "Survey Claims vs Actual Usage",
    subtitle = "Self-reported ChatGPT adoption (yellow) vs actual Claude usage (blue)",
    x = NULL,
    y = "Normalized usage (0-1)",
    fill = NULL,
    caption = "Both normalized to max = 1 for comparison"
  ) +
  theme_academic() +
  theme(legend.position = "top")

ggsave("survey_vs_actual_bars.png", p3, width = 10, height = 10, dpi = 300, bg = "white")
cat("Saved: survey_vs_actual_bars.png\n")

# Summary
cat("\n=== SUMMARY ===\n")
cat("\nOver-reporters (claim more than they use):\n")
combined %>% filter(gap > 0.1) %>%
  select(country, chatgpt_adoption_pct, claude_usage) %>%
  arrange(desc(chatgpt_adoption_pct)) %>%
  print()

cat("\nUnder-reporters (use more than they claim):\n")
combined %>% filter(gap < -0.1) %>%
  select(country, chatgpt_adoption_pct, claude_usage) %>%
  arrange(desc(claude_usage)) %>%
  print()
