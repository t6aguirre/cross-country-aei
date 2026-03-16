# AI Perception vs Claude Usage
# Do countries that are excited about AI actually use Claude more?

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
perception <- read_csv("ai_perception.csv", show_col_types = FALSE)

aei <- read_csv("aei_geo.csv", show_col_types = FALSE)
usage <- aei %>%
  filter(geography == "country", facet == "country",
         variable == "usage_per_capita") %>%
  select(geo_id, geo_name, usage = value) %>%
  filter(!is.na(usage) & usage > 0)

# Merge
combined <- perception %>%
  inner_join(usage, by = "geo_id") %>%
  mutate(
    log_usage = log10(usage),
    net_sentiment = excited - nervous  # positive = more excited than nervous
  )

cat("Countries matched:", nrow(combined), "\n\n")

# Correlation
cor_excited <- cor(combined$excited, combined$log_usage)
cor_nervous <- cor(combined$nervous, combined$log_usage)
cor_net <- cor(combined$net_sentiment, combined$log_usage)

cat("Correlations with log(Claude usage):\n")
cat("  Excited about AI:", round(cor_excited, 3), "\n")
cat("  Nervous about AI:", round(cor_nervous, 3), "\n")
cat("  Net sentiment:", round(cor_net, 3), "\n\n")

# Plot 1: Excitement vs Usage
p1 <- ggplot(combined, aes(x = excited, y = log_usage)) +
  geom_point(aes(color = nervous), size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#dc2626", linetype = "dashed") +
  geom_text(aes(label = geo_id), vjust = -1, family = "cm", size = 3) +
  scale_color_gradient(low = "#10b981", high = "#ef4444", name = "% Nervous") +
  labs(
    title = "AI Excitement vs Claude Usage",
    subtitle = sprintf("r = %.2f | Countries excited about AI don't necessarily use Claude more", cor_excited),
    x = "% Excited about AI products/services",
    y = "Claude usage per capita (log scale)",
    caption = "Data: Ipsos AI Monitor 2024 + Anthropic Economic Index"
  ) +
  theme_academic()

ggsave("perception_excited_vs_usage.png", p1, width = 12, height = 8, dpi = 300, bg = "white")
cat("Saved: perception_excited_vs_usage.png\n")

# Plot 2: Net sentiment (excited - nervous) vs Usage
p2 <- ggplot(combined, aes(x = net_sentiment, y = log_usage)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#888888") +
  geom_point(aes(color = net_sentiment > 0), size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#3b82f6") +
  geom_text(aes(label = geo_id), vjust = -1, family = "cm", size = 3) +
  scale_color_manual(values = c("#ef4444", "#10b981"),
                     labels = c("More nervous", "More excited"),
                     name = "Net sentiment") +
  labs(
    title = "AI Sentiment vs Claude Usage: The Paradox",
    subtitle = sprintf("r = %.2f | Countries nervous about AI actually use Claude MORE", cor_net),
    x = "Net AI sentiment (% excited - % nervous)",
    y = "Claude usage per capita (log scale)",
    caption = "Negative = more nervous than excited | Data: Ipsos + Anthropic"
  ) +
  theme_academic()

ggsave("perception_sentiment_vs_usage.png", p2, width = 12, height = 8, dpi = 300, bg = "white")
cat("Saved: perception_sentiment_vs_usage.png\n")

# Plot 3: Quadrant chart
combined <- combined %>%
  mutate(
    quadrant = case_when(
      excited > 50 & log_usage > median(log_usage) ~ "Excited & High Usage",
      excited > 50 & log_usage <= median(log_usage) ~ "Excited but Low Usage",
      excited <= 50 & log_usage > median(log_usage) ~ "Skeptical but High Usage",
      TRUE ~ "Skeptical & Low Usage"
    )
  )

p3 <- ggplot(combined, aes(x = excited, y = log_usage)) +
  geom_hline(yintercept = median(combined$log_usage), linetype = "dashed", color = "#888888") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "#888888") +
  geom_point(aes(color = quadrant), size = 5, alpha = 0.8) +
  geom_text(aes(label = geo_id), vjust = -1.2, family = "cm", size = 3) +
  scale_color_manual(values = c(
    "Excited & High Usage" = "#10b981",
    "Excited but Low Usage" = "#fbbf24",
    "Skeptical but High Usage" = "#3b82f6",
    "Skeptical & Low Usage" = "#ef4444"
  )) +
  annotate("text", x = 75, y = max(combined$log_usage), label = "Excited & Using",
           family = "cm", color = "#10b981", fontface = "bold", hjust = 1) +
  annotate("text", x = 35, y = max(combined$log_usage), label = "Skeptical but Using",
           family = "cm", color = "#3b82f6", fontface = "bold", hjust = 0) +
  annotate("text", x = 75, y = min(combined$log_usage), label = "Excited but Not Using",
           family = "cm", color = "#fbbf24", fontface = "bold", hjust = 1) +
  annotate("text", x = 35, y = min(combined$log_usage), label = "Skeptical & Not Using",
           family = "cm", color = "#ef4444", fontface = "bold", hjust = 0) +
  labs(
    title = "The AI Adoption Paradox",
    subtitle = "Rich skeptical countries use AI more than excited developing countries",
    x = "% Excited about AI",
    y = "Claude usage per capita (log scale)",
    caption = "Data: Ipsos AI Monitor 2024 + Anthropic Economic Index"
  ) +
  theme_academic() +
  theme(legend.position = "none")

ggsave("perception_quadrant.png", p3, width = 12, height = 9, dpi = 300, bg = "white")
cat("Saved: perception_quadrant.png\n")

# Summary table
cat("\nCountry breakdown:\n")
combined %>%
  arrange(desc(log_usage)) %>%
  select(country, excited, nervous, net_sentiment, usage) %>%
  mutate(usage = sprintf("%.6f", usage)) %>%
  print(n = 31)
