# Digging for curious findings in the AEI data

library(tidyverse)
library(showtext)

font_add_google("Libre Baskerville", "cm")
showtext_auto()
showtext_opts(dpi = 300)

theme_academic <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "cm") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.1), margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.8), color = "#555555", margin = margin(b = 12)),
      plot.caption = element_text(size = rel(0.6), color = "#888888", hjust = 1),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

aei <- read_csv("aei_geo.csv", show_col_types = FALSE)

cat("=== EXPLORING CURIOUS PATTERNS ===\n\n")

# 1. AUTOMATION VS AUGMENTATION BY COUNTRY
cat("1. AUTOMATION VS AUGMENTATION\n")
cat("   (Are some countries using Claude to replace work vs enhance it?)\n\n")

auto_aug <- aei %>%
  filter(variable %in% c("automation_pct", "augmentation_pct"),
         geography == "country") %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(automation_pct)) %>%
  mutate(auto_ratio = automation_pct / (automation_pct + augmentation_pct))

# Get usage data to filter for meaningful sample sizes
usage <- aei %>%
  filter(variable == "usage_count", geography == "country", facet == "country") %>%
  select(geo_id, usage_count = value)

auto_aug <- auto_aug %>%
  left_join(usage, by = "geo_id") %>%
  filter(usage_count > 500)  # Only countries with decent sample

cat("Highest automation (replacing human work):\n")
auto_aug %>% arrange(desc(automation_pct)) %>% head(10) %>%
  select(geo_name, automation_pct, augmentation_pct) %>% print()

cat("\nHighest augmentation (enhancing human work):\n")
auto_aug %>% arrange(desc(augmentation_pct)) %>% head(10) %>%
  select(geo_name, automation_pct, augmentation_pct) %>% print()

# Plot
p1 <- auto_aug %>%
  mutate(geo_name = fct_reorder(geo_name, automation_pct)) %>%
  ggplot(aes(x = automation_pct, y = augmentation_pct)) +
  geom_point(aes(size = usage_count), alpha = 0.6, color = "#3b82f6") +
  geom_text(aes(label = geo_id), vjust = -0.8, size = 2.5, family = "cm") +
  geom_abline(slope = -1, intercept = 100, linetype = "dashed", color = "gray50") +
  labs(
    title = "Automation vs Augmentation by Country",
    subtitle = "Do countries use Claude to replace work or enhance it?",
    x = "% Automation (replacing human tasks)",
    y = "% Augmentation (enhancing human work)",
    size = "Usage count",
    caption = "Countries with >500 conversations"
  ) +
  theme_academic()

ggsave("automation_vs_augmentation.png", p1, width = 11, height = 8, dpi = 300, bg = "white")
cat("\nSaved: automation_vs_augmentation.png\n")

# 2. COLLABORATION STYLES BY COUNTRY
cat("\n\n2. COLLABORATION STYLES\n")
cat("   (How do people interact with Claude differently?)\n\n")

collab <- aei %>%
  filter(variable == "collaboration_pct", geography == "country") %>%
  select(geo_id, geo_name, style = cluster_name, pct = value) %>%
  filter(!is.na(pct), style != "not_classified")

# Get top countries by usage
top_countries <- usage %>% arrange(desc(usage_count)) %>% head(20) %>% pull(geo_id)

collab_top <- collab %>%
  filter(geo_id %in% top_countries) %>%
  group_by(geo_id, geo_name) %>%
  mutate(total = sum(pct)) %>%
  ungroup()

# Find countries with unusual patterns
collab_wide <- collab %>%
  pivot_wider(names_from = style, values_from = pct) %>%
  filter(geo_id %in% top_countries)

cat("Collaboration styles (top 20 countries by usage):\n")
print(collab_wide)

# Most directive countries (just telling Claude what to do)
cat("\nMost directive (command-style):\n")
collab_wide %>% arrange(desc(directive)) %>% head(5) %>%
  select(geo_name, directive, `feedback loop`, learning) %>% print()

# Most learning-oriented
cat("\nMost learning-oriented:\n")
collab_wide %>% arrange(desc(learning)) %>% head(5) %>%
  select(geo_name, directive, learning, validation) %>% print()

# Most iterative (back-and-forth)
cat("\nMost iterative (task iteration + feedback loop):\n")
collab_wide %>%
  mutate(iterative = `task iteration` + `feedback loop`) %>%
  arrange(desc(iterative)) %>% head(5) %>%
  select(geo_name, `task iteration`, `feedback loop`, iterative) %>% print()

# Plot collaboration styles
p2 <- collab_top %>%
  filter(style %in% c("directive", "learning", "task iteration", "feedback loop")) %>%
  ggplot(aes(x = reorder(geo_name, -pct), y = pct, fill = style)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "How Countries Collaborate with Claude",
    subtitle = "Directive = commands | Learning = asking to understand | Iteration = back-and-forth",
    x = NULL, y = "% of conversations",
    fill = "Style"
  ) +
  theme_academic()

ggsave("collaboration_styles.png", p2, width = 12, height = 10, dpi = 300, bg = "white")
cat("\nSaved: collaboration_styles.png\n")

# 3. WHAT ARE PEOPLE ASKING CLAUDE TO DO?
cat("\n\n3. TOP REQUESTS BY COUNTRY\n")
cat("   (What tasks differ across countries?)\n\n")

requests <- aei %>%
  filter(variable == "request_pct", geography == "country", level == 2) %>%
  select(geo_id, geo_name, request = cluster_name, pct = value) %>%
  filter(!is.na(pct), !grepl("not_classified", request))

# Top requests globally
cat("Top requests globally:\n")
requests %>%
  group_by(request) %>%
  summarize(avg_pct = mean(pct)) %>%
  arrange(desc(avg_pct)) %>%
  head(10) %>%
  print()

# Find countries with unusual request patterns
requests_wide <- requests %>%
  filter(geo_id %in% top_countries) %>%
  pivot_wider(names_from = request, values_from = pct)

# Simplify column names for display
colnames(requests_wide) <- gsub("Help |Provide |across multiple.*|and technologies|professional ", "", colnames(requests_wide))

cat("\nWeb development heavy (relative):\n")
if ("develop and debug complete web applications and user interfaces" %in% names(requests_wide)) {
  requests_wide %>%
    arrange(desc(`develop and debug complete web applications and user interfaces`)) %>%
    head(5) %>%
    select(geo_name, 3:5) %>%
    print()
}

# 4. OCCUPATION PATTERNS
cat("\n\n4. OCCUPATIONS USING CLAUDE\n\n")

occupations <- aei %>%
  filter(variable == "soc_pct", geography == "country") %>%
  select(geo_id, geo_name, occupation = cluster_name, pct = value) %>%
  filter(!is.na(pct), !grepl("not_classified", occupation))

# Top occupations globally
cat("Top occupations globally:\n")
occupations %>%
  group_by(occupation) %>%
  summarize(avg_pct = mean(pct), countries = n()) %>%
  arrange(desc(avg_pct)) %>%
  head(10) %>%
  print()

# Countries with high non-tech usage
cat("\nCountries with diverse (non-tech-dominated) usage:\n")
occupations %>%
  filter(geo_id %in% top_countries) %>%
  group_by(geo_id, geo_name) %>%
  summarize(
    tech_pct = sum(pct[grepl("Computer|Mathematical", occupation)]),
    non_tech_pct = sum(pct[!grepl("Computer|Mathematical|not_class", occupation)]),
    .groups = "drop"
  ) %>%
  arrange(desc(non_tech_pct)) %>%
  head(10) %>%
  print()

# 5. GDP VS USAGE RESIDUALS - WHO OUTPERFORMS?
cat("\n\n5. WHO PUNCHES ABOVE THEIR WEIGHT?\n")
cat("   (Countries using Claude more than GDP predicts)\n\n")

gdp_usage <- aei %>%
  filter(variable %in% c("gdp_per_working_age_capita", "usage_per_capita"),
         geography == "country", facet == "country") %>%
  select(geo_id, geo_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!is.na(usage_per_capita), usage_per_capita > 0,
         !is.na(gdp_per_working_age_capita)) %>%
  mutate(
    log_gdp = log10(gdp_per_working_age_capita),
    log_usage = log10(usage_per_capita)
  )

# Simple regression
model <- lm(log_usage ~ log_gdp, data = gdp_usage)
gdp_usage$residual <- residuals(model)
gdp_usage$predicted <- fitted(model)

cat("Biggest overperformers (use Claude more than GDP predicts):\n")
gdp_usage %>%
  arrange(desc(residual)) %>%
  head(15) %>%
  select(geo_name, gdp_per_working_age_capita, usage_per_capita, residual) %>%
  mutate(gdp_per_working_age_capita = round(gdp_per_working_age_capita),
         usage_per_capita = signif(usage_per_capita, 3),
         residual = round(residual, 2)) %>%
  print()

cat("\nBiggest underperformers:\n")
gdp_usage %>%
  arrange(residual) %>%
  head(15) %>%
  select(geo_name, gdp_per_working_age_capita, usage_per_capita, residual) %>%
  mutate(gdp_per_working_age_capita = round(gdp_per_working_age_capita),
         usage_per_capita = signif(usage_per_capita, 3),
         residual = round(residual, 2)) %>%
  print()

# Plot
p3 <- ggplot(gdp_usage, aes(x = log_gdp, y = log_usage)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray70", fill = "gray90") +
  geom_point(aes(color = residual), size = 3, alpha = 0.8) +
  geom_text(data = gdp_usage %>% filter(abs(residual) > 0.3),
            aes(label = geo_id), vjust = -1, size = 3, family = "cm") +
  scale_color_gradient2(low = "#ef4444", mid = "gray70", high = "#10b981",
                        midpoint = 0, name = "Residual") +
  labs(
    title = "Who Punches Above Their Weight?",
    subtitle = "Countries above the line use Claude more than GDP predicts",
    x = "Log GDP per working-age capita",
    y = "Log Claude usage per capita",
    caption = "Green = overperformers | Red = underperformers"
  ) +
  theme_academic()

ggsave("gdp_residuals.png", p3, width = 11, height = 8, dpi = 300, bg = "white")
cat("\nSaved: gdp_residuals.png\n")

# 6. CORRELATION: AUTOMATION WITH GDP
cat("\n\n6. DOES WEALTH PREDICT AUTOMATION VS AUGMENTATION?\n\n")

auto_gdp <- auto_aug %>%
  left_join(gdp_usage %>% select(geo_id, log_gdp), by = "geo_id") %>%
  filter(!is.na(log_gdp))

cor_auto <- cor(auto_gdp$log_gdp, auto_gdp$automation_pct)
cat("Correlation GDP vs Automation:", round(cor_auto, 3), "\n")
cat("(Positive = richer countries automate more)\n")

p4 <- ggplot(auto_gdp, aes(x = log_gdp, y = automation_pct)) +
  geom_point(size = 3, alpha = 0.7, color = "#3b82f6") +
  geom_smooth(method = "lm", se = TRUE, color = "#dc2626") +
  geom_text(aes(label = geo_id), vjust = -1, size = 2.5, family = "cm") +
  labs(
    title = sprintf("Wealth vs Automation (r = %.2f)", cor_auto),
    subtitle = "Do richer countries use Claude more for automation?",
    x = "Log GDP per capita",
    y = "% Automation",
    caption = "Automation = using Claude to replace human tasks"
  ) +
  theme_academic()

ggsave("gdp_vs_automation.png", p4, width = 10, height = 7, dpi = 300, bg = "white")
cat("Saved: gdp_vs_automation.png\n")

cat("\n=== DONE ===\n")
