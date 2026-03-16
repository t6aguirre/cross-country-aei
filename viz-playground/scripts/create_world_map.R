library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(showtext)

font_add_google("Libre Baskerville", "cm")
showtext_auto()
showtext_opts(dpi = 300)

# Load data
aei <- read_csv("data/anthropic/aei_geo.csv", show_col_types = FALSE)

usage <- aei %>%
  filter(geography == "country", facet == "country", variable == "usage_per_capita") %>%
  select(geo_id, usage = value) %>%
  filter(!is.na(usage) & usage > 0) %>%
  mutate(log_usage = log10(usage))

auto_data <- aei %>%
  filter(variable == "automation_pct", geography == "country") %>%
  select(geo_id, automation_pct = value)

gdp_pc <- aei %>%
  filter(geography == "country", facet == "country", variable == "gdp_per_working_age_capita") %>%
  select(geo_id, gdp_pc = value) %>%
  mutate(log_gdp = log10(gdp_pc))

# Calculate residuals from GDP model
combined <- usage %>%
  left_join(auto_data, by = "geo_id") %>%
  left_join(gdp_pc, by = "geo_id")

# Residual from GDP-only model (who over/underperforms)
# Need to handle NAs properly
combined_complete <- combined %>% filter(!is.na(log_gdp))
m <- lm(log_usage ~ log_gdp, data = combined_complete)
combined_complete$resid <- residuals(m)

# Merge back
combined <- combined %>%
  left_join(combined_complete %>% select(geo_id, resid), by = "geo_id")

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry) %>%
  rename(geo_id = iso_a3)

# Merge with data
map_data <- world %>%
  left_join(combined, by = "geo_id")

# Theme for maps
theme_map <- function() {
  theme_minimal(base_family = "cm") +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#555555"),
      plot.caption = element_text(size = 8, color = "#888888"),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#f0f0f0", color = NA)
    )
}

# Map 1: Automation rate
p1 <- ggplot(map_data) +
  geom_sf(aes(fill = automation_pct), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 55,
    na.value = "#d9d9d9",
    name = "Automation %",
    limits = c(35, 75),
    oob = scales::squish
  ) +
  coord_sf(crs = "+proj=robin") +
  labs(
    title = "How Countries Use Claude: Automation vs Augmentation",
    subtitle = "Red = more task replacement | Blue = more task enhancement",
    caption = "Data: Anthropic Economic Index. Gray = no data."
  ) +
  theme_map()

ggsave("figures/map_automation.png", p1, width = 14, height = 8, dpi = 300, bg = "white")
cat("Saved: figures/map_automation.png\n")

# Map 2: Residuals (over/underperformers vs GDP)
p2 <- ggplot(map_data) +
  geom_sf(aes(fill = resid), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#d73027", mid = "#ffffbf", high = "#1a9850",
    midpoint = 0,
    na.value = "#d9d9d9",
    name = "Residual\n(log scale)",
    limits = c(-1.5, 1.5),
    oob = scales::squish
  ) +
  coord_sf(crs = "+proj=robin") +
  labs(
    title = "Who Uses Claude More (or Less) Than GDP Predicts?",
    subtitle = "Green = overperformer | Red = underperformer | Controlling for GDP per capita",
    caption = "Data: Anthropic Economic Index. Residuals from log(usage) ~ log(GDP). Gray = no data."
  ) +
  theme_map()

ggsave("figures/map_residuals.png", p2, width = 14, height = 8, dpi = 300, bg = "white")
cat("Saved: figures/map_residuals.png\n")

# Map 3: Log usage (raw adoption)
p3 <- ggplot(map_data) +
  geom_sf(aes(fill = log_usage), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "#d9d9d9",
    name = "Log Usage\nper Capita",
    direction = -1
  ) +
  coord_sf(crs = "+proj=robin") +
  labs(
    title = "Claude Usage Around the World",
    subtitle = "Usage per capita (log scale) | Darker = higher usage",
    caption = "Data: Anthropic Economic Index. Gray = no data."
  ) +
  theme_map()

ggsave("figures/map_usage.png", p3, width = 14, height = 8, dpi = 300, bg = "white")
cat("Saved: figures/map_usage.png\n")

cat("\nDone! Created 3 maps.\n")
