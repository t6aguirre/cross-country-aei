# Cartogram: World Map Distorted by Claude Usage
# Countries sized by their Claude usage per capita

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install/load required packages
packages <- c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata",
              "cartogram", "showtext", "cowplot")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cartogram)
library(showtext)
library(cowplot)

font_add_google("Libre Baskerville", "cm")
showtext_auto()
showtext_opts(dpi = 300)

cat("Loading Claude usage data...\n")

# Load AI usage data
aei <- read_csv("aei_geo.csv", show_col_types = FALSE)
ai_usage <- aei %>%
  filter(geography == "country", facet == "country",
         variable == "usage_per_capita") %>%
  select(geo_id, geo_name, usage = value) %>%
  filter(!is.na(usage) & usage > 0)

cat("Countries with usage data:", nrow(ai_usage), "\n")

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry) %>%
  filter(iso_a3 != "ATA")  # Remove Antarctica

# Join usage data
world_usage <- world %>%
  left_join(ai_usage, by = c("iso_a3" = "geo_id")) %>%
  mutate(
    usage = replace_na(usage, 0),
    # Scale usage to make distortion visible (multiply by 1M and add base)
    usage_scaled = ifelse(usage > 0, usage * 1000000 + 1, 0.1),
    log_usage = log10(usage + 0.0001)
  )

# Check how many matched
matched <- sum(!is.na(world_usage$geo_name))
cat("Countries matched:", matched, "\n")

# Transform to equal-area projection for cartogram
world_proj <- st_transform(world_usage, "+proj=robin")

cat("Creating cartogram...\n")

# Create Dorling cartogram (circles) - fast and visually striking
carto_dorling <- cartogram_dorling(world_proj, "usage_scaled", k = 0.5)

cat("Cartogram created!\n")

# Color palette
usage_colors <- scale_fill_gradient2(
  low = "#fee2e2",
  mid = "#3b82f6",
  high = "#1e3a8a",
  midpoint = median(log10(ai_usage$usage)),
  na.value = "#e5e5e5",
  name = "Usage per capita\n(log scale)"
)

# Theme
theme_map <- function() {
  theme_void(base_family = "cm") +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5,
                                margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#555555",
                                   margin = margin(b = 15)),
      plot.caption = element_text(size = 9, hjust = 1, color = "#888888",
                                  margin = margin(t = 15)),
      legend.position = "right",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Plot Dorling cartogram (circles - most visually striking)
p_dorling <- ggplot(carto_dorling) +
  geom_sf(aes(fill = log_usage), color = "white", linewidth = 0.3) +
  usage_colors +
  labs(
    title = "The World According to Claude",
    subtitle = "Circle size = Claude usage per capita",
    caption = "Data: Anthropic Economic Index | Dorling cartogram"
  ) +
  theme_map()

ggsave("cartogram_claude_usage.png", p_dorling, width = 16, height = 10, dpi = 300, bg = "white")
cat("Saved: cartogram_claude_usage.png\n")

# Side-by-side comparison
p_normal <- ggplot(world_proj) +
  geom_sf(aes(fill = log_usage), color = "white", linewidth = 0.2) +
  usage_colors +
  labs(
    title = "Normal Map",
    subtitle = "Geographic size"
  ) +
  theme_map() +
  theme(legend.position = "none")

p_carto <- ggplot(carto_dorling) +
  geom_sf(aes(fill = log_usage), color = "white", linewidth = 0.3) +
  usage_colors +
  labs(
    title = "Cartogram",
    subtitle = "Sized by Claude usage"
  ) +
  theme_map()

p_combined <- plot_grid(p_normal, p_carto, ncol = 2, rel_widths = c(0.45, 0.55))

ggsave("cartogram_comparison.png", p_combined, width = 20, height = 8, dpi = 300, bg = "white")
cat("Saved: cartogram_comparison.png\n")

# Print top countries by usage
cat("\nTop 15 countries by Claude usage per capita:\n")
ai_usage %>%
  arrange(desc(usage)) %>%
  head(15) %>%
  mutate(rank = row_number()) %>%
  select(rank, geo_name, usage) %>%
  print()
