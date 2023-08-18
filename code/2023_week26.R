library(tidyverse)
library(tidytuesdayR)
library(cowplot)
library(usmap)

# This script creates a bar plot of the number of historic places by state,
# showing the ten states with the highest number, and overlays a map of the US
# that indicates the states using the cowplot package.

tuesdata <- tidytuesdayR::tt_load(2023, week = 26)

us_place_names <- tuesdata$us_place_names
us_place_history <- tuesdata$us_place_history

# plot colors and font
bg_col <- "#08476B"
fill_col <- "#9CD1F9"
font <- "Helvetica"

# select states for places that have historic information
df <- us_place_names |>
  # filter out places without historic information
  right_join(us_place_history) |>
  filter(!is.na(history)) |>
  # order states for bar plot by frequency in data set
  mutate(
    state_name = state_name |>
      fct_infreq() |>
      fct_rev()
  ) |>
  select(state_name)

# ten states with the most historic places in the data set
top_ten_states <- df |>
  count(state_name) |>
  slice_max(n = 10, order_by = n) |>
  pull(state_name)

alaska_map <- usmap::plot_usmap(include = "AK", fill = fill_col, color = NA) +
  theme_void()

hawaii_map <- usmap::plot_usmap(include = "HI", fill = "white", color = NA) +
  theme_void()

us_contiguous_map <- map_data("state") |>
  ggplot() +
  geom_polygon(
    aes(
      x = long, y = lat, group = group,
      fill = region %in% tolower(top_ten_states)
    ),
    color = bg_col, linewidth = 0.2
  ) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = c("white", fill_col)) +
  theme_void() +
  theme(legend.position = "none")

base_bar_plot <- df |>
  filter(state_name %in% top_ten_states) |>
  ggplot(aes(y = state_name)) +
  geom_bar(fill = fill_col) +
  geom_text(
    stat = "count",
    aes(
      label = after_stat(count),
      x = after_stat(count) + 15 * (after_stat(count) < 1000) - 80
    ),
    vjust = 0.5, col = bg_col, size = 8, family = font
  ) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Number of Historic Places by State",
    caption = "TidyTuesday: 2023, week 26 | Source: USGS National Map"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      size = 22, hjust = 1, color = "white",
      family = font
    ),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = bg_col),
    plot.caption = element_text(color = "white", family = font),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.25, l = 0.5, "cm"),
    plot.title = element_text(
      hjust = 0.16, size = 26, color = "white",
      family = font
    )
  )

# combine bar plot with maps of contiguous US, Alaska, and Hawaii
cowplot::ggdraw(base_bar_plot) +
  cowplot::draw_plot(us_contiguous_map, x = 0.625, y = 0.05, width = 0.4, height = 0.4) +
  cowplot::draw_plot(alaska_map, x = 0.65, y = 0.03, width = 0.15, height = 0.15) +
  cowplot::draw_plot(hawaii_map, x = 0.85, y = 0.09, width = 0.07, height = 0.05)

ggsave(
  filename = "images/2023_week26.png",
  height = 8, width = 10
)
