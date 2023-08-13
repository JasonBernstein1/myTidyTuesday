library(tidyverse)
library(tidytuesdayR)
library(tigris)

# This script creates a map of population density by state.

tuesdata <- tidytuesdayR::tt_load(2023, week = 31)

df <- tuesdata$states |>
  mutate(
    popn_density = population_2020 / total_area_mi2,
    state = tolower(state),
    decile = cut(popn_density,
      breaks = quantile(popn_density, seq(0, 1, by = 0.1)),
      labels = paste0(
        seq(0, 90, by = 10), "-",
        seq(10, 100, by = 10), "%"
      ),
      include.lowest = TRUE
    )
  ) |>
  select(state, postal_abbreviation, popn_density, decile)

# map colors
bg_color <- "#282c36" # bluish-black
gray <- "gray100"
fill_colors <- colorRampPalette(c("lightgrey", "darkgrey", "tomato1"))(10)

us_map <- tigris::states(class = "sf", cb = TRUE) |>
  # move Alaska and Hawaii closer to continental US
  tigris::shift_geometry() |>
  # keep US states, remove US territories
  filter(GEOID < 60) |>
  mutate(
    NAME = tolower(NAME)
  ) |>
  left_join(df, by = c("NAME" = "state"))

highest_popn_density_states <- df |>
  slice_max(n = 5, order_by = popn_density) |>
  pull(postal_abbreviation) |>
  paste(collapse = ", ")

lowest_popn_density_states <- df |>
  slice_min(n = 5, order_by = popn_density) |>
  pull(postal_abbreviation) |>
  paste(collapse = ", ")

us_map |>
  ggplot(aes(fill = decile)) +
  geom_sf(color = bg_color) +
  coord_sf(crs = 5070, expand = T) +
  scale_fill_manual(values = fill_colors, na.translate = F) +
  labs(
    fill = "Population\nDensity",
    title = "Population Density by State in 2020",
    subtitle = glue::glue(
      "Highest: {highest_popn_density_states}; ",
      "Lowest: {lowest_popn_density_states}"
    ),
    caption = "TidyTuesday: 2023, week 31 | Source: Wikipedia"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_dark() +
  theme(
    legend.background = element_rect(fill = bg_color),
    legend.key = element_blank(),
    legend.text = element_text(color = gray, size = 12),
    legend.title = element_text(color = gray, size = 13, hjust = 0.5),
    panel.background = element_rect(fill = bg_color),
    # light lat/lon lines contrast with dark background
    panel.grid.major = element_line(color = "gray80", linewidth = 0.1),
    plot.background = element_rect(fill = bg_color, color = bg_color),
    plot.caption = element_text(color = gray),
    plot.subtitle = element_text(color = gray, size = 15),
    plot.title = element_text(color = gray, size = 22)
  )

ggsave(
  filename = "./images/2023_week31.png",
  height = 6, width = 10
)
