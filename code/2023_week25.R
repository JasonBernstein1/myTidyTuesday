library(tidyverse)
library(tidytuesdayR)
library(ggpmisc)
library(shadowtext)
library(emoGG)

# This script plots a map of the US and indicates states that had
# more UFO sightings than the national average.

tuesdata <- tidytuesdayR::tt_load(2023, week = 25)

ufo_sightings <- tuesdata$ufo_sightings

# count sightings by state
df <- ufo_sightings |>
  filter(country_code == "US") |>
  # change Fl to FL
  count(state_abbreviation = toupper(state)) |>
  arrange(-n) |>
  mutate(
    is_above_average = n > mean(n)
  ) |>
  # bring in full state names
  left_join(
    tibble(
      state_abbreviation = state.abb,
      region = tolower(state.name)
    )
  ) |>
  select(region, state_abbreviation, n, is_above_average)

state_info <- tibble(
  region = tolower(state.name),
  state_abbreviation = state.abb,
  long = state.center$x,
  lat = state.center$y
)

# where to place state abbreviations on map
state_centers <- df |>
  # only keep states that have an above average number of sightings
  left_join(state_info) |>
  filter(is_above_average)

# table of states with most sightings to place on map
top_states <- df |>
  slice_max(n = 7, order_by = n) |>
  select(State = state_abbreviation, Sightings = n)

# dates of first and last sighting
range_dates <- ufo_sightings |>
  pull(reported_date_time) |>
  range() |>
  as.Date() |>
  format(format = "%b %d, %Y")

emoji_map <- data.frame(
  long = c(-122, -79, -72),
  lat = c(31, 47, 21)
)

sightings_map <- map_data("state") |>
  left_join(df)

sightings_map |>
  ggplot() +
  geom_polygon(
    aes(x = long, y = lat, fill = is_above_average, group = group),
    color = "white",
    linewidth = 0.15
  ) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = c("lightgray", "steelblue")) +
  guides(fill = "none") +
  shadowtext::geom_shadowtext(
    data = state_centers,
    aes(x = long, y = lat, label = state_abbreviation),
    size = 3.5,
    col = "white"
  ) +
  emoGG::geom_emoji(
    data = emoji_map,
    aes(x = long, y = lat),
    emoji = "1f6f8"
  ) +
  annotate(
    geom = "text",
    x = -118,
    y = 26.5,
    size = 4,
    label = glue::glue(
      "Average number of\n sightings by state:",
      "{round(mean(df$n))}"
    )
  ) +
  annotate(
    geom = "table",
    x = -64,
    y = 22,
    label = list(top_states),
    size = 3,
    fontface = "bold",
    table.theme = ttheme_gtminimal
  ) +
  labs(
    title = "UFO Sightings in 18 States Exceed the National Average",
    subtitle = glue::glue(
      "Data collected from {range_dates[1]} to {range_dates[2]}"
    ),
    caption = "TidyTuesday: 2023, week 25 | Source: National UFO Reporting Center"
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#b0c4de", color = "#b0c4de"),
    plot.background = element_rect(fill = "#b0c4de", color = "#b0c4de"),
    plot.caption = element_text(hjust = 1.175),
    plot.margin = margin(0, 0, 0.1, 0, "cm"),
    plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5),
    plot.title = element_text(color = "black", size = 18, hjust = 0.5)
  )

ggsave(
  filename = "images/2023_week25.png",
  height = 4,
  width = 7
)
