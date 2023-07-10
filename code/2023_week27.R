library(tidyverse)
library(tidytuesdayR)
library(ggrepel)

# This script plots a map of the US and indicates locations of synagogues
# with historic markers.

tuesdata <- tidytuesdayR::tt_load(2023, week = 27)

df <- tuesdata$historical_markers |>
  filter(str_detect(title, 'Synagogue')) |>
  rename(
    lat = latitude_minus_s,
    long = longitude_minus_w
  ) |>
  mutate(
    id = rank(long),
    id_title = glue::glue('{id}. {title}')
  ) |>
  arrange(id) |>
  select(id, city_or_town, state_or_prov, long, lat, id_title)

# labels to place on map indicating locations of historic markers
# three markers in Columbia, South Carolina, so condense these to one label
map_labels <- df |>
  summarise(
    id = paste(id, collapse = ', '),
    .by = c(city_or_town, state_or_prov),
    lat = mean(lat),
    long = mean(long)
  ) |>
  mutate(id_location = glue::glue("{id}. {city_or_town}, {state_or_prov}"))

# map of US states combined with historic marker information
states_map <- map_data('state') |>
  # add variable indicating whether states contain synagogues or not
  mutate(has_synagogue = region %in% tolower(map_labels$state_or_prov))

# text for first 8 markers to place on west side of map
id_titles_west <- df |>
  filter(id <= 8) |>
  pull(id_title) |>
  paste(collapse = '\n')

# text for last 8 markers to place on east side of map
id_titles_east <- df |>
  filter(id >= 9) |>
  pull(id_title) |>
  paste(collapse = '\n')

# start with map of Canada and Mexico
ggplot(map_data('world', region = c('Canada', 'Mexico'))) +
  geom_polygon(
    aes(x = long, y = lat, group = group),
    fill = 'seashell', color = 'black', linewidth = 0.2
  ) +
  # add state outlines and add color to states with synagogues
  geom_polygon(
    data = states_map,
    aes(x = long, y = lat, group = group, fill = has_synagogue),
    color = 'black', linewidth = 0.2
  ) +
  scale_fill_manual(values = c('white', 'thistle1')) +
  # indicate locations of historic markers with blue dots
  geom_point(
    data = df, aes(x = long, y = lat),
    shape = 16, size = 1.2, alpha = 1, col = 'steelblue'
  ) +
  # add labels for location (city, state) of historic markers
  ggrepel::geom_label_repel(
    data = map_labels, aes(x = long, y = lat, label = id_location),
    size = 2, fill = 'lightyellow', seed = 1,
    segment.colour = 'red', min.segment.length = 0
  ) +
  # set map limits and orient map so south is 'up'
  coord_cartesian(
    xlim = c(-133, -62),
    ylim = rev(c(21, 52))
  ) +
  # add description of map to top left corner
  annotate(
    geom = 'text', x = -135, y = 23.5, size = 6, hjust = 0, fontface = 2,
    label = 'Locations of Synagogues\nwith Historic Markers \nin the United States'
  ) +
  # add historic marker titles to left and right sides of map
  annotate(
    geom = "text", x = -135, y = 31.5, label = list(id_titles_west),
    size = 2.25, hjust = 0
  ) +
  annotate(
    geom = "text", x = -79.5, y = 25, label = list(id_titles_east),
    size = 2.25, hjust = 0
  ) +
  # add caption
  annotate(
    geom = 'text', x = -68.5, y = 52.5, size = 1.8,
    label = 'TidyTuesday: 2023, week 27 | Source: Historical Marker Database'
  ) +
  theme_void() +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill = 'lightskyblue1', color = NA),
  )

ggsave(filename = './images/2023_week27.png',
       height = 5, width = 9)
