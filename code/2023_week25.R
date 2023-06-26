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
  filter(country_code == 'US') |>
  # change Fl to FL
  count(state_abb = toupper(state)) |>
  arrange(-n) |>
  mutate(is_above_average = n > mean(n)) |>
  # bring in full state names
  left_join(
    tibble(
      state_abb = state.abb,
      region = tolower(state.name)
    )
  ) |>
  select(region, state_abb, n, is_above_average)

# where to place state abbreviations on map
state_centers <- data.frame(
    region = tolower(state.name),
    state_abb = state.abb,
    long = state.center$x,
    lat =  state.center$y
  ) |>
  # only keep states that have an above average number of sightings
  left_join(df) |>
  filter(is_above_average)

# table of states with most sightings to place on map
top_states <- df |>
  slice_max(n = 7, order_by = n) |>
  select(State = state_abb, Sightings = n)

# dates of first and last sighting
range_dates <- ufo_sightings |>
  pull(reported_date_time) |>
  range() |>
  as.Date() |>
  format(format = "%b %d, %Y")

# where to place emojis on map
coords_emoji <- data.frame(
  long = c(-122, -79, -72),
  lat = c(31, 47, 21)
)

# combine map and sightings data
map_counts <- map_data("state") |>
  left_join(df)

# create map
ggplot() +
  # draw state outlines and color states with above average sightings
  geom_polygon(
    data = map_counts,
    aes(x = long, y = lat, fill = is_above_average, group = group),
    color = "white", linewidth = 0.15) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = c('lightgray', 'steelblue')) +
  guides(fill = 'none') +
  # add state abbreviation labels
  shadowtext::geom_shadowtext(
    data = state_centers,
    aes(x = long, y = lat, label = state_abb),
    size = 3.5, col = 'white'
  ) +
  # add UFO emojis to map
  emoGG::geom_emoji(
    data = coords_emoji,
    aes(x = long, y = lat),
    emoji = "1f6f8"
  ) +
  # add comment giving the average number of sightings over all states
  annotate(
    geom = 'text', x = -118, y = 26.5, size = 4,
    label = glue::glue('Average number of\n sightings by state:',
                       '{round(mean(df$n))}')
  ) +
  # add table with the number of sightings in the top states
  annotate(
    geom = "table", x = -64, y = 22, label = list(top_states),
           size = 3, fontface = "bold", table.theme = ttheme_gtminimal
  ) +
  labs(
    title = 'UFO Sightings in 18 States Exceed the National Average',
    subtitle = glue::glue(
      'Data collected from {range_dates[1]} to {range_dates[2]}'
    ),
    caption = 'TidyTuesday: 2023, week 25 | Source: National UFO Reporting Center'
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#b0c4de', color = '#b0c4de'),
    plot.background = element_rect(fill = '#b0c4de', color = '#b0c4de'),
    plot.caption = element_text(vjust = 2, hjust = 1.175),
    plot.subtitle = element_text(color = 'black', size = 12, hjust = 0.5),
    plot.title = element_text(color = 'black', size = 18, hjust = 0.5)
  )

ggsave(filename = './images/2023_week25.png',
       height = 4, width = 7)
