library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(patchwork)
library(ggmap)

# This script plots locations of squirrel sightings on a map of central
# park and creates histograms of sighting locations (tree, rock, etc.) by
# distance across the park, measured length-wise. The distance calculation
# is approximate.

tuesdata <- tidytuesdayR::tt_load(2023, week = 21)

# wrangle observation and location data for plot
df <- tuesdata$squirrel_data |>
  janitor::clean_names() |>
  rename(
    lon = x,
    lat = y
  ) |>
  # only keep records that have comments about location of squirrel
  filter(!is.na(specific_location)) |>
  # location variable indicates whether comment mentions tree, rock, etc.
  mutate(
    comment_location = tolower(specific_location),
    location = case_when(
      str_detect(comment_location, 'tree|trunk') ~ 'tree',
      str_detect(comment_location, 'rock') ~ 'rock',
      str_detect(comment_location, 'fence') ~ 'fence',
      str_detect(comment_location, 'bench') ~ 'bench',
      str_detect(comment_location, 'branch|log') ~ 'branch',
      str_detect(comment_location, 'grass|field|lawn') ~ 'grass',
      str_detect(comment_location, 'road') ~ 'road',
      str_detect(comment_location, 'bush') ~ 'bush',
      TRUE ~ 'other'
    )
  ) |>
  # format facet labels and add record counts by location
  mutate(location = str_to_sentence(location)) |>
  add_count(location, name = 'n') |>
  mutate(facet_label = glue::glue("{location} (n = {n})") |>
           fct_reorder(n) |>
           fct_rev()) |>
  select(lon, lat, location, facet_label)

# we plot an arrow with these coordinates along the side of the park
lon1 <- -73.981865 # start of arrow, Columbus Circle
lat1 <- 40.768050
lon2 <- -73.958194 # end of arrow, Cathedral Parkway 110th St.
lat2 <- 40.800590

# computes the distance from a point (x, y) to a line defined by
# points (x1, y1) and (x2, y2)
compDistPointToLine = function(x, y, x1, y1, x2, y2) {
  length_line <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
  d <- abs((x2 - x1) * (y1 - y) - (x1 - x) * (y2 - y1)) / length_line
  return(d)
}

# compute distances of observations across park length-wise,
# use Pythagorean theorem since this distance is the length of a side
# in a right triangle
df_dists <- df |>
  mutate(
    # distance from observations to their projection onto arrow
    dist_obs_to_proj = compDistPointToLine(lon, lat, lon1, lat1, lon2, lat2),
    # distance from start of arrow to observations, the hypotenuse
    dist_start_to_obs = sqrt((lon1 - lon) ^ 2 + (lat1 - lat) ^ 2),
    # distance from start of arrow to projection
    dist_start_to_proj = sqrt(dist_start_to_obs ^ 2 - dist_obs_to_proj ^ 2),
    # standardize distances to be a percentage of the maximum distance
    dist_percent = 100 * (dist_start_to_proj - min(dist_start_to_proj)) /
      diff(range(dist_start_to_proj))
  ) |>
  select(lon, lat, dist_percent, facet_label)

# background color for plot
bg_col <- "#FAEBD7"

# create histograms of location (tree, rock, etc) by distance along park
p_hist <- df_dists |>
  ggplot(aes(x = dist_percent)) +
  geom_histogram(alpha = 0.5, bins = 20, fill = 'darkgreen', col = 'white') +
  geom_vline(xintercept = seq(0, 100, by = 25), col = 'gray',
             linetype = 'dashed') +
  scale_y_continuous(limits = function(x) c(0, max(5, x)),
                     breaks = function(x) seq(0, 50, by = 5)) +
  facet_wrap(~ facet_label, scales = 'free_y') +
  labs(
    x = 'Distance Across Park (%)',
    y = 'Number of Observations'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = 'none',
    panel.grid = element_blank(),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    strip.text = element_text(size = 12)
  )

# create map of central park
bound_box <- c(-73.985, 40.762, -73.945, 40.802)
park_map <- ggmap(get_stamenmap(bbox = bound_box, zoom = 16,
                                maptype = "terrain-background"))

# quintile labels for arrow in plot
arrow_labels <- tibble(
  x_offsets = rep(c(0.0012, 0.002, 0.00275), c(1, 3, 1)),
  x_coords = seq(lon1, lon2, len = 5) - x_offsets,
  y_coords = seq(lat1, lat2, len = 5),
  text = paste0(seq(0, 100, len = 5), '%')
)

# add observation locations and arrow along park to map
p_map <- park_map +
  geom_point(data = df_dists, aes(x = lon, y = lat),
             shape = 1, size = 2, color = "black") +
  geom_segment(aes(x = lon1, y = lat1, xend = lon2, yend = lat2),
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(data = arrow_labels,
            aes(x = x_coords, y = y_coords, label = text)) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.85, 0.15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = bg_col, color = bg_col),
  )

# patch histograms and map together
p_map + p_hist +
  plot_layout(widths = c(0.8, 1), ncol = 2) +
  plot_annotation(
    title = 'Squirrel Sightings in Central Park',
    subtitle = 'Squirrels sighted most often in trees in south end of park',
    caption = 'TidyTuesday: 2023, week 21.',
    theme = theme(
      panel.background = element_rect(fill = bg_col),
      plot.background = element_rect(fill = bg_col),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.title = element_text(size = 22, hjust = 0.5)
    )
  )

ggsave(filename = './images/2023_week21.png',
       height = 8, width = 11)
