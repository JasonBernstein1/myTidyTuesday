library(tidyverse)
library(tidytuesdayR)
library(sf)
library(rnaturalearth)

# This script creates a plot indicating whether locations are
# actively in Daylight Savings Time (DST) as of April 9, 2023.

dfs <- tidytuesdayR::tt_load(2023, week = 13)

# filter locations by these dates
min_date <- as.Date("1960-01-01", format = "%Y-%m-%d")
max_date <- as.Date("2023-04-09", format = "%Y-%m-%d")

# get most recent DST status for each location in data
dst_status <- dfs$transitions |>
  mutate(begin = lubridate::as_datetime(begin)) |>
  filter(begin > min_date & begin < max_date) |>
  select(zone, begin, dst) |>
  # take the most recent DST status of each location
  slice_max(begin, n = 1, by = zone) |>
  # bring in lat/lon data, keeps locations with given DST status
  left_join(dfs$timezones) |>
  mutate(dst_active = ifelse(dst, 'DST Is Active', 'DST Is Inactive'))

# Phoenix is currently not in DST
dst_status |> filter(str_detect(zone, 'Phoenix')) |> select(zone, dst)

# Indiana has 11 locations currently in DST (3 missing coordinates)
dst_status |> filter(str_detect(zone, 'Indiana')) |> select(zone, dst)

# coordinate reference system (CRS) for World Geodetic System 1984
crs_wgs84 <- 4326

# spatial data frame of country polygons
countries <- rnaturalearth::ne_countries(returnclass = "sf") |>
  st_transform(crs = crs_wgs84)

# convert data to sf class so can plot with geom_sf
dst_status_sf <- dst_status |>
  # remove points with missing coordinates
  filter(!is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_wgs84)

# create plot indicating locations where DST is active or not
countries |>
  ggplot() +
  geom_sf(fill = 'white') +
  geom_sf(data = dst_status_sf, aes(fill = dst_active), alpha = 0.7,
          size = 2, shape = 21) +
  coord_sf(expand = F) +
  labs(
    x = 'Longitude',
    y = 'Latitude',
    fill = '',
    title = 'Where is Daylight Savings Time (DST) Currently Active?',
    caption = 'TidyTuesday: 2023, week 13.'
  ) +
  scale_x_continuous(breaks = seq(-180, 180, 60)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = 'top',
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "lightblue")
  )

ggsave(filename = './images/2023_week13.png', bg = 'white')
