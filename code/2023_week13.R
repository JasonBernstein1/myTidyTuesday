library(tidyverse)
library(tidytuesdayR)
library(sf)
library(rnaturalearth)

# This script creates a map indicating whether locations are
# actively in Daylight Savings Time (DST) as of April 9, 2023.

tuesdata <- tidytuesdayR::tt_load(2023, week = 13)

transitions <- tuesdata$transitions
timezones <- tuesdata$timezones

# filter locations by these dates
min_date <- as.Date("1960-01-01", format = "%Y-%m-%d")
max_date <- as.Date("2023-04-09", format = "%Y-%m-%d")

# get most recent DST status for each location
dst_status <- transitions |>
  mutate(
    begin = lubridate::as_datetime(begin),
    dst_active = ifelse(dst, "Active", "Inactive")
  ) |>
  filter(begin > min_date & begin < max_date) |>
  select(zone, begin, dst, dst_active) |>
  # take the most recent DST status of each location
  slice_max(begin, n = 1, by = zone) |>
  # bring in lat/lon data, keeps locations with given DST status
  left_join(timezones)

# Phoenix is currently not in DST
dst_status |>
  filter(str_detect(zone, "Phoenix")) |>
  select(zone, dst)

# Indiana has 11 locations currently in DST (3 missing coordinates)
dst_status |>
  filter(str_detect(zone, "Indiana")) |>
  select(zone, dst)

# coordinate reference system (CRS), World Geodetic System 1984
crs_wgs84 <- 4326

country_map <- rnaturalearth::ne_countries(returnclass = "sf") |>
  sf::st_transform(crs = crs_wgs84)

# convert data to sf class so can plot with geom_sf
dst_status_sf <- dst_status |>
  # remove points with missing coordinates
  filter(!is.na(latitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = crs_wgs84)

# create plot indicating locations where DST is active or not
country_map |>
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(
    data = dst_status_sf,
    aes(color = dst_active, shape = dst_active),
    alpha = 0.95,
    size = 5
  ) +
  scale_color_manual(
    name = "DST",
    labels = c("Active", "Inactive"),
    values = c("springgreen4", "firebrick1")
  ) +
  scale_shape_manual(
    name = "DST",
    labels = c("Active", "Inactive"),
    values = c("o", "x")
  ) +
  coord_sf(expand = F) +
  labs(
    title = "Where is Daylight Savings Time (DST) Currently Active?",
    caption = "TidyTuesday: 2023, week 13 | Source: clock and tzdb R packages"
  ) +
  scale_x_continuous(breaks = seq(-180, 180, 60)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.box.margin = margin(0.2, 0, 0.2, 0, "cm"),
    legend.position = "top",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    panel.background = element_rect(fill = "lightblue2"),
    panel.grid.major = element_line(color = "ghostwhite", linewidth = 0.3),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(0, 0.3, 0.1, 0.3, "cm"),
    plot.title = element_text(size = 22, hjust = 0.5)
  )

ggsave(
  filename = here::here("images", "2023_week13.png"),
  height = 5.3,
  width = 9
)
