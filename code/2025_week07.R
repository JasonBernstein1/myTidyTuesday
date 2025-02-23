library(maps)
library(tidyverse)

# This script plots the locations of law enforcement agencies that submit
# data to the Uniform Crime Reporting (UCR) Program.

tuesdata <- tidytuesdayR::tt_load(2025, week = 7)

# Only keep agencies in the continental US
df <- tuesdata$agencies |>
  filter(
    !(state_abbr %in% c("AK", "HI")),
    latitude > 20
  )

# Get state borders
states_map <- map_data("state")

# Create map with agency locations
df |>
  ggplot() +
  # Add water background
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.background = element_rect(fill = "lightblue", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 25)
  ) +
  # Draw states
  geom_polygon(
    data = states_map,
    aes(long, lat, group = group),
    fill = "snow",
    color = "black"
  ) +
  # Add agency locations
  geom_point(aes(longitude, latitude), size = 0.2) +
  labs(
    title = "Law Enforcement Agencies that Submitted Data \nto the Uniform Crime Reporting (UCR) Program",
    caption = "TidyTuesday: 2025, week 7 | Source: FBI Crime Data API."
  )

ggsave(
  filename = "images/2025_week07.png",
  height = 10,
  width = 15
)
