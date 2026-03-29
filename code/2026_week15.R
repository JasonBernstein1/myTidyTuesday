library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2026, week = 15)

birds <- tuesdata$birds
ships <- tuesdata$ships

df <- birds |>
  right_join(ships)

world <- ggplot2::map_data("world")

ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "darkgreen",
    color = NA,
    linewidth = 0
  ) +
  geom_point(
    data = df,
    aes(x = longitude, y = latitude),
    col = "gold",
    size = 0.5,
    alpha = 0.4
  ) +
  coord_fixed(1.3, xlim = c(30, 183), ylim = c(-80, -5), expand = FALSE) +
  annotate(
    "text",
    x = 54,
    y = -22,
    label = "Ship Positions at\nBird Sightings",
    color = "gold",
    size = 8,
    fontface = "bold",
    hjust = 0
  ) +
  labs(
    caption = "TidyTuesday: 2026, week 15 | Source: Te Papa Tongarewa, The Museum of New Zealand"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "black", color = NA),
    panel.border = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = "black", color = NA),
    plot.caption = element_text(color = "white", size = 8, hjust = 1),
    plot.margin = margin(0, 0, 0, 0),
  )

ggsave(
  filename = here::here("images", "2026_week15.png"),
  height = 5,
  width = 8
)
