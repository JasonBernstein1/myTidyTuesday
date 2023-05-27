library(tidyverse)
library(tidytuesdayR)
library(ggmap)

# This script plots locations of Neolithic founder crops on a map
# of the region.

tuesdata <- tidytuesdayR::tt_load(2023, week = 16)

df <- tuesdata$founder_crops |>
  filter(!is.na(category)) |>
  select(latitude, longitude, category) |>
  # create variable of category counts to show in facet label,
  # and order subplots by increasing n_category
  add_count(category, name = 'n_category') |>
  mutate(facet_label = glue::glue('{category} (n = {n_category})') |>
                       fct_reorder(.x = n_category))

# map boundaries
bound_box <- with(df,
                  c(0.99 * min(longitude), 0.99 * min(latitude),
                    1.01 * max(longitude), 1.01 * max(latitude)))

base_map <- ggmap(get_stamenmap(bbox = bound_box, zoom = 7,
                                maptype = "terrain"))

# create separate map for each crop category
base_map +
  geom_point(data = df,
           aes(x = longitude, y = latitude, fill = category),
           shape = 21, alpha = 0.7, size = 3) +
  facet_wrap(~ facet_label, strip.position = "bottom") +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = 'Locations of Neolithic Founder Crops',
    caption = 'TidyTuesday: 2023, week 16.'
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggsave(filename = './images/2023_week16.png', bg = 'white',
       height = 6, width = 9)
