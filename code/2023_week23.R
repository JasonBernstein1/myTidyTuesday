library(tidyverse)
library(tidytuesdayR)
library(ggtext)

# This script plots oil production over time for countries in OPEC and
# North America.

# countries we consider and whether they are in OPEC or North America
countries <- tibble(
  country = c(
    "Algeria",
    "Angola",
    "Congo",
    "Equatorial Guinea",
    "Gabon",
    "Iran",
    "Iraq",
    "Kuwait",
    "Libya",
    "Nigeria",
    "Saudi Arabia",
    "United Arab Emirates",
    "Venezuela",
    "United States",
    "Mexico",
    "Canada"
  ),
  region = rep(c("opec", "north_america"), c(13, 3))
)

# colors for OPEC and North American countries
colors <- c("opec" = "#005AB5", "north_america" = "#DC3220")

tuesdata <- tidytuesdayR::tt_load(2023, week = 23)

# wrangle yearly oil production data for plot
oilprod <- tuesdata$`owid-energy` |>
  # fill in missing year/country combinations
  complete(year, country, fill = list(oil_production = 0)) |>
  # bring in OPEC or North America information, filters out other countries
  right_join(countries) |>
  # no oil production for year 2022
  filter(year <= 2021) |>
  select(country, year, oil_production, region)

# rank countries by current oil production
ranks <- oilprod |>
  filter(year == 2021) |>
  mutate(
    rank = rank(-oil_production),
    facet_lab = glue::glue("{rank}. {country}") |>
      fct_reorder(rank)
  ) |>
  select(facet_lab, country) |>
  arrange(facet_lab)

# combine oil production and rank data
df <- left_join(oilprod, ranks)

# plot oil production over time for different countries
df |>
  ggplot(aes(x = year, y = oil_production)) +
  geom_line(aes(color = region), linewidth = 1.5) +
  scale_color_manual(values = colors) +
  facet_wrap(
    ~facet_lab,
    nrow = 4,
    scales = "free_y",
    strip.position = "bottom"
  ) +
  theme_void() +
  labs(
    title = glue::glue(
      "Oil Production for Countries in ",
      "**<span style = 'font-size:30pt; color:{colors['opec']}'>OPEC</span>** and ",
      "**<span style = 'font-size:30pt; color:{colors['north_america']}'>North America</span>** ",
      "from 1900 to 2021"
    ),
    subtitle = "Countries ranked by oil production in 2021",
    caption = "TidyTuesday: 2023, week 23."
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FFF6F6"),
    plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm"),
    plot.title = element_markdown(
      size = 26,
      hjust = 0,
      margin = margin(
        b = 0.5,
        t = 0.5,
        unit = "cm"
      )
    ),
    plot.subtitle = element_text(
      size = 24,
      hjust = 0,
      margin = margin(b = 0.5, unit = "cm")
    ),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(margin = margin(b = 5))
  )

ggsave(
  filename = "images/2023_week23.png",
  height = 10,
  width = 14
)
