library(tidyverse)
library(tidytuesdayR)

# This script plots the winning times of the London marathon from 1981-2022.
# Results are compared by male/female and wheelchair/running.

tuesdata <- tidytuesdayR::tt_load(2023, week = 17)

winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon

# wrangle data for plot
df <- winners |>
  # create variable indicating if record is men/women
  mutate(sex = str_detect(Category, 'Women') |>
               ifelse('Women', 'Men')) |>
  # create variable indicating if record is wheelchair/running
  mutate(race = str_detect(Category, 'Wheelchair') |>
                ifelse('Wheelchair', 'Running')) |>
  # use 'Date' in london_marathon instead of 'Year' in winners
  left_join(london_marathon) |>
  select(Date, Time, sex, race)

# plot time series of winning times by wheelchair/running and men/women
df |>
  ggplot(aes(x = Date, y = Time, color = sex)) +
  geom_line(linewidth = 0.4) +
  geom_point(aes(shape = sex)) +
  facet_wrap(~ race) +
  scale_color_manual(values = c("Men" = "#FF8C00", "Women" = "#FFFACD"),
                     name = NULL) +
  scale_shape_manual(values = c("Men" = 1, "Women" = 2), name = NULL) +
  scale_y_time(breaks = hms(c('02:00:00', '03:00:00', '04:00:00')),
               labels = paste(2:4, 'hours')) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = 'London Marathon Winning Times',
    subtitle = element_blank(),
    caption = 'TidyTuesday: 2023, week 17.',
    color = element_blank()
  ) +
  theme(
    axis.text = element_text(color = 'white', size = 12),
    legend.background = element_rect(fill = 'black'),
    legend.key = element_blank(),
    legend.position = c(0.85, 0.675),
    legend.text = element_text(color = 'white', size = 16),
    panel.background = element_rect(fill = 'black'),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'gray'),
    plot.background = element_rect(fill = 'black', color = 'black'),
    plot.title = element_text(hjust = 0.5, size = 18),
    strip.background = element_rect(fill = 'black'),
    strip.text = element_text(color = 'white', size = 16),
    text = element_text(color = 'white')
  )

ggsave(filename = './images/2023_week17.png',
  height = 6, width = 9)
