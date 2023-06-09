library(tidyverse)
library(tidytuesdayR)

# This script plots the distribution of tornado directions by magnitude.
# The direction is defined here as the angle from the starting (lon, lat)
# to the ending (lon, lat).

tuesdata <- tidytuesdayR::tt_load(2023, week = 20)

radians_to_degrees = function(radians) {
  degrees = radians * 180 / pi
  return(degrees)
}

# wrangle data for plot
df <- tuesdata$tornados |>
  rename(magnitude = mag) |>
  # remove records where the start and end positions are the same
  filter(!(slat == elat & slon == elon)) |>
  # remove records that have missing magnitude values
  filter(!is.na(magnitude)) |>
  # compute direction of path
  mutate(
    x = elon - slon,
    y = elat - slat,
    angle_rad = atan2(y, x),
    angle_deg = radians_to_degrees(angle_rad)
  ) |>
  # format facet labels to include record counts
  add_count(magnitude, name = 'n') |>
  mutate(magnitude_label = glue::glue('Magnitude {magnitude} (n = {n})')) |>
  select(magnitude, magnitude_label, x, y, angle_deg, angle_rad)

# compute circular mean for each magnitude using von Mises distribution
summary_stats <- df |>
  summarise(
    circular_mean_rad = atan2(mean(sin(angle_rad)), mean(cos(angle_rad))),
    circular_mean_deg = radians_to_degrees(circular_mean_rad),
    max_hist_count = max(hist(angle_deg, breaks = seq(-180, 180, 10),
                              plot = F)$counts),
    mean_text = glue::glue("Mean: {round(circular_mean_deg, 2)}{'\u00B0'}"),
    .by = magnitude_label
  )

# plot distribution of path angles by magnitude
df |>
  # shift angle by 180 degrees so histogram bins are centered in plot
  ggplot(aes(x = (angle_deg + 180) %% 360)) +
  geom_histogram(fill = 'maroon', color = 'black',
                 breaks = seq(0, 360, 10)) +
  geom_vline(xintercept = seq(0, 360, 90), col = 'black',
             linetype = 'dashed') +
  # note, 0 -> W, 90 -> S, 180 -> E, 270 -> N, since shifted angle by 180
  scale_x_continuous(breaks = seq(0, 360, by = 90),
                     labels = c('W', 'S', 'E', 'N', 'W')) +
  facet_wrap(~ magnitude_label, scales = 'free_y', ncol = 1) +
  geom_text(data = summary_stats,
    aes(x = 315, y = 0.8 * max_hist_count, label = mean_text),
    size = 5) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = 'Distribution of Tornado Directions by Magnitude',
    subtitle = 'Paths shift from SE to NE as magnitude increases.',
    caption = 'TidyTuesday: 2023, week 20.'
  ) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = 'beige'),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = 'beige'),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.title = element_text(size = 22, hjust = 0.5),
    strip.background = element_rect(fill = 'tan', color = 'black'),
    strip.text = element_text(size = 14)
  )

ggsave(filename = './images/2023_week20.png',
       height = 11, width = 8)
