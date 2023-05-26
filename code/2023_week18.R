library(tidyverse)
library(tidytuesdayR)

# This script plots the number of mice and rats observed over time
# by treatment.

tuesdata <- tidytuesdayR::tt_load(2023, week = 18)

species <- tuesdata$species
surveys <- tuesdata$surveys

# wrangle data for plot
df <- surveys |>
  left_join(species) |>
  # determine if each record is of a mouse or a rat
  mutate(mouse_or_rat = str_detect(commonname, 'mouse') |>
                        ifelse('mouse', 'rat')) |>
  # count records of mice and rats by treatment and year
  count(year, treatment, mouse_or_rat, name = 'n') |>
  # set n = 0 for missing year/treatment combinations
  complete(year, treatment, mouse_or_rat, fill = list(n = 0))

# plot the number of mice and rats over time by treatment
df |>
  ggplot() +
  geom_line(aes(x = year, y = n, col = str_to_sentence(treatment)),
            linewidth = 1.25) +
  facet_wrap(~ mouse_or_rat,
             strip.position = "left",
             labeller = as_labeller(c(mouse = "Mice", rat = "Rats")),
             ncol = 1) +
  scale_y_log10() +
  labs(
    x = element_blank(),
    y = element_blank(),
    col = element_blank(),
    title = 'Rodent Counts by Treatment over Time',
    caption = 'TidyTuesday: 2023, week 18.'
  ) +
  theme(
    axis.text = element_text(color = 'white', size = 12),
    legend.background = element_rect(fill = 'black'),
    legend.key = element_blank(),
    legend.position = 'top',
    legend.text = element_text(color = 'white', size = 12),
    panel.background = element_rect(fill = 'black'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dotted', color = 'gray'),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'black', color = 'black'),
    plot.caption = element_text(color = 'white'),
    plot.title = element_text(hjust = 0.5, size = 18),
    strip.background = element_rect(fill = 'black'),
    strip.placement = "outside",
    strip.text = element_text(color = 'white', size = 16),
    text = element_text(color = 'white')
  )

ggsave(filename = './images/2023_week18.png', bg = 'black',
       height = 6, width = 9)
