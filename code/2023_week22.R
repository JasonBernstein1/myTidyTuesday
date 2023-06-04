library(tidyverse)
library(tidytuesdayR)
library(ggtext)

# This script plots histograms of the ages of the 100 oldest males
# and females.  The plot also shows the names, birth year, and ages
# of the three oldest males and females.

tuesdata <- tidytuesdayR::tt_load(2023, week = 22)

# wrangle data for plot
df <- tuesdata$centenarians |>
  mutate(
    status = stringr::str_to_sentence(still_alive),
    birth_year = lubridate::year(birth_date)
  ) |>
  select(rank, name, age, birth_year, gender, status)

# format labels of three oldest males and females for plot
df_six_oldest <- df |>
  filter(rank <= 3) |>
  arrange(age) |>
  mutate(
    age = round(age, 1),
    label = glue::glue("{name} (b. {birth_year})\n{status} at {age}")
  ) |>
  select(name, gender, label)

# colors for male and female histograms
colors <- c(male = "#0072B2", female = "#CC79A7")

# create plot of histograms of ages by gender
df |>
  ggplot(aes(x = age, group = gender, fill = gender)) +
  geom_histogram(breaks = seq(110.5, 123, by = 0.5), color = 'black',
                 alpha = 0.5, position = 'identity') +
  # add labels for three oldest males and females
  geom_label(data = df_six_oldest, color = 'black', size = 5, alpha = 0.5,
             aes(x = 120, y = seq(32.5, 7.5, by = -5),
                 label = label, fill = gender)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = seq(0, 35, 5)) +
  scale_x_continuous(breaks = seq(110, 122, 2)) +
  labs(
    x = 'Age (Years)',
    y = 'Number of People',
    title = glue::glue("The 100 Oldest ",
      "<span style = 'color:{colors['male']}'>Males</span> and ",
      "<span style = 'color:{colors['female']}'>Females</span>"),
    caption = 'TidyTuesday: 2023, week 22.'
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.position = 'none',
    panel.border = element_rect(linewidth = 1.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 20, hjust = 0.5,
                                  margin = margin(b = 0.25, unit = 'cm')),
    plot.title.position = 'plot'
  )

ggsave(filename = './images/2023_week22.png', bg = 'white',
       height = 8, width = 8)
