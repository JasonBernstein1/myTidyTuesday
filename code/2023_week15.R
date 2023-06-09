library(tidyverse)
library(tidytuesdayR)

# This script plots the egg-to-hen ratio by production process and
# whether the eggs are for hatching or the table.

tuesdata <- tidytuesdayR::tt_load(2023, week = 15)

eggproduction <- tuesdata$`egg-production` |>
  # this is the variable we will plot over time
  mutate(egg_to_hen_ratio = n_eggs / n_hens)

# tabulate number of points by production process and type
with(eggproduction, table(prod_process, prod_type))

# The egg-to-hen ratio for 'cage-free (organic)' and 'cage-free (non-organic)'
# production processes are similar. Here we check ratios are similar for
# organic and non-organic production. Hence, we lump all cage-free eggs
# together for the final plot.
eggproduction |>
  filter(str_starts(prod_process, 'cage-free')) |>
  select(observed_month, prod_process, egg_to_hen_ratio) |>
  pivot_wider(names_from = prod_process, values_from = egg_to_hen_ratio) |>
  # plot ratios against each other to see they are similar
  ggplot() +
  geom_point(aes(x = `cage-free (organic)`, y = `cage-free (non-organic)`)) +
  geom_abline(intercept = 0, slope = 1, col = 'red') +
  labs(
    x = 'Egg-to-Hen Ratio for Cage-free (Organic) Eggs',
    y = 'Egg-to-Hen Ratio for Cage-free (Non-organic) Eggs',
    title = 'Egg-to-Hen Ratio for Organic and Non-Organic Production'
  ) +
  theme(aspect.ratio = 1)

# format data for plotting
df <- eggproduction |>
  # remove non-organic values since they are similar to organic values
  filter(prod_process != 'cage-free (non-organic)') |>
  # Capitalize variable names to appear nicer in plot
  mutate(prod_type = str_to_sentence(prod_type)) |>
  mutate(prod_process = str_to_sentence(prod_process)) |>
  # remove ' (organic)' from 'Cage-free (organic)'
  mutate(prod_process = str_remove(prod_process, ' \\(organic\\)')) |>
  # combine production type and process for plotting
  mutate(prod_method = glue::glue("{prod_type} ({prod_process})") |>
         fct_reorder(.x = egg_to_hen_ratio, .fun = mean, .desc = TRUE))

# plot the egg-to-hen ratio over time
df |>
  ggplot() +
  geom_line(aes(x = observed_month, y = egg_to_hen_ratio,
                col = prod_method), linewidth = 1.5) +
  # add vertical lines at Feb. 28 since noticeable dips here
  geom_vline(xintercept = as.Date(paste0(2017:2021, "-02-28")),
             linetype = 'dashed', alpha = 0.6) +
  scale_x_continuous(
    breaks = as.Date(paste0(2017:2021, "-02-28")),
    labels = paste('Feb. 28,\n', 2017:2021)
  ) +
  scale_y_continuous(
    limits = c(16, 26),
    breaks = seq(16, 26, by = 2)
  ) +
  scale_color_manual(
    values = thematic::okabe_ito(3)
  )+
  labs(
    x = element_blank(),
    y = 'Egg-to-Hen Ratio\n',
    col = 'Egg Production Type',
    title = 'Monthly Egg-to-Hen Ratio from July 2016 to Feb. 2021',
    caption = 'TidyTuesday: 2023, week 15.'
  ) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    legend.background = element_rect(fill = '#FFFAFA', color = 'black'),
    legend.key = element_rect(fill = '#FFFAFA'),
    legend.position = c(0.855, 0.125),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.background = element_rect(fill = '#FFFAFA'),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = 'gray', linetype = 'dotted'),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = '#FFFAFA'),
    plot.title = element_text(size = 26),
  )

ggsave(filename = './images/2023_week15.png',
       height = 10, width = 15)
