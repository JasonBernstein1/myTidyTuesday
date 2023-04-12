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

# The egg-to-hen ratio for cage-free organic and cage-free non-organic
# production processes are similar. 
# Here we check ratios are similar for organic and non-organic production.
# Hence, we lump all cage-free eggs together for the final plot.
eggproduction |>
  filter(prod_process == 'cage-free (organic)' |
         prod_process == 'cage-free (non-organic)') |>
  select(observed_month, prod_process, egg_to_hen_ratio) |>
  pivot_wider(names_from = prod_process, values_from = egg_to_hen_ratio) |>
  # plot ratios against each other to see they are similar
  ggplot() +
  geom_point(aes(`cage-free (organic)`, `cage-free (non-organic)`)) +
  geom_abline(intercept = 0, slope = 1, col = 'red') +
  labs(x = 'Egg-to-Hen Ratio for Cage-free (Organic) Eggs',
       y = 'Egg-to-Hen Ratio for Cage-free (Non-organic) Eggs',
       title = 'Egg-to-Hen Ratio for Organic and Non-Organic Production') +
  theme(aspect.ratio = 1)

# format data for plotting
df <- eggproduction |>
  # remove non-organic values since they are similar to organic values
  filter(!str_detect(prod_process, '(non-organic)')) |>
  # Capitalize variable names to appear nicer in plot
  mutate(prod_process = case_when(
    prod_process == 'all' ~ 'All',
    prod_process == 'cage-free (organic)' ~ 'Cage-free'
  )) |>
  mutate(prod_type = case_when(
    prod_type == 'hatching eggs' ~ 'Hatching Eggs',
    prod_type == 'table eggs' ~ 'Table Eggs'
  ))

# plot the egg-to-hen ratio over time
df |>
  ggplot() +
  geom_line(aes(observed_month, egg_to_hen_ratio, col = prod_process, 
                group = prod_process), linewidth = 1.1) +
  # add vertical lines at Feb. 28 since noticeable dips here
  geom_vline(xintercept = as.Date(paste(2017:2022, "-02-28", sep = "")),
             linetype = 'dashed', alpha = 0.6) +
  labs(x = 'Date', y = 'Egg-to-Hen Ratio',
       col = 'Production Process',
       title = 'Egg-to-Hen Ratio',
       subtitle = 'July 2016 - Feb. 2021',
       caption = 'Vertical lines indicate Feb. 28.\n
                  TidyTuesday: 2023, week 15.') +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = 'top',
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, vjust = -0.7),
    strip.background = element_rect(fill = 'lightgray', color = 'black'),
    panel.background = element_rect(fill = 'beige', color = 'black'),
    panel.grid.major = element_line(color = 'gray', linetype = 'dotted'),
    panel.grid.minor = element_blank()
  ) +
  facet_grid(~ prod_type)

ggsave(filename = './images/2023_week15.png', bg = 'beige',
       height = 10, width = 15)
