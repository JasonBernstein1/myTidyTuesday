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
# points are close to one. Hence, lump all cage-free eggs together for 
# this plot.

organic_eggprod <- eggproduction |>
  filter(prod_process == 'cage-free (organic)') |>
  select(observed_month, egg_to_hen_ratio)

nonorganic_eggprod <- eggproduction |>
  filter(prod_process == 'cage-free (non-organic)') |>
  select(observed_month, egg_to_hen_ratio)

# ratios close to one for organic or non-organic, so effectively the same
range(organic_eggprod$egg_to_hen_ratio / nonorganic_eggprod$egg_to_hen_ratio)

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
       caption = 'TidyTuesday: 2023, week 15.') +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_bw(base_size = 12) +
  theme(
    aspect.ratio = 1,
    legend.position = 'top',
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, vjust = -0.7),
    strip.background = element_rect(fill = 'lightgray', color = 'black'),
    panel.background = element_rect(fill = 'beige', color = 'black'),
    panel.grid.major = element_line(color = 'gray', linetype = 'dotted'),
    panel.grid.minor = element_blank()
  ) +
  facet_grid(~ prod_type)

ggsave(filename = './images/2023_week15.png', bg = 'beige')
