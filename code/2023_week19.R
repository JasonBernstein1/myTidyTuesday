library(tidyverse)
library(tidytuesdayR)
library(ggpmisc) # needed for annotate(geom = 'table')
library(broom)

# This script estimates the annual change in median cost of infant care
# by county in Michigan and plots a map of the estimates.

tuesdata <- tidytuesdayR::tt_load(2023, week = 19)

costs <- tuesdata$childcare_costs
counties <- tuesdata$counties

# 83 counties in Michigan
counties |>
  filter(state_abbreviation == 'MI') |>
  n_distinct()

# wrangle county and cost data for plot
df <- costs |>
  left_join(counties) |>
  filter(state_abbreviation == 'MI') |>
  mutate(mc_infant_annual = mc_infant * 52) |>
  # estimate yearly increase in cost for each county
  group_by(county_fips_code, county_name) |>
  do(broom::tidy(lm(mc_infant_annual ~ study_year, .))) |>
  ungroup() |>
  # keep rows for slope in linear models, remove rows for intercept
  filter(term == 'study_year') |>
  # format county name so can join this data with map data
  mutate(county_name = str_to_lower(county_name) |>
                       str_replace(' county', '') |>
                       # remove '.' from st. clair and st. joseph
                       str_replace('\\.', '')) |>
  select(county_fips_code, county_name, estimate)

# determine the five counties with the highest annual cost increases
top_counties <- df |>
  select(county_name, estimate) |>
  slice_max(n = 5, order_by = estimate) |>
  mutate(County = str_to_sentence(county_name),
         Estimate = round(estimate)) |>
  select(County, Estimate)

# positions of county labels on map
label_coords <- tribble(
  ~county_name, ~lon, ~lat,
  'Washtenaw', -83.2, 42.25, # Ann Arbor
  'Oakland',   -82.9, 42.68, # Detroit suburbs
  'Leelanau',  -86.4, 45.1,  # Traverse City
  'Kent',      -85.2, 43.0,  # Grand Rapids
  'Emmet',     -85.5, 45.6   # Petoskey
)

# combine map and cost estimate data
df_map <- map_data(map = "county", region = "michigan") |>
  select(lon = long, lat, group, county_name = subregion) |>
  left_join(df)

# map annual increase in cost of infant care by county
df_map |>
  ggplot() +
  geom_polygon(aes(lon, lat, group = group, fill = estimate),
               colour = "darkgray") +
  scale_fill_viridis_c(name = "Increase in Annual Cost ($)   ",
                       option = "inferno",
                       limits = c(100, 400)) +
  geom_label(data = label_coords, aes(x = lon, y = lat, label = county_name),
             size = 4) +
  annotate(geom = "table", x = -90, y = 43.25, label = list(top_counties),
           size = 5, fontface = "bold") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.width = unit(1, 'cm'),
    legend.position = 'top',
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.background = element_rect(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  labs(
    x = '',
    y = '',
    title = 'Estimated Increase in Annual Cost of Center-based Infant Care',
    caption = 'Data available for 2008-2018. Estimates based on linear model.
    \nTidyTuesday: 2023, week 19.'
  )

ggsave(filename = './images/2023_week19.png', bg = 'black')
