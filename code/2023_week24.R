library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(gt)

# This script produces a table of counts of items by village, and adds
# color and bold text to the table.

tuesdata <- tidytuesdayR::tt_load(2023, week = 24)

# wrangle data for table
df <- tuesdata$safi_data |>
  select(village, items_owned) |>
  filter(items_owned != "NULL") |>
  tidytext::unnest_tokens(Item, items_owned) |>
  count(Item, village) |>
  pivot_wider(names_from = village, values_from = n, values_fill = 0) |>
  mutate(
    Item = Item |>
      str_to_sentence() |>
      str_replace("_", " "),
    Item = case_when(
      Item == "Sterio" ~ "Stereo",
      TRUE ~ Item
    ),
    Total = rowSums(across(where(is.numeric)))
  ) |>
  arrange(desc(Total))

# first and last interview dates
start_date <- min(tuesdata$safi_data$interview_date)
end_date <- max(tuesdata$safi_data$interview_date)

village_colors <- hcl.colors(n = 3, palette = "Dark 2")

# create table
item_counts <- df |>
  gt::gt() |>
  tab_header(
    title = md("**Number of Items Recorded in Each Village**"),
    subtitle = glue::glue("Data collected from {start_date} to {end_date}")
  ) |>
  tab_style(
    style = cell_fill(color = "gray98"),
    locations = cells_title()
  ) |>
  # color items Chirodzo has the most of
  tab_style(
    style = list(
      cell_fill(color = village_colors[1], alpha = 0.25),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Chirodzo,
      rows = Chirodzo >= pmax(God, Ruaca)
    )
  ) |>
  # color items God has the most of
  tab_style(
    style = list(
      cell_fill(color = village_colors[2], alpha = 0.25),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = God,
      rows = God >= pmax(Chirodzo, Ruaca)
    )
  ) |>
  # color items Ruaca has the most of
  tab_style(
    style = list(
      cell_fill(color = village_colors[3], alpha = 0.25),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Ruaca,
      rows = Ruaca >= pmax(Chirodzo, God)
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "gray98"),
      cell_text(size = "small")
    ),
    locations = cells_source_notes()
  ) |>
  tab_source_note("TidyTuesday: 2023, week 24 | Source: SAFI Survey")

item_counts

gtsave(item_counts, filename = "images/2023_week24.png")
