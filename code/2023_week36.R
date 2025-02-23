library(tidyverse)
library(tidytuesdayR)
library(gt)
library(gtExtras)

# This script creates a table of average wages by demographic. The table
# includes an embedded bar plot that is based on the gtExtras::gt_plt_bar
# function.

tuesdata <- tidytuesdayR::tt_load(2023, week = 36)
wages <- tuesdata$wages

palette <- thematic::okabe_ito()[c(2, 4, 6)]

df <- wages |>
  filter(str_detect(facet, "demographics")) |>
  mutate(
    demographic = str_remove(facet, "demographics: ") |>
      str_to_sentence()
  ) |>
  summarise(
    mean_wage = mean(wage),
    .by = demographic
  ) |>
  arrange(desc(mean_wage)) |>
  mutate(
    bar_color = case_when(
      str_detect(demographic, "[Cc]ollege") ~ palette[1],
      str_detect(demographic, "[Ff]emale") ~ palette[2],
      TRUE ~ palette[3]
    )
  )

# modify gtExtras::gt_plt_bar function
gt_plt_bar2 <- gtExtras::gt_plt_bar |>
  capture.output() |>
  # remove vertical lines at x = 0
  str_remove("geom_vline.*$") |>
  str_remove("linewidth = 1\\) \\+ ") |>
  # give the bar plot column the name DUPE_COLUMN_PLT
  str_replace("cols_label\\(DUPE_COLUMN_PLT = col_bare\\) %>%", "") |>
  # remove last two lines of captured output
  str_remove("<bytecode.*") |>
  str_remove("<environment: namespace:gtExtras>") |>
  # convert text of modified function back to function
  (\(.x) {
    parse(text = .x)
  })() |>
  eval(envir = environment(gtExtras::gt_plt_bar))

wage_table <- df |>
  select(-bar_color) |>
  gt() |>
  gt_plt_bar2(
    column = mean_wage,
    keep_column = TRUE,
    width = 35,
    color = df$bar_color
  ) |>
  fmt_currency(columns = mean_wage) |>
  cols_align(
    columns = mean_wage,
    align = "center"
  ) |>
  cols_label(
    demographic = "Demographic",
    mean_wage = "Mean Wage",
    # remove column name from above bar plots
    DUPE_COLUMN_PLT = ""
  ) |>
  tab_options(
    heading.background.color = "grey90"
  ) |>
  opt_stylize(style = 1, color = "gray") |>
  tab_options(
    data_row.padding = px(1),
    row.striping.background_color = "gray98"
  ) |>
  tab_header(
    title = "Average Hourly Wage by Demographic",
    subtitle = md(
      glue::glue(
        "Data on ",
        "**<span style = 'color:{palette[1]}'>college</span>** goers, ",
        "**<span style = 'color:{palette[3]}'>males</span>**, and ",
        "**<span style = 'color:{palette[2]}'>females</span>** from 1973 to 2022."
      )
    )
  ) |>
  tab_footnote(md("Note, demographic groups are not mutually exclusive.")) |>
  tab_source_note("TidyTuesday: 2023, week 36 | Source: www.unionstats.com")

wage_table

gtsave(wage_table, filename = here::here("images", "2023_week36.png"))
