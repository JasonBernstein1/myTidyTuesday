library(tidyverse)
library(tidytuesdayR)
library(gt)
library(emo)

# This script creates a table of hot sauce scoville values over
# seasons of the Hot Ones.

tuesdata <- tidytuesdayR::tt_load(2023, week = 32)

sauces <- tuesdata$sauces |>
  mutate(
    kiloscoville = scoville / 1000
  ) |>
  select(season, sauce_number, kiloscoville) |>
  pivot_wider(
    names_from = sauce_number,
    values_from = kiloscoville,
    names_prefix = "sauce_"
  ) |>
  rowwise() |>
  mutate(
    mean = mean(c_across(matches("sauce_[0-9]"))),
    median = median(c_across(matches("sauce_[0-9]")))
  )

# define emojis for heading
pepper <- emo::ji("pepper")
fire <- emo::ji("fire")
tears <- emo::ji("tears")
emojis1 <- paste(pepper, pepper, fire, tears, pepper, pepper)
emojis2 <- paste(pepper, pepper, tears, fire, pepper, pepper)

hot_ones <- sauces |>
  gt() |>
  data_color(
    columns = 2:11,
    fn = scales::col_quantile(
      palette = "Reds",
      n = 9,
      domain = exp(seq(log(0.4), log(2001), len = 10))
    )
  ) |>
  grand_summary_rows(
    columns = 2:13,
    fns = list(
      Min ~ min(.),
      Mean ~ mean(.),
      Max ~ max(.)
    ),
    fmt = list(~ fmt_number(.,
      drop_trailing_zeros = T
    ))
  ) |>
  tab_spanner(
    label = "Sauce Number",
    columns = 2:11
  ) |>
  tab_spanner(
    label = "Stats",
    columns = 12:13
  ) |>
  cols_label_with(
    fn = ~ str_remove(., "^sauce_")
  ) |>
  cols_label(
    season = "Season",
    mean = "Mean",
    median = "Median"
  ) |>
  cols_align(
    align = "center"
  ) |>
  fmt_number(drop_trailing_zeros = TRUE) |>
  fmt_number(columns = c(mean, median), n_sigfig = 3) |>
  tab_header(title = md(paste(emojis1, "**Hot Sauce Heat Trends**", emojis2))) |>
  tab_footnote("Units are kiloscovilles, where 1 kiloscoville = 1,000 scoville.") |>
  tab_source_note("TidyTuesday: 2023, week 32 | Source: Wikipedia and Carl Börstell") |>
  opt_row_striping(row_striping = TRUE) |>
  tab_options(
    data_row.padding = px(4),
    data_row.padding.horizontal = px(8),
    heading.border.bottom.style = "hidden",
    heading.title.font.size = 28,
    row.striping.background_color = "azure",
    table.background.color = "aliceblue",
    table.border.bottom.style = "hidden",
    table.border.top.style = "hidden"
  )

hot_ones

gtsave(hot_ones, filename = "./images/2023_week32.png")
