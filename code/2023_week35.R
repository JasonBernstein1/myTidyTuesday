library(tidyverse)
library(tidytuesdayR)
library(glue)
library(gt)
library(gtExtras)
library(showtext)

# This script creates a table with embedded barplots showing the
# number of copyright cases where fair use is found, not found, or
# there is another outcome.

tuesdata <- tidytuesdayR::tt_load(2023, week = 35)

findings <- tuesdata$fair_use_findings

df <- findings |>
  mutate(
    # one year is "2017, affirmed 2018"
    year = as.numeric(str_sub(year, 1, 4)),
    decade = paste0(floor(year / 10) * 10, "'s"),
    # standardize outcome variable
    outcome = outcome |>
      tolower() |>
      dplyr::case_match(
        "fair use found" ~ "fair use found",
        "fair use not found" ~ "fair use not found",
        .default = "other"
      )
  ) |>
  count(outcome, decade) |>
  complete(outcome, decade, fill = list(n = NA))

# enable Lato font for counts in barplots
showtext::showtext_auto()
sysfonts::font_add_google(name = "Lato")

# modify gtExtras::gt_plt_bar_stack function, e.g., replace hard-coded values
gt_plt_bar_stack2 <- capture.output(gtExtras::gt_plt_bar_stack) |>
  # change size of counts in bar plots
  str_replace("size = 3", "size = 5") |>
  # change height of bar plots
  str_replace("height = 5", "height = 10") |>
  # replace || separating group labels for bar plots with white space
  str_replace("\\|\\|", "<span style='color:white'>...............</span>") |>
  # adjust margins around bar plots so they appear centered
  str_replace("margin\\(0, 0, 0, 0", "margin\\(3, 0, 0, 0") |>
  # replace font in gt_plt_bar_stack with font in gt_theme_espn()
  str_replace("mono", "Lato") |>
  # remove last two lines of captured output
  str_remove("<bytecode.*") |>
  str_remove("<environment: namespace:gtExtras>") |>
  # convert text of modified function back to function
  (\(.x) {eval(parse(text = .x))})()

environment(gt_plt_bar_stack2) <- environment(gtExtras::gt_plt_bar_stack)

tab <- df |>
  group_by(decade) |>
  summarise(
    list_data = list(n),
    total = sum(n, na.rm = TRUE)
  ) |>
  gt() |>
  gt_plt_bar_stack2(
    column = list_data,
    labels = unique(df$outcome),
    palette = thematic::okabe_ito(3),
    width = 120
  ) |>
  gt_theme_espn() |>
  cols_align(
    columns = c("decade", "total"),
    align = "center"
  ) |>
  tab_options(
    data_row.padding = px(3),
    row.striping.background_color = "snow1",
    table.font.size = px(18)
  ) |>
  tab_header(
    title = "Rulings on Fair Use in American Copyright Law",
    subtitle = glue::glue("{nrow(findings)} cases available from 1841-2022.")
  ) |>
  tab_footnote("Groupings have been simplified, some case outcomes are mixed or preliminary.") |>
  tab_source_note("TidyTuesday: 2023, week 35 | Source: U.S. Copyright Office Fair Use Index")

tab

gtsave(tab, filename = "images/2023_week35.png")
