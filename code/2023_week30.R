library(tidyverse)
library(tidytuesdayR)
library(gt)
library(RColorBrewer)

# This script creates a table showing scurvy symptom severity in seamen
# and whether they were fit for duty or not.

tuesdata <- tidytuesdayR::tt_load(2023, week = 30)

df <- tuesdata$scurvy |>
  select(study_id, ends_with("d6")) |>
  rename(subject = study_id) |>
  # format column names
  rename_with(~ stringr::str_remove_all(.x, pattern = "_d6|_the")) |>
  rename_with(~ stringr::str_replace_all(.x, pattern = "_", replacement = " ")) |>
  # remove numbers before symptom severities in table
  mutate(across(everything(), ~ stringr::str_remove(.x, "\\d_")))

scurvy_table <- df |>
  gt() |>
  # set same colors for all symptom severities across columns
  data_color(
    columns = 2:5,
    fn = scales::col_factor(
      palette = colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))(4),
      levels = c('none', 'mild', 'moderate', 'severe')
    )
  ) |>
  tab_style(
    style = list(
      cell_borders(
        sides = "all",
        color = "white",
        style = "solid",
        weight = px(1)
      )
    ),
    locations = cells_body()
  ) |>
  cols_label(
    everything() ~ stringr::str_to_sentence(names(df))
  ) |>
  cols_align(
    align = "center"
  ) |>
  tab_header(
    title = "Seamen with Scurvy Symptoms Unfit for Duty",
    subtitle = "One of Twelve Subjects in Study is Fit for Duty"
  ) |>
  # indicate fitness for duty, or not, in green or red text, respectively
  tab_style_body(
    fn = function(x) x == "yes",
    style = cell_text(color = 'darkgreen', weight = 'bold'),
    columns = `fit for duty`
  ) |>
  tab_style_body(
    fn = function(x) x == "no",
    style = cell_text(color = 'tomato', weight = 'bold'),
    columns = `fit for duty`
  ) |>
  opt_row_striping(row_striping = TRUE) |>
  tab_source_note('TidyTuesday: 2023, week 30 | Source: medicaldata R Package (P. Higgins)') |>
  tab_options(
    column_labels.background.color = 'white',
    heading.title.font.size = 22,
    heading.background.color = 'aliceblue',
    heading.subtitle.font.size = 18,
    row.striping.background_color = 'grey93',
  )

scurvy_table

gtsave(scurvy_table, filename = './images/2023_week30.png')
