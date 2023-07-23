library(tidyverse)
library(tidytuesdayR)
library(gt)

# This script creates a table from data on predictions from GPT detectors.

tuesdata <- tidytuesdayR::tt_load(2023, week = 29)

df <- tuesdata$detectors |>
  summarise(
    accuracy = mean(kind == .pred_class),
    sample_size = n(),
    .by = c(model, name)
  ) |>
  mutate(
    prompt_engineered = ifelse(str_detect(name, 'PE|Prompt Engineered'),
                               'Yes', 'No'),
    name = str_remove(name, ' Prompt Engineered|. PE')
  ) |>
  arrange(desc(accuracy))

gpt_table <- df |>
  group_by(model) |>
  gt() |>
  fmt_number(decimals = 2, columns = 'accuracy') |>
  tab_style(
    style = list(
      cell_fill(color = "coral2", alpha = 0.3),
      cell_borders(sides = "all", color = "black",
                   style = "solid", weight = px(2.5)),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups(groups = 1:3)
  ) |>
  tab_header(title = "GPT Detector Prediction Accuracy") |>
  tab_style(
    style = list(
      cell_fill(color = "coral", alpha = 0.05),
      cell_borders(sides = "all", color = "black",
                   style = "solid", weight = px(2))
    ),
    locations = cells_body()
  ) |>
  cols_align(
    align = c("center"),
    columns = c('accuracy', 'sample_size', 'prompt_engineered')
  ) |>
  cols_label(
    prompt_engineered = "**Prompt<br>Engineered**",
    name = "**Data Generating Process &<br>Prediction Source**",
    accuracy = "**Prediction<br>Accuracy**",
    sample_size = "**Sample<br>Size**",
    .fn = md
  ) |>
  tab_source_note('TidyTuesday: 2023, week 29 | Source: Liang et al. (2023), arXiv:2304.02819') |>
  tab_options(
    heading.background.color = "coral4",
    heading.title.font.size = 24,
    heading.border.bottom.color = 'black',
    heading.padding = "9px",
    table_body.border.bottom.color = 'black',
    table.border.bottom.color = 'black',
    table.border.top.color = 'black',
    table.border.bottom.width = "2.5px"
  )

gpt_table

gtsave(gpt_table, filename = './images/2023_week29.png')
