library(tidyverse)
library(tidytuesdayR)
library(fontawesome)
library(glue)
library(gt)

# This script creates a table summarizing the output of a logistic regression
# model fit with the glm function. This is an exercise in table making, not
# inference or prediction.

# Load and wrangle data ---------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 33)

spam <- tuesdata$spam |>
  mutate(
    y = ifelse(yesno == "y", 1, 0)
  )

# Helper functions --------------------------------------------------------

get_arrow_html <- function(x) {
  ifelse(
    x > 0,
    fontawesome::fa("arrow-up", fill = "green"),
    fontawesome::fa("arrow-down", fill = "red")
  ) |>
    gt::html()
}

format_coef <- function(variable_name, color) {
  # bold and add color to variable name in parentheses
  variable_name |>
    str_replace(
      "\\(",
      glue::glue("(**<span style=\"color:{color}\">")
    ) |>
    str_replace(
      "\\)",
      glue::glue("<span style=\"color:{color}\">**)")
    )
}

# Fit logistic regression model -------------------------------------------

# transform predictors using log1p function since values can be zero
# and span several orders of magnitude
fitted_model <- glm(
  y ~
    log1p(crl.tot) +
      log1p(dollar) +
      log1p(bang) +
      log1p(money) +
      log1p(n000) +
      log1p(make),
  family = "binomial",
  data = spam
)

# wrangle summary of model fit for table
df <- coef(summary(fitted_model)) |>
  as_tibble() |>
  mutate(
    Coefficient = rownames(coef(summary(fitted_model))),
    Effect = purrr::map(Estimate, get_arrow_html),
    signif = case_when(
      `Pr(>|z|)` > 0.1 ~ "",
      `Pr(>|z|)` < 0.1 & `Pr(>|z|)` > 0.05 ~ ".",
      `Pr(>|z|)` < 0.05 & `Pr(>|z|)` > 0.01 ~ "*",
      `Pr(>|z|)` < 0.01 & `Pr(>|z|)` > 0.001 ~ "**",
      TRUE ~ "***"
    )
  ) |>
  relocate(Coefficient, Effect, Estimate) |>
  arrange(desc(Estimate))

# Create summary table ----------------------------------------------------

footnote <- summary(fitted_model) |>
  capture.output() |>
  tibble() |>
  slice(17:25) |>
  pull(`capture.output(summary(fitted_model))`) |>
  paste(collapse = "<br>")

tab <- df |>
  mutate(
    Coefficient = format_coef(Coefficient, "maroon")
  ) |>
  gt() |>
  cols_align(
    align = "center",
    columns = 2:7
  ) |>
  cols_label(
    `z value` = "Statistic",
    `Pr(>|z|)` = "p-value",
    signif = "Signif."
  ) |>
  tab_style(
    style = cell_text(
      color = "darkblue",
      weight = "bolder"
    ),
    locations = cells_column_labels()
  ) |>
  fmt_number(columns = 3:5, n_sigfig = 3) |>
  fmt_scientific(columns = 6, decimals = 2) |>
  fmt_markdown(columns = "Coefficient") |>
  tab_header(
    title = "Spam E-mail Prediction",
    subtitle = "Logistic Regression Model Summary"
  ) |>
  tab_footnote(gt::html(footnote)) |>
  tab_source_note(
    "TidyTuesday: 2023, week 33 | \
     Source: Rdatasets package (V. Arel-Bundock)"
  ) |>
  opt_row_striping(row_striping = TRUE) |>
  tab_options(
    heading.title.font.size = 26,
    heading.subtitle.font.size = 18,
    row.striping.background_color = "gray96",
    table.border.bottom.style = "hidden",
    table.border.top.style = "hidden"
  )

tab

gtsave(tab, filename = here::here("images", "2023_week33.png"))
