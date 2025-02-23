library(tidyverse)
library(tidytuesdayR)
library(gt)
library(emo)

# This script creates a table summarizing asylum seeker data by
# country and includes flags of the countries.

tuesdata <- tidytuesdayR::tt_load(2023, week = 34)

population <- tuesdata$population

df <- population |>
  filter(coo_name != "Unknown") |>
  summarise(
    asylum_from = sum(asylum_seekers),
    .by = c(coo_name, coo_iso)
  ) |>
  rename(
    country = coo_name,
    country_iso = coo_iso
  ) |>
  slice_max(n = 15, asylum_from) |>
  left_join(
    population |>
      summarise(
        asylum_to = sum(asylum_seekers),
        .by = coa_name
      ),
    by = join_by(country == coa_name)
  ) |>
  mutate(
    # shorten country names for table
    country = case_match(
      country_iso,
      "COD" ~ "Congo-Kinshasa",
      "IRN" ~ "Iran",
      "SYR" ~ "Syria",
      "VEN" ~ "Venezuela",
      .default = as.character(country)
    ),
    # specify country id for flag emoji
    flag_emoji_id = case_when(
      country_iso == "VEN" ~ "Venezuela",
      country_iso == "SYR" ~ "Syria",
      # there are two flag emojis with the name Congo, emo::flag("Congo")
      # COD is the Dem. Rep. of Congo, or "Congo - Kinshasa"
      country_iso == "COD" ~ "Congo - Kinshasa",
      # there are three flag emojis with the name China, emo::flag("China")
      # CHN is the "Five-star Red Flag"
      country_iso == "CHN" ~ "^China",
      country_iso == "IRN" ~ "Iran",
      .default = as.character(country)
    ),
    flag = purrr::map(flag_emoji_id, emo::flag),
    .after = country
  ) |>
  arrange(desc(asylum_from)) |>
  select(country, flag, asylum_from, asylum_to)

# number of asylum seekers from unknown country of origin, format for table
n_unknown_coo <- population |>
  filter(coo_name == "Unknown") |>
  pull(asylum_seekers) |>
  sum() |>
  format(big.mark = ",", scientific = FALSE)

flag_table <- df |>
  gt() |>
  cols_label(
    country = "Country",
    asylum_from = "From",
    asylum_to = "To",
    flag = "Flag",
    .fn = \(x) md(glue::glue("**{x}**"))
  ) |>
  fmt_number(columns = 3:4, decimals = 0) |>
  grand_summary_rows(
    columns = 3:4,
    side = "bottom",
    fns = list(
      Total ~ sum(.)
    ),
    fmt = list(~ fmt_number(., drop_trailing_zeros = TRUE))
  ) |>
  tab_header(
    title = "Countries with the Highest Number of Asylum Seekers",
    subtitle = "Over 20 million asylum seekers from these 15 countries. 2010-2022."
  ) |>
  tab_footnote(md(glue::glue(
    "{n_unknown_coo} asylum seekers were from unknown countries of origin."
  ))) |>
  tab_source_note(md(
    "TidyTuesday: 2023, week 34 | Source: {<span style=\"color:darkred\">refugees</span>} R package"
  )) |>
  opt_table_font(
    font = list(
      gt::google_font(name = "Rubik")
    )
  ) |>
  opt_stylize(style = 1, color = "gray") |>
  tab_options(
    # override color defaults from opt_stylize(style = 1)
    row.striping.background_color = "gray96",
    stub.background.color = "gray96"
  )

flag_table

gtsave(flag_table, filename = here::here("images", "2023_week34.png"))
