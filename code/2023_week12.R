library(tidyverse)
library(tidytuesdayR)

# This script plots the 53 programming languages with the largest
# number of jobs per user. R is the 53rd language on this list.

tuesdata <- tidytuesdayR::tt_load(2023, week = 12)

# The type variable in tuesdata$languages is written in lower camel case.
# This function converts lower camel case to sentence case.
camelcase_to_sentence <- function(s) {
  str_replace_all(string = s, pattern = "([A-Z])", replacement = " \\1") |>
    str_to_lower() |>
    str_to_sentence()
}

# test can convert strings from camel case to sentence case
stopifnot(
  camelcase_to_sentence(s = "abcDefGhi") == "Abc def ghi",
  camelcase_to_sentence(s = "dataNotation") == "Data notation"
)

# format data for plot
df <- tuesdata$languages |>
  rename_with(~ str_replace(.x, "number_of", "n")) |>
  # remove languages with no users since jobs_per_user is then undefined
  filter(n_users > 0) |>
  mutate(
    jobs_per_user = n_jobs / n_users,
    rank = rank(-jobs_per_user),
    # expand pl, idl acronyms
    type = case_when(
      type == "idl" ~ "interfaceDefinitionLanguage",
      type == "pl" ~ "programmingLanguage",
      TRUE ~ type
    ),
    # improve formatting of type variable for clearer plot legend
    type = camelcase_to_sentence(type),
    type = ifelse(type == "Xml format", "XML format", type)
  )

# COBOL ranks 50th, R ranks 53rd, SAS ranks 55th in jobs per user
df |>
  filter(title %in% c("COBOL", "R", "SAS")) |>
  select(title, rank, n_jobs, n_users, jobs_per_user) |>
  arrange(rank)

rank_R <- df |>
  filter(title == "R") |>
  select(rank) |>
  as.numeric()

# plot programming languages by number of jobs per user
df |>
  slice_max(jobs_per_user, n = rank_R) |>
  ggplot(aes(x = reorder(title, jobs_per_user), y = jobs_per_user)) +
  geom_col(aes(fill = type), width = 0.75) +
  geom_label(
    aes(label = title, fill = type),
    size = 3.5,
    label.size = 0.05,
    show.legend = FALSE
  ) +
  scale_y_log10(limits = c(0.01, 100)) +
  coord_flip() +
  labs(
    x = element_blank(),
    y = "Jobs per User",
    fill = "Language Type",
    title = "Fifty-three Top Languages by Jobs per User",
    caption = "TidyTuesday: 2023, week 12."
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    legend.position = c(0.76, 0.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = 22)
  )

ggsave(
  filename = "images/2023_week12.png",
  height = 12,
  width = 9
)
