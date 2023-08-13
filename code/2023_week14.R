library(tidyverse)
library(tidytuesdayR)

# The data this week describes 380 matches between 20 teams.
# This script:
# - performs basic EDA on the matches and teams;
# - plots the percentage of wins, ties, and losses by team.

tuesdata <- tidytuesdayR::tt_load(2023, week = 14)

soccer <- tuesdata$soccer

# tabulate the number of games between pairs of teams
matches <- soccer |>
  select(HomeTeam, AwayTeam) |>
  table()

# 20 teams total
n_teams <- n_distinct(c(soccer$HomeTeam, soccer$AwayTeam))

# each team played 38 games, 19 away and 19 at home
rowSums(matches) # number of home games
colSums(matches) # number of away games

# check each team played every other team once away and once at home
all(matches + diag(n_teams) == 1)

wins_per_team <- soccer |>
  # remove ties
  filter(HS != AS) |>
  mutate(
    team = ifelse(HS > AS, HomeTeam, AwayTeam)
  ) |>
  count(team, name = "win")

losses_per_team <- soccer |>
  # remove ties
  filter(HS != AS) |>
  mutate(
    team = ifelse(HS < AS, HomeTeam, AwayTeam)
  ) |>
  count(team, name = "loss")

# compute number of wins, losses, and ties by team
team_stats <- wins_per_team |>
  full_join(losses_per_team) |>
  replace_na(list(loss = 0)) |>
  mutate(
    team = fct_reorder(team, win),
    # compute ties using win + loss + tie = 38
    tie = 38 - win - loss
  ) |>
  pivot_longer(
    cols = c(win, loss, tie),
    names_to = "outcome",
    values_to = "n"
  )

# plot percentage of wins, losses, and ties by team
team_stats |>
  ggplot(aes(x = n, y = team, fill = outcome)) +
  geom_col(alpha = 0.75, width = 0.8, position = "fill") +
  geom_vline(
    xintercept = seq(0.25, 0.75, by = 0.25),
    alpha = 0.4, linetype = "dashed"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = thematic::okabe_ito(3),
    guide = guide_legend(reverse = TRUE),
    labels = c("win" = "Win", "tie" = "Tie", "loss" = "Loss")
  ) +
  labs(
    x = "Match Outcome (%)",
    y = element_blank(),
    fill = element_blank(),
    title = "British Soccer Match Outcomes (2021-2022)",
    caption = "TidyTuesday: 2023, week 14."
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.position = "top",
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 20)
  )

ggsave(
  filename = "./images/2023_week14.png",
  height = 9, width = 9
)
