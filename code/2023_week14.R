library(tidyverse)
library(tidytuesdayR)

# The data this week describes 380 matches between 20 teams.
# This script:
# - performs basic EDA on the matches and teams;
# - plots the percentage of wins, ties, and losses by team.

# load data
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

# check each team played every other team exactly once
all(matches + diag(n_teams) == 1)

# tabulate wins per team
team_wins <- soccer |>
  # remove ties
  filter(HS != AS) |>
  # compute winning team
  mutate(team = case_when(
    HS > AS ~ HomeTeam,
    AS > HS ~ AwayTeam
  )) |>
  group_by(team) |>
  count(name = 'win') |>
  ungroup()

# tabulate losses per team
team_losses <- soccer |>
  # remove ties
  filter(HS != AS) |>
  # compute losing team
  mutate(team = case_when(
    HS < AS ~ HomeTeam,
    AS < HS ~ AwayTeam
  )) |>
  group_by(team) |>
  count(name = 'loss') |>
  ungroup()

# plot percentage of wins, losses, and ties by team
team_wins |>
  full_join(team_losses) |>
  replace_na(list(loss = 0)) |>
  mutate(team = fct_reorder(team, win)) |>
  # compute ties using win + loss + tie = 38
  mutate(tie = 38 - win - loss) |>
  pivot_longer(cols = c(win, loss, tie),
               names_to = 'outcome', values_to = 'n') |>
  group_by(team) |>
  mutate(proportion = n / sum(n)) |>
  ggplot(aes(team, proportion, fill = outcome)) +
  geom_col(alpha = 0.75, width = 0.8) +
  geom_hline(yintercept = seq(0.25, 0.75, by = 0.25), alpha = 0.4,
             linetype = 'dashed') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(breaks = c('win', 'tie', 'loss')) +
  coord_flip() +
  labs(
    x = '',
    y = 'Match Outcome (%)',
    fill = '',
    title = 'British Soccer Match Outcomes (2021-2022)',
    caption = 'TidyTuesday: 2023, week 14.'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    aspect.ratio = 1,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = 'top',
    plot.title = element_text(hjust = 0.5)
  )

ggsave(filename = './images/2023_week14.png', bg = 'white')
