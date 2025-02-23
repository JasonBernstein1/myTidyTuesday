library(scales)
library(tidyverse)

# This script plots views of Simpsons episodes over time, and includes
# labels for the highest and lowest viewed episodes.

tuesdata <- tidytuesdayR::tt_load(2025, week = 5)

episodes <- tuesdata$simpsons_episodes

episodes |>
  filter(views > 20000) |>
  ggplot() +
  geom_point(aes(x = original_air_date, y = views)) +
  geom_label(
    data = ~ filter(.x, views > 60000 | views < 33000),
    # Nudge some labels to avoid them overlapping each other
    aes(
      x = original_air_date +
        if_else(stringr::str_detect(title, c("XXIV")), -50, 0),
      y = views +
        if_else(stringr::str_detect(title, c("Brick")), -2200, 0) +
        if_else(stringr::str_detect(title, c("XXV")), 100, 0),
      label = stringr::str_replace(
        title,
        "Treehouse of Horror .*",
        "Treehouse of Horror"
      )
    ),
    vjust = -0.5,
    hjust = 0.5,
    size = 4,
    color = "black",
    fill = "white",
    label.size = 0.5,
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray70"),
    plot.background = element_rect(fill = "gray91"),
    plot.title = element_text(size = 22.5)
  ) +
  scale_x_date(
    breaks = as.Date(paste0(2010:2015, "-01-01")),
    date_labels = "%Y",
  ) +
  scale_y_continuous(labels = ~ scales::comma(.x / 1000)) +
  labs(
    title = "Highest and Lowest Viewed Simpsons Episodes Over Time",
    y = "Views (Thousands)",
    caption = "TidyTuesday: 2025, week 5 | Source: Simpsons Dataset from Kaggle."
  )

ggsave(
  filename = here::here("images", "2025_week05.png"),
  height = 9,
  width = 9
)
