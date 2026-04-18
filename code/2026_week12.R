library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2026, week = 12)
df <- tuesdata$pi_digits

# log-spaced sample positions to keep plot rendering fast
sample_positions <- unique(round(10^seq(0, 6, length.out = 20000)))

plot_data <- df |>
  arrange(digit_position) |>
  mutate(
    is_even = digit %% 2 == 0,
    rel_even = cumsum(is_even) / digit_position,
    rel_odd = cumsum(!is_even) / digit_position
  ) |>
  filter(digit_position %in% sample_positions) |>
  dplyr::select(digit_position, rel_even, rel_odd) |>
  pivot_longer(
    c(rel_even, rel_odd),
    names_to = "parity",
    values_to = "rel_freq"
  ) |>
  mutate(parity = if_else(parity == "rel_even", "Even", "Odd"))

ggplot(plot_data, aes(x = digit_position, y = rel_freq, color = parity)) +
  geom_step(linewidth = 1.0, alpha = 0.9) +
  scale_x_log10(
    limits = c(1, 1e6),
    breaks = 10^(0:6),
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(xlim = c(1, 1e6), ylim = c(0.15, 0.85)) +
  scale_color_manual(values = c("Even" = "#e66101", "Odd" = "#5e3c99")) +
  labs(
    title = "Relative Frequency of Even vs. Odd Digits in the First Million Digits of Pi",
    x = "Digit Position",
    y = "Relative Frequency",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.6),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(
      fill = "white",
      color = "grey80",
      linewidth = 0.3
    ),
    plot.title = element_text(size = 19, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.background = element_rect(fill = "#f0edf6", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  filename = here::here("images", "2026_week12.png"),
  height = 10,
  width = 12
)
