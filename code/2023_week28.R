library(tidyverse)
library(tidytuesdayR)

# This script plots temperature anomalies for the northern and southern
# hemispheres from Jan. 1880 to May 2023.

tuesdata <- tidytuesdayR::tt_load(2023, week = 28)

global_temps <- tuesdata$global_temps
nh_temps <- tuesdata$nh_temps
sh_temps <- tuesdata$sh_temps

# wrangle data for plot
df <- nh_temps |>
  bind_rows(sh_temps) |>
  mutate(
    hemisphere = rep(
      c("Northern", "Southern"),
      c(nrow(nh_temps), nrow(sh_temps))
    )
  ) |>
  # go from Jan, Feb, ..., Dec columns to single month column
  pivot_longer(
    cols = tidyselect::matches(
      match = "[A-Z][a-z][a-z]\\b",
      ignore.case = FALSE
    ),
    names_to = "month", values_to = "temp"
  ) |>
  mutate(
    date = lubridate::ymd(paste(Year, month, "01"))
  ) |>
  # smooth temperature data for each hemisphere with a moving average
  group_by(hemisphere) %>%
  mutate(
    temp_smoothed = stats::filter(temp, rep(1 / 24, 24), sides = 2)
  ) |>
  ungroup() |>
  # remove records with missing temp, June 2023 - Dec. 2023
  filter(!is.na(temp)) |>
  select(date, month, temp, temp_smoothed, hemisphere)

# extract max and min temperatures to indicate these values on the plot
maxmin_temp <- df |>
  filter(temp == min(temp) | temp == max(temp)) |>
  mutate(
    date_label = as.Date(date) |> format(format = "%b. %Y"),
    date_posn = ifelse(temp < 0, date %m+% years(18), date %m-% years(18))
  )

df |>
  ggplot() +
  # reference line for no anomaly
  geom_abline(intercept = 0, slope = 0) +
  geom_point(
    aes(x = date, y = temp),
    shape = 21, fill = "lightgray", color = "darkgray", alpha = 0.7
  ) +
  geom_line(
    aes(x = date, y = temp_smoothed, col = hemisphere),
    linewidth = 1
  ) +
  scale_color_manual(values = c("#DC143C", "#0000CD")) +
  annotate(
    geom = "label", x = lubridate::ymd("1925 01 01"), y = 1.5,
    label = "Global Surface Temperature\nAnomaly over Time",
    size = 9, fill = "#F5F5F5", label.padding = unit(0.5, "lines")
  ) +
  scale_x_continuous(
    breaks = lubridate::ymd(paste(seq(1875, 2025, 25), "01 01")),
    labels = seq(1875, 2025, 25)
  ) +
  # labels for the largest and smallest temperature anomalies
  geom_segment(
    data = maxmin_temp,
    aes(x = date_posn, y = temp, xend = date, yend = temp),
    arrow = arrow(length = unit(0.01, "npc")),
    linewidth = 0.5, alpha = 0.75
  ) +
  geom_label(
    data = maxmin_temp,
    aes(
      x = date_posn, y = temp,
      label = glue::glue("{date_label}: {temp} {'\u00B0'}C")
    ),
    fill = "lightgoldenrodyellow"
  ) +
  labs(
    color = "Hemisphere",
    x = element_blank(),
    y = glue::glue("Temperature Anomaly ({'\u00B0'}C)"),
    caption = "TidyTuesday: 2023, week 28 | Source: NASA GISS Surface Temperature Analysis"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.885, 0.275),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(0.3, 0.3, 0.2, 0.3, "cm")
  )

ggsave(
  filename = "images/2023_week28.png",
  height = 7, width = 9
)
