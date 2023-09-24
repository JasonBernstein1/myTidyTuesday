library(tidyverse)
library(tidytuesdayR)
library(ggraph)
library(tidygraph)

# This script creates a graph indicating which R package authors
# have not written an R package together. The nine authors with the
# most R packages are shown.

tuesdata <- tidytuesdayR::tt_load(2023, week = 38)

authors <- tuesdata$package_authors
nodes <- tuesdata$cran_graph_nodes
edges <- tuesdata$cran_graph_edges

top_authors <- authors |>
  rename(name = authorsR) |>
  count(name) |>
  # Note, Achim Zeileis and R Core Team tied with 69 packages each
  slice_max(order_by = n, n = 9, with_ties = FALSE)

top_author_nodes <- nodes |>
  mutate(
    node_id = row_number()
  ) |>
  right_join(top_authors, by = join_by(name)) |>
  arrange(desc(n)) |>
  mutate(
    name = str_replace(name, " ", "\n")
  ) |>
  select(name, node_id)

top_author_edges <- edges |>
  rename(
    from_id = from,
    to_id = to
  ) |>
  # only keep edges whose nodes are top authors
  inner_join(
    top_author_nodes |>
      rename(from_name = name),
    by = join_by(from_id == node_id)
  ) |>
  inner_join(
    top_author_nodes |>
      rename(to_name = name),
    by = join_by(to_id == node_id)
  ) |>
  rename(
    from = from_name,
    to = to_name
  ) |>
  select(from, to)

all_possible_edges <- top_author_nodes |>
  purrr::pluck("name") |>
  combn(2) |>
  t() |>
  as_tibble() |>
  rename(from = V1, to = V2)

missing_edges <- all_possible_edges |>
  anti_join(
    top_author_edges,
    by = join_by(from == from, to == to)
  ) |>
  anti_join(
    top_author_edges,
    by = join_by(from == to, to == from)
  ) |>
  # define variable to color edges in graph based on from node
  mutate(
    edge_id = case_when(
      from == "Rstudio" ~ "from_Rstudio",
      str_detect(from, "Gabor") ~ "from_Gabor",
      str_detect(from, "Dirk") ~ "from_Dirk",
      str_detect(from, "Hadley") ~ "from_Hadley",
      TRUE ~ "other"
    )
  )

graph <- tidygraph::tbl_graph(
  nodes = top_author_nodes,
  edges = missing_edges
)

graph |>
  ggraph::ggraph(layout = "grid") +
  ggraph::geom_edge_arc(
    aes(color = edge_id),
    strength = 0.1, edge_width = 1.2
  ) +
  ggraph::scale_edge_color_manual(
    values = setNames(
      object = thematic::okabe_ito(5),
      nm = purrr::pluck(missing_edges, "edge_id") |>
        unique()
    )
  ) +
  ggraph::geom_node_label(
    aes(label = name),
    size = 6, fill = "white", label.padding = unit(0.4, "lines"),
    label.size = 0.4
  ) +
  coord_cartesian(xlim = c(-0.2, 2.2), ylim = c(2.1, -0.1)) +
  labs(
    title = "Who has NOT written an R package together?",
    subtitle = "Showing the authors with the most packages",
    caption = "TidyTuesday: 2023, week 38 | Source: CRAN Collaboration Graph (David Schoch)"
  ) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#c5e6ed"),
    plot.background = element_rect(fill = "#c5e6ed"),
    plot.caption = element_text(family = "Arial", face = "plain"),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    plot.title = element_text(size = 24, hjust = 0.5)
  )

ggsave(
  filename = "images/2023_week38.png",
  height = 8, width = 8
)
