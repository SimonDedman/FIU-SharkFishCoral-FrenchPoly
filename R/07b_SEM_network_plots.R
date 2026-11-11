# 07b_SEM_network_plots.R
# Fig. S40 rebuilt for the reanalysis (Science review round 1, R1,9): DAG
# network diagrams carrying the island-random-effect Bayesian path slopes
# (05b, --variant=revised2 --island-re) in place of the LOO-R2 values drawn by
# 07_DAG_network_plots.R (which stays as the record of the submitted figure).
#
# Author: Simon Dedman <simondedman@gmail.com> (+ Claude)
# Created: 2026-10-16
#
# Two versions per geomorphology (Simon, 2026-10-15: Fig. S4 keeps the
# whole-ecosystem role; S40 should show only the tested elements, and both a
# current full version and a tested-only version are wanted):
#   full    every DAG node and edge (thin grey), with the 11 edges tested for
#           direction drawn in colour: top-down (TD, red) and bottom-up (BU,
#           green, reversed). Same node layout as Fig. S4.
#   tested  measured nodes in the SEM equations only; the other fitted paths
#           thin grey; the 11 direction-tested paths in colour.
# Edge width = |standardised slope| (posterior median); solid where the 95% CI
# excludes zero, dashed where it includes zero; label = signed slope (bold
# where the CI excludes zero). Top right: mean |slope| over the 11 paths with
# 95% CI (05b brms_mean_abs_slope_by_geomorphology.csv).
#
# Inputs:
#   R/dags/FigS4_layout_revised2_2026-10-16_dagitty-code.txt  (Fig. S4 layout; copy of Presentations/2026-10-16_dagitty-model_revised2_code.txt)
#   Results/SEM/variants/revised2_islandRE/brms_slopes_by_geomorphology.csv
#   Results/SEM/variants/revised2_islandRE/brms_mean_abs_slope_by_geomorphology.csv
#   Results/SEM/variants/revised2_islandRE/equations_TD.csv
#   R/dags/overlay_edges_TD-direction.csv
# Outputs: Results/SEM/variants/revised2_islandRE/figS40/<today>_FigS40_<full|tested>_<Atolls|HighIslands>.png

suppressPackageStartupMessages({
  library(dagitty)
  library(tidyverse)
  library(here)
  library(ggrepel)
})

res_dir <- here("Results", "SEM", "variants", "revised2_islandRE")
out_dir <- file.path(res_dir, "figS40")
dir.create(out_dir, showWarnings = FALSE)

# Layout: the Fig. S4 dagitty code (positions in export pixels / 1000, y down)
dag <- dagitty(paste(readLines(here("R", "dags", "FigS4_layout_revised2_2026-10-16_dagitty-code.txt")),
                     collapse = "\n"))
co <- coordinates(dag)
nodes <- tibble(name = names(co$x), x = co$x * 1000, y = -co$y * 1000,
                latent = name %in% latents(dag))
dag_edges <- as_tibble(edges(dag)) |> transmute(from = v, to = w)

slopes <- read_csv(file.path(res_dir, "brms_slopes_by_geomorphology.csv"), show_col_types = FALSE)
means <- read_csv(file.path(res_dir, "brms_mean_abs_slope_by_geomorphology.csv"), show_col_types = FALSE)
eq_td <- read_csv(file.path(res_dir, "equations_TD.csv"), show_col_types = FALSE)
overlay <- read_csv(here("R", "dags", "overlay_edges_TD-direction.csv"), show_col_types = FALSE)

# Tested overlay paths: those with a fitted slope (the two edges into latent
# offshore_prey cannot be estimated). TD keeps the DAG direction, BU reverses it.
tested <- slopes |>
  filter(overlay_path) |>
  transmute(scenario, geomorphology, from = predictor, to = response,
            estimate, lower95, upper95, sig = lower95 > 0 | upper95 < 0)
stopifnot(nrow(tested) == 11 * 2 * 2)

# Other fitted (non-overlay) TD paths, for the tested-only version
other_fitted <- eq_td |> filter(!overlay_path) |> transmute(from = predictor, to = response)
sem_nodes <- unique(c(eq_td$response, eq_td$predictor))

add_xy <- function(df) {
  df |>
    left_join(nodes |> select(name, x, y), by = c("from" = "name")) |>
    left_join(nodes |> select(name, xend = x, yend = y), by = c("to" = "name"))
}
# pull line ends back from the node centres so arrowheads sit outside the label
shorten <- function(df, by = 45) {
  df |> mutate(len = sqrt((xend - x)^2 + (yend - y)^2),
               ux = (xend - x) / len, uy = (yend - y) / len,
               x = x + ux * by, y = y + uy * by,
               xend = xend - ux * by, yend = yend - uy * by)
}

td_col <- "#F8766D"
bu_col <- "#1b9e77"
curv <- 0.12

draw <- function(geo, version) {
  tp <- tested |> filter(geomorphology == geo) |> add_xy() |> shorten()
  # label position: midpoint pushed off the chord the way geom_curve bends
  # (curvature > 0 bends to the right of travel); TD and BU run opposite ways,
  # so they land on opposite sides of the shared chord
  tp <- tp |> mutate(
    mx = (x + xend) / 2, my = (y + yend) / 2,
    lab_x = mx + uy * len * curv * 0.5, lab_y = my - ux * len * curv * 0.5,
    lab = sprintf("%+.2f", estimate))
  if (version == "full") {
    nd <- nodes
    bg <- dag_edges |> add_xy() |> shorten()
  } else {
    nd <- nodes |> filter(name %in% sem_nodes)
    bg <- other_fitted |> add_xy() |> shorten()
  }
  g_lab <- ifelse(geo == "Atolls", "Atolls", "High islands")
  m <- means |>
    filter(geomorphology == g_lab) |>
    mutate(txt = sprintf("%s mean |β|: %.2f (%.2f–%.2f)", scenario, estimate, lower95, upper95))
  xr <- range(nodes$x); yr <- range(nodes$y)
  title <- paste0(ifelse(geo == "Atolls", "Atolls", "High islands"),
                  ifelse(version == "full", ": full DAG", ": tested elements"))
  ggplot() +
    geom_segment(data = bg, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = "grey70", linewidth = 0.3,
                 arrow = arrow(length = unit(0.06, "inches"), type = "closed")) +
    geom_curve(data = filter(tp, scenario == "TD"),
               aes(x = x, y = y, xend = xend, yend = yend,
                   linewidth = abs(estimate), linetype = ifelse(sig, "solid", "dashed")),
               colour = td_col, curvature = curv, alpha = 0.9, lineend = "butt",
               arrow = arrow(length = unit(0.13, "inches"), type = "open")) +
    geom_curve(data = filter(tp, scenario == "BU"),
               aes(x = x, y = y, xend = xend, yend = yend,
                   linewidth = abs(estimate), linetype = ifelse(sig, "solid", "dashed")),
               colour = bu_col, curvature = curv, alpha = 0.9, lineend = "butt",
               arrow = arrow(length = unit(0.13, "inches"), type = "open")) +
    scale_linetype_identity() +
    scale_linewidth_continuous(range = c(0.3, 2.6), limits = c(0, 1)) +
    geom_text_repel(data = tp, aes(x = lab_x, y = lab_y, label = lab,
                                   colour = scenario, fontface = ifelse(sig, "bold", "plain")),
                    size = 3.2, bg.colour = "white", bg.r = 0.2,
                    point.size = NA, min.segment.length = Inf, box.padding = 0.05,
                    force = 0.5, max.overlaps = Inf, seed = 1) +
    scale_colour_manual(values = c(TD = td_col, BU = bu_col), guide = "none") +
    geom_point(data = nd, aes(x = x, y = y), size = 0.6) +
    geom_label(data = nd, aes(x = x, y = y, label = name),
               colour = ifelse(nd$latent, "grey45", "black"), size = 3.1,
               fill = "white", label.size = NA, label.padding = unit(0.05, "lines"),
               vjust = 1.5) +
    annotate("text", x = xr[2], y = yr[2] - c(0, 55), hjust = 1, size = 4,
             label = m$txt[match(c("Top-down", "Bottom-up"), m$scenario)],
             colour = c(td_col, bu_col)) +
    labs(title = title,
         subtitle = "Standardised path slopes, Bayesian SEM with island random intercept (brms)",
         caption = paste0(Sys.Date(), "; red = top-down, green = bottom-up; width = |slope|; ",
                          "solid / bold = 95% CI excludes zero, dashed = includes zero")) +
    coord_fixed(clip = "off") +
    theme_void(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(size = 8), legend.position = "none",
          plot.background = element_rect(fill = "white", colour = "white"),
          plot.margin = margin(10, 20, 10, 20))
}
for (geo in c("HighIslands", "Atolls")) {
  for (version in c("full", "tested")) {
    p <- draw(geo, version)
    f <- file.path(out_dir, paste0(Sys.Date(), "_FigS40_", version, "_", geo, ".png"))
    ggsave(f, p, dpi = 300, width = 10, height = 8)
    cat("wrote", f, "\n")
  }
}
cat("tested paths:", nrow(tested) / 4, "per scenario x geomorphology;",
    sum(tested$sig), "of", nrow(tested), "slopes with 95% CI excluding zero\n")
