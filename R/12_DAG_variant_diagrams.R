# 12_DAG_variant_diagrams.R
# Diagrams of the SEM DAG variants compared in 05b (Simon's Q1/Q4 decisions,
# 2026-09-25), drawn with the layer-1 stylised-DAG code from 09_DAG_stylised.R
# (same node layout, colours, and edge styling; 09 itself stays frozen).
#
# Author: Simon Dedman <simondedman@gmail.com> (+ Claude)
# Created: 2026-09-25
#
# Panels (Results/DAG/Stylised/<today>_DAG_variant_<name>.png):
#   original          TD DAG 7VHv3H73 + invertivores -> crown_of_thorns
#   revised           original + NPP/temperature -> each prey guild + a latent
#                     unmeasured_guild_driver -> the three guilds; the added
#                     edges are drawn in red
#   coral9 / coral7lat / coral7env
#                     hard_coral's equation under the three Q4 options: kept
#                     in-edges solid black, dropped ones dashed red. coral7lat
#                     drops the two latent-mediated paths (invertivores via
#                     crown_of_thorns; planktivores via spawning/recruitment),
#                     coral7env drops pop_dens and relief.
#
# Inputs: R/dags/*.dag via R/utils/dag_directions.R
# Packages: dagitty, ggdag, tidyverse, ggrepel, here

suppressPackageStartupMessages({
  library(dagitty)
  library(ggdag)
  library(tidyverse)
  library(ggrepel)
  library(here)
})
source(here("R", "utils", "dag_directions.R"))
out_dir <- here("Results", "DAG", "Stylised")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- copied from 09_DAG_stylised.R: tested links, colours, node layout -------
tested <- tribble(
  ~a, ~b, ~grp,
  "sicklefin_lemon_sharks", "reef_sharks",            "Large mesopredators",
  "sicklefin_lemon_sharks", "piscivores",             "Large mesopredators",
  "reef_sharks",            "planktivores",           "Mesopredators",
  "reef_sharks",            "invertivores",           "Mesopredators",
  "reef_sharks",            "herbivores",             "Mesopredators",
  "piscivores",             "planktivores",           "Mesopredators",
  "piscivores",             "invertivores",           "Mesopredators",
  "piscivores",             "herbivores",             "Mesopredators",
  "herbivores",             "crustose_coraline_algae","Basal",
  "herbivores",             "hard_coral",             "Basal",
  "herbivores",             "other_algae",            "Basal"
) |>
  mutate(key = pmap_chr(list(a, b), ~ paste(sort(c(...)), collapse = "|")))

grp_cols <- c(
  "Large mesopredators" = "#1F6FB2",
  "Mesopredators"       = "#E07B1A",
  "Basal"               = "#2E8B40"
)

node_pos <- tribble(
  ~name,                       ~x,   ~y,
  "pop_dens",                 3.7, 9.5,
  "sicklefin_lemon_sharks",   0.0,  9.3,
  "reef_sharks",             -1.7,  8.2,
  "piscivores",               1.7,  8.2,
  "other_offshore_prey_proxies", -4.4, 6.3,
  "offshore_prey",           -4.4,  7.0,
  "planktivores",            -2.7,  7.0,
  "invertivores",            -0.8,  7.0,
  "herbivores",               1.3,  7.0,
  "crown_of_thorns",         -2.3,  6.0,
  "invert",                  -1.3,  5.3,
  "zooplankton",             -2.7,  4.6,
  "hard_coral",               0.6,  4.0,
  "crustose_coraline_algae",  2.3,  4.0,
  "other_algae",              3.7,  4.3,
  "coral_recruitment",        0.0,  4.6,
  "coral_spawning",          -0.3,  5.1,
  "relief",                  -3.1,  2.6,
  "ave_npp",                  0.2,  2.7,
  "turbidity",                3.9,  2.9,
  "ave_temp",                 1.7,  2.1,
  "nutrient_run_off",        -2.2,  2.1,
  "bed_shear_stress",         3.4,  1.2,
  "depth",                    3.0,  -0.2,
  "cyclones",                -4.3,  1.5,
  "lagoon_size",             -2.6,  1.2,
  "wave_exposure",            4.4,  0.7,
  "emerged_land_area",        0.5,  1.5,
  "light",                    1.0,  0.2,
  "island_geomorphology",     0.0,  0.3,
  "latitude",                -1.5,  -0.5,
  # added in this script (nodes not in 09's DAG)
  "bleaching history",        2.9,  5.4,
  "disease history",          3.6,  5.9,
  "unmeasured_guild_driver",  3.3,  7.3
)

# ---- drawing function: 09's plot code, parameterised -----------------------
# highlight: data.frame(from, to, style) with style in c("added", "dropped");
# added = solid red, dropped = dashed red. Other edges as in 09.
draw_dag <- function(dag, title, highlight = NULL, file) {
  stopifnot(all(names(dag) %in% node_pos$name))
  e <- edges(dag) |>
    transmute(from = v, to = w,
              key = pmap_chr(list(from, to), ~ paste(sort(c(...)), collapse = "|"))) |>
    left_join(select(tested, key, grp), by = "key") |>
    mutate(tested = !is.na(grp))
  if (!is.null(highlight)) {
    e <- e |> left_join(highlight, by = c("from", "to")) |>
      mutate(style = replace_na(style, "normal"))
    # dropped edges may be absent from the DAG (latent-mediated paths): add them
    extra <- anti_join(highlight, e, by = c("from", "to")) |>
      mutate(key = NA_character_, grp = NA_character_, tested = FALSE)
    e <- bind_rows(e, extra)
  } else {
    e$style <- "normal"
  }
  e <- e |>
    left_join(node_pos, by = c("from" = "name")) |>
    left_join(rename(node_pos, xend = x, yend = y), by = c("to" = "name"))
  trim_r <- 0.22
  e <- e |>
    mutate(
      .len = sqrt((xend - x)^2 + (yend - y)^2),
      .f = if_else(.len > 0, trim_r / .len, 0),
      x0 = x + (xend - x) * .f, y0 = y + (yend - y) * .f,
      x1 = xend - (xend - x) * .f, y1 = yend - (yend - y) * .f
    )
  nodes <- node_pos |>
    filter(name %in% names(dag)) |>
    mutate(status = if_else(name %in% latents(dag), "Unmeasured", "Measured"), label = name)
  structural <- filter(e, !tested, style == "normal")
  tested_e <- filter(e, tested, style == "normal")
  added <- filter(e, style == "added")
  in_eq <- filter(e, style == "in_eq")
  dropped <- filter(e, style == "dropped")
  p <- ggplot() +
    geom_segment(data = structural, aes(x = x0, y = y0, xend = x1, yend = y1),
                 colour = "grey70", linewidth = 0.3,
                 arrow = arrow(length = unit(5, "pt"), type = "closed"), lineend = "round") +
    geom_segment(data = tested_e, aes(x = x0, y = y0, xend = x1, yend = y1, colour = grp),
                 linewidth = 2.4, arrow = arrow(length = unit(9, "pt"), ends = "both", type = "closed"),
                 lineend = "round") +
    geom_segment(data = added, aes(x = x0, y = y0, xend = x1, yend = y1),
                 colour = "#C0392B", linewidth = 1.4,
                 arrow = arrow(length = unit(8, "pt"), type = "closed"), lineend = "round") +
    geom_segment(data = in_eq, aes(x = x0, y = y0, xend = x1, yend = y1),
                 colour = "#6A3D9A", linewidth = 1.4,
                 arrow = arrow(length = unit(8, "pt"), type = "closed"), lineend = "round") +
    geom_segment(data = dropped, aes(x = x0, y = y0, xend = x1, yend = y1),
                 colour = "#6A3D9A", linewidth = 1.2, linetype = "22",
                 arrow = arrow(length = unit(8, "pt"), type = "closed"), lineend = "round") +
    scale_colour_manual(values = grp_cols, name = "Tested links") +
    geom_point(data = nodes, aes(x = x, y = y, fill = status),
               shape = 21, size = 7, colour = "grey30", stroke = 0.4) +
    scale_fill_manual(values = c(Measured = "#4DA6D6", Unmeasured = "#D2D2D2"), name = "Node") +
    ggrepel::geom_text_repel(data = nodes, aes(x = x, y = y, label = label),
                             size = 2.6, colour = "grey10", fontface = "bold",
                             bg.color = "white", bg.r = 0.32, force = 0, force_pull = 0,
                             max.overlaps = Inf, point.size = NA, min.segment.length = Inf,
                             box.padding = 0, seed = 1) +
    coord_fixed(clip = "off") +
    labs(title = title) +
    theme_void(base_size = 13) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", size = 13),
          plot.background = element_rect(fill = "white", colour = "white"),
          plot.margin = margin(20, 20, 20, 20))
  ggsave(file, plot = p, dpi = 200, width = 12, height = 11, bg = "white")
  message("saved: ", file)
  invisible(p)
}

dag_orig <- dag_td("FP_DAG_TopDown_2026-09-24_from7VHv3H73_plusInvertivoresCOTS.dag")
dag_rev <- dag_td("FP_DAG_TopDown_2026-09-25_revised_npp-temp-guilds_latentGuildDriver.dag")
ek <- function(g) paste(edges(g)$v, edges(g)$w)
added_edges <- edges(dag_rev)[!ek(dag_rev) %in% ek(dag_orig), c("v", "w")] |>
  transmute(from = v, to = w, style = "added")
stopifnot(nrow(added_edges) == 9)

# hard_coral's equation under the three options (predictors from 05b's rule)
coral_direct <- tibble(from = c("ave_npp", "ave_temp", "crustose_coraline_algae", "herbivores",
                                "other_algae", "pop_dens", "relief"), to = "hard_coral")
coral_latent <- tibble(from = c("invertivores", "planktivores"), to = "hard_coral") # via COTS / recruitment
coral_opts <- list(
  coral9 = bind_rows(coral_direct, coral_latent) |> mutate(style = "in_eq"),
  coral7lat = bind_rows(coral_direct |> mutate(style = "in_eq"), coral_latent |> mutate(style = "dropped")),
  coral7env = bind_rows(
    coral_direct |> mutate(style = if_else(from %in% c("pop_dens", "relief"), "dropped", "in_eq")),
    coral_latent |> mutate(style = "in_eq"))
)

stamp <- format(Sys.Date())
draw_dag(dag_orig, "Original DAG (TD 7VHv3H73 + invertivores → crown_of_thorns)",
         file = file.path(out_dir, paste0(stamp, "_DAG_variant_original.png")))
draw_dag(dag_rev, "Revised DAG: red = added (NPP, temperature → prey guilds; latent unmeasured_guild_driver → guilds)",
         highlight = added_edges,
         file = file.path(out_dir, paste0(stamp, "_DAG_variant_revised.png")))
for (nm in names(coral_opts)) {
  ttl <- c(coral9 = "hard_coral equation, option coral9: all 9 predictors (purple = term in the equation; nothing is added to the DAG)",
           coral7lat = "hard_coral equation, option coral7lat: 7 predictors (purple = in the equation; dashed = dropped from it)",
           coral7env = "hard_coral equation, option coral7env: 7 predictors (purple = in the equation; dashed = dropped from it)")[[nm]]
  draw_dag(dag_rev, ttl, highlight = coral_opts[[nm]],
           file = file.path(out_dir, paste0(stamp, "_DAG_variant_", nm, ".png")))
}
