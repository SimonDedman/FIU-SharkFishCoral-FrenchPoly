# 09_DAG_stylised.R
# Layer 1 of the "stylised DAG" workflow: a faithful, restyled render of the
# conceptual DAG (manuscript Fig. S4 / the DAG_Fig2_slide.png base plot).
#
# Author: Simon Dedman <simondedman@gmail.com> (+ Claude)
# Created: 2026-06
#
# Goal: reproduce ALL content of the dagitty model exactly — every node, edge,
# label, and coordinate — but rendered cleanly in ggplot so it can be styled for
# publication. Generative image tools (NotebookLM etc.) drop content; this does
# not. Style is added in code; AI is reserved later for background/icons only.
#
#   - thin grey arrows  = the dagitty causal structure (unchanged links)
#   - thick coloured dual-tip arrows = the 11 tested top-down/bottom-up links:
#         blue   = large mesopredator <-> mesopredator
#         orange = mesopredator       <-> teleost prey
#         green  = herbivore          <-> basal producer
#   - nodes: measured = blue, unmeasured (latent) = grey
#
# Output: Results/DAG/Stylised/<today>_DAG_stylised_layer1.png

library(dagitty)
library(ggdag)
library(tidyverse)
library(ggrepel)
library(here)

# ---- DAG model (copied verbatim from 07_DAG_network_plots.R) ----------------
dag <- dagitty(
  x = 'dag {
ave_npp [pos="-0.927,-0.696"]
ave_temp [pos="-0.890,-1.097"]
bed_shear_stress [latent,pos="-0.963,-1.079"]
coral_recruitment [latent,pos="-1.179,-0.495"]
coral_spawning [latent,pos="-1.212,-0.819"]
crown_of_thorns [latent,pos="-1.147,0.199"]
crustose_coraline_algae [pos="-0.923,-0.349"]
cyclones [latent,pos="-1.15,-1.307"]
depth [latent,pos="-0.838,-1.489"]
emerged_land_area [pos="-0.883,-1.585"]
hard_coral [outcome,pos="-1.030,0.171"]
herbivores [pos="-0.909,0.746"]
invert [latent,pos="-1.099,0.065"]
invertivores [pos="-1.108,0.673"]
island_geomorphology [pos="-0.970,-1.724"]
lagoon_size [pos="-1.086,-1.471"]
latitude [pos="-1.104,-1.716"]
light [latent,pos="-0.765,-1.037"]
nutrient_run_off [latent,pos="-1.076,-1.247"]
offshore_prey [latent,pos="-1.211,1.103"]
other_algae [pos="-0.825,-0.260"]
offshore_prey_proxies [latent,pos="-1.24,0.9"]
piscivores [pos="-0.825,1.103"]
planktivores [pos="-1.203,0.701"]
pop_dens [pos="-0.789,0.360"]
reef_sharks [exposure,pos="-1.024,1.445"]
relief [pos="-1.123,-0.674"]
sicklefin_lemon_sharks [pos="-1.024,2.134"]
turbidity [latent,pos="-0.773,-0.592"]
wave_exposure [latent,pos="-0.953,-1.322"]
zooplankton [pos="-1.191,0.011"]
ave_npp -> crustose_coraline_algae
ave_npp -> hard_coral
ave_npp -> invert
ave_npp -> offshore_prey
ave_npp -> turbidity
ave_npp -> zooplankton
ave_temp -> ave_npp
ave_temp -> crustose_coraline_algae
ave_temp -> hard_coral
ave_temp -> offshore_prey
ave_temp -> other_algae
bed_shear_stress -> crustose_coraline_algae
bed_shear_stress -> relief
coral_recruitment -> hard_coral
coral_spawning -> coral_recruitment
crown_of_thorns -> hard_coral
crustose_coraline_algae -> coral_recruitment
crustose_coraline_algae -> hard_coral
cyclones -> relief
depth -> bed_shear_stress
depth -> light
depth -> wave_exposure
emerged_land_area -> nutrient_run_off
emerged_land_area -> pop_dens
herbivores -> crustose_coraline_algae
herbivores -> hard_coral
herbivores -> other_algae
invertivores -> invert
invertivores -> other_algae
island_geomorphology -> ave_npp
island_geomorphology -> bed_shear_stress
island_geomorphology -> emerged_land_area
island_geomorphology -> lagoon_size
island_geomorphology -> nutrient_run_off
island_geomorphology -> offshore_prey
lagoon_size -> nutrient_run_off
latitude -> cyclones
latitude -> light
light -> ave_npp
light -> ave_temp
light -> turbidity
nutrient_run_off -> ave_npp
nutrient_run_off -> crown_of_thorns
nutrient_run_off -> crustose_coraline_algae
nutrient_run_off -> other_algae
nutrient_run_off -> turbidity
other_algae -> hard_coral
offshore_prey_proxies -> offshore_prey
piscivores -> herbivores
piscivores -> invertivores
piscivores -> planktivores
planktivores -> coral_spawning
pop_dens -> hard_coral
pop_dens -> herbivores
pop_dens -> invertivores
pop_dens -> nutrient_run_off
pop_dens -> offshore_prey
pop_dens -> piscivores
pop_dens -> planktivores
reef_sharks -> herbivores
reef_sharks -> invertivores
reef_sharks -> offshore_prey
reef_sharks -> planktivores
relief -> hard_coral
relief -> herbivores
relief -> invert
relief -> invertivores
relief -> planktivores
sicklefin_lemon_sharks -> offshore_prey
sicklefin_lemon_sharks <-> piscivores
sicklefin_lemon_sharks <-> reef_sharks
turbidity -> crustose_coraline_algae
turbidity -> hard_coral
turbidity -> other_algae
wave_exposure -> bed_shear_stress
zooplankton -> planktivores
}'
)

# ---- The 11 tested top-down/bottom-up relationships + their group ------------
# (these are drawn as thick coloured dual-tip arrows; everything else stays thin)
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

# ---- Node positions: explicit trophic layout (EDIT PER-NODE HERE) -----------
# y = trophic tier (higher value = higher up); x = horizontal spread.
# (the dagitty string above keeps its original pos= values as a fallback.)
node_pos <- tribble(
  ~name,                       ~x,   ~y,
  # apex anthropogenic driver
  "pop_dens",                 3.7, 9.5,
  # large mesopredator
  "sicklefin_lemon_sharks",   0.0,  9.3,
  # mesopredators
  "reef_sharks",             -1.7,  8.2,
  "piscivores",               1.7,  8.2,
  # prey teleosts + offshore (same tier as planktivores)
  "offshore_prey_proxies",   -4.4,  6.3,
  "offshore_prey",           -4.4,  7.0,
  "planktivores",            -2.7,  7.0,
  "invertivores",            -0.8,  7.0,
  "herbivores",               1.3,  7.0,
  # benthic invert stack: crown_of_thorns > invert > zooplankton > coral
  "crown_of_thorns",         -2.3,  6.0,
  "invert",                  -1.3,  5.3,
  "zooplankton",             -2.7,  4.6,
  # basal producers
  "hard_coral",               0.6,  4.0,
  "crustose_coraline_algae",  2.3,  4.0,
  "other_algae",              3.7,  4.3,
  # coral demography (below zooplankton)
  "coral_recruitment",       0.0,  4.6,
  "coral_spawning",          -0.3,  5.1,
  # environmental / base drivers
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
  # bottom three, in order: light > island_geomorphology > latitude
  "light",                    1.0,  0.2,
  "island_geomorphology",     0.0,  0.3,
  "latitude",                -1.5,  -0.5
)

dag_tidy <- tidy_dagitty(dag)

# safety: every model node must have a position (else it would silently drop)
stopifnot(all(names(dag) %in% node_pos$name))

# ---- Edges: connectivity from the model, coordinates from node_pos ----------
edges <- dag_tidy$data |>
  filter(!is.na(direction)) |>
  transmute(
    from = name, to, direction,
    key = pmap_chr(list(name, to), ~ paste(sort(c(...)), collapse = "|"))
  ) |>
  left_join(select(tested, key, grp), by = "key") |>
  mutate(tested = !is.na(grp)) |>
  left_join(node_pos, by = c("from" = "name")) |>
  left_join(rename(node_pos, xend = x, yend = y), by = c("to" = "name"))

# trim each segment back from the node centres so arrowheads sit just off the node
trim_r <- 0.22
edges <- edges |>
  mutate(
    .len = sqrt((xend - x)^2 + (yend - y)^2),
    .f   = if_else(.len > 0, trim_r / .len, 0),
    x0 = x    + (xend - x) * .f,
    y0 = y    + (yend - y) * .f,
    x1 = xend - (xend - x) * .f,
    y1 = yend - (yend - y) * .f
  )

nodes <- node_pos |>
  mutate(
    status = if_else(name %in% latents(dag), "Unmeasured", "Measured"),
    label  = name   # keep exact model labels (no renaming)
  )

# ---- Plot -------------------------------------------------------------------
structural <- filter(edges, !tested)
tested_e   <- filter(edges, tested)

p <- ggplot() +
  # thin grey causal structure
  geom_segment(
    data = structural,
    aes(x = x0, y = y0, xend = x1, yend = y1),
    colour = "grey70", linewidth = 0.3,
    arrow = arrow(length = unit(5, "pt"), type = "closed"), lineend = "round"
  ) +
  # thick coloured dual-tip tested links
  geom_segment(
    data = tested_e,
    aes(x = x0, y = y0, xend = x1, yend = y1, colour = grp),
    linewidth = 2.4,
    arrow = arrow(length = unit(9, "pt"), ends = "both", type = "closed"),
    lineend = "round"
  ) +
  scale_colour_manual(values = grp_cols, name = "Tested links") +
  # nodes
  geom_point(
    data = nodes, aes(x = x, y = y, fill = status),
    shape = 21, size = 7, colour = "grey30", stroke = 0.4
  ) +
  scale_fill_manual(values = c(Measured = "#4DA6D6", Unmeasured = "#D2D2D2"),
                    name = "Node") +
  # labels: centred on each node, with a white halo so they read over the fill
  ggrepel::geom_text_repel(
    data = nodes, aes(x = x, y = y, label = label),
    size = 2.6, colour = "grey10", fontface = "bold",
    bg.color = "white", bg.r = 0.32,        # white buffer around the glyphs
    force = 0, force_pull = 0,               # don't repel — keep centred on node
    max.overlaps = Inf, point.size = NA,
    min.segment.length = Inf, box.padding = 0, seed = 1
  ) +
  coord_fixed(clip = "off") +
  theme_void(base_size = 13) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  here("Results", "DAG", "Stylised",
       paste0(today(), "_DAG_stylised_layer1.png")),
  plot = p, dpi = 300, width = 12, height = 11, bg = "white"
)

message("saved: ", here("Results", "DAG", "Stylised",
                        paste0(today(), "_DAG_stylised_layer1.png")))
