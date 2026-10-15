# 10_DAG_stylised_layer2.R
# Layer 2: legend/key fix, open latent nodes, and an ecological trophic
# gradient canvas. Moderate styling on top of the
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
# Output: Results/DAG/Stylised/<today>_DAG_stylised_layer2.png
# (Layer 1 = R/09_DAG_stylised.R stays frozen as the faithful reference.)

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
  # apex anthropogenic driver (lowered to just above the mesopredators, freeing
  # the top of the frame for sky / high-island volcano / atoll in the background)
  "pop_dens",                 3.7,  8.9,
  # large mesopredator (lowered to sit just above reef sharks)
  "sicklefin_lemon_sharks",   0.0,  8.7,
  # mesopredators (piscivores set a little lower than reef sharks)
  "reef_sharks",             -1.7,  8.2,
  "piscivores",               1.7,  7.8,
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

# Optional horizontal shift of the whole graph within the composite frame.
# Negative = move the DAG LEFT, away from the right-side colourful reef, so the
# basal/environmental nodes no longer overlap it. Tier labels (x = -7.5) and the
# coord_fixed window stay fixed, so only the node cloud + its edges slide.
# Default 0 = unchanged; recommended composite value ~ -1.8 (rightmost node 4.4 -> 2.6,
# opening ~2.7 units of reef space on the right; leftmost node -4.4 -> -6.2, still
# clear of the labels at -7.5). Run e.g.:  DAG_XSHIFT=-1.8 DAG_OVERLAY=TRUE Rscript ...
dag_xshift <- suppressWarnings(as.numeric(Sys.getenv("DAG_XSHIFT", "0")))
if (is.na(dag_xshift)) dag_xshift <- 0
node_pos$x <- node_pos$x + dag_xshift

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

# ---- Trophic tier background: smooth blended ecological gradient -------------
# Six ecological tiers (apex/open-ocean at top -> seabed/substrate at base),
# interpolated into a soft vertical gradient that acts as a background canvas.
# Pure code, fully reproducible; the faithful DAG render is unaffected.
tier_cols <- c("#3A5A74","#6E93AD","#4FA3A0","#79A86A","#CC7A6E","#C9B587")  # top->bottom
tier_yc   <- c(8.8, 8.0, 6.9, 5.25, 3.85, 1.2)  # top two lowered to track apex/meso nodes
tier_lab  <- c("Anthropogenic &\nlarge mesopredator", "Mesopredators", "Prey teleosts",
               "Benthic\ninvertebrates", "Basal producers", "Environmental\ndrivers")
yr <- c(-0.9, 10.2); ng <- 400
ys <- seq(yr[1], yr[2], length.out = ng)
ay <- rev(tier_yc); m <- col2rgb(rev(tier_cols))
mixw <- function(v, w = 0.82) v * (1 - w) + 255 * w   # fade toward white (lower w = stronger)
ras <- matrix(rev(rgb(
  mixw(approx(ay, m[1, ], ys, rule = 2)$y),
  mixw(approx(ay, m[2, ], ys, rule = 2)$y),
  mixw(approx(ay, m[3, ], ys, rule = 2)$y), maxColorValue = 255)), ncol = 1)
# white-haloed tier labels (manual halo: 8 white offset copies + dark text on top,
# so they read over either the gradient or a vivid illustrated background)
tiers_df <- tibble(x = -7.5, y = tier_yc, label = tier_lab)
.tier_txt <- function(dx, dy, col)
  annotate("text", x = tiers_df$x + dx, y = tiers_df$y + dy, label = tiers_df$label,
           hjust = 0, vjust = 0.5, size = 3.2, colour = col,
           fontface = "italic", lineheight = 0.9)
# Dense circular halo: 16 compass directions x 2 radii (= 32 white copies) form a
# solid, gap-free white outline so the italic labels stay crisp over a busy
# illustrated background. The previous 8-point ring left diagonal gaps -> fuzzy edges.
.ho <- 0.028
.halo_dirs <- lapply(seq(0, 2 * pi, length.out = 17)[-17],
                     function(a) c(cos(a), sin(a)))
tier_text <- c(
  unlist(lapply(c(0.5, 1) * .ho,
                function(r) lapply(.halo_dirs,
                                   function(d) .tier_txt(d[1] * r, d[2] * r, "white"))),
         recursive = FALSE),
  list(.tier_txt(0, 0, "grey15"))
)
# OVERLAY mode (run with env DAG_OVERLAY=TRUE): emit a transparent DAG for compositing
# over an illustrated background -> drop the gradient canvas, keep the haloed tier labels.
overlay_mode <- isTRUE(as.logical(Sys.getenv("DAG_OVERLAY", "FALSE")))
gradient_canvas <- annotation_raster(ras, xmin = -7.7, xmax = 5.3,
                                     ymin = yr[1], ymax = yr[2], interpolate = TRUE)
tier_layers <- if (overlay_mode) tier_text else c(list(gradient_canvas), tier_text)
# Fallback (stepped bands): swap annotation_raster for one annotate("rect", ...) per tier
# using tier_cols at alpha ~0.16 with ymin/ymax band edges (see git history / mockups).

# ---- Plot -------------------------------------------------------------------
structural <- filter(edges, !tested)
tested_e   <- filter(edges, tested)

p <- ggplot() +
  tier_layers +
  # trophic gradient canvas (behind everything)
  # thin grey causal structure
  geom_segment(
    data = structural,
    aes(x = x0, y = y0, xend = x1, yend = y1),
    colour = "black", linewidth = 0.4,
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
  scale_colour_manual(
    values = grp_cols, name = "Tested links",
    breaks = c("Large mesopredators", "Mesopredators", "Basal")  # trophic order
  ) +
  # nodes: filled = measured, open/white = unmeasured (latent)
  geom_point(
    data = nodes, aes(x = x, y = y, fill = status),
    shape = 21, size = 7, colour = "grey62", stroke = 0.5
  ) +
  scale_fill_manual(
    values = c(Measured = "#69ACD2", Unmeasured = "white"),
    name = "Node", breaks = c("Measured", "Unmeasured")
  ) +
  # labels: centred on each node, with a white halo so they read over the fill
  ggrepel::geom_text_repel(
    data = nodes, aes(x = x, y = y, label = label),
    size = 2.6, colour = "grey10", fontface = "bold",
    bg.color = "white", bg.r = 0.32,        # white buffer around the glyphs
    force = 0, force_pull = 0,               # don't repel — keep centred on node
    max.overlaps = Inf, point.size = NA,
    min.segment.length = Inf, box.padding = 0, seed = 1
  ) +
  guides(
    colour = guide_legend(order = 1),  # "Tested links" above "Node"
    fill = guide_legend(
      order = 2,
      override.aes = list(shape = 21, colour = "grey62", stroke = 0.5, size = 4.5)
    )
  ) +
  (if (overlay_mode)
     coord_fixed(xlim = c(-7.7, 5.3), ylim = c(-0.9, 10.2), expand = FALSE, clip = "off")
   else coord_fixed(clip = "off")) +
  theme_void(base_size = 13) +
  theme(
    legend.position = if (overlay_mode) "none" else "right",
    plot.background = element_rect(
      fill   = if (overlay_mode) "transparent" else "white",
      colour = if (overlay_mode) NA else "white"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.margin = if (overlay_mode) margin(0, 0, 0, 0) else margin(20, 20, 20, 20)
  )

if (overlay_mode) {
  out <- here("Results", "DAG", "Stylised", paste0(today(), "_DAGoverlay.png"))
  ggsave(out, plot = p, width = 13, height = 11.1, dpi = 300, bg = "transparent")  # native size
} else {
  out <- here("Results", "DAG", "Stylised", paste0(today(), "_DAG_stylised_layer2.png"))
  ggsave(out, plot = p, dpi = 300, width = 14, height = 11, bg = "white")
}
message("saved: ", out)
