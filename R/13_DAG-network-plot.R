# TODO
# high islands: balance of arrows is 'nicer', whereas in atolls, arrows are all blown out by the huge (good) model results response planktivores which makes everything else less obvious.
## have arrows sized in proportion to only the other arrow in the pair?
## e.g. SLS vs RS, TD vs BU, arrows go driver/response same places, 1 red 1 blue. Scale those values to each other, and do that for everything.
## But disallows cross-pair comparisons
## but if we keep as is, currently atolls are blown out by 2 big BU scores response planktivores which makes sense, but we're saying atolls are TD because most results are TD

# FWIW I dicked around with the values to make higher numbers be better.  I can go back to the originals, but I figure 'proportion of 1' is intuitive for most people

# Spatial tweaks: input from other people?

# loo plots: I could add the elpdloo score above each dot, so we can see which relationships are driving the panel average elpdloo scores

# Select scenario ####
scenario <- "atolls" # "atolls" or "high_islands"
# scenario <- "high_islands" # "atolls" or "high_islands"

# Load Libraries and Input Data ####
# install.packages(c("dagitty", "ggdag", "ggplot2", "dplyr", "ggrepel"))
library(dagitty)
# devtools::install_github("r-causal/ggdag")
library(ggdag)
library(tidyverse)
library(ggrepel) # For potentially better label placement
library(here)
library(igraph)
library(tidygraph)
library(ggraph)
library(reporter) # superscript

## Import DAG response Dagitty ####
# MUST USE SINGLE QUOTES
# Have top invert the second position term for all elements to plot correctly in ggdag
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

# pre-location-tweak values:
# cyclones [latent,pos="-1.113,-1.307"]
# piscivores [pos="-0.880,1.502"]
# offshore_prey_proxies [latent,pos="-1.242,0.989"]

## Extract data to DFs ####
# Extract layout information BEFORE tidying, as tidy_dagitty might recalculate
dag_tidy <- tidy_dagitty(dag)

all_edges <- dag_tidy$data |>
  filter(!is.na(direction)) |> # Keep only edges
  select(name, to, x, y, xend, yend, direction, circular) |> # Select relevant columns
  rename(driver = name, response = to) # Rename for clarity

## rescale edge data for 1:1 x:y plotting ratio later ####
# Calculate ranges and center
delta_x <- max(all_edges$x, na.rm = TRUE) - min(all_edges$x, na.rm = TRUE) # 0.477
delta_y <- max(all_edges$y, na.rm = TRUE) - min(all_edges$y, na.rm = TRUE) # 3.858
center_x <- (min(all_edges$x, na.rm = TRUE) + max(all_edges$x, na.rm = TRUE)) /
  2 # -1.0035

## Check for zero range to avoid division by zero ####
if (delta_x == 0) {
  warning("X-coordinate range is zero. Cannot rescale based on Y range.")
} else {
  all_edges <- all_edges |>
    mutate(
      x = center_x + (x - center_x) * (delta_y / delta_x),
      xend = center_x + (xend - center_x) * (delta_y / delta_x)
    )
}

# df of all locations (nodes) from driver (x,y) and response (xend, yend) positions
edge_data <- all_edges |>
  select(driver, x, y) |>
  rbind(
    all_edges |>
      select(
        driver = response,
        x = xend,
        y = yend
      )
  ) |>
  arrange(driver) |>
  unique()

# extents for ggplot since edge_data has all locations
extents <- edge_data |>
  summarise(
    xmin = min(x, na.rm = TRUE),
    xmax = max(x, na.rm = TRUE),
    ymin = min(y, na.rm = TRUE),
    ymax = max(y, na.rm = TRUE)
  )


# Import stacking weights ####
# means for text box labels on plot
# 2025-09-15 no longer used.
# stackweights <- read_csv(here(
#   "Results",
#   "DAG",
#   "loo_model_weights_means.csv"
# )) |>
#   mutate(
#     label = paste0(
#       direction,
#       ": ",
#       round(stacking_weight, 3)
#     )
#   )

# all values to filter for stacking weight and apply dashing on lines
stacking_results <- read_csv(here(
  "Results",
  "DAG",
  "loo_compare_results.csv"
)) |>
  # convert stack_weight_weak values from "Weak" (<0.05) and "Fine" (>= 0.05) to 1 and 2
  mutate(
    stack_weight_weak = ifelse(
      stack_weight_weak == "Weak",
      "dashed",
      ifelse(stack_weight_weak == "Fine", "solid", NA)
    )
  )

bayesR2 <- read_csv(here("Results", "DAG", "bayesR2.csv"))
bayesR2summary <- read_csv(here("Results", "DAG", "bayesR2summary.csv")) # NEVER USED
# stacking_results replace stacking_weight with r2 and stack_weight_weak with post_int_width

stacking_results |>
  select(-stacking_weight, -stack_weight_weak) |>
  left_join(
    bayesR2 |>
      select(model, topo, direction, BayesR2, post_int_width),
    by = c("model", "topo", "direction")
  ) |>
  rename(stacking_weight = BayesR2) |>
  # replace Precise and Moderate with solid, Imprecise with dashed
  mutate(
    stack_weight_weak = ifelse(
      post_int_width == "Precise" | post_int_width == "Moderate",
      "solid",
      ifelse(post_int_width == "Imprecise", "dashed", NA)
    )
  ) |>
  select(-post_int_width) -> stacking_results


# Prepare Effect Size Data ####
effect_size_df <- read_csv(here(
  "Results",
  "DAG",
  "loo_compare_results.csv"
)) |> # Load your effect size data
  # convert elpd_loo to weighted using stacking weights
  # mutate(
  #   elpd_loo = elpd_loo * stacking_weight
  # ) |>
  # select relevant columns
  select(
    topo,
    direction,
    model,
    elpd_loo,
    stack_weight_weak
  ) |>
  # split model into response and driver using " on "
  separate(
    model,
    into = c("driver", "response"),
    sep = " on ",
    remove = TRUE
  ) |>
  mutate(
    # replace "." and " " with "_"
    response = str_replace_all(response, "[. ]", "_"),
    driver = str_replace_all(driver, "[. ]", "_"),
    # convert all driver lower case
    response = str_to_lower(response),
    driver = str_to_lower(driver),
    # if direction == BottomUp, then make the driver the response and the response the driver
    driver2 = ifelse(direction == "BottomUp", response, driver),
    response2 = ifelse(direction == "BottomUp", driver, response)
  ) |>
  select(-driver, -response, -stack_weight_weak) |>
  # rename driver and response to driver2 and response2
  rename(driver = driver2, response = response2) |>
  # pivot wider to create new columns from topo and direction
  pivot_wider(
    names_from = c(topo, direction),
    values_from = elpd_loo,
    names_sep = "_"
  )

## Create atoll & highisland-specific dfs ####
# Use the EXACT node names from your DAG.
### atolls ####
effect_size_atolls <- effect_size_df |>
  select(
    driver,
    response,
    Atolls_TopDown, # elpd_loo
    Atolls_BottomUp # elpd_loo
  ) |>
  mutate(
    grouppair = row_number(),
    BUgrouppair = row_number(),
  ) |>
  rename(
    effect_size = Atolls_TopDown # elpd_loo
  )

effect_size_atolls <- effect_size_atolls |>
  bind_rows(
    effect_size_atolls |>
      select(
        driver = response,
        response = driver,
        effect_size = Atolls_BottomUp,
        grouppair = BUgrouppair
      )
  ) |>
  select(
    driver,
    response,
    effect_size, # elpd_loo
    grouppair
  ) |>
  # join response with x & y
  left_join(edge_data) |>
  # join driver with xend & yend
  left_join(
    edge_data |>
      rename(
        response = driver,
        xend = x,
        yend = y
      )
  ) |>
  mutate(
    # Calculate midpoints for labels
    mid_x = (x + xend) / 2,
    mid_y = (y + yend) / 2,
    # rescale effect_size_plot to be between 0 and 1
    effect_size_plot = (effect_size - min(effect_size, na.rm = TRUE)) /
      (max(effect_size, na.rm = TRUE) - min(effect_size, na.rm = TRUE)),
    # recalibrate effect size so the most negative (best model) is the largest
    # and raise effect size so there are no negatives to pass to abs() (+1)
    # and multiple everythiong by 10 for nicer plot labels
    # and round to 2dp for plotting
    # effect_size_plot = round((-effect_size_plot + 1) * 10, 2) # 2025-05-09 removed, larger = better.
    # # then raise effect size so there are no negatives to pass to abs()
    # effect_size_plot = effect_size_plot + abs(min(effect_size_plot, na.rm = TRUE)), # 2025-05-09 already removed,
  ) |>
  # add stacking weights
  left_join(
    stacking_results |>
      filter(topo == "Atolls") |>
      # split model into response and driver using " on "
      separate(
        model,
        into = c("driver", "response"),
        sep = " on ",
        remove = TRUE
      ) |>
      mutate(
        # replace "." and " " with "_"
        response = str_replace_all(response, "[. ]", "_"),
        driver = str_replace_all(driver, "[. ]", "_"),
        # convert all driver lower case
        response = str_to_lower(response),
        driver = str_to_lower(driver)
      ) |>
      select(
        driver,
        response,
        stacking_weight,
        stack_weight_weak
      )
  )


### high islands ####
effect_size_high_islands <- effect_size_df |>
  select(
    driver,
    response,
    HighIslands_TopDown, # elpd_loo
    HighIslands_BottomUp # elpd_loo
  ) |>
  mutate(
    grouppair = row_number(),
    BUgrouppair = row_number(),
  ) |>
  rename(
    effect_size = HighIslands_TopDown # elpd_loo
  )
effect_size_high_islands <- effect_size_high_islands |>
  bind_rows(
    effect_size_high_islands |>
      select(
        driver = response,
        response = driver,
        effect_size = HighIslands_BottomUp, # elpd_loo
        grouppair = BUgrouppair
      )
  ) |>
  select(
    driver,
    response,
    effect_size, # elpd_loo
    grouppair
  ) |>
  left_join(edge_data) |>
  left_join(
    edge_data |>
      rename(
        response = driver,
        xend = x,
        yend = y
      )
  ) |>
  mutate(
    mid_x = (x + xend) / 2,
    mid_y = (y + yend) / 2,
    # scale to 0-1
    effect_size_plot = (effect_size - min(effect_size, na.rm = TRUE)) /
      (max(effect_size, na.rm = TRUE) - min(effect_size, na.rm = TRUE)),
    # effect_size_plot = round((-effect_size_plot + 1) * 10, 2)
  ) |>
  # add stacking weights
  left_join(
    stacking_results |>
      filter(topo == "HighIslands") |>
      # split model into response and driver using " on "
      separate(
        model,
        into = c("driver", "response"),
        sep = " on ",
        remove = TRUE
      ) |>
      mutate(
        # replace "." and " " with "_"
        response = str_replace_all(response, "[. ]", "_"),
        driver = str_replace_all(driver, "[. ]", "_"),
        # convert all driver lower case
        response = str_to_lower(response),
        driver = str_to_lower(driver)
      ) |>
      select(
        driver,
        response,
        stacking_weight,
        stack_weight_weak
      )
  )


# Merge Data and Handle Dual Arrows ####
# Strategy:
# a. Identify edges response the original DAG.
# b. Identify *all* desired edges (including reverses) response the effect size table.
# c. Create the final edge list by combining these, ensuring coordinates are correct for each direction.

# Prep ggplot & ggdag ####
# Define aesthetics
text_size <- 3
arrow_size_min <- 0.2 # min linewidth for effect size = 0
arrow_size_max <- 2.0 # max linewidth for max absolute effect size
topdowncolour <- "#F8766D"
bottomupcolour <- "#1b9e77"

# Create final_edges dataframe based on scenario
if (scenario == "atolls") {
  final_edges <- effect_size_atolls
} else if (scenario == "high_islands") {
  final_edges <- effect_size_high_islands
}
# add a column for which direction of each model/edge is best, to final_edges
# groupbest, used by Edge Labels topdown/bottomup bold
pairwisebest <- final_edges |>
  group_by(grouppair) |>
  # select the best stacking_weight i.e. Bayes LOO R2 for each pair
  slice(which.max(stacking_weight)) |>
  ungroup() |>
  mutate(groupbest = as.logical(TRUE)) |>
  select(driver, response, groupbest)

# create df with edges not included in the bottomup/topdown df
final_edges <- final_edges |>
  mutate(responsedriver = paste(response, driver, sep = "_")) |>
  # Make the larger of each value pair bold
  left_join(pairwisebest) |>
  # if any groupbest entries are not TRUE then make them FALSE
  mutate(
    groupbest = ifelse(is.na(groupbest), FALSE, groupbest),
    # convert stacking_weight to binned factor for edge printing,
    # to avoid negative values not printing, & align values to confidence levels.
    # need to convert stacking_weight from -INF:INF (realistically usually -1 to 1) to bins based on quality?
    # (0), 0.4, 0.7, 0.9 = shit, weak, moderate, strong
    bayesR2edge = case_when(
      stacking_weight < 0.4 ~ 1, # oldcolval ~ newcolval; shit
      between(stacking_weight, 0.4, 0.7) ~ 2, # weak
      between(stacking_weight, 0.7, 0.9) ~ 3, # moderate
      stacking_weight >= 0.9 ~ 4 # strong
    )
  )

all_edges <- all_edges |>
  select(-direction, -circular) |>
  # Calculate midpoints for labels
  mutate(
    mid_x = (x + xend) / 2,
    mid_y = (y + yend) / 2,
    responsedriver = paste(response, driver, sep = "_")
  ) |>
  # remove rows already covered in final_edges response and driver combinations
  # we don't need these since all_edges is really 'no data edges'
  filter(!responsedriver %in% final_edges$responsedriver)

# Determine range for scaling line width
# as final_edges$effect_size_plot is already scaled, this should always be 0:1
effect_range <- range(abs(final_edges$effect_size_plot), na.rm = TRUE)

if (length(effect_range) == 0 || diff(effect_range) == 0) {
  effect_range <- c(0, 1) # Avoid issues if only one or zero edges
}

# Split the data into two parts: topdown & bottomup
final_edges_topdown <- final_edges |>
  # slice only the top 50% of the rows
  slice(1:(nrow(final_edges) / 2))
final_edges_bottomup <- final_edges |>
  # slice only the bottom 50% of the rows
  slice((nrow(final_edges) / 2 + 1):nrow(final_edges))

# Split other data into TD & BU based on Y positions
all_edges_topdown <- all_edges |>
  filter(y > yend) # Topdown edges
all_edges_bottomup <- all_edges |>
  filter(y < yend) # Bottomup edges

# Split final_edges TD/BU based on groupbest for bold labels
final_edges_topdown_bold <- final_edges_topdown |> filter(groupbest == TRUE)
final_edges_bottomup_bold <- final_edges_bottomup |> filter(groupbest == TRUE)
final_edges_topdown_plain <- final_edges_topdown |> filter(groupbest == FALSE)
final_edges_bottomup_plain <- final_edges_bottomup |> filter(groupbest == FALSE)


# Run Ggplot ####
ggplot() +
  ## Edges ####
  ### topdown ####
  geom_curve(
    data = final_edges_topdown,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      # need to convert stacking_weight from -INF:INF (realistically usually -1 to 1) to bins based on quality
      # (0), 0.4, 0.7, 0.9 = shit, weak, moderate, strong
      linewidth = bayesR2edge / 4,
      linetype = stack_weight_weak # 1 = solid, 2 = dashed, ?aes_linetype_size_shape
    ),
    arrow = arrow(length = unit(0.15, "inches"), type = "open"),
    lineend = "butt",
    curvature = 0.02,
    color = topdowncolour, # Or map color to effect sign/type
    alpha = 0.85 # Adjust alpha for transparency. 0=clean, 1=full
  ) +
  ### bottomup ####
  geom_curve(
    data = final_edges_bottomup,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      linewidth = bayesR2edge / 4,
      linetype = stack_weight_weak # 1 = solid, 2 = dashed, ?aes_linetype_size_shape
    ),
    arrow = arrow(length = unit(0.15, "inches"), type = "open"),
    lineend = "butt",
    curvature = 0.02,
    color = bottomupcolour, # Or map color to effect sign/type
    alpha = 0.85 # Adjust alpha for transparency
  ) +
  scale_linetype_identity() + # ensures dashes and solids are drawn correctly
  ### other edges TD ####
  geom_segment(
    data = all_edges_topdown,
    aes(x = x, y = y, xend = xend, yend = yend, linewidth = 0.1),
    arrow = arrow(length = unit(0.06, "inches"), type = "closed"),
    lineend = "butt",
    color = topdowncolour,
    alpha = 0.25
  ) +
  ### other edges BU ####
  geom_segment(
    data = all_edges_bottomup,
    aes(x = x, y = y, xend = xend, yend = yend, linewidth = 0.1),
    arrow = arrow(length = unit(0.06, "inches"), type = "closed"),
    lineend = "butt",
    color = bottomupcolour,
    alpha = 0.25
  ) +

  # Scale the linewidth based on the absolute effect size
  scale_linewidth_continuous(
    range = c(arrow_size_min, arrow_size_max),
    limits = c(0, effect_range[2]) # Start response 0 for size mapping
  ) +

  ## Edge Labels ####
  ### topdown bold ####
  geom_label(
    data = final_edges_topdown_bold,
    aes(
      x = mid_x,
      y = mid_y,
      label = round(stacking_weight, 2)
    ),
    size = text_size * 0.9, # Slightly smaller than node text
    fontface = "bold",
    fill = "white",
    color = topdowncolour,
    label.size = NA, # No border around label
    label.padding = unit(0.1, "lines"), # Minimal padding
    nudge_y = 0.035 # Adjust nudge slightly off the line
  ) +
  ### topdown plain ####
  geom_label(
    data = final_edges_topdown_plain,
    aes(
      x = mid_x,
      y = mid_y,
      label = round(stacking_weight, 2)
    ),
    size = text_size * 0.9, # Slightly smaller than node text
    fill = "white",
    color = topdowncolour,
    label.size = NA, # No border around label
    label.padding = unit(0.1, "lines"), # Minimal padding
    nudge_y = 0.035 # Adjust nudge slightly off the line
  ) +
  ### bottomup bold ####
  geom_label(
    data = final_edges_bottomup_bold,
    aes(x = mid_x, y = mid_y, label = round(stacking_weight, 2)),
    size = text_size * 0.9, # Slightly smaller than node text
    fontface = "bold",
    fill = "white",
    colour = bottomupcolour,
    label.size = NA, # No border around label
    label.padding = unit(0.1, "lines"), # Minimal padding
    nudge_y = -0.035 # Adjust nudge slightly off the line
  ) +
  ### bottomup plain ####
  geom_label(
    data = final_edges_bottomup_plain,
    aes(x = mid_x, y = mid_y, label = round(stacking_weight, 2)),
    size = text_size * 0.9, # Slightly smaller than node text
    fill = "white",
    colour = bottomupcolour,
    label.size = NA, # No border around label
    label.padding = unit(0.1, "lines"), # Minimal padding
    nudge_y = -0.035 # Adjust nudge slightly off the line
  ) +

  ## Nodes ####
  geom_point(
    data = edge_data,
    aes(
      x = x,
      y = y
    ),
    size = 0.3,
    color = "black"
  ) +
  ### Node Labels ####
  geom_label(
    data = edge_data,
    aes(x = x, y = y, label = driver),
    size = text_size,
    fill = "white",
    label.size = NA, # No border around label
    label.padding = unit(0.1, "lines"), # Minimal padding
    vjust = 1.75, # label under dot
    hjust = 0.5, # Center horizontally
  ) +

  ### Stacking Weights Labels TopDown####
  geom_text(
    data = final_edges_topdown |>
      summarise(
        label = paste0(
          "TopDown: ",
          round(mean(stacking_weight), 3)
        )
      ),
    # data = stackweights |>
    #   filter(
    #     direction == "TopDown",
    #     if (scenario == "atolls") {
    #       topo == "Atolls"
    #     } else if (scenario == "high_islands") {
    #       topo == "HighIslands"
    #     }
    #   ),
    aes(
      x = extents$xmin + 0.95 * (extents$xmax - extents$xmin), # rightmost point, brought in a little
      y = extents$ymin + 1 * (extents$ymax - extents$ymin), # bottommost point, brought in a little
      label = label
    ),
    colour = topdowncolour,
    size = text_size * 1.3,
    vjust = 1.75, # label under dot
    hjust = 0.5, # Center horizontally
  ) +

  ### Stacking Weights Labels BottomUp####
  geom_text(
    data = final_edges_bottomup |>
      summarise(
        label = paste0(
          "BottomUp: ",
          round(mean(stacking_weight), 3)
        )
      ),
    # data = stackweights |>
    #   filter(
    #     direction == "BottomUp",
    #     if (scenario == "atolls") {
    #       topo == "Atolls"
    #     } else if (scenario == "high_islands") {
    #       topo == "HighIslands"
    #     }
    #   ),
    aes(
      x = extents$xmin + 0.95 * (extents$xmax - extents$xmin), # rightmost point, brought in a little
      y = extents$ymin + 0.97 * (extents$ymax - extents$ymin), # bottommost point, brought in a little
      label = label
    ),
    colour = bottomupcolour,
    size = text_size * 1.3,
    vjust = 1.75, # label under dot
    hjust = 0.5, # Center horizontally
  ) +

  ## Labels & Theme ####
  labs(
    title = ifelse(
      scenario == "atolls",
      "DAG of French Polynesia Coral Reef Ecosystem - Atolls",
      "DAG of French Polynesia Coral Reef Ecosystem - High Islands"
    ),
    subtitle = expression("Modelled Bayesian R"^2 * " of Gaussian STAN GLMs"), # Legend title for linewidth
    caption = paste0(
      today(),
      # "; dashed lines indicate stacking weights < 0.05: high uncertainty / weak support / contributes little to predictive performance vs top model"
      "; dashed lines indicate posterior interval >0.5 i.e. worse than moderate"
    ) # Add caption for date created
  ) +
  ggdag::theme_dag(base_size = 14) + # Use ggdag theme
  theme(
    legend.position = "none", # "bottom", # Adjust legend position
    legend.box = "vertical",
    plot.title = element_text(hjust = 0.5), # Center title
    plot.subtitle = element_text(hjust = 0.5), # Center title
    plot.caption = element_text(size = 8), # Adjust caption size
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  ) +
  # Maintain aspect ratio based on coordinates
  # expand the plot background to ensure labels aren't cut off
  coord_fixed(clip = "off", expand = TRUE)

## Save the plot ####
ggsave(
  here(
    "Results",
    "DAG",
    ifelse(
      scenario == "atolls",
      paste0(today(), "_DAG_Atolls.png"),
      paste0(today(), "_DAG_HighIslands.png")
    )
  ),
  dpi = 300,
  width = 10,
  height = 8
)
