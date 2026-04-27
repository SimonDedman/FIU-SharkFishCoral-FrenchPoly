# Suchinta Arif, 2025-03-14 & earlier
# https://dagitty.net/dags.html?id=GBrpiZXW

# Load packages ####
# install.packages("rstanarm")
# install.packages("brms")
library(brms)
# library(rstanarm)
library(here)
library(tidyverse)
options(future.globals.maxSize = 1.5 * 1024^3) # 1.5 GiB otherwise add_criterion(models_list[[11]], "loo", reloo = TRUE) can run out of memory

## make standardisation function ####
stdize <- function(x) {
  # centre around mean, scale to SD, then rescale to 0:1
  x <- (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE)) # centre & scale to SD
  x <- x - min(x, na.rm = TRUE) # shift so min is 0
  x <- x / max(x, na.rm = TRUE) # scale so max is 1
  return(x)
}

# Load & prepare data ####
ReefWideBRUVUVC.DAGtested <- readRDS(here(
  "NFF_data",
  "ch4_reef_wide_df2.RData"
)) |>
  # ReefWideBRUVUVC.DAGtested <- readr::read_csv(here("NFF_data", "ReefWideBRUVUVC-DAGtested.csv")) |>
  # remove dud column
  dplyr::select(-sum) |>
  # apply standardisation function
  dplyr::mutate(dplyr::across(where(is.numeric), stdize)) |>
  # subset to high islands, where we believe less healthy shark populations result in a bottom-up system
  dplyr::filter(topo %in% c("near atoll", "high barrier")) # High Island; from n=24 to n=11
# dplyr::filter(topo %in% c("open atoll", "closed atoll")) # Atoll; from n=24 to n=13

# STAN GLM effects ####
models_list <- list()
## reef sharks on sicklefin ####
models_list[[1]] <- brm(
  sicklefin_lemon_sharks ~ reef_sharks,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(), # change if your response is non-continuous
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4, # number of MCMC chains
  iter = 2000, # number of iterations per chain
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_1_SLS-RS_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
) # for reproducibility
models_list[[1]] <- add_criterion(models_list[[1]], "loo", reloo = TRUE)
### Summary ####
# Print a summary of the Bayesian model (includes posterior means, etc.)
# This is now saved to models_list and extracted later
print(models_list[[1]])
### 95% credible intervals ####
# Extract the 95% credible intervals for the coefficients
intervals_list <- list()
intervals_list[[1]] <- as.data.frame(posterior_interval(
  models_list[[1]],
  prob = 0.95
))

## reef_sharks on transient pelagics ####
# models_list[[2]] <- brm(
#   transient_pelagic_sharks ~ reef_sharks,
#   data = ReefWideBRUVUVC.DAGtested,
#   family = gaussian(),
#   prior = c(
#     set_prior("normal(0, 5)", class = "Intercept"),
#     set_prior("normal(0, 2.5)", class = "b")
#   ), # weakly informative prior for slopes
#   chains = 4,
#   iter = 2000,
#   seed = 123,
#   save_pars = save_pars(all = TRUE) # Needed for reloo
# )
# ### 95% credible intervals ####
# intervals_list[[2]] <- as.data.frame(posterior_interval(
#   models_list[[2]],
#   prob = 0.95
# ))

## piscivores on sicklefin ####
models_list[[3]] <- brm(
  sicklefin_lemon_sharks ~
    biomass_g_per_m2_Piscivore +
      biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Invertivore +
      biomass_g_per_m2_Planktivore +
      pop.dens,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_2_SLS-pisc_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[3]] <- add_criterion(models_list[[3]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[3]] <- as.data.frame(posterior_interval(
  models_list[[3]],
  prob = 0.95
))

## piscivores on transient pelagics ####
# models_list[[4]] <- brm(
#   # transient_piscivores_bayes
#   transient_pelagic_sharks ~
#     biomass_g_per_m2_Piscivore +
#       biomass_g_per_m2_Herbivore +
#       biomass_g_per_m2_Invertivore +
#       biomass_g_per_m2_Planktivore +
#       pop.dens,
#   data = ReefWideBRUVUVC.DAGtested,
#   family = gaussian(),
#   prior = c(
#     set_prior("normal(0, 5)", class = "Intercept"),
#     set_prior("normal(0, 2.5)", class = "b")
#   ), # weakly informative prior for slopes
#   chains = 4,
#   iter = 2000,
#   seed = 123,
#   save_pars = save_pars(all = TRUE) # Needed for reloo
# )
# ### 95% credible intervals ####
# intervals_list[[4]] <- as.data.frame(posterior_interval(
#   models_list[[4]],
#   prob = 0.95
# ))

## herbivores on reef_sharks ####
models_list[[5]] <- brm(
  reef_sharks ~
    biomass_g_per_m2_Herbivore +
      CCA +
      Hard.Coral +
      Other.Algae +
      pop.dens +
      Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_3_RS-herb_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[5]] <- add_criterion(models_list[[5]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[5]] <- as.data.frame(posterior_interval(
  models_list[[5]],
  prob = 0.95
))

## invertivores on reef_sharks ####
models_list[[6]] <- brm(
  reef_sharks ~
    biomass_g_per_m2_Invertivore +
      ave_npp +
      Other.Algae +
      pop.dens +
      Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_4_RS-inv_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[6]] <- add_criterion(models_list[[6]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[6]] <- as.data.frame(posterior_interval(
  models_list[[6]],
  prob = 0.95
))

## planktivores on reef_sharks ####
models_list[[7]] <- brm(
  reef_sharks ~
    biomass_g_per_m2_Planktivore +
      ave_npp +
      pop.dens +
      Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_5_RS-plank_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[7]] <- add_criterion(models_list[[7]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[7]] <- as.data.frame(posterior_interval(
  models_list[[7]],
  prob = 0.95
))

## herbivores on piscivores ####
models_list[[8]] <- brm(
  biomass_g_per_m2_Piscivore ~
    biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Invertivore +
      biomass_g_per_m2_Planktivore +
      pop.dens,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_6_pisc-herb_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[8]] <- add_criterion(models_list[[8]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[8]] <- as.data.frame(posterior_interval(
  models_list[[8]],
  prob = 0.95
))

## planktivores on piscivores ####
models_list[[9]] <- brm(
  biomass_g_per_m2_Piscivore ~
    biomass_g_per_m2_Planktivore +
      ave_npp +
      pop.dens +
      Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_7_pisc-plank_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[9]] <- add_criterion(models_list[[9]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[9]] <- as.data.frame(posterior_interval(
  models_list[[9]],
  prob = 0.95
))

## invertivores on piscivores ####
models_list[[10]] <- brm(
  biomass_g_per_m2_Piscivore ~
    biomass_g_per_m2_Invertivore +
      biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Planktivore +
      pop.dens,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_8_pisc-inv_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[10]] <- add_criterion(models_list[[10]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[10]] <- as.data.frame(posterior_interval(
  models_list[[10]],
  prob = 0.95
))

## hard_coral on herbivores ####
models_list[[11]] <- brm(
  biomass_g_per_m2_Herbivore ~ Hard.Coral,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_9_herb-HC_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[11]] <- add_criterion(models_list[[11]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[11]] <- as.data.frame(posterior_interval(
  models_list[[11]],
  prob = 0.95
))

## CCA on herbivores ####
models_list[[12]] <- brm(
  biomass_g_per_m2_Herbivore ~ CCA,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_10_herb-CCA_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[12]] <- add_criterion(models_list[[12]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[12]] <- as.data.frame(posterior_interval(
  models_list[[12]],
  prob = 0.95
))

## other algae on herbivores ####
models_list[[13]] <- brm(
  biomass_g_per_m2_Herbivore ~
    Other.Algae +
      CCA +
      Hard.Coral +
      pop.dens +
      Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 5)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b")
  ), # weakly informative prior for slopes
  chains = 4,
  iter = 2000,
  seed = 123,
  save_model = here(
    "Results",
    "DAG",
    "brm_models",
    "HI_BU_11_herb-OA_model.stan"
  ),
  save_pars = save_pars(all = TRUE) # Needed for reloo
)
models_list[[13]] <- add_criterion(models_list[[13]], "loo", reloo = TRUE)
### 95% credible intervals ####
intervals_list[[13]] <- as.data.frame(posterior_interval(
  models_list[[13]],
  prob = 0.95
))

# Save 95% credible intervals ####
do.call(rbind, intervals_list) |>
  ## FnGp incl/excl SLS ####
  # dplyr::slice(4:6, 10:n()) |> # remove sicklefin lemon sharks which are otherwise doublecounted
  dplyr::slice(1:3, 7:9, 13:n()) |> # remove apex sharks which have low n
  tibble::rownames_to_column("Value") |>
  readr::write_csv(
    file = here(
      "Results",
      "DAG",
      # "95pct_intervals_list_HighIslands_BottomUp.csv"
      ## Marquesas ####
      "95pct_intervals_list_HighIslands_BottomUp.csv"
    )
  )

# Save models list ####
saveRDS(
  # object = models_list,
  ## FnGp incl/excl SLS ####
  # object = models_list[c(2, 4:13)],
  object = models_list[c(1, 3, 5:13)], # no apex sharks
  file = here(
    "Results",
    "DAG",
    # "models_list_HighIslands_BottomUp.Rds"
    ## Marquesas ####
    "models_list_HighIslands_BottomUp.Rds"
  ),
  compress = "xz"
)

# Plot data ####
# Load the ggplot2 package
library(ggplot2)

# Create the data frame with effect sizes, confidence intervals, and group labels
dfBUhighisland <- data.frame(
  ## FnGp incl/excl SLS ####
  label = c(
    "Reef sharks on sicklefin lemon sharks",
    "Piscivores on sicklefin lemon sharks",
    "Herbivores on reef sharks",
    "Invertivores on reef sharks",
    "Planktivores on reef sharks",
    "Herbivores on piscivores",
    "Planktivores on piscivores",
    "Invertivores on piscivores",
    "Hard coral on herbivores",
    "Crustose coraline algae on herbivores",
    "Other algae on herbivores"
  ),
  effect = c(
    fixef(models_list[[1]])[2, "Estimate"], # comment out for FnGp incl/excl SLS
    fixef(models_list[[3]])[2, "Estimate"], # comment out for FnGp incl/excl SLS
    fixef(models_list[[5]])[2, "Estimate"],
    fixef(models_list[[6]])[2, "Estimate"],
    fixef(models_list[[7]])[2, "Estimate"],
    fixef(models_list[[8]])[2, "Estimate"],
    fixef(models_list[[9]])[2, "Estimate"],
    fixef(models_list[[10]])[2, "Estimate"],
    fixef(models_list[[11]])[2, "Estimate"],
    fixef(models_list[[12]])[2, "Estimate"],
    fixef(models_list[[13]])[2, "Estimate"]
  ),
  lower = c(
    # 2.5%
    intervals_list[[1]][2, 1], # comment out for FnGp incl/excl SLS
    intervals_list[[3]][2, 1], # comment out for FnGp incl/excl SLS
    intervals_list[[5]][2, 1],
    intervals_list[[6]][2, 1],
    intervals_list[[7]][2, 1],
    intervals_list[[8]][2, 1],
    intervals_list[[9]][2, 1],
    intervals_list[[10]][2, 1],
    intervals_list[[11]][2, 1],
    intervals_list[[12]][2, 1],
    intervals_list[[13]][2, 1]
  ),
  upper = c(
    # 97.5%
    intervals_list[[1]][2, 2], # comment out for FnGp incl/excl SLS
    intervals_list[[3]][2, 2], # comment out for FnGp incl/excl SLS
    intervals_list[[5]][2, 2],
    intervals_list[[6]][2, 2],
    intervals_list[[7]][2, 2],
    intervals_list[[8]][2, 2],
    intervals_list[[9]][2, 2],
    intervals_list[[10]][2, 2],
    intervals_list[[11]][2, 2],
    intervals_list[[12]][2, 2],
    intervals_list[[13]][2, 2]
  ),
  group = factor(
    c(
      rep("Group 1", 2), # transient shark effects # comment out for FnGp incl/excl SLS/Apex
      rep("Group 2", 3), # reef shark effects
      rep("Group 3", 3), # piscivore effects
      rep("Group 4", 3) # herbivore effects
    ),
    levels = c("Group 1", "Group 2", "Group 3", "Group 4")
  )
)

# Reverse the order of the labels so the first appears at the top
dfBUhighisland$label <- factor(
  dfBUhighisland$label,
  levels = rev(dfBUhighisland$label)
)

# Define custom colours for each group
colors <- c(
  "Group 1" = "blue",
  "Group 2" = "red",
  "Group 3" = "green",
  "Group 4" = "purple"
)

# Create the plot without a legend or title, ensuring the x-axis is clearly visible
BUhighislandplot <- ggplot(
  dfBUhighisland,
  aes(x = effect, y = label, color = group)
) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1) + # 2.5%, 97.5%
  scale_color_manual(values = colors) +
  labs(x = "Effect Size", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    # Add an x-axis line for clarity (minimal themes sometimes omit it)
    axis.line.x = element_line(color = "black"),
    plot.background = element_rect(fill = "white", colour = "grey50") # white background
  ) +
  # add a dashed grey vertical line at the point where x = 0
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  # define minor and major x axis grid lines
  scale_x_continuous(
    breaks = seq(-5, 6, by = 1),
    minor_breaks = seq(-4.5, 5.5, by = 0.5),
    limits = c(-4.5, 5.5)
  )
BUhighislandplot

ggsave(
  filename = paste0(
    lubridate::today(),
    # "_DAG-results-HighIslands-BottomUp.png"
    ## Marquesas ####
    "_DAG-results-HighIslands-BottomUp.png"
  ),
  device = "png",
  path = here("Results", "DAG")
)

saveRDS(
  object = BUhighislandplot,
  file = here(
    "Results",
    "DAG",
    "BUhighislandplot.Rds"
  ),
  compress = "xz"
)
