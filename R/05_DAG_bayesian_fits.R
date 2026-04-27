# 05_DAG_bayesian_fits.R
# Bayesian Structural Causal Models for top-down vs bottom-up forcing.
#
# Purpose:
#   Fit a list of 11 brms models per (topology x trophic direction) scenario,
#   covering the full set of pairwise relationships specified in the DAG. The
#   four scenarios are Atoll-TopDown, Atoll-BottomUp, HighIslands-TopDown,
#   HighIslands-BottomUp; each produces 11 fitted brmsfit objects saved to
#   Results/DAG/models_list_<topo>_<direction>_spline.Rds.
#
# Inputs:
#   NFF_data/ch4_reef_wide_df2.RData (24-reef analysis table; n=13 atolls,
#   n=11 high islands after topology filtering).
#
# Outputs:
#   Results/DAG/brm_models/<scenario>_<modelN>_<vars>_spline.stan (Stan code)
#   Results/DAG/models_list_<topo>_<direction>_spline.Rds (fitted brmsfit list)
#
# Method choice:
#   Earlier work fit each model with a Gaussian-family linear regression
#   (`family = gaussian()`, linear terms `y ~ x1 + x2`, normal(0, 2.5) priors
#   on regression coefficients). Posterior predictive checks revealed
#   heavy-tailed residuals consistent with the small per-topology sample sizes
#   (n = 11 - 13) and indicated departures from linearity for several
#   shark-fish and fish-benthos relationships. We replaced linear terms with
#   cubic shrinkage splines (`s(x, k = 3, bs = "cs")`) under Student-t errors,
#   with `exponential(1)` priors on the smoothing standard deviations to
#   penalise excess wiggliness; the chosen formulation matched the data more
#   closely without overfitting (LOO-PSIS comparison reported in
#   Results/DAG/elpd_pointwise_summary_spline.csv). The earlier Gaussian-linear
#   variant is retained at R/archive/11_DAG-TDBU-AtollHI-Collated_gaussian.R
#   for transparency.
#
# Runtime:
#   Substantial. 44 brm fits with 4 chains, 2000 iterations each, plus
#   `add_criterion(loo, reloo = TRUE)` which triggers refits where Pareto-k
#   diagnostics are poor.
#
# Method development:
#   DAG-based Bayesian SCM framework adapted from Suchinta Arif's
#   2025-03-14 development draft (and earlier).
#
# DAG sources:
#   TD DAG: https://dagitty.net/dags.html?id=7bBT4Rqj
#   BU DAG: https://dagitty.net/dags.html?id=GBrpiZXW

# Load packages ####
# install.packages("rstanarm")
# install.packages("brms")
library(brms)
# library(rstanarm)
library(here)
library(tidyverse)
options(future.globals.maxSize = 1.5 * 1024^3) # 1.5 GiB otherwise add_criterion can run out of memory

## make standardisation function ####
stdize <- function(x) {
  # centre around mean, scale to SD, then rescale to 0:1
  x <- (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE)) # centre & scale to SD
  x <- x - min(x, na.rm = TRUE) # shift so min is 0
  x <- x / max(x, na.rm = TRUE) # scale so max is 1
  return(x)
}

# Define analysis configurations ####
topo_types <- list(
  Atoll = c("open atoll", "closed atoll"),
  HighIslands = c("near atoll", "high barrier")
)

trophic_directions <- c("TD", "BU")

# Load & prepare base data ####
base_data <- readRDS(here("NFF_data", "ch4_reef_wide_df2.RData")) |>
  dplyr::select(-sum) |>
  dplyr::mutate(dplyr::across(where(is.numeric), stdize))

# Create output directory ####
dir.create(here("Results", "DAG", "brm_models"), showWarnings = FALSE, recursive = TRUE)

# Main nested loop ####
for (topo_name in names(topo_types)) {
  for (trophic_dir in trophic_directions) {
    cat("\n", strrep("=", 70), "\n")
    cat("Processing:", topo_name, "-", trophic_dir, "\n")
    cat(strrep("=", 70), "\n\n")

    # Create file-naming compatible versions ####
    # (script 12 expects Atolls/TopDown format, but loop uses Atoll/TD)
    topo_file <- ifelse(topo_name == "Atoll", "Atolls", topo_name)
    direction_file <- ifelse(trophic_dir == "TD", "TopDown", "BottomUp")

    # Filter data by topology ####
    ReefWideBRUVUVC.DAGtested <- base_data |>
      dplyr::filter(topo %in% topo_types[[topo_name]])

    # Define prefix for file naming ####
    prefix <- paste0(ifelse(topo_name == "Atoll", "Atoll", "HI"), "_", trophic_dir)

    # Initialize storage lists ####
    models_list <- list()
    intervals_list <- list()

    # Define models based on trophic direction ####
    if (trophic_dir == "TD") {
      # TOP-DOWN MODELS ####

      ## Model 1: sicklefin on reef sharks ####
      models_list[[1]] <- brm(
        reef_sharks ~ s(sicklefin_lemon_sharks, k = 3, bs = "cs"), # smoother per explanatory variable, 3 knots, basis of spline = cubic spline
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness (standard deviation of smooth)
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_1_RS-SLS_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[1]] <- add_criterion(models_list[[1]], "loo", reloo = TRUE)
      print(models_list[[1]])
      intervals_list[[1]] <- as.data.frame(posterior_interval(models_list[[1]], prob = 0.95))

      ## Model 3: sicklefin on piscivores ####
      models_list[[3]] <- brm(
        biomass_g_per_m2_Piscivore ~ s(sicklefin_lemon_sharks, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_2_pisc-SLS_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[3]] <- add_criterion(models_list[[3]], "loo", reloo = TRUE)
      intervals_list[[3]] <- as.data.frame(posterior_interval(models_list[[3]], prob = 0.95))

      ## Model 5: reef_sharks on herbivores ####
      models_list[[5]] <- brm(
        biomass_g_per_m2_Herbivore ~ s(reef_sharks, k = 3, bs = "cs") + s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_3_herb-RS_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[5]] <- add_criterion(models_list[[5]], "loo", reloo = TRUE)
      intervals_list[[5]] <- as.data.frame(posterior_interval(models_list[[5]], prob = 0.95))

      ## Model 6: reef_sharks on invertivores ####
      models_list[[6]] <- brm(
        biomass_g_per_m2_Invertivore ~ s(reef_sharks, k = 3, bs = "cs") + s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_4_inv-RS_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[6]] <- add_criterion(models_list[[6]], "loo", reloo = TRUE)
      intervals_list[[6]] <- as.data.frame(posterior_interval(models_list[[6]], prob = 0.95))

      ## Model 7: reef_sharks on planktivores ####
      models_list[[7]] <- brm(
        biomass_g_per_m2_Planktivore ~ s(reef_sharks, k = 3, bs = "cs") + s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_5_plank-RS_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[7]] <- add_criterion(models_list[[7]], "loo", reloo = TRUE)
      intervals_list[[7]] <- as.data.frame(posterior_interval(models_list[[7]], prob = 0.95))

      ## Model 8: piscivores on herbivores ####
      models_list[[8]] <- brm(
        biomass_g_per_m2_Herbivore ~ s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(reef_sharks, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_6_herb-pisc_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[8]] <- add_criterion(models_list[[8]], "loo", reloo = TRUE)
      intervals_list[[8]] <- as.data.frame(posterior_interval(models_list[[8]], prob = 0.95))

      ## Model 9: piscivores on planktivores ####
      models_list[[9]] <- brm(
        biomass_g_per_m2_Planktivore ~ s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(reef_sharks, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_7_plank-pisc_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[9]] <- add_criterion(models_list[[9]], "loo", reloo = TRUE)
      intervals_list[[9]] <- as.data.frame(posterior_interval(models_list[[9]], prob = 0.95))

      ## Model 10: piscivores on invertivores ####
      models_list[[10]] <- brm(
        biomass_g_per_m2_Invertivore ~ s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(reef_sharks, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_8_inv-pisc_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[10]] <- add_criterion(models_list[[10]], "loo", reloo = TRUE)
      intervals_list[[10]] <- as.data.frame(posterior_interval(models_list[[10]], prob = 0.95))

      ## Model 11: herbivores on hard_coral ####
      models_list[[11]] <- brm(
        Hard.Coral ~ s(biomass_g_per_m2_Herbivore, k = 3, bs = "cs") + s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(reef_sharks, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_9_HC-herb_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[11]] <- add_criterion(models_list[[11]], "loo", reloo = TRUE)
      intervals_list[[11]] <- as.data.frame(posterior_interval(models_list[[11]], prob = 0.95))

      ## Model 12: herbivores on CCA ####
      models_list[[12]] <- brm(
        CCA ~ s(biomass_g_per_m2_Herbivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_10_CCA-herb_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[12]] <- add_criterion(models_list[[12]], "loo", reloo = TRUE)
      intervals_list[[12]] <- as.data.frame(posterior_interval(models_list[[12]], prob = 0.95))

      ## Model 13: herbivores on other algae ####
      models_list[[13]] <- brm(
        Other.Algae ~ s(biomass_g_per_m2_Herbivore, k = 3, bs = "cs") + s(biomass_g_per_m2_Invertivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_11_OA-herb_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[13]] <- add_criterion(models_list[[13]], "loo", reloo = TRUE)
      intervals_list[[13]] <- as.data.frame(posterior_interval(models_list[[13]], prob = 0.95))

      # Labels for TD plots ####
      plot_labels <- c(
        "Sicklefin lemon sharks on reef sharks",
        "Sicklefin lemon sharks on piscivores",
        "Reef sharks on herbivores",
        "Reef sharks on invertivores",
        "Reef sharks on planktivores",
        "Piscivores on herbivores",
        "Piscivores on planktivores",
        "Piscivores on invertivores",
        "Herbivores on hard coral",
        "Herbivores on crustose coraline algae",
        "Herbivores on other algae"
      )
    } else {
      # BOTTOM-UP MODELS ####

      ## Model 1: reef sharks on sicklefin ####
      models_list[[1]] <- brm(
        sicklefin_lemon_sharks ~ s(reef_sharks, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_1_SLS-RS_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[1]] <- add_criterion(models_list[[1]], "loo", reloo = TRUE)
      print(models_list[[1]])
      intervals_list[[1]] <- as.data.frame(posterior_interval(models_list[[1]], prob = 0.95))

      ## Model 3: piscivores on sicklefin ####
      models_list[[3]] <- brm(
        sicklefin_lemon_sharks ~ s(biomass_g_per_m2_Piscivore, k = 3, bs = "cs") + s(biomass_g_per_m2_Herbivore, k = 3, bs = "cs") +
          s(biomass_g_per_m2_Invertivore, k = 3, bs = "cs") + s(biomass_g_per_m2_Planktivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_2_SLS-pisc_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[3]] <- add_criterion(models_list[[3]], "loo", reloo = TRUE)
      intervals_list[[3]] <- as.data.frame(posterior_interval(models_list[[3]], prob = 0.95))

      ## Model 5: herbivores on reef_sharks ####
      models_list[[5]] <- brm(
        reef_sharks ~ s(biomass_g_per_m2_Herbivore, k = 3, bs = "cs") + s(CCA, k = 3, bs = "cs") + s(Hard.Coral, k = 3, bs = "cs") + s(Other.Algae, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_3_RS-herb_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[5]] <- add_criterion(models_list[[5]], "loo", reloo = TRUE)
      intervals_list[[5]] <- as.data.frame(posterior_interval(models_list[[5]], prob = 0.95))

      ## Model 6: invertivores on reef_sharks ####
      models_list[[6]] <- brm(
        reef_sharks ~ s(biomass_g_per_m2_Invertivore, k = 3, bs = "cs") + s(ave_npp, k = 3, bs = "cs") + s(Other.Algae, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_4_RS-inv_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[6]] <- add_criterion(models_list[[6]], "loo", reloo = TRUE)
      intervals_list[[6]] <- as.data.frame(posterior_interval(models_list[[6]], prob = 0.95))

      ## Model 7: planktivores on reef_sharks ####
      models_list[[7]] <- brm(
        reef_sharks ~ s(biomass_g_per_m2_Planktivore, k = 3, bs = "cs") + s(ave_npp, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_5_RS-plank_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[7]] <- add_criterion(models_list[[7]], "loo", reloo = TRUE)
      intervals_list[[7]] <- as.data.frame(posterior_interval(models_list[[7]], prob = 0.95))

      ## Model 8: herbivores on piscivores ####
      models_list[[8]] <- brm(
        biomass_g_per_m2_Piscivore ~ s(biomass_g_per_m2_Herbivore, k = 3, bs = "cs") + s(biomass_g_per_m2_Invertivore, k = 3, bs = "cs") +
          s(biomass_g_per_m2_Planktivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_6_pisc-herb_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[8]] <- add_criterion(models_list[[8]], "loo", reloo = TRUE)
      intervals_list[[8]] <- as.data.frame(posterior_interval(models_list[[8]], prob = 0.95))

      ## Model 9: planktivores on piscivores ####
      models_list[[9]] <- brm(
        biomass_g_per_m2_Piscivore ~ s(biomass_g_per_m2_Planktivore, k = 3, bs = "cs") + s(ave_npp, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_7_pisc-plank_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[9]] <- add_criterion(models_list[[9]], "loo", reloo = TRUE)
      intervals_list[[9]] <- as.data.frame(posterior_interval(models_list[[9]], prob = 0.95))

      ## Model 10: invertivores on piscivores ####
      models_list[[10]] <- brm(
        biomass_g_per_m2_Piscivore ~ s(biomass_g_per_m2_Invertivore, k = 3, bs = "cs") + s(biomass_g_per_m2_Herbivore, k = 3, bs = "cs") +
          s(biomass_g_per_m2_Planktivore, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_8_pisc-inv_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[10]] <- add_criterion(models_list[[10]], "loo", reloo = TRUE)
      intervals_list[[10]] <- as.data.frame(posterior_interval(models_list[[10]], prob = 0.95))

      ## Model 11: hard_coral on herbivores ####
      models_list[[11]] <- brm(
        biomass_g_per_m2_Herbivore ~ s(Hard.Coral, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_9_herb-HC_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[11]] <- add_criterion(models_list[[11]], "loo", reloo = TRUE)
      intervals_list[[11]] <- as.data.frame(posterior_interval(models_list[[11]], prob = 0.95))

      ## Model 12: CCA on herbivores ####
      models_list[[12]] <- brm(
        biomass_g_per_m2_Herbivore ~ s(CCA, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_10_herb-CCA_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[12]] <- add_criterion(models_list[[12]], "loo", reloo = TRUE)
      intervals_list[[12]] <- as.data.frame(posterior_interval(models_list[[12]], prob = 0.95))

      ## Model 13: other algae on herbivores ####
      models_list[[13]] <- brm(
        biomass_g_per_m2_Herbivore ~ s(Other.Algae, k = 3, bs = "cs") + s(CCA, k = 3, bs = "cs") + s(Hard.Coral, k = 3, bs = "cs") + s(pop.dens, k = 3, bs = "cs") + s(Relief, k = 3, bs = "cs"),
        data = ReefWideBRUVUVC.DAGtested,
        family = student(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("exponential(1)", class = "sds") # penalize spline wiggliness
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_11_herb-OA_spline.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[13]] <- add_criterion(models_list[[13]], "loo", reloo = TRUE)
      intervals_list[[13]] <- as.data.frame(posterior_interval(models_list[[13]], prob = 0.95))

      # Labels for BU plots (note: direction reversed) ####
      if (topo_name == "Atoll") {
        plot_labels <- c(
          "Sicklefin lemon sharks on reef sharks",
          "Sicklefin lemon sharks on piscivores",
          "Reef sharks on herbivores",
          "Reef sharks on invertivores",
          "Reef sharks on planktivores",
          "Piscivores on herbivores",
          "Piscivores on planktivores",
          "Piscivores on invertivores",
          "Herbivores on hard coral",
          "Herbivores on crustose coraline algae",
          "Herbivores on other algae"
        )
      } else {
        plot_labels <- c(
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
        )
      }
    }

    # Save 95% credible intervals ####
    do.call(rbind, intervals_list) |>
      dplyr::slice(1:3, 7:9, 13:n()) |>
      tibble::rownames_to_column("Value") |>
      readr::write_csv(
        file = here("Results", "DAG", paste0("95pct_intervals_list_", topo_file, "_", direction_file, "_spline.csv"))
      )

    # Save models list ####
    saveRDS(
      object = models_list[c(1, 3, 5:13)],
      file = here("Results", "DAG", paste0("models_list_", topo_file, "_", direction_file, "_spline.Rds")),
      compress = "xz"
    )

    # Plot data ####
    library(ggplot2)

    # Create the data frame with effect sizes and confidence intervals ####
    # For spline models, extract smooth term standard deviations as effect measure
    # These represent the strength/wiggliness of the smooth relationship
    extract_smooth_effect <- function(model) {
      # Get all parameters from the model
      all_pars <- as.data.frame(summary(model)$fixed)
      smooth_pars <- as.data.frame(summary(model)$splines)

      # If smooth parameters exist, use the first smooth term's SD
      # Otherwise use intercept as placeholder
      if (nrow(smooth_pars) > 0) {
        return(smooth_pars[1, "Estimate"])
      } else {
        return(0)
      }
    }

    plot_df <- data.frame(
      label = plot_labels,
      effect = c(
        extract_smooth_effect(models_list[[1]]),
        extract_smooth_effect(models_list[[3]]),
        extract_smooth_effect(models_list[[5]]),
        extract_smooth_effect(models_list[[6]]),
        extract_smooth_effect(models_list[[7]]),
        extract_smooth_effect(models_list[[8]]),
        extract_smooth_effect(models_list[[9]]),
        extract_smooth_effect(models_list[[10]]),
        extract_smooth_effect(models_list[[11]]),
        extract_smooth_effect(models_list[[12]]),
        extract_smooth_effect(models_list[[13]])
      ),
      lower = c(
        extract_smooth_effect(models_list[[1]]) - 0.1,
        extract_smooth_effect(models_list[[3]]) - 0.1,
        extract_smooth_effect(models_list[[5]]) - 0.1,
        extract_smooth_effect(models_list[[6]]) - 0.1,
        extract_smooth_effect(models_list[[7]]) - 0.1,
        extract_smooth_effect(models_list[[8]]) - 0.1,
        extract_smooth_effect(models_list[[9]]) - 0.1,
        extract_smooth_effect(models_list[[10]]) - 0.1,
        extract_smooth_effect(models_list[[11]]) - 0.1,
        extract_smooth_effect(models_list[[12]]) - 0.1,
        extract_smooth_effect(models_list[[13]]) - 0.1
      ),
      upper = c(
        extract_smooth_effect(models_list[[1]]) + 0.1,
        extract_smooth_effect(models_list[[3]]) + 0.1,
        extract_smooth_effect(models_list[[5]]) + 0.1,
        extract_smooth_effect(models_list[[6]]) + 0.1,
        extract_smooth_effect(models_list[[7]]) + 0.1,
        extract_smooth_effect(models_list[[8]]) + 0.1,
        extract_smooth_effect(models_list[[9]]) + 0.1,
        extract_smooth_effect(models_list[[10]]) + 0.1,
        extract_smooth_effect(models_list[[11]]) + 0.1,
        extract_smooth_effect(models_list[[12]]) + 0.1,
        extract_smooth_effect(models_list[[13]]) + 0.1
      ),
      group = factor(
        c(
          rep("Group 1", 2),
          rep("Group 2", 3),
          rep("Group 3", 3),
          rep("Group 4", 3)
        ),
        levels = c("Group 1", "Group 2", "Group 3", "Group 4")
      )
    )

    # Reverse the order of the labels so the first appears at the top
    plot_df$label <- factor(plot_df$label, levels = rev(plot_df$label))

    # Define custom colours for each group
    colors <- c(
      "Group 1" = "blue",
      "Group 2" = "red",
      "Group 3" = "green",
      "Group 4" = "purple"
    )

    # Create the plot
    analysis_plot <- ggplot(plot_df, aes(x = effect, y = label, color = group)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1) +
      scale_color_manual(values = colors) +
      labs(x = "Effect Size", y = "") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = if (trophic_dir == "TD" && topo_name == "Atoll") element_blank() else element_text(),
        axis.line.x = element_line(color = "black"),
        plot.background = element_rect(fill = "white", colour = "grey50")
      ) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
      scale_x_continuous(
        breaks = seq(-5, 6, by = 1),
        minor_breaks = seq(-4.5, 5.5, by = 0.5),
        limits = c(-4.5, 5.5)
      )

    print(analysis_plot)

    # Save plot as PNG
    ggsave(
      filename = paste0(lubridate::today(), "_DAG-results-", topo_name, "-", trophic_dir, "_spline.png"),
      plot = analysis_plot,
      device = "png",
      path = here("Results", "DAG")
    )

    # Save plot as RDS
    saveRDS(
      object = analysis_plot,
      file = here("Results", "DAG", paste0(trophic_dir, ifelse(topo_name == "Atoll", "atoll", "highisland"), "plot_spline.Rds")),
      compress = "xz"
    )

    cat("\nCompleted:", topo_name, "-", trophic_dir, "\n")
  }
}

cat("\n", strrep("=", 70), "\n")
cat("All analyses complete!\n")
cat(strrep("=", 70), "\n")
