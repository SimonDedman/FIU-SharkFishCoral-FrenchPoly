# Suchinta Arif, 2025-03-14 & earlier
# Synthesized script combining TD/BU and Atoll/HighIsland analyses
# TD DAG: https://dagitty.net/dags.html?id=7bBT4Rqj#
# BU DAG: https://dagitty.net/dags.html?id=GBrpiZXW

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
        reef_sharks ~ sicklefin_lemon_sharks,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_1_RS-SLS_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[1]] <- add_criterion(models_list[[1]], "loo", reloo = TRUE)
      print(models_list[[1]])
      intervals_list[[1]] <- as.data.frame(posterior_interval(models_list[[1]], prob = 0.95))

      ## Model 3: sicklefin on piscivores ####
      models_list[[3]] <- brm(
        biomass_g_per_m2_Piscivore ~ sicklefin_lemon_sharks,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_2_pisc-SLS_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[3]] <- add_criterion(models_list[[3]], "loo", reloo = TRUE)
      intervals_list[[3]] <- as.data.frame(posterior_interval(models_list[[3]], prob = 0.95))

      ## Model 5: reef_sharks on herbivores ####
      models_list[[5]] <- brm(
        biomass_g_per_m2_Herbivore ~ reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_3_herb-RS_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[5]] <- add_criterion(models_list[[5]], "loo", reloo = TRUE)
      intervals_list[[5]] <- as.data.frame(posterior_interval(models_list[[5]], prob = 0.95))

      ## Model 6: reef_sharks on invertivores ####
      models_list[[6]] <- brm(
        biomass_g_per_m2_Invertivore ~ reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_4_inv-RS_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[6]] <- add_criterion(models_list[[6]], "loo", reloo = TRUE)
      intervals_list[[6]] <- as.data.frame(posterior_interval(models_list[[6]], prob = 0.95))

      ## Model 7: reef_sharks on planktivores ####
      models_list[[7]] <- brm(
        biomass_g_per_m2_Planktivore ~ reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_5_plank-RS_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[7]] <- add_criterion(models_list[[7]], "loo", reloo = TRUE)
      intervals_list[[7]] <- as.data.frame(posterior_interval(models_list[[7]], prob = 0.95))

      ## Model 8: piscivores on herbivores ####
      models_list[[8]] <- brm(
        biomass_g_per_m2_Herbivore ~ biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_6_herb-pisc_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[8]] <- add_criterion(models_list[[8]], "loo", reloo = TRUE)
      intervals_list[[8]] <- as.data.frame(posterior_interval(models_list[[8]], prob = 0.95))

      ## Model 9: piscivores on planktivores ####
      models_list[[9]] <- brm(
        biomass_g_per_m2_Planktivore ~ biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_7_plank-pisc_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[9]] <- add_criterion(models_list[[9]], "loo", reloo = TRUE)
      intervals_list[[9]] <- as.data.frame(posterior_interval(models_list[[9]], prob = 0.95))

      ## Model 10: piscivores on invertivores ####
      models_list[[10]] <- brm(
        biomass_g_per_m2_Invertivore ~ biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_8_inv-pisc_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[10]] <- add_criterion(models_list[[10]], "loo", reloo = TRUE)
      intervals_list[[10]] <- as.data.frame(posterior_interval(models_list[[10]], prob = 0.95))

      ## Model 11: herbivores on hard_coral ####
      models_list[[11]] <- brm(
        Hard.Coral ~ biomass_g_per_m2_Herbivore + biomass_g_per_m2_Piscivore + pop.dens + reef_sharks + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_9_HC-herb_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[11]] <- add_criterion(models_list[[11]], "loo", reloo = TRUE)
      intervals_list[[11]] <- as.data.frame(posterior_interval(models_list[[11]], prob = 0.95))

      ## Model 12: herbivores on CCA ####
      models_list[[12]] <- brm(
        CCA ~ biomass_g_per_m2_Herbivore + pop.dens + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_10_CCA-herb_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[12]] <- add_criterion(models_list[[12]], "loo", reloo = TRUE)
      intervals_list[[12]] <- as.data.frame(posterior_interval(models_list[[12]], prob = 0.95))

      ## Model 13: herbivores on other algae ####
      models_list[[13]] <- brm(
        Other.Algae ~ biomass_g_per_m2_Herbivore + biomass_g_per_m2_Invertivore + pop.dens + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_11_OA-herb_model.stan")),
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
        sicklefin_lemon_sharks ~ reef_sharks,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_1_SLS-RS_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[1]] <- add_criterion(models_list[[1]], "loo", reloo = TRUE)
      print(models_list[[1]])
      intervals_list[[1]] <- as.data.frame(posterior_interval(models_list[[1]], prob = 0.95))

      ## Model 3: piscivores on sicklefin ####
      models_list[[3]] <- brm(
        sicklefin_lemon_sharks ~ biomass_g_per_m2_Piscivore + biomass_g_per_m2_Herbivore +
          biomass_g_per_m2_Invertivore + biomass_g_per_m2_Planktivore + pop.dens,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_2_SLS-pisc_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[3]] <- add_criterion(models_list[[3]], "loo", reloo = TRUE)
      intervals_list[[3]] <- as.data.frame(posterior_interval(models_list[[3]], prob = 0.95))

      ## Model 5: herbivores on reef_sharks ####
      models_list[[5]] <- brm(
        reef_sharks ~ biomass_g_per_m2_Herbivore + CCA + Hard.Coral + Other.Algae + pop.dens + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_3_RS-herb_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[5]] <- add_criterion(models_list[[5]], "loo", reloo = TRUE)
      intervals_list[[5]] <- as.data.frame(posterior_interval(models_list[[5]], prob = 0.95))

      ## Model 6: invertivores on reef_sharks ####
      models_list[[6]] <- brm(
        reef_sharks ~ biomass_g_per_m2_Invertivore + ave_npp + Other.Algae + pop.dens + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_4_RS-inv_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[6]] <- add_criterion(models_list[[6]], "loo", reloo = TRUE)
      intervals_list[[6]] <- as.data.frame(posterior_interval(models_list[[6]], prob = 0.95))

      ## Model 7: planktivores on reef_sharks ####
      models_list[[7]] <- brm(
        reef_sharks ~ biomass_g_per_m2_Planktivore + ave_npp + pop.dens + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_5_RS-plank_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[7]] <- add_criterion(models_list[[7]], "loo", reloo = TRUE)
      intervals_list[[7]] <- as.data.frame(posterior_interval(models_list[[7]], prob = 0.95))

      ## Model 8: herbivores on piscivores ####
      models_list[[8]] <- brm(
        biomass_g_per_m2_Piscivore ~ biomass_g_per_m2_Herbivore + biomass_g_per_m2_Invertivore +
          biomass_g_per_m2_Planktivore + pop.dens,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_6_pisc-herb_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[8]] <- add_criterion(models_list[[8]], "loo", reloo = TRUE)
      intervals_list[[8]] <- as.data.frame(posterior_interval(models_list[[8]], prob = 0.95))

      ## Model 9: planktivores on piscivores ####
      models_list[[9]] <- brm(
        biomass_g_per_m2_Piscivore ~ biomass_g_per_m2_Planktivore + ave_npp + pop.dens + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_7_pisc-plank_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[9]] <- add_criterion(models_list[[9]], "loo", reloo = TRUE)
      intervals_list[[9]] <- as.data.frame(posterior_interval(models_list[[9]], prob = 0.95))

      ## Model 10: invertivores on piscivores ####
      models_list[[10]] <- brm(
        biomass_g_per_m2_Piscivore ~ biomass_g_per_m2_Invertivore + biomass_g_per_m2_Herbivore +
          biomass_g_per_m2_Planktivore + pop.dens,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_8_pisc-inv_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[10]] <- add_criterion(models_list[[10]], "loo", reloo = TRUE)
      intervals_list[[10]] <- as.data.frame(posterior_interval(models_list[[10]], prob = 0.95))

      ## Model 11: hard_coral on herbivores ####
      models_list[[11]] <- brm(
        biomass_g_per_m2_Herbivore ~ Hard.Coral,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_9_herb-HC_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[11]] <- add_criterion(models_list[[11]], "loo", reloo = TRUE)
      intervals_list[[11]] <- as.data.frame(posterior_interval(models_list[[11]], prob = 0.95))

      ## Model 12: CCA on herbivores ####
      models_list[[12]] <- brm(
        biomass_g_per_m2_Herbivore ~ CCA,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_10_herb-CCA_model.stan")),
        save_pars = save_pars(all = TRUE)
      )
      models_list[[12]] <- add_criterion(models_list[[12]], "loo", reloo = TRUE)
      intervals_list[[12]] <- as.data.frame(posterior_interval(models_list[[12]], prob = 0.95))

      ## Model 13: other algae on herbivores ####
      models_list[[13]] <- brm(
        biomass_g_per_m2_Herbivore ~ Other.Algae + CCA + Hard.Coral + pop.dens + Relief,
        data = ReefWideBRUVUVC.DAGtested,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        save_model = here("Results", "DAG", "brm_models", paste0(prefix, "_11_herb-OA_model.stan")),
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
        file = here("Results", "DAG", paste0("95pct_intervals_list_", topo_name, "_", trophic_dir, ".csv"))
      )

    # Save models list ####
    saveRDS(
      object = models_list[c(1, 3, 5:13)],
      file = here("Results", "DAG", paste0("models_list_", topo_name, "_", trophic_dir, ".Rds")),
      compress = "xz"
    )

    # Plot data ####
    library(ggplot2)

    # Create the data frame with effect sizes and confidence intervals ####
    plot_df <- data.frame(
      label = plot_labels,
      effect = c(
        fixef(models_list[[1]])[2, "Estimate"],
        fixef(models_list[[3]])[2, "Estimate"],
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
        intervals_list[[1]][2, 1],
        intervals_list[[3]][2, 1],
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
        intervals_list[[1]][2, 2],
        intervals_list[[3]][2, 2],
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
      filename = paste0(lubridate::today(), "_DAG-results-", topo_name, "-", trophic_dir, ".png"),
      plot = analysis_plot,
      device = "png",
      path = here("Results", "DAG")
    )

    # Save plot as RDS
    saveRDS(
      object = analysis_plot,
      file = here("Results", "DAG", paste0(trophic_dir, ifelse(topo_name == "Atoll", "atoll", "highisland"), "plot.Rds")),
      compress = "xz"
    )

    cat("\nCompleted:", topo_name, "-", trophic_dir, "\n")
  }
}

cat("\n", strrep("=", 70), "\n")
cat("All analyses complete!\n")
cat(strrep("=", 70), "\n")
