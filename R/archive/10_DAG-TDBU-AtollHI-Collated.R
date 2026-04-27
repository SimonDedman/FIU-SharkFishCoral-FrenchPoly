# ChatGPT consolidation of all 4 DAG scripts 10 & 11.
# Needs checking, formula look off

library(brms)
library(tidyverse)
library(here)
options(future.globals.maxSize = 1.5 * 1024^3) # 1.5 GiB otherwise add_criterion(models_list[[11]], "loo", reloo = TRUE) can run out of memory


# Define standardization function
stdize <- function(x) {
  if (is.numeric(x)) as.numeric(scale(x)) else x
}

## make standardisation function ####
stdize <- function(x) {
  # centre around mean, scale to SD, then rescale to 0:1
  x <- (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE)) # centre & scale to SD
  x <- x - min(x, na.rm = TRUE) # shift so min is 0
  x <- x / max(x, na.rm = TRUE) # scale so max is 1
  return(x)
}


# Load your data here
# data <- read_csv("your_dataset.csv")
# Filter dataset
d <- data %>%
  filter(region != "Marquesas") %>%
  filter(
    if (topo == "High") topo %in% c("high island", "mixed island") else
      topo %in% c("open atoll", "closed atoll")
  ) %>%
  mutate(across(everything(), stdize))


# Load & prepare data ####
dag_data_raw <- readRDS(here(
  "NFF_data",
  "ch4_reef_wide_df2.RData"
)) |>
  # ReefWideBRUVUVC.DAGtested <- readr::read_csv(here("NFF_data", "ReefWideBRUVUVC-DAGtested.csv")) |>
  # remove dud column
  dplyr::select(-sum) |>
  # apply standardisation function
  dplyr::mutate(dplyr::across(where(is.numeric), stdize))


# Define custom colours for each ggplot group
colors <- c(
  "Group 1" = "blue",
  "Group 2" = "red",
  "Group 3" = "green",
  "Group 4" = "purple"
)


# Define DAG-based formulas for top-down and bottom-up models
# Note: These are applied identically for Atolls and High Islands

# Top-down model formulas (excluding commented out lines and apex sharks)
topdown_formulas <- list(
  reef_sharks ~ sicklefin_lemon_sharks,
  biomass_g_per_m2_Piscivore ~ sicklefin_lemon_sharks,
  biomass_g_per_m2_Herbivore ~
    reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
  biomass_g_per_m2_Invertivore ~
    reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
  biomass_g_per_m2_Planktivore ~
    reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
  biomass_g_per_m2_Herbivore ~
    biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
  biomass_g_per_m2_Planktivore ~
    biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
  biomass_g_per_m2_Invertivore ~
    biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
  Hard.Coral ~
    biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Piscivore +
      pop.dens +
      reef_sharks +
      Relief,
  CCA ~ biomass_g_per_m2_Herbivore + pop.dens + Relief,
  data = ReefWideBRUVUVC.DAGtested,
  Other.Algae ~
    biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Invertivore +
      pop.dens +
      Relief
)

# Bottom-up model formulas (excluding commented out lines and apex sharks)
bottomup_formulas <- list(
  sicklefin_lemon_sharks ~ reef_sharks,
  sicklefin_lemon_sharks ~
    biomass_g_per_m2_Piscivore +
      biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Invertivore +
      biomass_g_per_m2_Planktivore +
      pop.dens,
  reef_sharks ~
    biomass_g_per_m2_Herbivore +
      CCA +
      Hard.Coral +
      Other.Algae +
      pop.dens +
      Relief,
  reef_sharks ~
    biomass_g_per_m2_Invertivore +
      ave_npp +
      Other.Algae +
      pop.dens +
      Relief,
  reef_sharks ~
    biomass_g_per_m2_Planktivore +
      ave_npp +
      pop.dens +
      Relief,
  biomass_g_per_m2_Piscivore ~
    biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Invertivore +
      biomass_g_per_m2_Planktivore +
      pop.dens,
  biomass_g_per_m2_Piscivore ~
    biomass_g_per_m2_Planktivore +
      ave_npp +
      pop.dens +
      Relief,
  biomass_g_per_m2_Piscivore ~
    biomass_g_per_m2_Invertivore +
      biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Planktivore +
      pop.dens,
  biomass_g_per_m2_Herbivore ~ Hard.Coral,
  biomass_g_per_m2_Herbivore ~ CCA,
  biomass_g_per_m2_Herbivore ~
    Other.Algae +
      CCA +
      Hard.Coral +
      pop.dens +
      Relief
)

# Topographies and directions
topos <- c("Atolls", "HighIslands")
directions <- list(
  TopDown = topdown_formulas,
  BottomUp = bottomup_formulas
)

# Fit and save brms models
for (topo in topos) {
  # subset data to high islands/atolls
  # dplyr::filter(topo %in% c("near atoll", "high barrier")) # High Island; from n=24 to n=11
  dag_data <- dag_data_raw |>
    if (topo == "Atolls") {
      dplyr::filter(topo %in% c("open atoll", "closed atoll")) # Atoll; from n=24 to n=13
    } else {
      dplyr::filter(topo %in% c("near atoll", "high barrier")) # High Island; from n=24 to n=11
    }

  for (direction in names(directions)) {
    formulas <- directions[[direction]]
    models_list <- list()
    intervals_list <- list()

    for (i in seq_along(formulas)) {
      formula <- formulas[[i]]
      print(paste("Fitting:", direction, topo, deparse(formula)))

      model <- brm(
        formula = formula,
        data = dag_data,
        family = gaussian(),
        prior = c(
          set_prior("normal(0, 5)", class = "Intercept"),
          set_prior("normal(0, 2.5)", class = "b")
        ),
        chains = 4,
        iter = 2000,
        seed = 123,
        silent = 2,
        save_model = here(
          "Results",
          "DAG",
          "brm_models",
          paste0(
            topo,
            "_",
            direction,
            "_model_",
            i,
            ".stan"
          )
        ),
        save_pars = save_pars(all = TRUE) # Needed for reloo
      )
      models_list[[i]] <- model
      models_list[[i]] <- add_criterion(models_list[[i]], "loo", reloo = TRUE)

      intervals_list[[i]] <- as.data.frame(posterior_interval(
        models_list[[i]],
        prob = 0.95
      ))
    }

    # Save model list
    saveRDS(
      models_list,
      here(
        "Results",
        "DAG",
        paste0("models_list_", topo, "_", direction, ".Rds")
      )
    )

    # Save 95% credible intervals ####
    do.call(rbind, intervals_list) |>
      dplyr::slice(1:3, 7:9, 13:n()) |> # remove apex sharks which have low n
      tibble::rownames_to_column("Value") |>
      readr::write_csv(
        file = here(
          "Results",
          "DAG",
          paste0("95pct_intervals_list_", topo, "_", direction, ".csv")
        )
      )

    # Plot data ####
    # Create the data frame with effect sizes, confidence intervals, and group labels
    dfggplot <- data.frame(
      ## FnGp incl/excl SLS ####
      label = if (direction == TopDown) {
        c(
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
        c(
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
      }, # ifelse direction
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
        # 2.5%
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
        # 97.5%
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
          rep("Group 1", 2), # SLS shark effects
          rep("Group 2", 3), # reef shark effects
          rep("Group 3", 3), # piscivore effects
          rep("Group 4", 3) # herbivore effects
        ),
        levels = c("Group 1", "Group 2", "Group 3", "Group 4")
      ) # group
    ) # data frame

    # Reverse the order of the labels so the first appears at the top
    dfggplot$label <- factor(dfggplot$label, levels = rev(dfggplot$label))

    # Create the plot without a legend or title, ensuring the x-axis is clearly visible
    BUatollplot <- ggplot(dfggplot, aes(x = effect, y = label, color = group)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1) + # 2.5%, 97.5%
      scale_color_manual(values = colors) +
      labs(x = "Effect Size", y = "") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
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

    BUatollplot

    ggsave(
      filename = paste0(
        lubridate::today(),
        "_DAG-results-",
        topo,
        "-",
        direction,
        ".png"
      ),
      device = "png",
      path = here("Results", "DAG")
    )
  } # for (direction in names(directions))
} # for (topo in topos)
