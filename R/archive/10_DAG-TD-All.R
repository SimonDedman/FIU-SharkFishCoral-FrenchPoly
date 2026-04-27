# Suchinta Arif, 2025-03-14 & earlier
# https://dagitty.net/dags.html?id=7bBT4Rqj#

# Load packages ####
# install.packages("rstanarm")
library(rstanarm)
library(here)
library(tidyverse)

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
  dplyr::mutate(dplyr::across(where(is.numeric), stdize)) # |>
# subset to atolls, where we believe healthier shark populations beget top-down trophic cascades
# dplyr::filter(topo %in% c("open atoll", "closed atoll")) # Atolls; from n=24 to n=13
# dplyr::filter(topo %in% c("near atoll", "high barrier")) # High Islands; from n=24 to n=11

# STAN GLM effects ####
## sicklefin on reef sharks ####
models_list <- list()
models_list[[1]] <- stan_glm(
  # sicklefin_reef_sharks
  reef_sharks ~ sicklefin_lemon_sharks, # use dagitty to check for backdoor paths, include all from ONE bullet point
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(), # change if your response is non-continuous
  prior = normal(0, 2.5), # weakly informative prior for slopes
  prior_intercept = normal(0, 5), # weakly informative prior for intercept
  chains = 4, # number of MCMC chains
  iter = 2000, # number of iterations per chain
  seed = 123
) # for reproducibility
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

## transient pelagics on reef_sharks ####
models_list[[2]] <- stan_glm(
  # transient_reef_sharks_bayes
  reef_sharks ~ transient_pelagic_sharks,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[2]] <- as.data.frame(posterior_interval(
  models_list[[2]],
  prob = 0.95
))

## sicklefin on piscivores ####
models_list[[3]] <- stan_glm(
  # sicklefin_piscivores_bayes
  biomass_g_per_m2_Piscivore ~ sicklefin_lemon_sharks,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[3]] <- as.data.frame(posterior_interval(
  models_list[[3]],
  prob = 0.95
))

## transient pelagics on piscivores ####
models_list[[4]] <- stan_glm(
  # transient_piscivores_bayes
  biomass_g_per_m2_Piscivore ~ transient_pelagic_sharks,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[4]] <- as.data.frame(posterior_interval(
  models_list[[4]],
  prob = 0.95
))

## reef_sharks on herbivores ####
models_list[[5]] <- stan_glm(
  # reef_sharks_herbivores_bayes
  biomass_g_per_m2_Herbivore ~
    reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[5]] <- as.data.frame(posterior_interval(
  models_list[[5]],
  prob = 0.95
))

## reef_sharks on invertivores ####
models_list[[6]] <- stan_glm(
  # reef_sharks_invertivores_bayes
  biomass_g_per_m2_Invertivore ~
    reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[6]] <- as.data.frame(posterior_interval(
  models_list[[6]],
  prob = 0.95
))

## reef_sharks on planktivores ####
models_list[[7]] <- stan_glm(
  # reef_sharks_planktivores_bayes
  biomass_g_per_m2_Planktivore ~
    reef_sharks + biomass_g_per_m2_Piscivore + pop.dens,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[7]] <- as.data.frame(posterior_interval(
  models_list[[7]],
  prob = 0.95
))

## piscivores on herbivores ####
models_list[[8]] <- stan_glm(
  # piscivore_herbivore_bayes
  biomass_g_per_m2_Herbivore ~
    biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[8]] <- as.data.frame(posterior_interval(
  models_list[[8]],
  prob = 0.95
))

## piscivores on planktivores ####
models_list[[9]] <- stan_glm(
  # piscivore_herbivore_bayes
  biomass_g_per_m2_Planktivore ~
    biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[9]] <- as.data.frame(posterior_interval(
  models_list[[9]],
  prob = 0.95
))

## piscivores on invertivores ####
models_list[[10]] <- stan_glm(
  biomass_g_per_m2_Invertivore ~
    biomass_g_per_m2_Piscivore + pop.dens + reef_sharks,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[10]] <- as.data.frame(posterior_interval(
  models_list[[10]],
  prob = 0.95
))

## herbivores on hard_coral ####
models_list[[11]] <- stan_glm(
  Hard.Coral ~
    biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Piscivore +
      pop.dens +
      reef_sharks +
      Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[11]] <- as.data.frame(posterior_interval(
  models_list[[11]],
  prob = 0.95
))

## herbivores on CCA ####
models_list[[12]] <- stan_glm(
  CCA ~ biomass_g_per_m2_Herbivore + pop.dens + Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
### 95% credible intervals ####
intervals_list[[12]] <- as.data.frame(posterior_interval(
  models_list[[12]],
  prob = 0.95
))

## herbivores on other algae ####
models_list[[13]] <- stan_glm(
  Other.Algae ~
    biomass_g_per_m2_Herbivore +
      biomass_g_per_m2_Invertivore +
      pop.dens +
      Relief,
  data = ReefWideBRUVUVC.DAGtested,
  family = gaussian(),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 5),
  chains = 4,
  iter = 2000,
  seed = 123
)
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
      # "95pct_intervals_list_All_TopDown.csv"
      ## Marquesas ####
      "95pct_intervals_list_All_TopDown_NoMarquesas.csv"
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
    # "models_list_All_TopDown.Rds"
    ## Marquesas ####
    "models_list_All_TopDown_NoMarquesas.Rds"
  ),
  compress = "xz"
)

# Plot data ####
# Load the ggplot2 package
library(ggplot2)

# Create the data frame with effect sizes, confidence intervals, and group labels
dfTDall <- data.frame(
  ## FnGp incl/excl SLS ####
  label = c(
    "Sicklefin lemon sharks on reef sharks",
    # "Transient pelagic sharks on reef sharks",
    "Sicklefin lemon sharks on piscivores",
    # "Transient pelagic sharks on piscivores",
    "Reef sharks on herbivores",
    "Reef sharks on invertivores",
    "Reef sharks on planktivores",
    #
    # "Apex sharks on meso sharks",
    # "Apex sharks on piscivores",
    # "Meso sharks on herbivores",
    # "Meso sharks on invertivores",
    # "Meso sharks on planktivores",
    #
    "Piscivores on herbivores",
    "Piscivores on planktivores",
    "Piscivores on invertivores",
    "Herbivores on hard coral",
    "Herbivores on crustose coraline algae",
    "Herbivores on other algae"
  ),
  effect = c(
    models_list[[1]]$coefficients[2], # comment out for FnGp incl/excl SLS
    # models_list[[2]]$coefficients[2], # comment out for FnGp incl/excl Apex sharks
    models_list[[3]]$coefficients[2], # comment out for FnGp incl/excl SLS
    # models_list[[4]]$coefficients[2], # comment out for FnGp incl/excl Apex sharks
    models_list[[5]]$coefficients[2],
    models_list[[6]]$coefficients[2],
    models_list[[7]]$coefficients[2],
    models_list[[8]]$coefficients[2],
    models_list[[9]]$coefficients[2],
    models_list[[10]]$coefficients[2],
    models_list[[11]]$coefficients[2],
    models_list[[12]]$coefficients[2],
    models_list[[13]]$coefficients[2]
  ),
  lower = c(
    intervals_list[[1]][2, 1], # comment out for FnGp incl/excl SLS
    # intervals_list[[2]][2, 1], # comment out for FnGp incl/excl Apex sharks
    intervals_list[[3]][2, 1], # comment out for FnGp incl/excl SLS
    # intervals_list[[4]][2, 1], # comment out for FnGp incl/excl Apex sharks
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
  tenpct = c(
    # 10%
    models_list[[1]]$stan_summary[2, 5], # comment out for FnGp incl/excl SLS
    # models_list[[2]]$stan_summary[2, 5], # comment out for FnGp incl/excl Apex sharks
    models_list[[3]]$stan_summary[2, 5], # comment out for FnGp incl/excl SLS
    # models_list[[4]]$stan_summary[2, 5], # comment out for FnGp incl/excl Apex sharks
    models_list[[5]]$stan_summary[2, 5],
    models_list[[6]]$stan_summary[2, 5],
    models_list[[7]]$stan_summary[2, 5],
    models_list[[8]]$stan_summary[2, 5],
    models_list[[9]]$stan_summary[2, 5],
    models_list[[10]]$stan_summary[2, 5],
    models_list[[11]]$stan_summary[2, 5],
    models_list[[12]]$stan_summary[2, 5],
    models_list[[13]]$stan_summary[2, 5]
  ),
  twentyfivepct = c(
    # 25%
    models_list[[1]]$stan_summary[2, 6], # comment out for FnGp incl/excl SLS
    # models_list[[2]]$stan_summary[2, 6], # comment out for FnGp incl/excl Apex sharks
    models_list[[3]]$stan_summary[2, 6], # comment out for FnGp incl/excl SLS
    # models_list[[4]]$stan_summary[2, 6], # comment out for FnGp incl/excl Apex sharks
    models_list[[5]]$stan_summary[2, 6],
    models_list[[6]]$stan_summary[2, 6],
    models_list[[7]]$stan_summary[2, 6],
    models_list[[8]]$stan_summary[2, 6],
    models_list[[9]]$stan_summary[2, 6],
    models_list[[10]]$stan_summary[2, 6],
    models_list[[11]]$stan_summary[2, 6],
    models_list[[12]]$stan_summary[2, 6],
    models_list[[13]]$stan_summary[2, 6]
  ),
  seventyfivepct = c(
    # 75%
    models_list[[1]]$stan_summary[2, 8], # comment out for FnGp incl/excl SLS
    # models_list[[2]]$stan_summary[2, 8], # comment out for FnGp incl/excl Apex sharks
    models_list[[3]]$stan_summary[2, 8], # comment out for FnGp incl/excl SLS
    # models_list[[4]]$stan_summary[2, 8], # comment out for FnGp incl/excl Apex sharks
    models_list[[5]]$stan_summary[2, 8],
    models_list[[6]]$stan_summary[2, 8],
    models_list[[7]]$stan_summary[2, 8],
    models_list[[8]]$stan_summary[2, 8],
    models_list[[9]]$stan_summary[2, 8],
    models_list[[10]]$stan_summary[2, 8],
    models_list[[11]]$stan_summary[2, 8],
    models_list[[12]]$stan_summary[2, 8],
    models_list[[13]]$stan_summary[2, 8]
  ),
  ninetypct = c(
    # 90%
    models_list[[1]]$stan_summary[2, 9], # comment out for FnGp incl/excl SLS
    # models_list[[2]]$stan_summary[2, 9], # comment out for FnGp incl/excl Apex sharks
    models_list[[3]]$stan_summary[2, 9], # comment out for FnGp incl/excl SLS
    # models_list[[4]]$stan_summary[2, 9], # comment out for FnGp incl/excl Apex sharks
    models_list[[5]]$stan_summary[2, 9],
    models_list[[6]]$stan_summary[2, 9],
    models_list[[7]]$stan_summary[2, 9],
    models_list[[8]]$stan_summary[2, 9],
    models_list[[9]]$stan_summary[2, 9],
    models_list[[10]]$stan_summary[2, 9],
    models_list[[11]]$stan_summary[2, 9],
    models_list[[12]]$stan_summary[2, 9],
    models_list[[13]]$stan_summary[2, 9]
  ),
  upper = c(
    intervals_list[[1]][2, 2], # comment out for FnGp incl/excl SLS
    # intervals_list[[2]][2, 2], # comment out for FnGp incl/excl Apex sharks
    intervals_list[[3]][2, 2], # comment out for FnGp incl/excl SLS
    # intervals_list[[4]][2, 2], # comment out for FnGp incl/excl Apex sharks
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
      # rep("Group 1", 4), # sicklefin & transient shark effects  # comment out for FnGp incl/excl SLS
      rep("Group 1", 2), # transient shark effects # comment out for FnGp incl/excl SLS/Apex
      rep("Group 2", 3), # reef shark effects
      rep("Group 3", 3), # piscivore effects
      rep("Group 4", 3) # herbivore effects
    ),
    levels = c("Group 1", "Group 2", "Group 3", "Group 4")
  )
)

# Reverse the order of the labels so the first appears at the top
dfTDall$label <- factor(dfTDall$label, levels = rev(dfTDall$label))

# Define custom colours for each group
colors <- c(
  "Group 1" = "blue",
  "Group 2" = "red",
  "Group 3" = "green",
  "Group 4" = "purple"
)

# Create the plot without a legend or title, ensuring the x-axis is clearly visible
TDallplot <- ggplot(dfTDall, aes(x = effect, y = label, color = group)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.1) + # 2.5%, 97.5%
  geom_errorbarh(aes(xmin = tenpct, xmax = ninetypct), height = 0.2) + # 10/90
  geom_errorbarh(
    aes(xmin = twentyfivepct, xmax = seventyfivepct),
    height = 0.3
  ) + # 25/75
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
TDallplot

ggsave(
  filename = paste0(
    lubridate::today(),
    # "_DAG-results-All-TopDown.png"
    ## Marquesas ####
    "_DAG-results-All-TopDown_NoMarquesas.png"
  ),
  device = "png",
  path = here("Results", "DAG")
)
