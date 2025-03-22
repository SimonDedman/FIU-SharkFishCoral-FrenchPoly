# Suchinta Arif, 2025-03-14 & earlier
# https://dagitty.net/dags.html?id=GBrpiZXW

# Load packages ####
# install.packages("rstanarm")
library(rstanarm)
library(here)

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
models_list[[1]] <- stan_glm(
  sicklefin_lemon_sharks ~ reef_sharks,
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

## reef_sharks on transient pelagics ####
models_list[[2]] <- stan_glm(
  transient_pelagic_sharks ~ reef_sharks,
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

## piscivores on sicklefin ####
models_list[[3]] <- stan_glm(
  sicklefin_lemon_sharks ~
    chi_Piscivore_percent +
      chi_Herbivore_percent +
      chi_Invertivore_percent +
      chi_Planktivore_percent +
      pop.dens,
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

## piscivores on transient pelagics ####
models_list[[4]] <- stan_glm(
  # transient_piscivores_bayes
  transient_pelagic_sharks ~
    chi_Piscivore_percent +
      chi_Herbivore_percent +
      chi_Invertivore_percent +
      chi_Planktivore_percent +
      pop.dens,
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

## herbivores on reef_sharks ####
models_list[[5]] <- stan_glm(
  reef_sharks ~
    chi_Herbivore_percent +
      CCA +
      Hard.Coral +
      Other.Algae +
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
intervals_list[[5]] <- as.data.frame(posterior_interval(
  models_list[[5]],
  prob = 0.95
))

## invertivores on reef_sharks ####
models_list[[6]] <- stan_glm(
  reef_sharks ~
    chi_Invertivore_percent +
      ave_npp +
      Other.Algae +
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
intervals_list[[6]] <- as.data.frame(posterior_interval(
  models_list[[6]],
  prob = 0.95
))

## planktivores on reef_sharks ####
models_list[[7]] <- stan_glm(
  reef_sharks ~
    chi_Planktivore_percent +
      ave_npp +
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
intervals_list[[7]] <- as.data.frame(posterior_interval(
  models_list[[7]],
  prob = 0.95
))

## herbivores on piscivores ####
models_list[[8]] <- stan_glm(
  chi_Piscivore_percent ~
    chi_Herbivore_percent +
      chi_Invertivore_percent +
      chi_Planktivore_percent +
      pop.dens,
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

## planktivores on piscivores ####
models_list[[9]] <- stan_glm(
  chi_Piscivore_percent ~
    chi_Planktivore_percent +
      ave_npp +
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
intervals_list[[9]] <- as.data.frame(posterior_interval(
  models_list[[9]],
  prob = 0.95
))

## invertivores on piscivores ####
models_list[[10]] <- stan_glm(
  chi_Piscivore_percent ~
    chi_Invertivore_percent +
      chi_Herbivore_percent +
      chi_Planktivore_percent +
      pop.dens,
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

## hard_coral on herbivores ####
models_list[[11]] <- stan_glm(
  chi_Herbivore_percent ~ Hard.Coral,
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

## CCA on herbivores ####
models_list[[12]] <- stan_glm(
  chi_Herbivore_percent ~ CCA,
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

## other algae on herbivores ####
models_list[[13]] <- stan_glm(
  chi_Herbivore_percent ~
    Other.Algae +
      CCA +
      Hard.Coral +
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
  tibble::rownames_to_column("Value") |>
  readr::write_csv(
    file = here(
      "Results",
      "DAG",
      "95pct_intervals_list_HighIslands_BottomUp.csv"
      # "95pct_intervals_list_Atolls_BottomUp.csv"
      # "95pct_intervals_list_All_BottomUp.csv"
    )
  )

# Save models list ####
saveRDS(
  object = models_list,
  file = here(
    "Results",
    "DAG",
    "models_list_HighIslands_BottomUp.Rds"
    # "models_list_Atolls_BottomUp.Rds"
    # "models_list_All_BottomUp.Rds"
  ),
  compress = "xz"
)

# Plot data ####
# Load the ggplot2 package
library(ggplot2)

# Create the data frame with effect sizes, confidence intervals, and group labels
df <- data.frame(
  label = c(
    "Effect of reef sharks on sicklefin lemon sharks",
    "Effect of reef sharks on transient pelagic sharks",
    "Effect of piscivores on sicklefin lemon sharks",
    "Effect of piscivores on transient pelagic sharks",
    "Effect of herbivores on reef sharks",
    "Effect of invertivores on reef sharks",
    "Effect of planktivores on reef sharks",
    "Effect of herbivores on piscivores",
    "Effect of planktivores on piscivores",
    "Effect of invertivores on piscivores",
    "Effect of hard coral on herbivores",
    "Effect of crustose coraline algae on herbivores",
    "Effect of other algae on herbivores"
  ),
  effect = c(
    models_list[[1]]$coefficients[2],
    models_list[[2]]$coefficients[2],
    models_list[[3]]$coefficients[2],
    models_list[[4]]$coefficients[2],
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
    intervals_list[[1]][2, 1],
    intervals_list[[2]][2, 1],
    intervals_list[[3]][2, 1],
    intervals_list[[4]][2, 1],
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
    models_list[[1]]$stan_summary[2, 5], # same as models_list[[1]]$stan_summary[2, 5]
    models_list[[2]]$stan_summary[2, 5],
    models_list[[3]]$stan_summary[2, 5],
    models_list[[4]]$stan_summary[2, 5],
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
    models_list[[1]]$stan_summary[2, 6], # same as models_list[[1]]$stan_summary[2, 5]
    models_list[[2]]$stan_summary[2, 6],
    models_list[[3]]$stan_summary[2, 6],
    models_list[[4]]$stan_summary[2, 6],
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
    # 25%
    models_list[[1]]$stan_summary[2, 8],
    models_list[[2]]$stan_summary[2, 8],
    models_list[[3]]$stan_summary[2, 8],
    models_list[[4]]$stan_summary[2, 8],
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
    # 25%
    models_list[[1]]$stan_summary[2, 9],
    models_list[[2]]$stan_summary[2, 9],
    models_list[[3]]$stan_summary[2, 9],
    models_list[[4]]$stan_summary[2, 9],
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
    intervals_list[[1]][2, 2],
    intervals_list[[2]][2, 2],
    intervals_list[[3]][2, 2],
    intervals_list[[4]][2, 2],
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
      rep("Group 1", 4), # sicklefin & transient shark effects
      rep("Group 2", 3), # reef shark effects
      rep("Group 3", 3), # piscivore effects
      rep("Group 4", 3) # herbivore effects
    ),
    levels = c("Group 1", "Group 2", "Group 3", "Group 4")
  )
)

# Reverse the order of the labels so the first appears at the top
df$label <- factor(df$label, levels = rev(df$label))

# Define custom colours for each group
colors <- c(
  "Group 1" = "blue",
  "Group 2" = "red",
  "Group 3" = "green",
  "Group 4" = "purple"
)

# Create the plot without a legend or title, ensuring the x-axis is clearly visible
ggplot(df, aes(x = effect, y = label, color = group)) +
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
    # Add an x-axis line for clarity (minimal themes sometimes omit it)
    axis.line.x = element_line(color = "black"),
    plot.background = element_rect(fill = "white", colour = "grey50") # white background
  ) +
  # add a dashed grey vertical line at the point where x = 0
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") # +
# manually add x limits to frame the plot
# xlim(c(-1, 1.25))

ggsave(
  filename = paste0(
    lubridate::today(),
    "_DAG-results-HighIslands-BottomUp.png"
    # "_DAG-results-Atolls-BottomUp.png"
    # "_DAG-results-All-BottomUp.png"
  ),
  device = "png",
  path = here("Results", "DAG")
)
