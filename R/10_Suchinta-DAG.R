# Suchinta Arif, 2025-03-14 & earlier

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
  dplyr::mutate(dplyr::across(where(is.numeric), stdize))

# STAN GLM effects ####
## sicklefin on reef sharks ####
models_list <- list()
models_list[[1]] <- stan_glm(
  # sicklefin_reef_sharks
  reef_sharks ~ sicklefin_lemon_sharks,
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

## sicklefin on piscivores ####
models_list[[2]] <- stan_glm(
  # sicklefin_piscivores_bayes
  chi_Piscivore_percent ~ sicklefin_lemon_sharks,
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

## transient pelagics on reef_sharks ####
models_list[[3]] <- stan_glm(
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
intervals_list[[3]] <- as.data.frame(posterior_interval(
  models_list[[3]],
  prob = 0.95
))

## transient pelagics on piscivores ####
models_list[[4]] <- stan_glm(
  # transient_piscivores_bayes
  chi_Piscivore_percent ~ transient_pelagic_sharks,
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
  chi_Herbivore_percent ~ reef_sharks + chi_Piscivore_percent + pop.dens,
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
  chi_Herbivore_percent ~ reef_sharks + chi_Invertivore_percent + pop.dens,
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
  chi_Herbivore_percent ~ reef_sharks + chi_Planktivore_percent + pop.dens,
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
  chi_Herbivore_percent ~ chi_Piscivore_percent + pop.dens + reef_sharks,
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
  chi_Planktivore_percent ~ chi_Piscivore_percent + pop.dens + reef_sharks,
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
  # piscivore_herbivore_bayes
  chi_Invertivore_percent ~ chi_Piscivore_percent + pop.dens + reef_sharks,
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
  # herbivores_hard_coral_bayes
  Hard.Coral ~
    chi_Herbivore_percent +
      chi_Planktivore_percent +
      chi_Piscivore_percent +
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
  # herbivores_crustose_bayes
  CCA ~ chi_Herbivore_percent + pop.dens + Relief,
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
  # herbivores_other_bayes
  Other.Algae ~
    chi_Herbivore_percent + chi_Invertivore_percent + pop.dens + Relief,
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
  readr::write_csv(file = here("Results", "DAG", "95pct_intervals_list.csv"))

# Save models list ####
saveRDS(
  object = models_list,
  file = here("Results", "DAG", "models_list.Rds"),
  compress = "xz"
)

# Plot data ####
# Load the ggplot2 package
library(ggplot2)

# Create the data frame with effect sizes, confidence intervals, and group labels
df <- data.frame(
  label = c(
    "Effect of sicklefin lemon sharks on reef sharks",
    "Effect of sicklefin lemon sharks on piscivores",
    "Effect of transient pelagic sharks on reef sharks",
    "Effect of transient pelagic sharks on piscivores",
    "Effect of reef sharks on herbivores",
    "Effect of reef sharks on invertivores",
    "Effect of reef sharks on planktivores",
    "Effect of piscivores on herbivores",
    "Effect of piscivores on planktivores",
    "Effect of piscivores on insectivores",
    "Effect of herbivores on hard coral",
    "Effect of herbivores on crustose coraline algae",
    "Effect of herbivores on other algae"
  ),
  # TODO
  # convert effect lower upper to code
  # effect: sicklefin_reef_sharks$coefficients[2]  ?
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
  # c(0.003, -0.07, -0.313, 0.01, -0.258, -0.215, -0.23, 0.54, -0.29, -0.144, 0.25),
  # lower: posterior_interval(sicklefin_reef_sharks, prob = 0.95)[2,1]  ?
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
  # c(-0.518, -0.634, -0.681, -0.39, -0.55, -0.55, -0.50, 0.27, -0.82, -0.59, -0.47),
  # upper: posterior_interval(sicklefin_reef_sharks, prob = 0.95)[2,2]  ?
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
  # c(0.52, 0.048, 0.053, 0.42, 0.03, 0.11, 0.04, 0.81, 0.23, 0.30, 0.97),
  group = factor(
    c(
      rep("Group 1", 4), # sicklefin & transient shark effects
      rep("Group 2", 3), # reef shark effects
      rep("Group 3", 3), # piscivore effects
      rep("Group 4", 3)
    ), # herbivore effects
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
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
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
  # manually add x limits to frame the plot
  xlim(c(-1, 1.25))

ggsave(
  filename = paste0(lubridate::today(), "_DAG-results.png"),
  device = "png",
  path = here("Results", "DAG")
)
