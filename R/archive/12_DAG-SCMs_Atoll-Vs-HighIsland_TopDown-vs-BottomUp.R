# loo (leave one out) expected log pointwise predictive density metrics
# For SCMs with 2 topographies and 2 directions from scripts 10_Suchinta-DAG.R & 11_Si-DAG-bottomup.R
# Simon Dedman, 2025-03-20 simondedman@gmail.com

library(here)
library(loo)
library(brms)
library(tidyverse)

# pull raw data for reef names
rawdata <- readRDS(here(
  "NFF_data",
  "ch4_reef_wide_df2.RData"
)) |>
  # Mutate to create a new column 'topo' based on the 'topo' column
  mutate(
    topo2 = case_when(
      topo %in% c("open atoll", "closed atoll") ~ "Atolls",
      topo %in% c("near atoll", "high barrier") ~ "HighIslands",
      TRUE ~ topo
    )
  ) |>
  arrange(topo2)

reefnames <- rawdata |>
  select(reef_name, topo2)

# ?loo:
# point estimates and standard errors of the:
# expected log pointwise predictive density (elpd_loo),
# the effective number of parameters (p_loo) and the
# LOO information criterion looic (-2 * elpd_loo, i.e. converted to deviance scale).
#
# Model with lower elpd_loo generally preferred re: predictive accuracy.
# SE elpd_loo can help assess whether diff between 2 models is meaningful.

# Store ELPD-LOO objects for comparison
loo_list <- list()
posteriors_list <- list()
elpd_obs_list <- list()
bayesR2_list <- list()

topdown_formulas <- c(
  "Sicklefin lemon sharks on reef sharks",
  "Sicklefin lemon sharks on piscivores",
  "Reef sharks on planktivores",
  "Reef sharks on invertivores",
  "Reef sharks on herbivores",
  "Piscivores on planktivores",
  "Piscivores on invertivores",
  "Piscivores on herbivores",
  "Herbivores on hard coral",
  "Herbivores on crustose coraline algae",
  "Herbivores on other algae"
)

bottomup_formulas <- c(
  "Reef sharks on sicklefin lemon sharks",
  "Piscivores on sicklefin lemon sharks",
  "Planktivores on reef sharks",
  "Invertivores on reef sharks",
  "Herbivores on reef sharks",
  "Planktivores on piscivores",
  "Invertivores on piscivores",
  "Herbivores on piscivores",
  "Hard coral on herbivores",
  "Crustose coraline algae on herbivores",
  "Other algae on herbivores"
)

original_labels <- c(topdown_formulas, bottomup_formulas)

# Loop through models & calculate loo & posteriors ####
resultsdf <- data.frame(
  topo = character(),
  direction = character(),
  model = character(),
  elpd_loo = numeric(),
  elpd_loo_SE = numeric(),
  p_loo = numeric(),
  p_loo_SE = numeric(),
  looic = numeric(),
  looic_SE = numeric(),
  elpd_loo_rank = numeric(),
  stringsAsFactors = FALSE
)

counter <- 1
for (topo in c(
  # topo <- "Atolls"
  "Atolls",
  "HighIslands"
)) {
  for (direction in c("TopDown", "BottomUp")) {
    # direction <- "TopDown"
    message(paste0(
      "Topo/Direction Combo: ",
      counter,
      "/4; Topography: ",
      topo,
      "; Direction: ",
      direction
    ))

    # Read model list
    loopmodelslist <- readRDS(here(
      "Results",
      "DAG",
      paste0("models_list_", topo, "_", direction, ".Rds")
    ))
    # Fails across OSes ####
    # If created on linux, won't open in windows.

    # Create empty lists to hold LOO results for this combo
    loo_list[[topo]][[direction]] <- list()
    posteriors_list[[topo]][[direction]] <- list()
    elpd_obs_list[[topo]][[direction]] <- list()
    bayesR2_list[[topo]][[direction]] <- list()
    # if (is.null(elpd_obs_list[[topo]])) elpd_obs_list[[topo]] <- list()
    # if (is.null(elpd_obs_list[[topo]][[direction]])) elpd_obs_list[[topo]][[direction]] <- list()

    for (i in seq_along(loopmodelslist)) {
      # i <- 2
      message(paste0(
        topo,
        ", ",
        direction,
        ", Model ",
        i,
        " of ",
        length(loopmodelslist)
      ))
      model <- loopmodelslist[[i]]
      # Save full loo object for later comparison
      loo_result <- loo(model, k_threshold = 0.7, reloo = TRUE)
      loo_list[[topo]][[direction]][[i]] <- loo_result
      # Store the pointwise (per observation i.e. reef) ELPD contributions per model
      elpd_obs_list[[topo]][[direction]][[i]] <- data.frame(
        # remove any instance of 2 or more consecutive spaces and replace with a single space
        # happens when formulae are long and run to 2 lines, hence paste deparse
        model = gsub(
          " {2,}",
          " ",
          paste(deparse(model$formula$formula), collapse = "")
        ),
        reef = reefnames |> filter(topo2 == topo) |> pull(reef_name),
        elpd = loo_result$pointwise[, "elpd_loo"]
      )
      posteriors_list[[topo]][[direction]][[i]] <- posterior_epred(model) # 13 col 4000 row

      # bayesR2_list[[topo]][[direction]][[i]] <- bayes_R2(model) # doesn't penalise for more terms so a more complex model will have a higher R2
      bayesR2_list[[topo]][[direction]][[i]] <- brms::loo_R2(model)

      # for (loopmodel in 1:length(loopmodelslist)) {
      #   # loopmodel <- 5
      #   tmp <- loo(loopmodelslist[[loopmodel]], k_threshold = 0.7)
      resultsdf <- rbind(
        resultsdf,
        data.frame(
          topo = topo,
          direction = direction,
          # model = deparse1(loopmodelslist[[loopmodel]]$formula),
          # elpd_loo = tmp$estimates[1, 1],
          # elpd_loo_SE = tmp$estimates[1, 2],
          # p_loo = tmp$estimates[2, 1],
          # p_loo_SE = tmp$estimates[2, 2],
          # looic = tmp$estimates[3, 1],
          # looic_SE = tmp$estimates[3, 2]
          model = deparse1(model$formula),
          elpd_loo = loo_result$estimates["elpd_loo", "Estimate"],
          elpd_loo_SE = loo_result$estimates["elpd_loo", "SE"],
          p_loo = loo_result$estimates["p_loo", "Estimate"],
          p_loo_SE = loo_result$estimates["p_loo", "SE"],
          looic = loo_result$estimates["looic", "Estimate"],
          looic_SE = loo_result$estimates["looic", "SE"]
        )
      )
    } # for (i in seq_along(loopmodelslist))
    counter <- counter + 1
  } # for (direction in c("TopDown", "BottomUp"))
  elpd_loo_rank <- rank(
    resultsdf$elpd_loo,
    ties.method = "first"
  )
} # for (topo in c("Atolls","HighIslands"))

resultsdf <- resultsdf |> dplyr::mutate(model = rep(original_labels, times = 2))

# Save results, overwrite model names ####
readr::write_csv(
  resultsdf,
  here(
    "Results",
    "DAG",
    "loo_results.csv"
  )
)

# In csv, add summary table like I did manually in Excel.
resultsdf |>
  group_by(topo, direction) |>
  summarise(mean_elpd_loo = round(mean(elpd_loo), digits = 3)) |>
  write_csv(here(
    "Results",
    "DAG",
    "loo_results_summary.csv"
  ))

# Save posteriors list: "Compare posteriors" later.
posteriors_list |>
  saveRDS(
    here(
      "Results",
      "DAG",
      "posteriors_list.Rds"
    )
  )

# Save BayesR2 list
bayesR2_list |>
  saveRDS(
    here(
      "Results",
      "DAG",
      "bayesR2_list.Rds"
    )
  )

# Save BayesR2 table ####
bayesR2 <- data.frame(
  matrix(
    unlist(
      bayesR2_list
    ),
    nrow = 44,
    byrow = TRUE
  ),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    topo = rep(c("Atolls", "HighIslands"), each = 22),
    model = c(
      topdown_formulas,
      bottomup_formulas,
      topdown_formulas,
      bottomup_formulas
    ),
    direction = rep(c("TopDown", "BottomUp", "TopDown", "BottomUp"), each = 11),
    direction = factor(direction, levels = c("TopDown", "BottomUp")),
    trophic = rep(1:11, times = 4),
    r2correlation = case_when(
      X1 < 0.2 ~ "Very weak",
      between(X1, 0.2, 0.4) ~ "Weak",
      between(X1, 0.4, 0.7) ~ "Moderate",
      between(X1, 0.7, 0.9) ~ "Strong",
      between(X1, 0.9, 1) ~ "Very strong",
      .default = "Reference"
    ),
    posterior_interval = X4 - X3,
    post_int_width = case_when(
      posterior_interval < 0.2 ~ "Precise",
      between(posterior_interval, 0.2, 0.5) ~ "Moderate",
      posterior_interval > 0.5 ~ "Imprecise",
      .default = "Reference"
    ),
    post_int_width = factor(
      post_int_width,
      levels = c("Imprecise", "Moderate", "Precise")
    )
  ) |>
  dplyr::rename(
    "BayesR2" = "X1",
    "Est.Error" = "X2",
    "Q2.5" = "X3",
    "Q97.5" = "X4"
  ) |>
  dplyr::select(
    topo,
    direction,
    trophic,
    model,
    BayesR2,
    r2correlation,
    everything(),
    posterior_interval
  )
write_csv(bayesR2, here("Results", "DAG", "bayesR2.csv"))

bayesR2 |>
  group_by(topo, direction) |>
  summarise(
    BayesR2 = mean(BayesR2),
    posterior_interval = mean(posterior_interval)
  ) |>
  write_csv(here("Results", "DAG", "bayesR2summary.csv"))

## Loo Bayes R2 ggplot, direction facet ####
ggplot(
  bayesR2 |>
    # make model a factor, with levels in (reverse) order of formulas
    mutate(
      model = factor(
        model,
        levels = c(
          rev(topdown_formulas),
          rev(bottomup_formulas)
        )
      )
    ),
  aes(
    x = model,
    y = BayesR2,
    ymin = Q2.5,
    ymax = Q97.5,
    colour = topo, # instead of faceting by topo, colour by it
    linewidth = post_int_width
  )
) +
  geom_pointrange(position = position_dodge(width = 0.4)) + # avoid overlapping lines on Y
  coord_flip() +
  facet_wrap(~direction, nrow = 2, scales = "free_y") + # scales free removes missing categories
  # vertical lines for bayes r2 correlation strength
  geom_hline(
    yintercept = 0.4,
    linetype = "dotted",
    color = "black",
    linewidth = 0.5
  ) + # weak/moderate
  geom_hline(
    yintercept = 0.7,
    linetype = "dotted",
    color = "black",
    linewidth = 0.5
  ) + # moderate/strong
  geom_hline(
    yintercept = 0.9,
    linetype = "dotted",
    color = "black",
    linewidth = 0.5
  ) + # strong/very strong
  scale_colour_manual(
    values = c("Atolls" = "#1f77b4", "HighIslands" = "#ff7f0e")
  ) +
  scale_linewidth_discrete(range = c(0.4, 1.4)) +
  labs(
    y = "Bayesian R² (model explanatory power)",
    x = "Model (relationship)"
  ) + # labels flipped due to coord flip
  theme_minimal() %+replace%
  theme(
    panel.border = element_blank(), # remove plot border
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white"), # white background, hide border
    legend.position = c(0.95, 1),
    legend.justification = c("right", "top"),
    legend.spacing.y = unit(0.01, "cm"),
    legend.key.spacing = unit(0.01, "cm"),
    legend.margin = margin(unit(0.01, "cm")),
    legend.direction = "horizontal",
    legend.text.position = "right",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.text.x = element_text(size = 10)
  )
# Save plot
ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_BayesR2_facetDirection.png"
    )
  ),
  width = 8,
  height = 8
)


rep(
  bayesR2 |>
    # replace " on " with " - " in model column entries
    dplyr::mutate(model = gsub(" on ", " - ", model)) |>
    dplyr::filter(
      topo == "Atolls",
      direction == "TopDown"
    ) |>
    dplyr::select(model) |>
    dplyr::pull(),
  times = 4
)


bayesR2 |>
  # replace " on " with " - " in model column entries
  dplyr::mutate(model = gsub(" on ", " - ", model)) |>
  dplyr::filter(
    topo == "Atolls",
    direction == "TopDown"
  ) |>
  dplyr::select(model) |>
  dplyr::pull()




## Loo Bayes R2 ggplot, topo facet ####
ggplot(
  bayesR2 |>
    # make model a factor, with levels in paired order of formulas from high to low trophic level
    mutate(
      model = factor(
        rep(
          bayesR2 |>
            # replace " on " with " - " in model column entries
            dplyr::mutate(model = gsub(" on ", " - ", model)) |>
            dplyr::filter(
              topo == "Atolls",
              direction == "TopDown"
            ) |>
            dplyr::select(model) |>
            dplyr::pull(),
          times = 4
        ),
        levels = rev(bayesR2 |>
          # replace " on " with " - " in model column entries
          dplyr::mutate(model = gsub(" on ", " - ", model)) |>
          dplyr::filter(
            topo == "Atolls",
            direction == "TopDown"
          ) |>
          dplyr::select(model) |>
          dplyr::pull())
      )
    ),
  aes(
    x = model,
    y = BayesR2,
    ymin = Q2.5,
    ymax = Q97.5,
    colour = direction, # instead of faceting by topo, colour by it
    linewidth = post_int_width
  )
) +
  geom_pointrange(position = position_dodge(width = 0.4)) + # avoid overlapping lines on Y
  coord_flip() +
  facet_wrap(~topo, nrow = 2, scales = "free_y") + # scales free removes missing categories
  # vertical lines for bayes r2 correlation strength
  geom_hline(
    yintercept = 0.4,
    linetype = "dotted",
    color = "black",
    linewidth = 0.5
  ) + # weak/moderate
  geom_hline(
    yintercept = 0.7,
    linetype = "dotted",
    color = "black",
    linewidth = 0.5
  ) + # moderate/strong
  geom_hline(
    yintercept = 0.9,
    linetype = "dotted",
    color = "black",
    linewidth = 0.5
  ) + # strong/very strong
  scale_colour_manual(
    values = c("TopDown" = "#F8766D", "BottomUp" = "#1b9e77")
  ) +
  scale_linewidth_discrete(range = c(0.4, 1.4)) +
  labs(
    y = "Bayesian R² (model explanatory power)",
    x = "Model (relationship)"
  ) + # labels flipped due to coord flip
  theme_minimal() %+replace%
  theme(
    panel.border = element_blank(), # remove plot border
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white"), # white background, hide border
    legend.position = c(0.95, 1),
    legend.justification = c("right", "top"),
    legend.spacing.y = unit(0.01, "cm"),
    legend.key.spacing = unit(0.01, "cm"),
    legend.margin = margin(unit(0.01, "cm")),
    legend.direction = "horizontal",
    legend.text.position = "right",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.text.x = element_text(size = 10)
  )
# Save plot
ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_BayesR2_facetTopo.png"
    )
  ),
  width = 8,
  height = 8
)

## Boxplot of BayesR2 results ####
ggplot(
  bayesR2,
  aes(
    x = topo,
    y = BayesR2,
    ymin = Q2.5,
    ymax = Q97.5,
    colour = factor(direction, levels = c("TopDown", "BottomUp")),
    group = interaction(
      topo,
      factor(direction, levels = c("TopDown", "BottomUp"))
    )
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.75)
  ) + # avoid overlapping lines on Y
  # scale_colour_manual(values = c("Atolls" = "#1f77b4", "HighIslands" = "#ff7f0e")) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 7,
    # color = "black",
    # fill = "black",
    position = position_dodge(width = 0.75)
  ) +
  scale_colour_manual(
    values = c("TopDown" = "#F8766D", "BottomUp" = "#1b9e77")
  ) +
  labs(
    y = "Bayesian R² (model explanatory power)",
    x = "Island geomorphology",
  ) + # labels flipped due to coord flip
  theme_minimal() %+replace%
  theme(
    panel.border = element_blank(), # remove plot border
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white"), # white background, hide border
    legend.position = c(0.95, 1),
    legend.justification = c("right", "top"),
    legend.spacing.y = unit(0.01, "cm"),
    legend.key.spacing = unit(0.01, "cm"),
    legend.margin = margin(unit(0.01, "cm")),
    legend.direction = "horizontal",
    legend.text.position = "right",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
# Save plot
ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_BayesR2boxplots.png"
    )
  ),
  width = 8,
  height = 8
)


# Calculate posteriors per model/relationship ####
slope_summary_df <- data.frame()

for (topo in c("Atolls", "HighIslands")) {
  for (direction in c("TopDown", "BottomUp")) {
    message(paste("Extracting slope posteriors for", topo, direction))
    # Load models
    model_path <- here(
      "Results",
      "DAG",
      paste0("models_list_", topo, "_", direction, ".Rds")
    )
    models <- readRDS(model_path)

    for (i in seq_along(models)) {
      model <- models[[i]]
      # Get posterior summary for all fixed effects
      fe <- brms::fixef(model)
      # Extract the first predictor only (the causal one)
      predictor_name <- rownames(fe)[2] # [1] is Intercept

      slope_summary_df <- rbind(
        slope_summary_df,
        data.frame(
          topo = topo,
          direction = direction,
          model_index = i,
          predictor = predictor_name,
          slope_mean = fe[predictor_name, "Estimate"],
          slope_lower = fe[predictor_name, "Q2.5"],
          slope_upper = fe[predictor_name, "Q97.5"]
        )
      )
    }
  }
}

# rename slope mean lower upper to same terms as elsewhere

readr::write_csv(
  slope_summary_df,
  here("Results", "DAG", "model_slope_posteriors.csv")
)

slope_summary_df |>
  mutate(
    # creare relationship column from topdown_formulas repeated 4 times
    relationship = rep(c(topdown_formulas), times = 4),
    # replace " on " in the predictor column with " - "
    relationship = str_replace_all(relationship, " on ", " - "),
    # # make relationship a factor with reversed levels order
    relationship = factor(relationship, levels = unique(relationship)),
    # make direction a factor with levels in order
    direction = factor(direction, levels = c("TopDown", "BottomUp"))
  ) |>
  ggplot(
    aes(
      x = relationship,
      y = slope_mean,
      ymin = slope_lower,
      ymax = slope_upper,
      color = topo,
      shape = direction
    )
  ) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  scale_shape_manual(values = c("BottomUp" = 24, "TopDown" = 25)) +
  scale_colour_manual(values = c("Atolls" = "#1f77b4", "HighIslands" = "#ff7f0e")) +
  labs(
    x = "Relationship",
    y = "Standardized coefficient (mean ± 95% CI)",
    color = "Direction",
    shape = "Topography"
  ) +
  # theme_minimal(base_size = 12) +
  # theme(
  #   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)
  #       )
  theme_minimal() %+replace%
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25),
    panel.border = element_blank(), # remove plot border
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white"), # white background, hide border
    legend.position = c(0.95, 1),
    legend.justification = c("right", "top"),
    legend.spacing.y = unit(0.01, "cm"),
    legend.key.spacing = unit(0.01, "cm"),
    legend.margin = margin(unit(0.01, "cm")),
    legend.direction = "horizontal",
    legend.text.position = "right",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
# Save plot
ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_slope_posteriors.png"
    )
  ),
  width = 10,
  height = 10
)


# LOO-COMPARE ELPD####
# Compare TopDown vs BottomUp by summing ELPD over the 11 models

loo_compare_results <- list()

stacking_results <- data.frame(
  topo = character(),
  direction = character(),
  model_formula = character(),
  stacking_weight = numeric(),
  stringsAsFactors = FALSE
)

# Extract ELPD estimates
get_elpd <- function(x) x$estimates["elpd_loo", "Estimate"]

for (topo in c(
  # topo <- "Atolls"
  "Atolls",
  "HighIslands"
)) {
  topdown_loos <- loo_list[[topo]][["TopDown"]]
  topdown_scores <- sapply(topdown_loos, get_elpd)
  # rank(topdown_scores)
  bottomup_loos <- loo_list[[topo]][["BottomUp"]]
  bottomup_scores <- sapply(bottomup_loos, get_elpd)
  # rank(bottomup_scores)

  named_loos <- setNames(
    c(topdown_loos, bottomup_loos),
    original_labels
  )

  ordered_loos <- c(
    topdown_loos[order(topdown_scores, decreasing = TRUE)],
    bottomup_loos[order(bottomup_scores, decreasing = TRUE)]
  )
  names(ordered_loos) <- c(
    topdown_formulas[order(topdown_scores, decreasing = TRUE)],
    bottomup_formulas[order(bottomup_scores, decreasing = TRUE)]
  )

  comparison <- loo_compare(ordered_loos)
  # Step 4: Reorder comparison to match original formula order
  comparison <- comparison[original_labels, , drop = FALSE]

  print(comparison)
  # Save comparison
  loo_compare_results[[topo]] <- comparison

  # Combine all models in one list
  all_models <- c(topdown_loos, bottomup_loos)
  all_directions <- c(
    rep("TopDown", length(topdown_loos)),
    rep("BottomUp", length(bottomup_loos))
  )

  # Compute weights (pseudobma+ or stacking)
  weights <- loo_model_weights(all_models, method = "stacking")

  # Store results
  stacking_results <- rbind(
    stacking_results,
    data.frame(
      topo = topo,
      direction = all_directions,
      model_formula = original_labels,
      stacking_weight = weights,
      stringsAsFactors = FALSE
    ) # data frame
  ) # stacking results rbind
} # for topo loop

## loo-compare results, elpd & stacking ####
comp <- bind_rows(
  lapply(names(loo_compare_results), function(topo) {
    comp <- as.data.frame(loo_compare_results[[topo]])
    comp$model <- rownames(comp)
    comp$topo <- topo
    comp
  })
) |>
  dplyr::mutate(
    model = c(
      topdown_formulas,
      bottomup_formulas,
      topdown_formulas,
      bottomup_formulas
    ),
    direction = rep(c("TopDown", "BottomUp", "TopDown", "BottomUp"), each = 11),
    trophic = rep(1:11, times = 4)
  ) |>
  dplyr::select(topo, direction, trophic, model, everything()) |>
  bind_cols(
    stacking_results |>
      select(stacking_weight)
  ) |>
  dplyr::arrange(
    topo,
    desc(elpd_loo)
  ) |>
  dplyr::mutate(
    stack_weight_weak = case_when(
      stacking_weight < 0.05 ~ "Weak",
      .default = "Fine"
    ),
    # If elpd_diff > 2 × se_diff, consider support strong (Vehtari et al., 2017).
    # Between 1–2 × se_diff: moderate.
    # <1 × se_diff: weak or inconclusive.
    elpd_support = case_when(
      abs(elpd_diff) > 2 * se_diff ~ "Strong",
      abs(elpd_diff) > 1 * se_diff ~ "Moderate",
      abs(elpd_diff) < 1 * se_diff ~ "Weak",
      .default = "Reference"
    ),
    # data points powering each model, i.e. number of reefs per topo. Atolls 13 HI 11
    n = c(
      rep(ncol(posteriors_list$Atolls$TopDown[[1]]), 11), # 13
      rep(ncol(posteriors_list$Atolls$BottomUp[[1]]), 11), # 13, same
      rep(ncol(posteriors_list$HighIslands$TopDown[[1]]), 11), # 11
      rep(ncol(posteriors_list$HighIslands$BottomUp[[1]]), 11)
    ), # 11, same
    # Vehtari et al. 2017 “Large p_loo values may indicate overfitting or high model flexibility, especially when they approach the number of data points.”
    p_loo_overfit = case_when(
      p_loo > 0.8 * n ~ "OverfitRisk",
      p_loo < 0.8 * n ~ "Acceptable",
      TRUE ~ ""
    )
  ) |>
  dplyr::select(-n)
write_csv(comp, here("Results", "DAG", "loo_compare_results.csv"))


# compare elpd_loo (higher is better) for TopDown vs BottomUp (direction) models
# where stacking_weight > 0.05
loo_compare_results_summary <- comp |>
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
  select(-driver, -response) |>
  # rename driver and response to driver2 and response2
  rename(driver = driver2, response = response2) |>
  unite(col = model, sep = " - ", c(driver, response)) |>
  select(model, everything())

# Save summary
loo_compare_results_summary |>
  group_by(topo, model) |>
  # summarise: report which direction has the highest elpd_loo score
  summarise(
    best_direction = direction[which.max(elpd_loo)],
    best_elpd_loo = max(elpd_loo),
    .groups = "drop"
  ) |>
  write_csv(here(
    "Results",
    "DAG",
    "loo_compare_results_summary.csv"
  ))

# Save summary only where stacking weights are fine
loo_compare_results_summary |>
  filter(stack_weight_weak == "Fine") |>
  group_by(topo, model) |>
  summarise(
    best_direction = direction[which.max(elpd_loo)],
    best_elpd_loo = max(elpd_loo),
    .groups = "drop"
  ) |>
  write_csv(here(
    "Results",
    "DAG",
    "loo_compare_results_summary_stackweightsfine.csv"
  ))

# Which links reverse direction of support across topo
best_direction_per_topo <- loo_compare_results_summary |>
  select(model, topo, direction, elpd_loo) |>
  # pivot wider to create new columns from topo and direction
  pivot_wider(
    names_from = c(direction),
    values_from = elpd_loo,
    names_sep = "_"
  ) |>
  mutate(
    # create new column elpd_diff
    elpd_diff = TopDown - BottomUp,
    # create new column sign
    sign = ifelse(elpd_diff > 0, "TopDown", "BottomUp")
  ) |>
  select(topo, model, everything())
write_csv(
  best_direction_per_topo,
  here(
    "Results",
    "DAG",
    "best_direction_per_topo.csv"
  )
)

best_direction_per_topo |>
  select(model, topo, sign) |>
  pivot_wider(
    names_from = c(topo),
    values_from = sign
  ) |>
  # create new column to capture whether the sign is the same across topologies
  # labelling them as "BothTD" when both are TopDown, "BothBU" when both are BottomUp,
  # "BUTD" when the Atoll value is TopDown and HighIsland value is BottomUp, and
  # "TDBU" when the Atoll value is BottomUp and HighIsland value is TopDown
  mutate(
    sign = case_when(
      Atolls == "TopDown" & HighIslands == "TopDown" ~ "BothTD",
      Atolls == "BottomUp" & HighIslands == "BottomUp" ~ "BothBU",
      Atolls == "BottomUp" & HighIslands == "TopDown" ~ "BUTD",
      Atolls == "TopDown" & HighIslands == "BottomUp" ~ "TDBU"
    )
  ) |>
  write_csv(
    here(
      "Results",
      "DAG",
      "best_direction_per_topo_sign.csv"
    )
  )


stacking_results |>
  aggregate(stacking_weight ~ topo + direction, mean) |>
  write_csv(here(
    "Results",
    "DAG",
    "loo_model_weights_means.csv"
  ))


## Results per topo, direction, reef (observation), model ####
elpd_obs_df <- bind_rows(
  lapply(names(elpd_obs_list), function(topo) {
    lapply(names(elpd_obs_list[[topo]]), function(direction) {
      bind_rows(elpd_obs_list[[topo]][[direction]], .id = "model_index") |>
        mutate(topo = topo, direction = direction)
    }) |>
      bind_rows()
  }),
  .id = "topo_index"
) |>
  mutate(
    # Split into response and predictors
    response = str_trim(str_extract(model, "^[^~]+")),
    predictors = str_trim(str_extract(model, "(?<=~).*")),
    # Extract first predictor only (assumed causal)
    predictors = str_trim(str_split_fixed(predictors, "\\+", 2)[, 1]),
    # Apply cleaning steps to both columns
    across(
      .cols = c(response, predictors),
      .fns = ~ .x |>
        str_replace("biomass_g_per_m2_", "") |>
        str_to_lower() |>
        str_replace_all("[._]", " ") |>
        str_replace_all("cca", "crustose coraline algae") |>
        str_squish() |>
        (\(x)
        case_when(
          x %in% c("piscivore", "invertivore", "herbivore", "planktivore") ~
            paste0(x, "s"),
          TRUE ~ x
        ))(),
      .names = "{.col}"
    ),
    predictors = predictors |> str_to_sentence(),
    # Construct causal label
    model = paste(predictors, "on", response)
  ) |>
  select(topo, direction, reef, model, elpd, response, predictors) |>
  left_join(
    comp |>
      select(topo, direction, model, stacking_weight),
    by = c("topo", "direction", "model")
  ) |>
  mutate(
    # if direction == BottomUp, then make the driver the response and the response the driver
    # allows pivot_wider given same model name for TD & BU
    predictors = predictors |> str_to_lower(),
    predictors2 = ifelse(direction == "BottomUp", response, predictors),
    response2 = ifelse(direction == "BottomUp", predictors, response),
    predictors2 = predictors2 |> str_to_sentence(),
    model = paste(predictors2, "on", response2)
  ) |>
  select(topo, direction, reef, model, elpd, stacking_weight)
# Save elpd_obs_df
elpd_obs_df |>
  write_csv(here(
    "Results",
    "DAG",
    "elpd_sw_topo_direction_reef_model.csv"
  ))

# widen by direction
elpd_obs_df |>
  group_by(topo, direction, model) |>
  summarise(elpd = mean(elpd), stacking_weight = mean(stacking_weight)) |>
  pivot_wider(names_from = direction, values_from = c(elpd, stacking_weight)) |>
  mutate(
    elpd_diff = elpd_TopDown - elpd_BottomUp,
    stacking_weight_diff = stacking_weight_TopDown - stacking_weight_BottomUp,
    elpd_stronger = case_when(
      elpd_diff > 0 ~ "TopDown",
      elpd_diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    ),
    stacking_weight_stronger = case_when(
      stacking_weight_diff > 0 ~ "TopDown",
      stacking_weight_diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    )
  ) |>
  arrange(
    topo,
    factor(
      model,
      levels = c(
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
    )
  ) |>
  select(
    topo,
    model,
    elpd_TopDown,
    elpd_BottomUp,
    elpd_diff,
    elpd_stronger,
    stacking_weight_TopDown,
    stacking_weight_BottomUp,
    stacking_weight_diff,
    stacking_weight_stronger
  ) |>
  write_csv(here(
    "Results",
    "DAG",
    "elpd_sw_topo_model.csv"
  ))

# weight elpd by stacking weight & widen by direction
elpd_obs_df |>
  mutate(elpd_weighted = elpd * stacking_weight) |>
  group_by(topo, direction, model) |>
  summarise(elpd_weighted = mean(elpd_weighted)) |>
  pivot_wider(names_from = direction, values_from = elpd_weighted) |>
  mutate(
    elpd_diff = TopDown - BottomUp,
    elpd_stronger = case_when(
      elpd_diff > 0 ~ "TopDown",
      elpd_diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    )
  ) |>
  arrange(
    topo,
    factor(
      model,
      levels = c(
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
    )
  ) |>
  select(topo, model, TopDown, everything()) |>
  write_csv(here(
    "Results",
    "DAG",
    "elpd_sw_topo_model_weighted.csv"
  ))


# widen by direction
elpd_obs_wide <- elpd_obs_df |>
  pivot_wider(names_from = direction, values_from = c(elpd, stacking_weight)) |>
  mutate(
    elpd_diff = elpd_TopDown - elpd_BottomUp,
    stacking_weight_diff = stacking_weight_TopDown - stacking_weight_BottomUp,
    elpd_stronger = case_when(
      elpd_diff > 0 ~ "TopDown",
      elpd_diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    ),
    stacking_weight_stronger = case_when(
      stacking_weight_diff > 0 ~ "TopDown",
      stacking_weight_diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    ),
    sign = case_when(
      elpd_stronger == "TopDown" & stacking_weight_stronger == "TopDown" ~
        "BothTD",
      elpd_stronger == "BottomUp" & stacking_weight_stronger == "BottomUp" ~
        "BothBU",
      elpd_stronger == "BottomUp" & stacking_weight_stronger == "TopDown" ~
        "BUTD",
      elpd_stronger == "TopDown" & stacking_weight_stronger == "BottomUp" ~
        "TDBU"
    )
  )

elpd_obs_wide <- elpd_obs_wide |>
  group_by(topo, reef) |>
  summarise(across(elpd_TopDown:stacking_weight_BottomUp, mean)) |>
  mutate(
    elpd_diff = elpd_TopDown - elpd_BottomUp,
    stacking_weight_diff = stacking_weight_TopDown - stacking_weight_BottomUp,
    elpd_stronger = case_when(
      elpd_diff > 0 ~ "TopDown",
      elpd_diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    ),
    stacking_weight_stronger = case_when(
      stacking_weight_diff > 0 ~ "TopDown",
      stacking_weight_diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    ),
    sign = case_when(
      elpd_stronger == "TopDown" & stacking_weight_stronger == "TopDown" ~
        "BothTD",
      elpd_stronger == "BottomUp" & stacking_weight_stronger == "BottomUp" ~
        "BothBU",
      elpd_stronger == "BottomUp" & stacking_weight_stronger == "TopDown" ~
        "BUTD",
      elpd_stronger == "TopDown" & stacking_weight_stronger == "BottomUp" ~
        "TDBU"
    )
  )
write_csv(
  elpd_obs_wide,
  here(
    "Results",
    "DAG",
    "elpd_sw_topo_direction_reef.csv"
  )
)

# How to interpret loo_model_weights() (stacking method)
# The stacking weight for a model reflects how useful that model is in a weighted ensemble for out-of-sample prediction. That is:
# A weight of 1.0 means a model dominates the predictive performance.
# A weight of 0.5 shared between two models means both contribute roughly equally.
# A weight of 0 means that model contributes nothing to the ensemble's predictive accuracy.
# These weights are relative within the compared set (here: 22 models = 11 top-down + 11 bottom-up per topo).
# So a model with a higher stacking weight is more predictively useful given the others in the set.

## Plot stacking weights ####
# Ensure model order is preserved (or ordered by weight)
ggplot(
  comp |>
    mutate(
      stacking_weight = as.numeric(stacking_weight),
      direction = factor(direction, levels = c("TopDown", "BottomUp")),
      label = paste(direction, model, sep = " - ") # Unique label per row
    ),
  aes(
    x = direction,
    y = model,
    fill = stacking_weight
  )
) +
  geom_tile(color = "white") +
  facet_wrap(~topo) +
  scale_fill_viridis_c(name = "Stacking weight", option = "C") +
  labs(
    title = "Model Stacking Weights by Direction and Topology",
    x = "Direction",
    y = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )

ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_stacking_weights.png"
    )
  ),
  width = 10,
  height = 6
)


## Barplot of elpd_loo per model ####
ggplot(
  comp |>
    group_by(topo) |>
    mutate(model = fct_reorder(model, elpd_loo, .desc = TRUE)) |>
    ungroup(),
  aes(
    x = model,
    y = elpd_loo,
    fill = direction
  )
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~topo, scales = "free_x") +
  scale_fill_manual(values = c("TopDown" = "#F8766D", "BottomUp" = "#1b9e77")) +
  labs(
    title = "elpd_loo by Model and Direction",
    x = "Model",
    y = "elpd_loo",
    fill = "Direction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1
    ),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )
ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_elpd_loo_by_model_direction.png"
    )
  ),
  width = 10,
  height = 6
)


# POSTERIORS: Summarize using model-averaged predictions ####
# Initialize output list to store results
posterior_avg_list <- list()

# Loop over topologies and directions
for (topo in c("Atolls", "HighIslands")) {
  # names(posteriors_list)
  posterior_avg_list[[topo]] <- list()

  for (direction in c("TopDown", "BottomUp")) {
    model_posteriors <- posteriors_list[[topo]][[direction]]

    # Retrieve stacking weights for current topo/direction
    weights <- comp |>
      filter(topo == !!topo, direction == !!direction) |>
      arrange(match(model, names(model_posteriors))) |>
      pull(stacking_weight)

    print(dim(model_posteriors[[1]])) # Should be iterations x observations (e.g., 4000 x 13)
    print(length(model_posteriors)) # Should be number of models (e.g., 11)

    # Stack predictions into a 3D array: [model, iteration, observation]
    pred_array <- simplify2array(model_posteriors) # dims: iterations x obs x models

    print(dim(pred_array)) # Expecting: iterations x observations x models

    # Rearrange to models x iterations x obs
    pred_array <- aperm(pred_array, c(3, 1, 2))

    # Weighted average over models: collapse model dimension
    weighted_avg <- apply(pred_array, c(2, 3), function(x) sum(x * weights))
    # Result: iterations x observations

    # Summarize posterior predictions: mean and 95% CI per observation
    posterior_avg_mean <- apply(weighted_avg, 2, mean)
    posterior_avg_ci <- apply(
      weighted_avg,
      2,
      quantile,
      probs = c(0.025, 0.975)
    )

    # Store results
    posterior_avg_list[[topo]][[direction]] <- list(
      mean = posterior_avg_mean,
      ci_lower = posterior_avg_ci[1, ],
      ci_upper = posterior_avg_ci[2, ]
    )
  }
}

# Now compare the posterior predictions for TopDown vs BottomUp on the same topo
# Initialize
direction_summary <- list()

for (topo in names(posterior_avg_list)) {
  topdown_post <- posterior_avg_list[[topo]][["TopDown"]]
  bottomup_post <- posterior_avg_list[[topo]][["BottomUp"]]

  # Create comparison data frame
  df <- data.frame(
    # node = topdown_formulas, # names(topdown_post$mean)
    topdown_mean = topdown_post$mean,
    topdown_lower = topdown_post$ci_lower,
    topdown_upper = topdown_post$ci_upper,
    bottomup_mean = bottomup_post$mean,
    bottomup_lower = bottomup_post$ci_lower,
    bottomup_upper = bottomup_post$ci_upper
  )

  direction_summary[[topo]] <- df
}

# Start an empty data frame for final predictive summaries
final_preds <- data.frame()

# Loop through topologies and directions
for (topo in names(posterior_avg_list)) {
  for (direction in names(posterior_avg_list[[topo]])) {
    # Compute summary statistics per observation
    df <- data.frame(
      topo = topo,
      direction = direction,
      observation = seq_along(posterior_avg_list[[topo]][[direction]]$mean),
      pred_mean = posterior_avg_list[[topo]][[direction]]$mean,
      lower_ci = posterior_avg_list[[topo]][[direction]]$ci_lower,
      upper_ci = posterior_avg_list[[topo]][[direction]]$ci_upper
    )
    # Combine
    final_preds <- rbind(final_preds, df)
  }
}

# final_preds are observation-level i.e. per reef not per model.
# Add reef names & details
rawdata |>
  slice(
    # rearrange rows to match final_preds
    1:13,
    1:13,
    14:24,
    14:24
  ) |>
  select(reef_name, site_name, archi, isl_grp, Season) |>
  cbind(final_preds) |>
  select(-observation) -> final_preds


final_preds |>
  write_csv(here(
    "Results",
    "DAG",
    "posterior_predictions.csv"
  ))

# Strong predictive support only: credible intervals not overlapping zero
final_preds |>
  filter(lower_ci > 0 | upper_ci < 0) |>
  write_csv(here(
    "Results",
    "DAG",
    "posterior_predictions_credible.csv"
  ))


# which direction, on average, has stronger or more confident predictive effects?
final_preds |>
  group_by(topo, direction) |>
  summarise(
    mean_effect = mean(pred_mean),
    prop_strong = mean(lower_ci > 0 | upper_ci < 0)
  ) |>
  write_csv(here(
    "Results",
    "DAG",
    "posterior_predictions_summary.csv"
  ))
# topo        direction      mean_effect  prop_strong
# Atolls      BottomUp       0.129        1
# Atolls      TopDown        0.103        0.923
# HighIslands BottomUp       0.0203       0.636
# HighIslands TopDown        0.113        1

# Assess model support across directions
# for each site which direction leads to higher model-averaged predictions.
# large & consistent differences suggest one direction may better explain variation in the data.
prediction_diffs <- final_preds |>
  select(topo, direction, reef_name, pred_mean) |>
  pivot_wider(names_from = direction, values_from = pred_mean) |>
  mutate(
    diff = TopDown - BottomUp,
    stronger = case_when(
      diff > 0 ~ "TopDown",
      diff < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    )
  )
prediction_diffs |>
  write_csv(here(
    "Results",
    "DAG",
    "posterior_prediction_diffs.csv"
  ))

# join elpd & sw to posteriors
elpd_post_obs <- elpd_obs_wide |>
  left_join(
    prediction_diffs |>
      rename(
        reef = reef_name,
        posterior_TopDown = TopDown,
        posterior_BottomUp = BottomUp,
        posterior_diff = diff,
        posterior_stronger = stronger
      ),
    by = c("topo", "reef")
  )

elpd_post_obs |>
  write_csv(here(
    "Results",
    "DAG",
    "elpd_sw_topo_direction_reef_posterior.csv"
  ))

# weight ELPD by stacking weight
elpd_post_obs_weight <- elpd_post_obs |>
  mutate(
    elpd_TopDown_weighted = elpd_TopDown * stacking_weight_TopDown,
    elpd_BottomUp_weighted = elpd_BottomUp * stacking_weight_BottomUp,
    elpd_diff_weighted = elpd_TopDown_weighted * elpd_BottomUp_weighted,
    elpd_stronger_weighted = case_when(
      elpd_diff_weighted > 0 ~ "TopDown",
      elpd_diff_weighted < 0 ~ "BottomUp",
      TRUE ~ "Equal"
    ),
    sign = case_when(
      elpd_stronger_weighted == "TopDown" & posterior_stronger == "TopDown" ~
        "BothTD",
      elpd_stronger_weighted == "BottomUp" & posterior_stronger == "BottomUp" ~
        "BothBU",
      elpd_stronger_weighted == "BottomUp" & posterior_stronger == "TopDown" ~
        "BUTD",
      elpd_stronger_weighted == "TopDown" & posterior_stronger == "BottomUp" ~
        "TDBU"
    )
  ) |>
  select(
    topo,
    reef,
    elpd_TopDown_weighted,
    elpd_BottomUp_weighted,
    elpd_diff_weighted,
    elpd_stronger_weighted,
    posterior_TopDown:posterior_stronger,
    sign
  )

write_csv(
  elpd_post_obs_weight,
  here(
    "Results",
    "DAG",
    "elpd_sw_topo_direction_reef_posterior_weighted.csv"
  )
)


# threshold (for ELPD)
# This is a minimum difference in ELPD (TopDown - BottomUp) you consider meaningful at the reef level.
# Small differences in ELPDLOO (e.g., < 1) are often considered statistically negligible (within model noise). You want to emphasize only meaningful predictive superiority.
# Set threshold = 2 or 3 (units are elpd, like deviance)
# This is loosely inspired by the LOO model comparison rule-of-thumb:
# Δelpd < 2: negligible
# 2–4: moderate
# 4: strong
threshold <- 2


# small_threshold (for ambiguity)
# This is the same idea but used to classify reefs where ELPD differences are too small to matter.
# If abs(elpd_diff) < small_threshold, then call it "Ambiguous ELPD".
small_threshold <- 2
# So: If abs(elpd_diff) > 2, call it a meaningful difference.
# If abs(elpd_diff) < 2, be cautious or label ambiguous

elpd_post_obs_class <- elpd_post_obs |>
  left_join(
    final_preds |>
      select(topo, reef_name, direction:last_col()) |>
      rename(reef = reef_name) |>
      pivot_wider(
        names_from = direction,
        values_from = c(pred_mean, lower_ci, upper_ci)
      ) |>
      mutate(
        posterior_strong_TopDown = (lower_ci_TopDown > 0 |
          upper_ci_TopDown < 0),
        posterior_strong_BottomUp = (lower_ci_BottomUp > 0 |
          upper_ci_BottomUp < 0)
      )
  ) |>
  mutate(
    posterior_strong_flag = case_when(
      posterior_stronger == "TopDown" ~ posterior_strong_TopDown,
      posterior_stronger == "BottomUp" ~ posterior_strong_BottomUp,
      TRUE ~ FALSE # if Equal, call it not strong
    ),
    classification = case_when(
      posterior_stronger == elpd_stronger &
        posterior_strong_flag &
        abs(elpd_diff) > threshold ~
        paste(elpd_stronger, "Controlled"),
      posterior_stronger != elpd_stronger ~ "Mixed Evidence",
      !posterior_strong_flag ~ "Weak Posterior",
      abs(elpd_diff) < small_threshold ~ "Ambiguous ELPD",
      TRUE ~ "Uncertain"
    )
  )

elpd_post_obs_class |>
  write_csv(here(
    "Results",
    "DAG",
    "elpd_sw_topo_direction_reef_posterior_classification.csv"
  ))

elpd_post_obs_class |>
  mutate(
    classification = factor(
      classification,
      levels = c(
        "TopDown Controlled",
        "BottomUp Controlled",
        "Mixed Evidence",
        "Weak Posterior",
        "Ambiguous ELPD",
        "Uncertain"
      )
    )
  ) |>
  group_by(topo, classification) |>
  summarise(n = n()) |>
  write_csv(here(
    "Results",
    "DAG",
    "elpd_sw_topo_direction_reef_posterior_classification_summary.csv"
  ))


# which response variables (e.g., herbivores, planktivores) are most affected in each direction.
ggplot(
  final_preds,
  aes(
    x = reef_name,
    y = pred_mean,
    ymin = lower_ci,
    ymax = upper_ci
  )
) +
  geom_pointrange() +
  facet_grid(topo ~ direction, scales = "free") + # scales free removes missing categories
  # add a vertical line at 0 on the x axis for both facets
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_x_discrete(limits = rev) + # reverse order of site names
  labs(
    y = "Posterior mean of the model-averaged prediction (via stacking)",
    x = "<-- HighIslands        Reef Name        Atolls -->"
  ) + # labels flipped due to coord flip
  theme_minimal() %+replace%
  theme(
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )

# Save plot
ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_posterior_predictions.png"
    )
  ),
  width = 16,
  height = 8
)


rm(tmp, counter, topo, direction, loopmodelslist, loopmodel)


# Pointwise ELPD diff TD vs BU ####
# Create list to store all pointwise differences
elpd_pointwise_diff_list <- list()
elpd_pointwise_summary <- data.frame()

for (topo in c("Atolls", "HighIslands")) {
  topdown_loos <- loo_list[[topo]][["TopDown"]]
  bottomup_loos <- loo_list[[topo]][["BottomUp"]]

  # Check model count match
  if (length(topdown_loos) != length(bottomup_loos)) {
    warning(paste("Unequal model counts for", topo))
    next
  }

  for (i in seq_along(topdown_loos)) {
    td <- topdown_loos[[i]]
    bu <- bottomup_loos[[i]]

    elpd_diff <- td$pointwise[, "elpd_loo"] - bu$pointwise[, "elpd_loo"]

    elpd_pointwise_diff_list[[topo]][[i]] <- elpd_diff

    elpd_pointwise_summary <- rbind(
      elpd_pointwise_summary,
      data.frame(
        topo = topo,
        model_index = i,
        model_name = ifelse(
          topo == "Atolls",
          original_labels[i],
          original_labels[11 + i]
        ),
        mean_elpd_diff = mean(elpd_diff),
        sd_elpd_diff = sd(elpd_diff),
        proportion_TD_better = mean(elpd_diff > 0),
        proportion_BU_better = mean(elpd_diff < 0)
      )
    )
  }

  # Combine all ELPD diffs into one vector per topo
  all_diffs <- unlist(elpd_pointwise_diff_list[[topo]])
  reef_ids <- seq_along(all_diffs)

  # Violin plot
  violin_df <- data.frame(
    reef = reef_ids,
    elpd_diff = all_diffs,
    topo = topo
  )

  g <- ggplot(violin_df, aes(x = topo, y = elpd_diff)) +
    geom_violin(fill = "skyblue", alpha = 0.5) +
    geom_jitter(width = 0.1, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Pointwise ELPD Differences (TopDown - BottomUp):", topo),
      y = "ELPD Difference",
      x = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12),
      panel.grid = element_blank(),
      panel.border = element_blank(), # remove plot border
      plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
    )

  ggsave(
    filename = here(
      "Results",
      "DAG",
      paste0("violin_elpd_diff_", topo, ".png")
    ),
    plot = g,
    width = 6,
    height = 4
  )
}

# Save summary table
readr::write_csv(
  elpd_pointwise_summary,
  here("Results", "DAG", "elpd_pointwise_summary.csv")
)


# Plot all outputs in grid ####
# Pull plot outputs from all TD & BU scripts & compile as I did with powerpoint.
library(cowplot)

resultssum <- elpd_post_obs_weight |>
  group_by(topo) |>
  summarise(
    elpd_TopDown_weighted = mean(elpd_TopDown_weighted),
    elpd_BottomUp_weighted = mean(elpd_BottomUp_weighted),
  ) |>
  pivot_longer(
    cols = c(elpd_TopDown_weighted, elpd_BottomUp_weighted),
    names_to = "direction",
    values_to = "elpd_weighted"
  ) |>
  # tidy direction names, remove "_elpd" and "_weighted"
  mutate(
    direction = str_replace(direction, "elpd_", ""),
    direction = str_replace(direction, "_weighted", ""),
    elpd_weighted = round(elpd_weighted, 4)
  )

## 4 panel ####
TDhighislandplot <- readRDS(here("Results", "DAG", "TDhighislandplot.Rds"))
TDatollplot <- readRDS(here("Results", "DAG", "TDatollplot.Rds"))
BUhighislandplot <- readRDS(here("Results", "DAG", "BUhighislandplot.Rds"))
BUatollplot <- readRDS(here("Results", "DAG", "BUatollplot.Rds"))

plot_grid(
  TDhighislandplot,
  TDatollplot,
  BUhighislandplot,
  BUatollplot,
  nrow = 2,
  ncol = 2,
  rel_widths = c(1.33, 1), # first column wider for labels
  labels = c(
    paste0(
      "A: HI,TD:",
      resultssum |>
        filter(topo == "HighIslands", direction == "TopDown") |>
        pull(elpd_weighted)
    ),
    paste0(
      "B: Atoll,TD:",
      resultssum |>
        filter(topo == "Atolls", direction == "TopDown") |>
        pull(elpd_weighted)
    ),
    paste0(
      "C: HI,BU:",
      resultssum |>
        filter(topo == "HighIslands", direction == "BottomUp") |>
        pull(elpd_weighted)
    ),
    paste0(
      "D: Atoll,BU:",
      resultssum |>
        filter(topo == "Atolls", direction == "BottomUp") |>
        pull(elpd_weighted)
    )
  ),
  label_size = 12
)

ggsave(
  here(
    "Results",
    "DAG",
    paste0(
      today(),
      "_loo_plots_4panel.png"
    )
  ),
  width = 16,
  height = 8
)

# ALL BELOW HERE NOW DEFUNCT 2025-05-09 ####

# # Once both sets of models (AS-MS & TPS-SLS-RS) are run, compare them ####
# # Compare pairwise links across models
# TPSSLSRS <- read_csv(here("Results", "DAG", "loo_results_TPS-SLS-RS.csv")) |>
#   mutate(combo = "TPS-SLS-RS") |>
#   select(combo, topo, direction, model, elpd_loo)
#
# ASMS <- read_csv(here("Results", "DAG", "loo_results_AS-MS.csv")) |>
#   mutate(combo = "AS-MS") |>
#   select(combo, topo, direction, model, elpd_loo)
# # rbind both dfs
# alltests <- rbind(TPSSLSRS, ASMS) |>
#   rename(TDmodel = model) |>
#   # pivot wider from combo, topo, direction to elpd_loo
#   pivot_wider(names_from = c(combo, topo, direction), values_from = elpd_loo)
# alltests$BUmodel <- alltests$TDmodel[c(14:26, rep(NA, 13))]
# alltests$`AS-MS_All_BottomUp`[1:13] <- alltests$`AS-MS_All_BottomUp`[14:26]
# alltests$`AS-MS_HighIslands_BottomUp`[
#   1:13
# ] <- alltests$`AS-MS_HighIslands_BottomUp`[14:26]
# alltests$`AS-MS_Atolls_BottomUp`[1:13] <- alltests$`AS-MS_Atolls_BottomUp`[
#   14:26
# ]
# alltests$`TPS-SLS-RS_All_BottomUp`[1:13] <- alltests$`TPS-SLS-RS_All_BottomUp`[
#   14:26
# ]
# alltests$`TPS-SLS-RS_HighIslands_BottomUp`[
#   1:13
# ] <- alltests$`TPS-SLS-RS_HighIslands_BottomUp`[14:26]
# alltests$`TPS-SLS-RS_Atolls_BottomUp`[
#   1:13
# ] <- alltests$`TPS-SLS-RS_Atolls_BottomUp`[14:26]
# alltests <- alltests[-c(14:26), ]
#
#
# # Once both sets of models (SLS-RS-All & SLS-RS-NoMarquesas) are run, compare them ####
# # Compare pairwise links across models
# TPSSLSRS <- read_csv(here("Results", "DAG", "loo_results_SLS-RS.csv")) |>
#   mutate(combo = "All") |>
#   select(combo, topo, direction, model, elpd_loo)
#
# ASMS <- read_csv(here(
#   "Results",
#   "DAG",
#   "loo_results_SLS-RS.csv"
# )) |>
#   mutate(combo = "NoMarquesas") |>
#   select(combo, topo, direction, model, elpd_loo)
#
# # rbind both dfs
# alltests <- rbind(TPSSLSRS, ASMS) |>
#   rename(TDmodel = model) |>
#   # pivot wider from combo, topo, direction to elpd_loo
#   pivot_wider(names_from = c(combo, topo, direction), values_from = elpd_loo)
# alltests$BUmodel <- alltests$TDmodel[c(12:22, rep(NA, 11))]
# alltests$`All_Atolls_BottomUp`[1:11] <- alltests$`All_Atolls_BottomUp`[12:22]
# alltests$`All_HighIslands_BottomUp`[
#   1:11
# ] <- alltests$`All_HighIslands_BottomUp`[12:22]
# alltests$`All_All_BottomUp`[1:11] <- alltests$`All_All_BottomUp`[12:22]
# alltests$`NoMarquesas_Atolls_BottomUp`[
#   1:11
# ] <- alltests$`NoMarquesas_Atolls_BottomUp`[12:22]
# alltests$`NoMarquesas_HighIslands_BottomUp`[
#   1:11
# ] <- alltests$`NoMarquesas_HighIslands_BottomUp`[12:22]
# alltests$`NoMarquesas_All_BottomUp`[
#   1:11
# ] <- alltests$`NoMarquesas_All_BottomUp`[12:22]
# alltests <- alltests[-c(12:22), ]
#
#
# ## HighIslands BottomUp only ####
# HIBU <- alltests |>
#   select(
#     BUmodel,
#     `TPS-SLS-RS_HighIslands_BottomUp`,
#     `AS-MS_HighIslands_BottomUp`
#   ) |>
#   mutate(
#     diff = `TPS-SLS-RS_HighIslands_BottomUp` - `AS-MS_HighIslands_BottomUp`,
#     # round all numbers to 3 decimal places
#     across(where(is.numeric), \(x) round(x, digits = 3))
#   ) |>
#   # Rename columns to remove "_HighIslands_BottomUp"
#   rename(
#     `TPS-SLS-RS` = `TPS-SLS-RS_HighIslands_BottomUp`,
#     `AS-MS` = `AS-MS_HighIslands_BottomUp`
#   )
#
#
# ## All tests synthesis: TPS-SLS-RS vs AS-MS ####
# alltests <- alltests |>
#   mutate(
#     `TPS-SLS-RS_Atolls` = ifelse(
#       `TPS-SLS-RS_Atolls_TopDown` < `TPS-SLS-RS_Atolls_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `TPS-SLS-RS_HighIslands` = ifelse(
#       `TPS-SLS-RS_HighIslands_TopDown` < `TPS-SLS-RS_HighIslands_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `TPS-SLS-RS_All` = ifelse(
#       `TPS-SLS-RS_All_TopDown` < `TPS-SLS-RS_All_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `AS-MS_Atolls` = ifelse(
#       `AS-MS_Atolls_TopDown` < `AS-MS_Atolls_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `AS-MS_HighIslands` = ifelse(
#       `AS-MS_HighIslands_TopDown` < `AS-MS_HighIslands_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `AS-MS_All` = ifelse(
#       `AS-MS_All_TopDown` < `AS-MS_All_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     # if same, report "BothTD" or "BothBU" depending on values, else "Diff", unless contains NA in which case NA
#     Atolls = ifelse(
#       `TPS-SLS-RS_Atolls` == `AS-MS_Atolls`,
#       ifelse(`TPS-SLS-RS_Atolls` == "TD", "BothTD", "BothBU"),
#       "Diff"
#     ),
#     HighIslands = ifelse(
#       `TPS-SLS-RS_HighIslands` == `AS-MS_HighIslands`,
#       ifelse(`TPS-SLS-RS_HighIslands` == "TD", "BothTD", "BothBU"),
#       "Diff"
#     ),
#     All = ifelse(
#       `TPS-SLS-RS_All` == `AS-MS_All`,
#       ifelse(`TPS-SLS-RS_All` == "TD", "BothTD", "BothBU"),
#       "Diff"
#     ),
#     BothReefs = ifelse(
#       Atolls == HighIslands,
#       ifelse(Atolls == "BothTD", "BothTD", "BothBU"),
#       "Diff"
#     )
#   ) |>
#   select(TDmodel, BUmodel, Atolls, HighIslands, BothReefs, everything())
#
# write_csv(
#   alltests,
#   here("Results", "DAG", "loo_results_comparison_TPS-SLS-RS_AS-MS.csv")
# )
#
#
# ## All tests synthesis: All vs NoMarquesas ####
# alltests <- alltests |>
#   mutate(
#     `All_Atolls` = ifelse(
#       `All_Atolls_TopDown` < `All_Atolls_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `All_HighIslands` = ifelse(
#       `All_HighIslands_TopDown` < `All_HighIslands_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `All_All` = ifelse(
#       `All_All_TopDown` < `All_All_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `NoMarquesas_Atolls` = ifelse(
#       `NoMarquesas_Atolls_TopDown` < `NoMarquesas_Atolls_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `NoMarquesas_HighIslands` = ifelse(
#       `NoMarquesas_HighIslands_TopDown` < `NoMarquesas_HighIslands_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     `NoMarquesas_All` = ifelse(
#       `NoMarquesas_All_TopDown` < `NoMarquesas_All_BottomUp`,
#       "TD",
#       "BU"
#     ),
#     # if same, report "BothTD" or "BothBU" depending on values, else "Diff", unless contains NA in which case NA
#     Atolls = ifelse(
#       `All_Atolls` == `NoMarquesas_Atolls`,
#       ifelse(`All_Atolls` == "TD", "BothTD", "BothBU"),
#       "Diff"
#     ),
#     HighIslands = ifelse(
#       `All_HighIslands` == `NoMarquesas_HighIslands`,
#       ifelse(`All_HighIslands` == "TD", "BothTD", "BothBU"),
#       "Diff"
#     ),
#     All = ifelse(
#       `All_All` == `NoMarquesas_All`,
#       ifelse(`All_All` == "TD", "BothTD", "BothBU"),
#       "Diff"
#     ),
#     BothReefs = ifelse(
#       Atolls == HighIslands,
#       ifelse(Atolls == "BothTD", "BothTD", "BothBU"),
#       "Diff"
#     )
#   ) |>
#   select(TDmodel, BUmodel, everything()) # Atolls, HighIslands, BothReefs,
#
# write_csv(
#   alltests,
#   here("Results", "DAG", "loo_results_comparison_All.csv")
# )
#
# # Quick view:
# tmp <- alltests |> select(TDmodel, BUmodel, All_Atolls:BothReefs)
