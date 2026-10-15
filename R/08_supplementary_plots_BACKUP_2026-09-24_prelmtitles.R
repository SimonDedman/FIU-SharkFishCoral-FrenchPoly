# 08_supplementary_plots.R
# Supplementary-material figures, raw-data summaries, diagnostic
# checks, and quotable statistics that don't fit elsewhere in the
# pipeline.
#
# Author: Simon Dedman <simondedman@gmail.com>
# Created: 2025-03-20
# Updated: 2026-04 (renamed from 12_DAG-SCMs_RawData_Review.R; absorbed
#                   the predator-teleost diagnostic and the Other Algae
#                   independence check from 01_data_prep.R, plus the
#                   SST/reef-sharks deep-dive from the legacy
#                   05_nat_FPDAG_consistency_check.R)
#
# Purpose:
#   A single home for everything that supports the manuscript's
#   supplementary material without belonging to the main BRT or DAG
#   stages. Sections are independent: each can be run standalone after
#   01_data_prep.R has produced the reef-wide tables.
#
# Inputs:
#   - NFF_data/ch4_reef_wide_df2.RData
#   - NFF_data/wide.df1.teleosts.csv         (predator-teleost diagnostic)
#   - NFF_data/survey.wide.df2.RData         (predator-teleost diagnostic)
#   - NFF_data/BenthicSurveyDataSheets/Algae_breakdown_2025_07.xlsx
#                                            (Other Algae check)
#   - NFF_data/fixed_bethic_uvc_final_2023_02_26.csv (Other Algae check)
#   - NFF_data/ReefWideBRUVUVC.csv           (SST deep-dive)
#
# Sections (in execution order), with rationale and outputs:
#
#   Section 1: Column plots, raw data (L128+)
#        Rationale: per-reef bar plots of the BRT and DAG explanatory
#        variables, faceted by topology. Sanity-check that values look
#        right and to provide eyeball-comparable reef rankings.
#        Outputs: Results/ColumnPlots/<expvar>_per_reef.png
#
#   Section 2: Linear-model scatter plots, DAG variable pairs (L183+)
#        Rationale: pairwise lm(y ~ x) for each adjacent edge in the
#        proposed DAG. Helps interpret the DAG structure visually
#        before running the Bayesian SCMs.
#        Outputs: Results/LMplots_DAG/<y> ~ <x>.png
#
#   Section 3: Linear-model scatter plots, BRT-significant pairs (L250+)
#        Rationale: same as 2 but for variable pairs surfaced by the
#        BRT relative-influence rankings.
#        Outputs: Results/LMplots_BRT/<response>/<y> ~ <x>.png
#
#   Section 4: Focused diagnostic - chi_benthos_percent ~ maxn_shark (L309+)
#        Rationale: explicit check on the headline shark -> benthos
#        relationship, with reef-name labels for outliers.
#        Outputs: Results/Scatterplots/maxn_shark_vs_chi_benthos.png
#
#   Section 5: Mike's requested plots, 2025-08-15 (L350+)
#        Rationale: scatter / box panels Mike Heithaus requested for
#        an internal review meeting. Retained for transparency.
#        Outputs: Results/Scatterplots/<various>.png
#
#   Section 6: Scatterplots - maxn_shark vs piscivore biomass with
#        coral cover as third axis (L394+)
#        Rationale: visual cross-check of the canonical predator-prey
#        relationship.
#        Outputs: Results/Scatterplots/maxn_shark_vs_biomass_g_per_m2_Piscivore_coralcover.png
#
#   Section 7: Sharks and piscivores per reef (L578+)
#        Rationale: per-reef bar/column plots focused specifically on
#        shark MaxN and piscivore biomass.
#        Outputs: Results/ColumnPlots/<various>.png
#
#   Section 8: Non-plot stats to quote (L644+)
#        Rationale: numeric summaries (mean, median, IQR) cited in the
#        manuscript text or methods.
#        Outputs: console only
#
#   Section 9: UVC and BRUV results, per-reef means (L659+)
#        Rationale: produces the per-reef mean biomass table referenced
#        in the SM tables.
#        Outputs: NFF_data/Mean_biomass_g_per_m2_per_reef.csv
#
#  Section 10: Predator-teleost BRUV MaxN vs UVC piscivore biomass (L688+)
#        Rationale: data-quality diagnostic; BRUV MaxN of seven
#        predator-teleost families should correlate with UVC piscivore
#        biomass per reef. Originally part of 01_data_prep.R.
#        Outputs: NFF_data/scatter_pred_tel_plot1.png
#
#  Section 11: Other Algae proportions independence check (L750+)
#        Rationale: tests whether Other.Algae cover is independent of
#        Fleshy.Macroalgae and Turf.Algae components, using `propr`
#        compositional correlation. Result: rho ~ -0.36 between turf
#        and fleshy macroalgae (mildly anti-proportional, consistent
#        with ecological expectation that they squeeze each other out
#        at high cover).
#        Outputs: Results/Boxplots/<date>_benthic_proportions_heatmap.png
#
#  Section 12: SST / reef-sharks linear model (post-hoc, L863+)
#        Rationale: the d-separation test for full_BU in
#        04_DAG_consistency.R flags ave_temp -> reef_sharks as one of
#        the larger inconsistencies. This block fits the lm explicitly
#        as a sanity check (slope positive, p ~ 0.05).
#        Outputs: console summary, base-graphics diagnostic plots,
#        ggplot scatter (not currently saved)
#
# Packages:
#   here, tidyverse, ggplot2, ggpubr, viridis, propr, readxl, lubridate,
#   reshape2

library(here)
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
    ),
    log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
    log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
    log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
    log_piscivore = log1p(biomass_g_per_m2_Piscivore)
  ) |>
  arrange(topo2)

# Section 1: Column plots, raw data ####
dir.create(here(
  "Results",
  "ColumnPlots"
))

myvars <- c(
  "sicklefin_lemon_sharks",
  "biomass_g_per_m2_Piscivore",
  "biomass_g_per_m2_Herbivore",
  "biomass_g_per_m2_Invertivore",
  "biomass_g_per_m2_Planktivore",
  "pop.dens",
  "CCA",
  "Hard.Coral",
  "Other.Algae",
  "Relief",
  "reef_sharks"
)

# column plots of explanatory variables faceted by topo
for (whichvar in myvars) {
  ggplot(data = rawdata) +
    geom_col(mapping = aes(x = reef_name, y = .data[[whichvar]])) +
    # remove absent entries
    facet_wrap(
      vars(topo2),
      drop = TRUE,
      scales = "free_x"
    ) +
    labs(
      title = paste("Input data by topo and reef:", whichvar),
      y = whichvar,
      x = "Reef"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.grid = element_blank(),
      panel.border = element_blank(), # remove plot border
      plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
    )

  ggsave(
    filename = here(
      "Results",
      "ColumnPlots",
      paste0("colplot_", whichvar, ".png")
    ),
    width = 6,
    height = 4
  )
}

# Section 2: Linear-model scatter plots, DAG variable pairs ####
source("~/Dropbox/Galway/Analysis/R/MiscScripts/R/lmplot.R")
dir.create(here(
  "Results",
  "LMplots_DAG"
))

predictors <- c(
  "sicklefin_lemon_sharks",
  "sicklefin_lemon_sharks",
  "reef_sharks",
  "reef_sharks",
  "reef_sharks",
  "biomass_g_per_m2_Piscivore",
  "biomass_g_per_m2_Piscivore",
  "biomass_g_per_m2_Piscivore",
  "biomass_g_per_m2_Herbivore",
  "biomass_g_per_m2_Herbivore",
  "biomass_g_per_m2_Herbivore"
)

responses <- c(
  "reef_sharks",
  "biomass_g_per_m2_Piscivore",
  "biomass_g_per_m2_Planktivore",
  "biomass_g_per_m2_Invertivore",
  "biomass_g_per_m2_Herbivore",
  "biomass_g_per_m2_Planktivore",
  "biomass_g_per_m2_Invertivore",
  "biomass_g_per_m2_Herbivore",
  "Hard.Coral",
  "CCA",
  "Other.Algae"
)

predresp <- tibble(
  predictor = predictors,
  response = responses
)

# Single plot test
lmplot(
  x = rawdata[, predresp$predictor[1]],
  y = rawdata[, predresp$response[1]],
  xname = predresp$predictor[1],
  yname = predresp$response[1],
  plotname = paste0(predresp$response[1], " ~ ", predresp$predictor[1]),
  pointtext = TRUE,
  savedir = here("Results", "LMplots")
)

# Loop through predictors and responses
for (i in 1:nrow(predresp)) {
  Expvar <- predresp$predictor[i]
  Resvar <- predresp$response[i]

  lmplot(
    x = rawdata[, Expvar],
    y = rawdata[, Resvar],
    xname = Expvar,
    yname = Resvar,
    savedir = here("Results", "LMplots"),
    plotname = paste0(Expvar, " ~ ", Resvar)
  )
}


# Section 3: Linear-model scatter plots, BRT-significant pairs ####
source("~/Dropbox/Galway/Analysis/R/MiscScripts/R/lmplot.R")
dir.create(here(
  "Results",
  "LMplots_BRT"
))

responses <- c(
  "chi_benthos_percent",
  "log_Herbivore",
  "log_Invertivore",
  "log_Planktivore",
  "log_piscivore"
)

predictors <- c(
  "ave_temp",
  "ave_npp",
  "topo",
  "pop.dens",
  "isl_grp",
  "lagoon.size",
  "maxn_shark",
  "Relief",
  "log_Planktivore",
  "log_Herbivore",
  "log_Invertivore",
  "log_piscivore"
)

for (j in 1:length(responses)) {
  dir.create(here(
    "Results",
    "LMplots_BRT",
    responses[j]
  ))

  predresp <- tibble(
    predictor = predictors,
    response = responses[j]
  )

  # Loop through predictors and responses
  for (i in 1:nrow(predresp)) {
    Expvar <- predresp$predictor[i]
    Resvar <- predresp$response[i]

    lmplot(
      x = rawdata[, Expvar],
      y = rawdata[, Resvar],
      xname = Expvar,
      yname = Resvar,
      savedir = here("Results", "LMplots_BRT", responses[j]),
      plotname = paste0(Resvar, " ~ ", Expvar)
    )
  } # close for i predictors
} # close for j responses


# Section 4: Focused diagnostic - chi_benthos_percent ~ maxn_shark ####
# Fit model
x <- "maxn_shark"
y <- "chi_benthos_percent"
fit <- lm(rawdata[, y] ~ rawdata[, x], data = rawdata)
r2 <- summary(fit)$r.squared
pval <- summary(fit)$coefficients[2, 4]

# Build plot
ggplot(rawdata, aes(x = rawdata[, x], y = rawdata[, y])) +
  geom_point(aes(colour = topo2), size = 3) +
  labs(x = x, y = y) +
  theme_minimal(base_size = 18) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  ggtitle(paste0(
    x,
    " (x) vs ",
    y,
    " (y). ",
    "Rsquared: ",
    round(r2, 3),
    ", P: ",
    round(pval, 3)
  ))

# Save plot
ggsave(
  filename = paste0(
    "chi_benthos_percent",
    " ~ ",
    "maxn_shark",
    "topoColour.png"
  ),
  path = here("Results", "LMplots_BRT", "chi_benthos_percent"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 150,
  bg = "white"
)

# Section 5: Mike's requested plots (2025-08-15) ####
# median and first quartile value for predatory teleost biomass and shark max n on reefs
# Boxplot of BayesR2 results
maxn_shark
biomass_g_per_m2_Piscivore

# medians overall
median(rawdata$biomass_g_per_m2_Piscivore, na.rm = TRUE) # 62.10019
median(rawdata$maxn_shark, na.rm = TRUE) # 2.572874

# medians by geomorphology
rawdata %>%
  group_by(topo2) %>%
  summarise(
    median_maxn_shark = median(maxn_shark, na.rm = TRUE),
    median_biomass_g_per_m2_Piscivore = median(
      biomass_g_per_m2_Piscivore,
      na.rm = TRUE
    )
  )
# topo2       median_maxn_shark median_biomass_g_per_m2_Piscivore
# Atolls                   4.05                             204.
# HighIslands              1.44                              22.1

# first quartile value for predatory teleost biomass and shark max n on reefs
# overall
quantile(rawdata$biomass_g_per_m2_Piscivore, probs = 0.25, na.rm = TRUE) # 20.95141
quantile(rawdata$maxn_shark, probs = 0.25, na.rm = TRUE) # 1.573806

# by geomorphology
rawdata %>%
  group_by(topo2) %>%
  summarise(
    Q1_maxn_shark = quantile(maxn_shark, probs = 0.25, na.rm = TRUE),
    Q1_biomass_g_per_m2_Piscivore = quantile(
      biomass_g_per_m2_Piscivore,
      probs = 0.25,
      na.rm = TRUE
    )
  )
# topo2       Q1_maxn_shark Q1_biomass_g_per_m2_Piscivore
# Atolls               3.11                          72.3
# HighIslands          1.08                          17.8

# Section 6: Scatterplots - maxn_shark vs piscivore biomass with coral cover ####
## maxn_shark vs biomass_g_per_m2_Piscivore ####
# with dots coloured by coral cover
rawdata |>
  # create coralcover variable by adding Hard.Coral and CCA and soft.Coral
  mutate(coralcover = Hard.Coral + CCA + Soft.Coral) |>
  ggplot(
    aes(
      x = maxn_shark,
      y = biomass_g_per_m2_Piscivore,
      colour = coralcover
    )
  ) +
  geom_point(size = 3) +
  # add a vertical line at the mean value for maxn_shark
  geom_vline(
    xintercept = mean(rawdata$maxn_shark, na.rm = TRUE),
    linetype = "dashed",
    color = "black",
    size = 1
  ) +
  # add a horizontal line at the mean value for biomass_g_per_m2_Piscivore
  geom_hline(
    yintercept = mean(rawdata$biomass_g_per_m2_Piscivore, na.rm = TRUE),
    linetype = "dashed",
    color = "black",
    size = 1
  ) +
  labs(
    x = "MaxN Shark",
    y = "Biomass g/m2 Piscivore",
    title = "MaxN Shark vs Biomass g/m2 Piscivore"
  ) +
  scale_colour_gradient(low = "blue", high = "red") +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )
# Save scatterplot
ggsave(
  filename = "maxn_shark_vs_biomass_g_per_m2_Piscivore_coralcover.png",
  path = here("Results", "Scatterplots"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 150,
  bg = "white"
)

# create a factorial variable in rawdata which indicates whether each reef (row) has a higher or lower-than-average MaxN shark, and higher or lower-than-average biomass_g_per_m2_Piscivore
rawdata |>
  mutate(
    MaxN_shark_factor = ifelse(
      maxn_shark > mean(maxn_shark, na.rm = TRUE),
      "High",
      "Low"
    ),
    Biomass_Piscivore_factor = ifelse(
      biomass_g_per_m2_Piscivore >
        mean(biomass_g_per_m2_Piscivore, na.rm = TRUE),
      "High",
      "Low"
    ),
    # create a factorial variable which combines the two factors, called "above_means", with values "both", "shark", "piscivore", "neither"
    above_means = factor(
      case_when(
        MaxN_shark_factor == "High" & Biomass_Piscivore_factor == "High" ~
          "both",
        MaxN_shark_factor == "High" & Biomass_Piscivore_factor == "Low" ~
          "shark",
        MaxN_shark_factor == "Low" & Biomass_Piscivore_factor == "High" ~
          "piscivore",
        MaxN_shark_factor == "Low" & Biomass_Piscivore_factor == "Low" ~
          "neither"
      ),
      levels = c("both", "shark", "piscivore", "neither")
    )
  ) |>
  # make a column plot of the number of reefs in each category of above_means
  group_by(above_means) |>
  summarise(
    n = n()
  ) |>
  ggplot(aes(x = above_means, y = n, fill = above_means)) +
  geom_col() +
  labs(
    x = "Reefs above which means",
    y = "Number of Reefs",
    title = "Number of Reefs above means of shark maxN and piscivore biomass"
  ) +
  scale_fill_manual(
    values = c(
      "both" = "blue",
      "shark" = "red",
      "piscivore" = "green",
      "neither" = "grey"
    )
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )
# Save the plot
ggsave(
  filename = "above_means_plot.png",
  path = here("Results", "ColumnPlots"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 150,
  bg = "white"
)

# create a box plot of the above_means variable, with the y-axis being coralcover
rawdata |>
  # create coralcover variable by adding Hard.Coral and CCA and soft.Coral
  mutate(
    coralcover = Hard.Coral + CCA + Soft.Coral,
    MaxN_shark_factor = ifelse(
      maxn_shark > mean(maxn_shark, na.rm = TRUE),
      "High",
      "Low"
    ),
    Biomass_Piscivore_factor = ifelse(
      biomass_g_per_m2_Piscivore >
        mean(biomass_g_per_m2_Piscivore, na.rm = TRUE),
      "High",
      "Low"
    ),
    # create a factorial variable which combines the two factors, called "above_means", with values "both", "shark", "piscivore", "neither"
    above_means = case_when(
      MaxN_shark_factor == "High" & Biomass_Piscivore_factor == "High" ~ "Both",
      MaxN_shark_factor == "High" & Biomass_Piscivore_factor == "Low" ~ "shark",
      MaxN_shark_factor == "Low" & Biomass_Piscivore_factor == "High" ~
        "piscivore",
      MaxN_shark_factor == "Low" & Biomass_Piscivore_factor == "Low" ~ "Neither"
    )
  ) |>
  # remove shark and piscivore rows from MaxN_shark_factor
  filter(!above_means %in% c("shark", "piscivore")) |>
  mutate(above_means = factor(above_means, levels = c("Both", "Neither"))) |>
  # make a box plot of the above_means variable, with the y-axis being coralcover
  ggplot(aes(x = above_means, y = coralcover, fill = above_means)) +
  geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    position = position_dodge(width = 0.75)
  ) +
  labs(
    x = "Reefs above shark maxN & piscivore biomass means",
    y = "Coral Cover",
    title = "                          Coral Cover by Reefs > mean of shark maxN & piscivore biomass"
  ) +
  scale_fill_manual(values = c("Both" = "blue", "Neither" = "red")) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal(base_size = 18) +
  guides(fill = guide_legend(title = "Above which means?")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )
# Save the plot
ggsave(
  filename = "above_means_coralcover_boxplot.png",
  path = here("Results", "Boxplots"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 150,
  bg = "white"
)

# Section 7: Sharks and piscivores per reef ####
# plot maxn_shark and biomass_g_per_m2_Piscivore per reef
# order by topo (high barrier then near atoll then closed atoll then open atoll) and isl_grp
rawdata |>
  rename(geomorphology = topo) |>
  ggplot(aes(x = isl_grp, y = maxn_shark)) + # reorder(reef_name, topo, isl_grp)
  geom_col(aes(fill = geomorphology), position = "dodge") +
  # geom_col(aes(y = biomass_g_per_m2_Piscivore), position = "dodge", alpha = 0.5) +
  labs(
    x = "Reef",
    y = "Shark MaxN",
    title = "Shark MaxN per Reef"
  ) +
  # scale_fill_manual(values = c("Atolls" = "blue", "HighIslands" = "red")) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )
# Save the plot
ggsave(
  filename = "shark_maxn_per_reef.png",
  path = here("Results", "ColumnPlots"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 150,
  bg = "white"
)


rawdata |>
  rename(geomorphology = topo) |>
  ggplot(aes(x = isl_grp, y = biomass_g_per_m2_Piscivore)) + # reorder(reef_name, topo, isl_grp)
  geom_col(aes(fill = geomorphology), position = "dodge") +
  labs(
    x = "Reef",
    y = "Piscivore Biomass (g/m2)",
    title = "Piscivore Biomass per Reef"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_blank(), # remove plot border
    plot.background = element_rect(fill = "white", colour = "white") # white background, hide border
  )
# Save t
# Save the plot
ggsave(
  filename = "Piscivore_biomass_per_reef.png",
  path = here("Results", "ColumnPlots"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 150,
  bg = "white"
)


# Section 8: Non-plot stats to quote ####
# mean and sd of teleost groups
rawdata |>
  summarise(
    mean_log_piscivore = mean(biomass_g_per_m2_Piscivore, na.rm = TRUE),
    sd_log_piscivore = sd(biomass_g_per_m2_Piscivore, na.rm = TRUE),
    mean_log_herbivore = mean(biomass_g_per_m2_Herbivore, na.rm = TRUE),
    sd_log_herbivore = sd(biomass_g_per_m2_Herbivore, na.rm = TRUE),
    mean_log_invertivore = mean(biomass_g_per_m2_Invertivore, na.rm = TRUE),
    sd_log_invertivore = sd(biomass_g_per_m2_Invertivore, na.rm = TRUE),
    mean_log_planktivore = mean(biomass_g_per_m2_Planktivore, na.rm = TRUE),
    sd_log_planktivore = sd(biomass_g_per_m2_Planktivore, na.rm = TRUE)
  )


# Section 9: UVC and BRUV results, per-reef means ####
# mean biomass g/m2 with SD for each teleost group
rawdata |>
  summarise(
    mean_piscivore = mean(biomass_g_per_m2_Piscivore, na.rm = TRUE),
    sd_piscivore = sd(biomass_g_per_m2_Piscivore, na.rm = TRUE),
    mean_herbivore = mean(biomass_g_per_m2_Herbivore, na.rm = TRUE),
    sd_herbivore = sd(biomass_g_per_m2_Herbivore, na.rm = TRUE),
    mean_invertivore = mean(biomass_g_per_m2_Invertivore, na.rm = TRUE),
    sd_invertivore = sd(biomass_g_per_m2_Invertivore, na.rm = TRUE),
    mean_planktivore = mean(biomass_g_per_m2_Planktivore, na.rm = TRUE),
    sd_planktivore = sd(biomass_g_per_m2_Planktivore, na.rm = TRUE)
  )
# mean_piscivore sd_piscivore mean_herbivore sd_herbivore mean_invertivore sd_invertivore mean_planktivore sd_planktivore
#       131.4863     178.1659       217.7118     205.0503         34.43839       17.88578         73.47647       207.8293

# mean biomass g/m2 per reef_name for each teleost group
rawdata |>
  group_by(reef_name) |>
  summarise(
    mean_piscivore = mean(biomass_g_per_m2_Piscivore, na.rm = TRUE),
    mean_herbivore = mean(biomass_g_per_m2_Herbivore, na.rm = TRUE),
    mean_invertivore = mean(biomass_g_per_m2_Invertivore, na.rm = TRUE),
    mean_planktivore = mean(biomass_g_per_m2_Planktivore, na.rm = TRUE)
  ) |>
  arrange(reef_name) |>
  write_csv(here("NFF_data", "Mean_biomass_g_per_m2_per_reef.csv"))


# Section 10: Predator-teleost BRUV MaxN vs UVC piscivore biomass ####
# Originally part of 01_data_prep.R (section "Compare Bruvs Pred. Teleost
# BRUVS vs UVC"). Moved here as a data-quality cross-check rather than a
# step in the main pipeline. Produces NFF_data/scatter_pred_tel_plot1.png.

teleost.bruv.raw <- read.csv(
  here("NFF_data", "wide.df1.teleosts.csv"),
  header = TRUE,
  as.is = TRUE
) |>
  mutate(across(
    .cols = c(geo, isl_grp, archi, Season, bait, topo),
    .fns = ~ factor(.x)
  ))

survey.wide.df2 <- readRDS(here("NFF_data", "survey.wide.df2.RData"))

compare.tel.df1 <- teleost.bruv.raw |>
  dplyr::select(
    -c(lutjanidae_maxN:lethrinidae_maxN_a, carangidae_maxN_b:total_maxN_b)
  ) |>
  dplyr::rename(
    teleost_maxn = total_maxN_a,
    site_name = site,
    reef_name = reef
  ) |>
  group_by(site_name, reef_name) |>
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), \(x) first(x)),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  merge(
    survey.wide.df2 |>
      dplyr::select(reef_name, biomass_g_per_m2_Piscivore) |>
      group_by(reef_name) |>
      summarise(
        biomass_g_per_m2_Piscivore = mean(
          biomass_g_per_m2_Piscivore,
          na.rm = TRUE
        )
      ),
    by = c("reef_name")
  ) |>
  filter(site_name != c("Nuka Hiva", "Uapou"))

scatter_pred_tel_plot1 <- ggplot(
  compare.tel.df1,
  aes(x = teleost_maxn, y = biomass_g_per_m2_Piscivore)
) +
  geom_point(size = 6, shape = 21, aes(fill = reef_name), colour = "black") +
  viridis::scale_fill_viridis(option = "turbo", discrete = TRUE, name = "Site") +
  ggpubr::theme_pubr(base_size = 14) +
  xlab("Pred. Teleost MaxN BRUVS") +
  ylab("Pred. Teleost Biomass (g/m2) UVC") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

ggsave(
  filename = here("NFF_data", "scatter_pred_tel_plot1.png"),
  plot = scatter_pred_tel_plot1
)


# Section 11: Other Algae proportions independence check (2025-08-26) ####
# Originally part of 01_data_prep.R. Tests whether Other.Algae proportions
# are independent of their component categories (Fleshy Macroalgae + Turf
# Algae) by computing the difference and using `propr` for compositional
# correlation analysis (rho metric, CLR-transformed). Result: turf and
# fleshy macroalgae are mildly anti-proportional (rho ~ -0.36), consistent
# with the ecological expectation that they squeeze each other out at high
# cover. Produces Results/Boxplots/<date>_benthic_proportions_heatmap.png.

library(propr)  # installed via renv (was previously installed in-script via devtools::install_github("tpq/propr"))

otheralgae <- readxl::read_excel(
  here::here(
    "NFF_data",
    "BenthicSurveyDataSheets",
    "Algae_breakdown_2025_07.xlsx"
  ),
  sheet = "Sheet1"
) |>
  dplyr::rename(
    Other.Algae = `Other Algae`,
    Fleshy.Macroalgae = `Fleshy Macroalgae`,
    Turf.Algae = Turf
  ) |>
  dplyr::filter(is.na(FilterOut)) |>
  dplyr::select(UniqueID, Fleshy.Macroalgae, Turf.Algae)

benthos <- readr::read_csv(here::here(
  "NFF_data",
  "fixed_bethic_uvc_final_2023_02_26.csv"
)) |>
  dplyr::left_join(otheralgae, by = "UniqueID") |>
  dplyr::mutate(
    OAdiff = Other.Algae - (Fleshy.Macroalgae + Turf.Algae),
    AllBenthos = Sand +
      Rubble +
      Pavement +
      CCA +
      Hard.Coral +
      Soft.Coral +
      Invert +
      Fleshy.Macroalgae +
      Turf.Algae
  ) |>
  tidyr::drop_na() |>
  dplyr::filter(UniqueID != "NUK2_4") |> # FM & Turf don't add to OA total
  dplyr::select(
    Sand,
    Rubble,
    Pavement,
    CCA,
    Fleshy.Macroalgae,
    Turf.Algae,
    Hard.Coral,
    Soft.Coral,
    Invert
  )

pr <- propr(
  counts = benthos,
  metric = "rho",
  ivar = "clr",
  alpha = NA,
  p = 100
)
getResults(pr)
# Turf-fleshy: rho ~ -0.36 (mildly anti-proportional, consistent with
# ecological expectation that they squeeze each other out at high cover).

heatmap_df <- reshape2::melt(
  pr@matrix,
  varnames = c("Var1", "Var2"),
  value.name = "propr"
)
# Keep only lower triangle (unique pairs, no diagonals)
heatmap_df <- subset(
  heatmap_df,
  as.numeric(factor(Var1, levels = colnames(pr@matrix))) >
    as.numeric(factor(Var2, levels = colnames(pr@matrix)))
)

ggplot2::ggplot(heatmap_df, ggplot2::aes(Var1, Var2, fill = propr)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = round(propr, 2)), size = 3) +
  ggplot2::scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.background = ggplot2::element_rect(fill = "white", colour = "grey50"),
    plot.background = ggplot2::element_rect(fill = "white", colour = "grey50"),
    panel.border = ggplot2::element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1
    )
  ) +
  ggplot2::labs(x = "", y = "", fill = "Propr (rho)")

ggplot2::ggsave(
  filename = paste0(lubridate::today(), "_benthic_proportions_heatmap.png"),
  device = "png",
  path = here::here("Results", "Boxplots"),
  width = 8,
  height = 8,
  units = "in"
)


# Section 12: SST / reef-sharks linear model (post-hoc, 2024-09-03) ####
# The d-separation test for full_BU in 04_DAG_consistency.R flags
# ave_temp ~ reef_sharks as one of the larger inconsistencies. This
# block fits an explicit lm to that pair as a sanity check on the
# strength and sign of the relationship. The regression is borderline
# significant (p ~ 0.05) with positive slope, suggesting reef-shark
# abundance may scale weakly with sea surface temperature; this is
# noted in the manuscript supplementary methods rather than treated
# as a primary finding. Originally part of legacy script
# 05_nat_FPDAG_consistency_check.R, now relocated here.

dat_sst <- read.csv(here::here("NFF_data", "ReefWideBRUVUVC.csv"))

sst.lm <- lm(formula = reef_sharks ~ ave_temp, data = dat_sst)
summary(sst.lm)
# Reported in 2024-09-03 run:
#                Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.1395     0.1206   1.157   0.2579
# ave_temp      0.3481     0.1721   2.023   0.0535 .
# Residual standard error: 0.2523 on 26 degrees of freedom
# Multiple R-squared: 0.136, Adjusted R-squared: 0.1028
# F-statistic: 4.093 on 1 and 26 DF, p-value: 0.05345

# Diagnostic plots
plot(sst.lm)

# Scatter with regression line
ggplot2::ggplot(
  dat_sst,
  ggplot2::aes(x = ave_temp, y = reef_sharks)
) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm")
