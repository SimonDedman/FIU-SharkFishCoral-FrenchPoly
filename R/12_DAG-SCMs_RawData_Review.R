# loo (leave one out) expected log pointwise predictive density metrics
# For SCMs with 2 topographies and 2 directions from scripts 10_Suchinta-DAG.R & 11_Si-DAG-bottomup.R
# Simon Dedman, 2025-03-20 simondedman@gmail.com

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

# Column plots raw data ####
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

# Linear model plots DAG variables ####
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


# Linear model plots BRT variables ####
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


# Specifically chi benthos ~ maxn shark ####
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

# 2025-08-15 Mike requests ####
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

# Scatterplots ####
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

## sharks & piscivores per reef ####
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


# Non-plot stats to quote ####
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


# 3.1 UVC results & BRUV results ####
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
