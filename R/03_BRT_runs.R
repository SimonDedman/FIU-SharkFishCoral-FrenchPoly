# TODO
# fix gbm.auto issue
# comment out combos sections, leave preset hyperparams uncommented
# delete both brt_.R scripts, redundant


library(tidyverse)
# library(MASS)
# library(vegan)
# library(reshape)
# library(doBy)
# library(utils)
# library(RcmdrMisc)
# library(ResourceSelection)
# library(boot)
# library(ggplot2)
# library(gplots)
# library(rstatix)
# library(dismo)
# library(ggBRT) # not available for R 4.4.1
# library(ape)
# library(gbm) # Loaded gbm 2.2.2: This version of gbm is no longer under development. Consider transitioning to gbm3, https://github.com/gbm-developers/gbm3
remotes::install_github("SimonDedman/gbm.auto")
library(gbm.auto)
library(here)
#library(remotes)
#remotes::install_github("SimonDedman/gbm.auto", force = TRUE)

# import dataframes ####
island.brt.df1 <- readRDS(here("NFF_data", "ch4_reef_wide_df2.RData")) %>%  # high islands
  mutate(log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
         log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
         log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
         log_piscivore = log1p(biomass_g_per_m2_Piscivore))

atoll.brt.df1 <- readRDS(here("NFF_data", "ch4_atoll_reef_wide_df2.RData")) %>%  # atolls
  mutate(log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
         log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
         log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
         log_piscivore = log1p(biomass_g_per_m2_Piscivore))

# Coral Health Index Benthos % ####
## Common expvars for BRTs ####
expvars <- c("ave_temp",
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
             "log_piscivore")

## Islands ####
### histogram ####
hist(island.brt.df1$chi_benthos_percent)

### gbm.auto combos ####

# Error in gbm.fit(x = x, y = y, offset = offset, distribution = distribution,: The data set is too small or the subsampling rate is too large: `nTrain * bag.fraction <= 2 * n.minobsinnode + 1`

gbm.auto(
  samples = island.brt.df1,
  expvar = expvars,
  resvar = "chi_benthos_percent",
  tc = c(
    # 1,
    # 2,
    # 3,
    # 4,
    5
  ), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(
    0.00001 #,
    # 0.001,
    # 0.005,
    # 0.01
    ),
  bf = c(
    # 0.5,
    # 0.65,
    # 0.7,
    # 0.75
    0.9
  ),
  ZI = FALSE, # forces fam2 only, gaussian only run
  # fam1 = "gaussian",
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png" #,
  # gaus = FALSE
  )

### preset hyperparameters ####
gbm.auto(
  samples = island.brt.df1, expvar = expvars, resvar = "chi_benthos_percent",
  tc = 5, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.01,
  bf = 0.65,
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")



## Atolls ####
### histogram ####
hist(atoll.brt.df1$chi_benthos_percent)

### bf check ####
gbm.bfcheck(samples = atoll.brt.df1,
            resvar = "chi_benthos_percent")

### gbm.auto combos ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars, resvar = "chi_benthos_percent",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

### preset hyperparameters ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars, resvar = "chi_benthos_percent",
  tc = 5, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.01,
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")




# Herbivore Biomass ####
## Common expvars for BRTs ####
expvars <- c("ave_temp",
             "ave_npp",
             "topo",
             "pop.dens",
             "isl_grp",
             "lagoon.size",
             "maxn_shark",
             "Relief",
             "chi_benthos_percent",
             "log_piscivore")


## Islands ####
### hist ####
hist(island.brt.df1$log_Herbivore)

### gbm.auto param combos ####
gbm.auto(
  samples = island.brt.df1, expvar = expvars,
  resvar = "log_Herbivore",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

### preset hyperparameters ####
gbm.auto(
  samples = island.brt.df1, expvar = expvars,
  resvar = "log_Herbivore",
  tc = 5, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.01,
  bf = 0.65,
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

## Atolls ####
### hist ####
hist(atoll.brt.df1$log_prey)

### gbm.auto param combos ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars,
  resvar = "log_Herbivore",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

### preset hyperparameters ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars,
  resvar = "log_Herbivore",
  tc = 4, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.001,
  bf = 0.7,
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")




# Invertivore Biomass ####
## Islands ####
### hist ####
hist(island.brt.df1$log_Invertivore)

### gbm.auto param combos ####
gbm.auto(
  samples = island.brt.df1, expvar = expvars,
  resvar = "log_Invertivore",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

### preset hyperparameters ####
gbm.auto(
  samples = island.brt.df1, expvar = expvars,
  resvar = "log_Invertivore",
  tc = 5, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.01,
  bf = 0.65,
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

## Atolls ####
### hist ####
hist(atoll.brt.df1$log_prey)

### gbm.auto param combos ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars,
  resvar = "log_Invertivore",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

### preset hyperparameters ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars,
  resvar = "log_Invertivore",
  tc = 4, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.001,
  bf = 0.7,
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")




# Planktivore Biomass ####
## Islands ####
### hist ####
hist(island.brt.df1$log_Planktivore)

### gbm.auto param combos ####
gbm.auto(
  samples = island.brt.df1, expvar = expvars,
  resvar = "log_Planktivore",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

### preset hyperparameters ####
gbm.auto(
  samples = island.brt.df1, expvar = expvars,
  resvar = "log_Planktivore",
  tc = 5, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.01,
  bf = 0.65,
  smooth = TRUE, savedir = here("Results", "BRT", "Islands"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

## Atolls ####
### hist ####
hist(atoll.brt.df1$log_prey)

### gbm.auto param combos ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars,
  resvar = "log_Planktivore",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")

### preset hyperparameters ####
gbm.auto(
  samples = atoll.brt.df1, expvar = expvars,
  resvar = "log_Planktivore",
  tc = 4, # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = 0.001,
  bf = 0.7,
  smooth = TRUE, savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE, alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png")
