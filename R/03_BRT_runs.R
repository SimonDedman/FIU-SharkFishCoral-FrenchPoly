# TODO
# replace savedir with here()
# set pngtype by OS?!
# comment out combos sections, leave preset hyperparams uncommented
# 1 BRT per prey class: herb invert plank
# Collapse param lines of gbm.auto where common, remove c() from single values
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
library(gbm.auto)
library(here)
#library(remotes)
#remotes::install_github("SimonDedman/gbm.auto", force = TRUE)

# import dataframes ####
island.data.df1 <- readRDS(here("NFF_data", "ch4_reef_wide_df2.RData")) %>%  # high islands
  mutate(log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
         log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
         log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
         log_piscivore = log1p(biomass_g_per_m2_Piscivore))

atoll.data.df1 <- readRDS(here("NFF_data", "ch4_atoll_reef_wide_df2.RData")) %>%  # atolls
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
gbm.auto(
  samples = island.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "chi_benthos_percent",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"))

### preset hyperparameters ####
gbm.auto(
  samples = island.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "chi_benthos_percent",
  tc = c(5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.01),
  bf = c(0.65),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"))




## Atolls ####
### histogram ####
hist(atoll.brt.df1$chi_benthos_percent)

### bf check ####
gbm.bfcheck(samples = atoll.brt.df1,
            resvar = "chi_benthos_percent")

### gbm.auto combos ####
gbm.auto(
  samples = atoll.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "chi_benthos_percent",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE, # change to true for final runs
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"))

### preset hyperparameters ####
gbm.auto(
  samples = atoll.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "chi_benthos_percent",
  tc = c(5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.01),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz")) # quartz for mac  this one for windows : "cairo-png"




# FROMHERE ####
# Need to do 1 per prey class: herb invert plank
# log_prey_tel <- c("log_Herbivore", "log_Invertivore", "log_Planktivore", "log_piscivore")

# Prey Fish Biomass ####
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
hist(island.brt.df1$log_prey)

### gbm.auto param combos ####
gbm.auto(
  samples = island.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "log_prey_tel",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"))

### preset hyperparameters ####
gbm.auto(
  samples = island.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "log_prey_tel",
  tc = c(5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.01),
  bf = c(0.65),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"))


## Atolls ####
### hist ####
hist(atoll.brt.df1$log_prey)

### gbm.auto param combos ####
gbm.auto(
  samples = atoll.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "log_prey_tel",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"))

### preset hyperparameters ####
gbm.auto(
  samples = atoll.brt.df1,
  expvar = expvars, # fix to final variables
  resvar = "log_prey_tel",
  tc = c(4), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.001),
  bf = c(0.7),
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = FALSE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"))
