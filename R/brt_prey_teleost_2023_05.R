# setwd("~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT") ## Change to appropriate working directory ##
library(tidyverse)
library(MASS)
library(vegan)
library(reshape)
library(doBy)
library(utils)
library(RcmdrMisc)
library(ResourceSelection)
library(boot)
library(ggplot2)
library(gplots)
library(rstatix)
library(dismo)
# library(ggBRT) # not available for R 4.4.1
library(ape)
library(gbm)
library(gbm.auto)
library(here)
#library(remotes)
#remotes::install_github("SimonDedman/gbm.auto", force = TRUE)

# import dataframes ####
island.data.df1 <- readRDS(here("NFF_data", "ch4_reef_wide_df2.RData")) %>%  # high islands
  mutate(log_prey_tel = log1p(prey_fish_biomass_g_per_m2)) %>%
  mutate(log_piscivore = log1p(piscivore_biomass_g_per_m2))
atoll.data.df1 <- readRDS(here("NFF_data", "ch4_atoll_reef_wide_df2.RData")) %>%  # atolls
  mutate(log_prey_tel = log1p(prey_fish_biomass_g_per_m2)) %>%
  mutate(log_piscivore = log1p(piscivore_biomass_g_per_m2))


# Prey Fish Biomass ####
## hist ####
hist(island.brt.df1$log_prey)

## gbm.auto param combos ####
gbm.auto(
  grids = NULL,
  samples = island.brt.df1,
  expvar = c("ave_temp",
             "ave_npp",
             "topo",
             "isl_grp",
             "lagoon.size",
             "pop.dens",
             "maxn_shark",
             "Relief",
             "chi_benthos_percent",
             "log_piscivore"), # fix to final variables
  resvar = "log_prey_tel",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  ZI = "CHECK",
  fam1 = c("binomial"),
  fam2 = c("gaussian"), #
  simp = TRUE,
  gridslat = 2,
  gridslon = 1,
  multiplot = TRUE,
  cols = grey.colors(1, 1, 1),
  linesfiles = TRUE, # change to true for final run
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = TRUE, # change to true for final runs
  loadgbm = NULL,
  varint = TRUE,
  map = TRUE,
  shape = NULL,
  RSB = TRUE,
  BnW = TRUE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"), # quartz for mac  this one for windows : "cairo-png"
  gaus = TRUE,
  MLEvaluate = TRUE,
  brv = NULL,
  grv = NULL,
  Bin_Preds = NULL,
  Gaus_Preds = NULL)

## preset hyperparameters ####
gbm.auto(
  grids = NULL,
  samples = island.brt.df1,
  expvar = c("ave_temp",
             "ave_npp",
             "topo",
             "isl_grp",
             "lagoon.size",
             "pop.dens",
             "maxn_shark",
             "Relief",
             "chi_benthos_percent",
             "log_piscivore"), # fix to final variables
  resvar = "log_prey_tel",
  tc = c(5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.01),
  bf = c(0.65),
  ZI = "CHECK",
  fam1 = c("binomial"),
  fam2 = c("gaussian"), #
  simp = TRUE,
  gridslat = 2,
  gridslon = 1,
  multiplot = TRUE,
  cols = grey.colors(1, 1, 1),
  linesfiles = TRUE, # change to true for final run
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = TRUE, # change to true for final runs
  loadgbm = NULL,
  varint = TRUE,
  map = TRUE,
  shape = NULL,
  RSB = TRUE,
  BnW = TRUE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"), # quartz for mac  this one for windows : "cairo-png"
  gaus = TRUE,
  MLEvaluate = TRUE,
  brv = NULL,
  grv = NULL,
  Bin_Preds = NULL,
  Gaus_Preds = NULL)




# Atoll only ####
## hist ####
hist(atoll.brt.df1$log_prey)

## Prey Fish Biomass ####
## gbm.auto param combos ####
gbm.auto(
  grids = NULL,
  samples = atoll.brt.df1,
  expvar = c("ave_temp",
             "ave_npp",
             "topo",
             "isl_grp",
             "lagoon.size",
             "pop.dens",
             "maxn_shark",
             "Relief",
             "chi_benthos_percent",
             "log_piscivore"), # fix to final variables
  resvar = "log_prey_tel",
  tc = c(1, 2, 3, 4, 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.0005, 0.001, 0.005, 0.01),
  bf = c(0.5, 0.55, 0.5, 0.65, 0.7, 0.75),
  ZI = "CHECK",
  fam1 = c("binomial"),
  fam2 = c("gaussian"), #
  simp = TRUE,
  gridslat = 2,
  gridslon = 1,
  multiplot = TRUE,
  cols = grey.colors(1, 1, 1),
  linesfiles = TRUE, # change to true for final run
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = TRUE, # change to true for final runs
  loadgbm = NULL,
  varint = TRUE,
  map = TRUE,
  shape = NULL,
  RSB = TRUE,
  BnW = TRUE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"), # quartz for mac  this one for windows : "cairo-png"
  gaus = TRUE,
  MLEvaluate = TRUE,
  brv = NULL,
  grv = NULL,
  Bin_Preds = NULL,
  Gaus_Preds = NULL)

## preset hyperparameters ####
gbm.auto(
  grids = NULL,
  samples = atoll.brt.df1,
  expvar = c("ave_temp",
             "ave_npp",
             "topo",
             "isl_grp",
             "lagoon.size",
             "pop.dens",
             "maxn_shark",
             "Relief",
             "chi_benthos_percent",
             "log_piscivore"), # fix to final variables
  resvar = "log_prey_tel",
  tc = c(4), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.001),
  bf = c(0.7),
  ZI = "CHECK",
  fam1 = c("binomial"),
  fam2 = c("gaussian"), #
  simp = TRUE,
  gridslat = 2,
  gridslon = 1,
  multiplot = TRUE,
  cols = grey.colors(1, 1, 1),
  linesfiles = TRUE, # change to true for final run
  smooth = TRUE,
  savedir = "~/Documents/My Documents/FinPrint French Poly/Analysis/DataExploration_05_2023/Chapter_4/BRT",
  savegbm = TRUE, # change to true for final runs
  loadgbm = NULL,
  varint = TRUE,
  map = TRUE,
  shape = NULL,
  RSB = TRUE,
  BnW = TRUE,
  alerts = FALSE, # this is the noise alerts
  pngtype = c("quartz"), # quartz for mac  this one for windows : "cairo-png"
  gaus = TRUE,
  MLEvaluate = TRUE,
  brv = NULL,
  grv = NULL,
  Bin_Preds = NULL,
  Gaus_Preds = NULL)
