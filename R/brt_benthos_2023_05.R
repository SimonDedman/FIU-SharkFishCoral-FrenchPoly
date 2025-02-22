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
library(ggBRT)
library(ape)
library(gbm)
library(gbm.auto)
library(here)
#library(remotes)
#remotes::install_github("SimonDedman/gbm.auto", force = TRUE)

# import dataframe ####
island.data.df1 <- readRDS(here("NFF_data", "island.survey.wide.df2.RData")) # high islands
atoll.data.df1 <- readRDS(here("NFF_data", "atoll.survey.wide.df2.RData")) # atoplls
all.data.df1 <- readRDS(here("NFF_data", "survey.wide.df2.RData"))

## rename columns to match DAG ####
island.data.df1 <- island.data.df1 |> dplyr::rename_with(~ gsub("pred_tel", "piscivore", .x))
atoll.data.df1 <- atoll.data.df1 |> dplyr::rename_with(~ gsub("pred_tel", "piscivore", .x))
all.data.df1 <- all.data.df1 |> dplyr::rename_with(~ gsub("pred_tel", "piscivore", .x))

# average teleost biomasses for analysis #
# calculate pop. dens ####
island.brt.df1 <- island.data.df1 %>%
  group_by(site_name, reef_name) %>%
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), first),
    across(where(is.numeric), mean)
  ) %>%
  select(c(reef_name, site_name, piscivore_biomass_g_per_m2, prey_fish_biomass_g_per_m2)) %>%
  dplyr::rename(piscivore_reef = piscivore_biomass_g_per_m2, prey_fish_reef = prey_fish_biomass_g_per_m2) %>%
  merge(island.data.df1, by = c("reef_name", "site_name"))

## histogram ####
hist(island.brt.df1$chi_benthos_percent)




# island cca + hard coral ####
gbm.auto(
  grids = NULL,
  samples = island.brt.df1,
  expvar = c("ave_temp",
             "ave_npp",
             "topo",
             "pop.dens",
             "isl_grp",
             "lagoon.size",
             "maxn_shark",
             "Relief",
             "piscivore_reef",
             "prey_fish_reef"), # fix to final variables
  resvar = "chi_benthos_percent",
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
             "pop.dens",
             "isl_grp",
             "lagoon.size",
             "maxn_shark",
             "Relief",
             "piscivore_reef",
             "prey_fish_reef"), # fix to final variables
  resvar = "chi_benthos_percent",
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




# atoll ####
atoll.brt.df1 <- atoll.data.df1 %>%
  group_by(site_name, reef_name) %>%
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), first),
    across(where(is.numeric), mean)
  ) %>%
  select(c(reef_name, site_name, piscivore_biomass_g_per_m2, prey_fish_biomass_g_per_m2 )) %>%
  dplyr::rename(piscivore_reef = piscivore_biomass_g_per_m2, prey_fish_reef = prey_fish_biomass_g_per_m2  ) %>%
  merge(atoll.data.df1, by = c("reef_name", "site_name"))

## hist ####
hist(atoll.brt.df1$chi_benthos_percent)

## bf check ####
gbm.bfcheck(samples = atoll.brt.df1,
            resvar = "chi_benthos_percent",
            ZI = "CHECK",
            grv = NULL)




# atoll cca+ hard coral  ####
gbm.auto(
  grids = NULL,
  samples = atoll.brt.df1,
  expvar = c("ave_temp",
             "ave_npp",
             "topo",
             "pop.dens",
             "isl_grp",
             "lagoon.size",
             "maxn_shark",
             "Relief",
             "piscivore_reef",
             "prey_fish_reef"), # fix to final variables
  resvar = "chi_benthos_percent",
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
             "pop.dens",
             "isl_grp",
             "lagoon.size",
             "maxn_shark",
             "Relief",
             "piscivore_reef",
             "prey_fish_reef"), # fix to final variables
  resvar = "chi_benthos_percent",
  tc = c( 5), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c( 0.01),
  bf = c( 0.5),
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
