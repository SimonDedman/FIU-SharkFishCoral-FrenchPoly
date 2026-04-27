library(tidyverse)
# remotes::install_github("SimonDedman/gbm.auto")
library(gbm.auto)
library(here)

# import dataframes ####
brt.df1 <- readRDS(here("NFF_data", "ch4_reef_wide_df2.RData")) %>% # high islands
  mutate(
    log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
    log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
    log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
    log_piscivore = log1p(biomass_g_per_m2_Piscivore)
  )
# geo as expvar option

island.brt.df1 <- readRDS(here(
  "NFF_data",
  "ch4_island_reef_wide_df2.RData"
)) %>% # high islands
  mutate(
    log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
    log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
    log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
    log_piscivore = log1p(biomass_g_per_m2_Piscivore)
  )

atoll.brt.df1 <- readRDS(here("NFF_data", "ch4_atoll_reef_wide_df2.RData")) %>% # atolls
  mutate(
    log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
    log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
    log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
    log_piscivore = log1p(biomass_g_per_m2_Piscivore)
  )

# Coral Health Index Benthos % ####
## expvars ####
expvars <- c(
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

## All: Islands + Atolls ####
### histogram ####
hist(brt.df1$chi_benthos_percent)


### gbm.auto combos ####
tmp <- rbind(brt.df1, brt.df1)
gbm.bfcheck(samples = brt.df1, resvar = "chi_benthos_percent", ZI = FALSE) # 0.875

# Error in gbm.fit(x = x, y = y, offset = offset, distribution = distribution,: The data set is too small or the subsampling rate is too large: `nTrain * bag.fraction <= 2 * n.minobsinnode + 1`
gbm.auto(
  samples = brt.df1, # change to island
  expvar = c(expvars),
  resvar = "chi_benthos_percent",
  randomvar = TRUE,
  tc = c(
    # 1:12
    1
    # 2,
    # 3,
    # 4,
    # 12
  ), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(
    # 0.5, 0.25, 0.1, 0.075, 0.05, 0.01
    0.00001
    # 0.05 #,
    # 0.001,
    # 0.005,
    # 0.01
  ),
  bf = c(
    # (5:9)/10
    # 0.5,
    # 0.65,
    0.9
    # 0.75
    # 0.5
  ),
  n.trees = 100,
  # simp = FALSE,
  ZI = FALSE, # forces fam2 only, gaussian only run
  # fam1 = "gaussian",
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png" #,
  # gaus = FALSE
)

### preset hyperparameters ####
gbm.auto(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "chi_benthos_percent",
  # randomvar = TRUE,
  tc = 1,
  lr = 0.5,
  bf = 0.7,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)

### NFF worked-before hyperparameters ####
# gbm.auto( # working but with double dataset hack
#   samples = tmp,
#   expvar = c(expvars), resvar = "chi_benthos_percent",
#   # randomvar = TRUE,
#   tc = 5,
#   lr = 0.005,
#   bf = 0.75,
#   n.trees = 100,
#   # ZI = FALSE,
#   ZI = "CHECK",
#   fam1 = c("binomial"),
#   fam2 = c("gaussian"),
#   smooth = TRUE, savedir = here("Results", "BRT", "All"), savegbm = FALSE, alerts = FALSE,
#   pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
# )

### gbm.loop run ####
gbm.loop(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "chi_benthos_percent",
  # randomvar = TRUE,
  tc = 1,
  lr = 0.5,
  bf = 0.7,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)

#### stitch together successful loops ####
gbm.loop(
  runautos = FALSE,
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "chi_benthos_percent",
  # tc = 1,
  # lr = 0.5,
  # bf = 0.7,
  # n.trees = 100,
  # ZI = FALSE,
  # smooth = TRUE,
  savedir = here("Results", "BRT", "All", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)


## Islands ####
### histogram ####
dir.create(here("Results", "BRT", "HighIslands"))
hist(island.brt.df1$chi_benthos_percent)
tmp <- rbind(island.brt.df1, island.brt.df1, island.brt.df1)

### gbm.auto combos ####
gbm.auto(
  samples = tmp, # change to island
  expvar = c(expvars),
  resvar = "chi_benthos_percent",
  randomvar = TRUE,
  tc = c(12, 8, 6, 4, 3, 2, 1), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.005, 0.001, 0.05, 0.01, 0.5),
  bf = c(0.9, 0.8, 0.75), # , 0.7, 0.65, 0.5
  n.trees = 100,
  simp = FALSE, # test simplify after finding the best model first
  ZI = FALSE, # forces fam2 only, gaussian only run
  # fam1 = "gaussian",
  smooth = TRUE,
  savedir = here("Results", "BRT", "HighIslands"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png" #,
  # gaus = FALSE
)

### preset hyperparameters ####
gbm.auto(
  samples = tmp,
  expvar = expvars,
  resvar = "chi_benthos_percent",
  n.trees = 150,
  simp = FALSE,
  tc = 12,
  lr = 0.005,
  bf = 0.75,
  smooth = TRUE,
  savedir = here("Results", "BRT", "HighIslands"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)

dir.create(here("Results", "BRT", "HighIslands", "Loop"))
gbm.loop(
  samples = tmp,
  expvar = expvars,
  resvar = "chi_benthos_percent",
  n.trees = 150,
  simp = FALSE,
  tc = 12,
  lr = 0.005,
  bf = 0.75,
  smooth = TRUE,
  savedir = here("Results", "BRT", "HighIslands", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)


## Atolls ####
### histogram ####
hist(atoll.brt.df1$chi_benthos_percent)
dir.create(here("Results", "BRT", "Atolls"))
tmp <- rbind(atoll.brt.df1, atoll.brt.df1, atoll.brt.df1)

### bf check ####
gbm.bfcheck(
  samples = tmp, # Gaussian bag fraction must be at least 1.615. n = 13: Gaussian bag fraction must be at least 0.538. n = 39
  resvar = "chi_benthos_percent"
) # Gaussian bag fraction must be at least 0.538. n = 39: tmp

### gbm.auto combos ####
gbm.auto(
  samples = tmp,
  expvar = expvars,
  resvar = "chi_benthos_percent",
  tc = c(12, 8, 6, 4, 3, 2, 1), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(0.005, 0.001, 0.05, 0.01, 0.5),
  bf = c(0.9, 0.8, 0.75), # , 0.7, 0.65, 0.5
  randomvar = TRUE,
  simp = FALSE,
  n.trees = 100,
  smooth = TRUE,
  savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
### preset hyperparameters ####
gbm.auto(
  samples = tmp, # atoll.brt.df1
  expvar = expvars,
  resvar = "chi_benthos_percent",
  n.trees = 150,
  simp = FALSE,
  tc = 1,
  lr = 0.005,
  bf = 0.75,
  smooth = TRUE,
  savedir = here("Results", "BRT", "Atolls"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
# FROMHERE ####
dir.create(here("Results", "BRT", "Atolls", "Loop"))
gbm.loop(
  samples = tmp,
  expvar = expvars,
  resvar = "chi_benthos_percent",
  n.trees = 150,
  simp = FALSE,
  tc = 1,
  lr = 0.005,
  bf = 0.75,
  smooth = TRUE,
  savedir = here("Results", "BRT", "Atolls", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)


# Herbivore Biomass ####
## expvars ####
expvars <- c(
  "ave_temp",
  "ave_npp",
  "topo",
  "pop.dens",
  "isl_grp",
  "lagoon.size",
  "maxn_shark",
  "Relief",
  "chi_benthos_percent",
  "log_piscivore",
  "log_Planktivore",
  "log_Invertivore"
)

## All: Islands + Atolls ####
### hist ####
hist(brt.df1$log_Herbivore)

### gbm.auto param combos ####
tmp <- rbind(brt.df1, brt.df1)
gbm.bfcheck(samples = brt.df1, resvar = "log_Herbivore", ZI = FALSE) # 0.875
gbm.auto(
  samples = tmp, # change to island
  expvar = c(expvars),
  resvar = "log_Herbivore",
  randomvar = TRUE,
  tc = c(
    # 1:12
    3:5 # winning range all comps
  ), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(
    # 0.5, 0.25, 0.1, 0.075, 0.05, 0.01
    0.5 # winner in all comps
  ),
  bf = c(
    # (5:9)/10
    0.9 # winner all comps
  ),
  n.trees = 100,
  # simp = FALSE,
  ZI = FALSE, # forces fam2 only, gaussian only run
  # fam1 = "gaussian",
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png" #,
  # gaus = FALSE
)

### preset hyperparameters ####
gbm.auto(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Herbivore",
  tc = 4,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)

### gbm.loop run ####
dir.create(here("Results", "BRT", "All", "log_Herbivore", "Loop"))
dir.create(here("Results", "BRT", "All", "log_Herbivore", "Loop", "safe"))
gbm.loop(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Herbivore",
  tc = 4,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All", "log_Herbivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
#### stitch together successful loops ####
gbm.loop(
  runautos = FALSE,
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Herbivore",
  savedir = here("Results", "BRT", "All", "log_Herbivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)


# Invertivore Biomass ####
## expvars ####
expvars <- c(
  "ave_temp",
  "ave_npp",
  "topo",
  "pop.dens",
  "isl_grp",
  "lagoon.size",
  "maxn_shark",
  "Relief",
  "chi_benthos_percent",
  "log_piscivore",
  "log_Planktivore",
  "log_Herbivore"
)
## All: Islands + Atolls ####
### hist ####
hist(tmp$log_Invertivore)

### gbm.auto param combos ####
gbm.auto(
  samples = tmp, # change to island
  expvar = c(expvars),
  resvar = "log_Invertivore",
  randomvar = TRUE,
  tc = c(
    # 1:12
    3:5 # winning range all comps
  ), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(
    # 0.5, 0.25, 0.1, 0.075, 0.05, 0.01
    0.5 # winner in all comps
  ),
  bf = c(
    # (5:9)/10
    0.9 # winner all comps
  ),
  n.trees = 100,
  # simp = FALSE,
  ZI = FALSE, # forces fam2 only, gaussian only run
  # fam1 = "gaussian",
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png" #,
  # gaus = FALSE
)

### preset hyperparameters ####
gbm.auto(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Invertivore",
  # randomvar = TRUE,
  tc = 3,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
### gbm.loop run ####
dir.create(here("Results", "BRT", "All", "log_Invertivore", "Loop"))
dir.create(here("Results", "BRT", "All", "log_Invertivore", "Loop", "safe"))

gbm.loop(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Invertivore",
  tc = 3,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All", "log_Invertivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
#### stitch together successful loops ####
gbm.loop(
  loops = 11, # just because I ended up with 1 extra good run
  runautos = FALSE,
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Invertivore",
  savedir = here("Results", "BRT", "All", "log_Invertivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)


# Planktivore Biomass ####
## expvars ####
expvars <- c(
  "ave_temp",
  "ave_npp",
  "topo",
  "pop.dens",
  "isl_grp",
  "lagoon.size",
  "maxn_shark",
  "Relief",
  "chi_benthos_percent",
  "log_piscivore",
  "log_Invertivore",
  "log_Herbivore"
)
## All: Islands + Atolls ####
### hist ####
hist(tmp$log_Planktivore)

### gbm.auto param combos ####
gbm.auto(
  samples = tmp, # change to island
  expvar = c(expvars),
  resvar = "log_Planktivore",
  randomvar = TRUE,
  tc = c(
    # 1:12
    3:5 # winning range all comps
  ), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(
    # 0.5, 0.25, 0.1, 0.075, 0.05, 0.01
    0.5 # winner in all comps
  ),
  bf = c(
    # (5:9)/10
    0.9 # winner all comps
  ),
  n.trees = 100,
  # simp = FALSE,
  ZI = FALSE, # forces fam2 only, gaussian only run
  # fam1 = "gaussian",
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png" #,
  # gaus = FALSE
)

### preset hyperparameters ####
gbm.auto(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Planktivore",
  # randomvar = TRUE,
  tc = 5,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)

### gbm.loop run ####
dir.create(here("Results", "BRT", "All", "log_Planktivore", "Loop"))
dir.create(here("Results", "BRT", "All", "log_Planktivore", "Loop", "safe"))

gbm.loop(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Planktivore",
  tc = 5,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All", "log_Planktivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
#### stitch together successful loops ####
gbm.loop(
  loops = 10, # just because I ended up with 2 extra good runs
  runautos = FALSE,
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_Planktivore",
  savedir = here("Results", "BRT", "All", "log_Planktivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)


# Piscivore Biomass ####
## expvars ####
expvars <- c(
  "ave_temp",
  "ave_npp",
  "topo",
  "pop.dens",
  "isl_grp",
  "lagoon.size",
  "maxn_shark",
  "Relief",
  "chi_benthos_percent",
  "log_piscivore",
  "log_Invertivore",
  "log_Herbivore"
)
## All: Islands + Atolls ####
### hist ####
hist(tmp$log_piscivore)

### gbm.auto param combos ####
gbm.auto(
  samples = tmp, # change to island
  expvar = c(expvars),
  resvar = "log_piscivore",
  randomvar = TRUE,
  tc = c(
    1:12
    # 1
    # 2,
    # 3,
    # 4,
    # 12
  ), # add combos you want to see for initial runs and it will try each. doesn't run the whole gambit like the loops do
  lr = c(
    0.5,
    0.25,
    0.1,
    0.075,
    0.05,
    0.01
    # 0.00001
    # 0.05 #,
    # 0.001,
    # 0.005,
    # 0.01
  ),
  bf = c(
    (5:9) / 10
    # 0.5,
    # 0.65,
    # 0.9
    # 0.75
    # 0.5
  ),
  n.trees = 100,
  # simp = FALSE,
  ZI = FALSE, # forces fam2 only, gaussian only run
  # fam1 = "gaussian",
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png" #,
  # gaus = FALSE
)

### preset hyperparameters ####
gbm.auto(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_piscivore",
  # randomvar = TRUE,
  tc = 4,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
### gbm.loop run ####
dir.create(here("Results", "BRT", "All", "log_piscivore", "Loop"))
dir.create(here("Results", "BRT", "All", "log_piscivore", "Loop", "safe"))

gbm.loop(
  # working but with double dataset hack
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_piscivore",
  tc = 4,
  lr = 0.5,
  bf = 0.9,
  n.trees = 100,
  ZI = FALSE,
  smooth = TRUE,
  savedir = here("Results", "BRT", "All", "log_piscivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
#### stitch together successful loops ####
gbm.loop(
  runautos = FALSE,
  samples = tmp, # brt.df1
  expvar = c(expvars),
  resvar = "log_piscivore",
  savedir = here("Results", "BRT", "All", "log_piscivore", "Loop"),
  savegbm = FALSE,
  alerts = FALSE,
  pngtype = if (Sys.info()["sysname"] == "Darwin") "quartz" else "cairo-png"
)
