# NK: Threw this together, hope it helps!!

library(dplyr)
library(dagitty)
library(base64enc)
library(readr)
library(DataCombine)

rm(list = ls())
# wd <- "~/Library/CloudStorage/OneDrive-DalhousieUniversity/Documents/Manuscripts/French Polynesia Reef Sharks/DAG consistency checks"
# wd <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/NFF Data code/"
# setwd(wd)


# DOWNLOAD THE DAG ####
DAG <- dagitty('dag {
ave_npp [pos="-0.927,0.696"]
ave_temp [pos="-0.890,1.097"]
bed_shear_stress [latent,pos="-0.963,1.079"]
coral_recruitment [latent,pos="-1.179,0.495"]
coral_spawning [latent,pos="-1.212,0.819"]
crown_of_thorns [latent,pos="-1.147,-0.199"]
crustose_coraline_algae [pos="-0.923,0.349"]
cyclones [latent,pos="-1.113,1.307"]
depth [latent,pos="-0.838,1.489"]
emerged_land_area [pos="-0.883,1.585"]
hard_coral [outcome,pos="-1.030,-0.171"]
herbivores [pos="-0.909,-0.746"]
invert [latent,pos="-1.099,-0.065"]
invertivores [pos="-1.108,-0.673"]
island_geomorphology [pos="-0.970,1.724"]
lagoon_size [pos="-1.086,1.471"]
latitude [pos="-1.104,1.716"]
light [latent,pos="-0.765,1.037"]
nutrient_run_off [latent,pos="-1.076,1.247"]
offshore_prey [latent,pos="-1.211,-1.103"]
other_algae [pos="-0.825,0.260"]
other_offshore_prey_proxies [latent,pos="-1.242,-0.989"]
piscivores [pos="-0.880,-1.502"]
planktivores [pos="-1.203,-0.701"]
pop_dens [pos="-0.789,-0.360"]
reef_sharks [exposure,pos="-1.024,-1.445"]
relief [pos="-1.123,0.674"]
sicklefin_lemon_sharks [pos="-1.095,-2.097"]
transient_pelagic_sharks [pos="-0.933,-2.078"]
turbidity [latent,pos="-0.773,0.592"]
wave_exposure [latent,pos="-0.953,1.322"]
zooplankton [latent,pos="-1.191,-0.011"]
ave_npp -> crustose_coraline_algae
ave_npp -> hard_coral
ave_npp -> invert
ave_npp -> offshore_prey
ave_npp -> turbidity
ave_npp -> zooplankton
ave_temp -> ave_npp
ave_temp -> crustose_coraline_algae
ave_temp -> hard_coral
ave_temp -> offshore_prey
ave_temp -> other_algae
bed_shear_stress -> crustose_coraline_algae
bed_shear_stress -> relief
coral_recruitment -> hard_coral
coral_spawning -> coral_recruitment
crown_of_thorns -> hard_coral
crustose_coraline_algae -> coral_recruitment
crustose_coraline_algae -> hard_coral
cyclones -> relief
depth -> bed_shear_stress
depth -> light
depth -> wave_exposure
emerged_land_area -> nutrient_run_off
emerged_land_area -> pop_dens
herbivores -> crustose_coraline_algae
herbivores -> hard_coral
herbivores -> other_algae
invertivores -> invert
invertivores -> other_algae
island_geomorphology -> ave_npp
island_geomorphology -> bed_shear_stress
island_geomorphology -> emerged_land_area
island_geomorphology -> lagoon_size
island_geomorphology -> nutrient_run_off
island_geomorphology -> offshore_prey
lagoon_size -> nutrient_run_off
latitude -> cyclones
latitude -> light
light -> ave_npp
light -> ave_temp
light -> turbidity
nutrient_run_off -> ave_npp
nutrient_run_off -> crown_of_thorns
nutrient_run_off -> crustose_coraline_algae
nutrient_run_off -> other_algae
nutrient_run_off -> turbidity
other_algae -> hard_coral
other_offshore_prey_proxies -> offshore_prey
piscivores -> herbivores
piscivores -> invertivores
piscivores -> planktivores
planktivores -> coral_spawning
pop_dens -> hard_coral
pop_dens -> herbivores
pop_dens -> invertivores
pop_dens -> nutrient_run_off
pop_dens -> offshore_prey
pop_dens -> piscivores
pop_dens -> planktivores
reef_sharks -> herbivores
reef_sharks -> invertivores
reef_sharks -> offshore_prey
reef_sharks -> planktivores
relief -> hard_coral
relief -> herbivores
relief -> invert
relief -> invertivores
relief -> planktivores
sicklefin_lemon_sharks -> offshore_prey
sicklefin_lemon_sharks -> piscivores
sicklefin_lemon_sharks -> reef_sharks
transient_pelagic_sharks -> offshore_prey
transient_pelagic_sharks -> piscivores
transient_pelagic_sharks -> reef_sharks
turbidity -> crustose_coraline_algae
turbidity -> hard_coral
turbidity -> other_algae
wave_exposure -> bed_shear_stress
zooplankton -> planktivores
}')

names(DAG)

# IMPORT DATA ######
# dat <- ReefWideBRUVUVC
dat <- read.csv(here("NFF_data", "/ReefWideBRUVUVC.csv"))
str(dat)
dat <- as.data.frame(dat)

# wd <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/"
# setwd(wd)
name_match <- read.csv(here("Nat_resources", "FPDAG_match_table.csv"))
str(name_match)


# REPLACE NAMES #####
var_names <- data.frame(current_name = colnames(dat))

var_names$corrected_name <- FindReplace(var_names,
                                        "current_name",
                                        name_match,
                                        from = "name_in_data",
                                        to = "name_in_dag",
                                        exact = T,
                                        vector = F
)

colnames(dat) <- as.character(var_names$corrected_name$current_name)


# SUBSET DATA TO MATCH DAG ######
ddat <- dat[, colnames(dat) %in% names(DAG)]
str(ddat)


# ANY VARIABLES TO TRANSFORM? ######
num_vars <- select_if(ddat, is.numeric)

graphics.off()
par(mfrow = c(3, 3))
for (i in 1:ncol(num_vars)) {
  num_var_min <- min(num_vars[, i], na.rm = TRUE)

  num_var_log <- if (num_var_min > 0) {
    log(num_vars[, i])
  } else {
    log(num_vars[, i] + ceiling(abs(num_var_min)) + 1)
  }

  hist(num_vars[, i], main = NA)
  title(colnames(num_vars)[i])

  hist(num_var_log, main = NA)
  title(paste("log", colnames(num_vars)[i]))

  hist((num_vars[, i]^2), main = NA)
  title(paste("square", colnames(num_vars)[i]))
}

# N didn't actually look through to see if anything needs to be transformed...


# TRANSFORM DATA ######

## Function to standardize ####
# Don't need to do this as already done at end of TeleostFunctionalGroupDiets.qmd
# stdize <- function(x) {
#   (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE) * 2)
# } # Sometimes do sd*2 in this...because MacNeil does it in his code...no better reason
# ddat[, 3:29] <- lapply(ddat[, 3:29], stdize)


# check % of data which NAs ####
nacheck <- sapply(ddat, function(x) paste(round(sum(is.na(x)) / length(x), 2) * 100, "%", sep = ""))
nacheck[which(nacheck != "0%")]
# browser 18%. Is marked as latent in dag import.
# Just browser...it will be angry so we remove rows with NA...
ddat <- ddat[complete.cases(ddat), ]
# SD: loses 5 of 28 rows, would be better to lose the whole browser column? Or use dummy data?

# VARIABLES IN DATA BUT NOT DAG ####
names(ddat)[!names(ddat) %in% names(DAG)]

# VARIABLES IN DAG BUT NOT DATA ####
InDagNotData <- names(DAG)[!names(DAG) %in% names(ddat)]
noquote(InDagNotData)
# bed_shear_stress
# coral_recruitment
# coral_spawning
# crown_of_thorns
# cyclones
# depth
# light
# nutrient_run_off
# offshore_prey
# other_offshore_prey_proxies
# turbidity
# wave_exposure
# zooplankton

# Reload DAG omitting latent variables & browser ####
# paste full dag from above then manually remove InDagNotData entries. Shrink console.
DAG <- dagitty('dag {
ave_npp [pos="-0.927,0.696"]
ave_temp [pos="-0.890,1.097"]
crustose_coraline_algae [pos="-0.923,0.349"]
emerged_land_area [pos="-0.883,1.585"]
hard_coral [outcome,pos="-1.030,-0.171"]
herbivores [pos="-0.909,-0.746"]
invert [latent,pos="-1.099,-0.065"]
invertivores [pos="-1.108,-0.673"]
island_geomorphology [pos="-0.970,1.724"]
lagoon_size [pos="-1.086,1.471"]
latitude [pos="-1.104,1.716"]
other_algae [pos="-0.825,0.260"]
piscivores [pos="-0.880,-1.502"]
planktivores [pos="-1.203,-0.701"]
pop_dens [pos="-0.789,-0.360"]
reef_sharks [exposure,pos="-1.024,-1.445"]
relief [pos="-1.123,0.674"]
sicklefin_lemon_sharks [pos="-1.095,-2.097"]
transient_pelagic_sharks [pos="-0.933,-2.078"]
ave_npp -> crustose_coraline_algae
ave_npp -> hard_coral
ave_npp -> invert
ave_temp -> ave_npp
ave_temp -> crustose_coraline_algae
ave_temp -> hard_coral
ave_temp -> other_algae
crustose_coraline_algae -> hard_coral
emerged_land_area -> pop_dens
herbivores -> crustose_coraline_algae
herbivores -> hard_coral
herbivores -> other_algae
invertivores -> invert
invertivores -> other_algae
island_geomorphology -> ave_npp
island_geomorphology -> emerged_land_area
island_geomorphology -> lagoon_size
other_algae -> hard_coral
piscivores -> herbivores
piscivores -> invertivores
piscivores -> planktivores
pop_dens -> hard_coral
pop_dens -> herbivores
pop_dens -> invertivores
pop_dens -> piscivores
pop_dens -> planktivores
reef_sharks -> herbivores
reef_sharks -> invertivores
reef_sharks -> planktivores
relief -> hard_coral
relief -> herbivores
relief -> invert
relief -> invertivores
relief -> planktivores
sicklefin_lemon_sharks -> piscivores
sicklefin_lemon_sharks -> reef_sharks
transient_pelagic_sharks -> piscivores
transient_pelagic_sharks -> reef_sharks
}')

# CREATE FAKE VARIABLES ####
# The test won't work with missing variables so will need to create some fake ones for now
# Suchinta recommends against this, and itr's obviated by doing the trimmed DAG
# ddat$`bed_shear_stress` <- runif(n = nrow(ddat), 0, 1)
# # ddat$`browser` <- runif(n = nrow(ddat), 0, 1) # browser added, observed with 5 NAs, marked as unobserved and adding fake data. Added this to the 5 NAs instead
# ddat$`cloud_cover` <- runif(n = nrow(ddat), 0, 1)
# ddat$`common_blacktip_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`coral_spawning` <- runif(n = nrow(ddat), 0, 1)
# ddat$`crown_of_thorns` <- runif(n = nrow(ddat), 0, 1)
# ddat$`cyclones` <- runif(n = nrow(ddat), 0, 1)
# ddat$`depth` <- runif(n = nrow(ddat), 0, 1)
# ddat$`great_hammerhead_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`isl_grp` <- runif(n = nrow(ddat), 0, 1)
# ddat$`light` <- runif(n = nrow(ddat), 0, 1)
# ddat$`nutrient_run_off` <- runif(n = nrow(ddat), 0, 1)
# ddat$`offshore_prey` <- runif(n = nrow(ddat), 0, 1)
# ddat$`other_offshore_prey_proxies` <- runif(n = nrow(ddat), 0, 1)
# ddat$`scalloped_hammerhead_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`tiger_shark` <- runif(n = nrow(ddat), 0, 1)
# ddat$`turbidity` <- runif(n = nrow(ddat), 0, 1)
# ddat$`wave_exposure` <- runif(n = nrow(ddat), 0, 1)
# ddat$`zooplankton` <- runif(n = nrow(ddat), 0, 1)
#
# ddat <- ddat |> select(names(DAG))
# identical(colnames(ddat), names(DAG))
# names(DAG)[!names(DAG) %in% names(ddat)] # Gucci


# EVALUATE THE D-SEPARATION IMPLICATIONS OF THE DAG ####
test <- dagitty::localTests(x = DAG,
                            data = ddat,
                            abbreviate.names = FALSE)
write.csv(x = test,
          file = here("Nat_resources", "dag_inconsistencies_all.csv"),
          row.names = TRUE)


# SUBSET DATA BASED ON CORRELATION VALUE OR P VALUE ####
# can define pass or fail based on thresholds of p-value (meh) or correlation coefficient (sure)
testf2 <- subset(test, estimate >= 0.3 | estimate <= -0.3)
testf2 <- testf2[rev(order(abs(testf2$estimate))), ] # Sort test results by effect size

# SHOW THE INDEPENDENCIES THAT FAILED ####
write.csv(x = testf2,
          file = here("Nat_resources", "dag_inconsistencies.csv"),
          row.names = TRUE)
dev.off()
dagitty::plotLocalTestResults(head(testf2, 20)) # Plot 20 results with largest effect size

# Save ddat for analysis stage ####
write.csv(x = ddat,
          file = here("Nat_resources", "ReefWideBRUVUVC-DAGtested.csv"),
          row.names = FALSE)




# DAG for BU arrows up ####
dag {
  ave_npp [pos="-0.929,0.701"]
  ave_temp [pos="-0.883,1.029"]
  bed_shear_stress [latent,pos="-0.963,1.079"]
  coral_recruitment [latent,pos="-1.179,0.495"]
  coral_spawning [latent,pos="-1.212,0.819"]
  crown_of_thorns [latent,pos="-1.147,-0.199"]
  crustose_coraline_algae [pos="-0.922,0.295"]
  cyclones [latent,pos="-1.113,1.307"]
  depth [latent,pos="-0.838,1.489"]
  emerged_land_area [pos="-0.883,1.585"]
  hard_coral [exposure,pos="-1.023,-0.240"]
  herbivores [pos="-0.909,-0.746"]
  invert [latent,pos="-1.099,-0.065"]
  invertivores [pos="-1.108,-0.673"]
  island_geomorphology [pos="-0.970,1.724"]
  lagoon_size [pos="-1.086,1.471"]
  latitude [pos="-1.104,1.716"]
  light [latent,pos="-0.765,1.037"]
  nutrient_run_off [latent,pos="-1.076,1.247"]
  offshore_prey [latent,pos="-1.185,-1.199"]
  other_algae [pos="-0.825,0.260"]
  other_offshore_prey_proxies [latent,pos="-1.219,-1.030"]
  piscivores [pos="-0.880,-1.502"]
  planktivores [pos="-1.203,-0.701"]
  pop_dens [pos="-0.789,-0.360"]
  reef_sharks [outcome,pos="-1.024,-1.445"]
  relief [pos="-1.123,0.674"]
  sicklefin_lemon_sharks [pos="-1.095,-2.097"]
  transient_pelagic_sharks [pos="-0.933,-2.078"]
  turbidity [latent,pos="-0.773,0.592"]
  wave_exposure [latent,pos="-0.953,1.322"]
  zooplankton [pos="-1.191,-0.011"]
  ave_npp -> crustose_coraline_algae
  ave_npp -> hard_coral
  ave_npp -> offshore_prey
  ave_npp -> turbidity
  ave_temp -> ave_npp
  ave_temp -> crustose_coraline_algae
  ave_temp -> hard_coral
  ave_temp -> offshore_prey
  ave_temp -> other_algae
  bed_shear_stress -> crustose_coraline_algae
  bed_shear_stress -> relief
  coral_recruitment -> hard_coral
  coral_spawning -> coral_recruitment
  crown_of_thorns -> hard_coral
  crustose_coraline_algae -> coral_recruitment
  crustose_coraline_algae -> hard_coral
  crustose_coraline_algae -> herbivores
  cyclones -> relief
  depth -> bed_shear_stress
  depth -> light
  depth -> wave_exposure
  emerged_land_area -> nutrient_run_off
  emerged_land_area -> pop_dens
  hard_coral -> herbivores
  herbivores -> piscivores
  herbivores -> reef_sharks
  invert -> ave_npp
  invert -> invertivores
  invertivores -> other_algae
  invertivores -> piscivores
  invertivores -> reef_sharks
  island_geomorphology -> ave_npp
  island_geomorphology -> bed_shear_stress
  island_geomorphology -> emerged_land_area
  island_geomorphology -> lagoon_size
  island_geomorphology -> nutrient_run_off
  island_geomorphology -> offshore_prey
  lagoon_size -> nutrient_run_off
  latitude -> cyclones
  latitude -> light
  light -> ave_npp
  light -> ave_temp
  light -> turbidity
  nutrient_run_off -> ave_npp
  nutrient_run_off -> crown_of_thorns
  nutrient_run_off -> crustose_coraline_algae
  nutrient_run_off -> other_algae
  nutrient_run_off -> turbidity
  offshore_prey -> reef_sharks
  offshore_prey -> sicklefin_lemon_sharks
  offshore_prey -> transient_pelagic_sharks
  other_algae -> hard_coral
  other_algae -> herbivores
  other_offshore_prey_proxies -> offshore_prey
  piscivores -> sicklefin_lemon_sharks
  piscivores -> transient_pelagic_sharks
  planktivores -> coral_spawning
  planktivores -> piscivores
  planktivores -> reef_sharks
  planktivores -> zooplankton
  pop_dens -> hard_coral
  pop_dens -> herbivores
  pop_dens -> invertivores
  pop_dens -> nutrient_run_off
  pop_dens -> offshore_prey
  pop_dens -> piscivores
  pop_dens -> planktivores
  reef_sharks -> sicklefin_lemon_sharks
  reef_sharks -> transient_pelagic_sharks
  relief -> hard_coral
  relief -> herbivores
  relief -> invert
  relief -> invertivores
  relief -> planktivores
  turbidity -> crustose_coraline_algae
  turbidity -> hard_coral
  turbidity -> other_algae
  wave_exposure -> bed_shear_stress
  zooplankton -> ave_npp
}

# 2024-08-27 test SST as instrument for Instrumental Variable ####
# SST (ave_temp) as exposure, sharks as outcome

# InDagNotData: remove from below
bed_shear_stress
coral_recruitment
coral_spawning
crown_of_thorns
cyclones
depth
light
nutrient_run_off
offshore_prey
other_offshore_prey_proxies
turbidity
wave_exposure
zooplankton

DAG <- dagitty('dag {
  ave_npp [pos="-0.927,0.696"]
  ave_temp [exposure,pos="-0.890,1.097"]
  crustose_coraline_algae [pos="-0.923,0.349"]
  emerged_land_area [pos="-0.883,1.585"]
  hard_coral [pos="-1.030,-0.171"]
  herbivores [pos="-0.909,-0.746"]
  invert [latent,pos="-1.099,-0.065"]
  invertivores [pos="-1.108,-0.673"]
  island_geomorphology [pos="-0.970,1.724"]
  lagoon_size [pos="-1.086,1.471"]
  latitude [pos="-1.104,1.716"]
  other_algae [pos="-0.825,0.260"]
  piscivores [pos="-0.880,-1.502"]
  planktivores [pos="-1.203,-0.701"]
  pop_dens [pos="-0.789,-0.360"]
  reef_sharks [outcome,pos="-1.024,-1.445"]
  relief [pos="-1.123,0.674"]
  sicklefin_lemon_sharks [pos="-1.095,-2.097"]
  transient_pelagic_sharks [pos="-0.933,-2.078"]
  ave_npp -> crustose_coraline_algae
  ave_npp -> hard_coral
  ave_npp -> invert
  ave_temp -> ave_npp
  ave_temp -> crustose_coraline_algae
  ave_temp -> hard_coral
  ave_temp -> other_algae
  crustose_coraline_algae -> hard_coral
  emerged_land_area -> pop_dens
  herbivores -> crustose_coraline_algae
  herbivores -> hard_coral
  herbivores -> other_algae
  invertivores -> invert
  invertivores -> other_algae
  island_geomorphology -> ave_npp
  island_geomorphology -> emerged_land_area
  island_geomorphology -> lagoon_size
  other_algae -> hard_coral
  piscivores -> herbivores
  piscivores -> invertivores
  piscivores -> planktivores
  pop_dens -> hard_coral
  pop_dens -> herbivores
  pop_dens -> invertivores
  pop_dens -> piscivores
  pop_dens -> planktivores
  reef_sharks -> herbivores
  reef_sharks -> invertivores
  reef_sharks -> planktivores
  relief -> hard_coral
  relief -> herbivores
  relief -> invert
  relief -> invertivores
  relief -> planktivores
  sicklefin_lemon_sharks -> piscivores
  sicklefin_lemon_sharks -> reef_sharks
  transient_pelagic_sharks -> piscivores
  transient_pelagic_sharks -> reef_sharks
}')


# EVALUATE THE D-SEPARATION IMPLICATIONS OF THE DAG
test <- dagitty::localTests(x = DAG,
                            data = ddat,
                            abbreviate.names = FALSE)
write.csv(x = test,
          file = here("Nat_resources", "dag_inconsistencies_all.csv"),
          row.names = TRUE)


## SUBSET DATA BASED ON CORRELATION VALUE OR P VALUE ####
# can define pass or fail based on thresholds of p-value (meh) or correlation coefficient (sure)
testf2 <- subset(test, estimate >= 0.3 | estimate <= -0.3)
testf2 <- testf2[rev(order(abs(testf2$estimate))), ] # Sort test results by effect size

## SHOW THE INDEPENDENCIES THAT FAILED ####
write.csv(x = testf2,
          file = here("Nat_resources", "dag_inconsistencies_ave-temp-reefsharks.csv"),
          row.names = TRUE)
dev.off()
dagitty::plotLocalTestResults(head(testf2, 20)) # Plot 20 results with largest effect size

# 2024-09-03 SST/reef_sharks linear model ####
sst.lm <- lm(formula = reef_sharks ~ ave_temp,
             data = dat)
summary(sst.lm)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -0.29436 -0.19908 -0.07844  0.21558  0.57011
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.1395     0.1206   1.157   0.2579
# ave_temp      0.3481     0.1721   2.023   0.0535 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2523 on 26 degrees of freedom
# Multiple R-squared:  0.136,	Adjusted R-squared:  0.1028
# F-statistic: 4.093 on 1 and 26 DF,  p-value: 0.05345
plot(sst.lm)


library(ggplot2)
ggplot(data = dat,
       aes(x = ave_temp,
           y = reef_sharks)) +
  geom_point() +
  stat_smooth(method = "lm")
