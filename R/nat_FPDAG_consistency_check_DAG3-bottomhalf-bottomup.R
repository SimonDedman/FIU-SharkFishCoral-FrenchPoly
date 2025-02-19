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
cloud_cover [latent,pos="-0.785,1.464"]
crustose_coraline_algae [pos="-0.923,0.349"]
cyclones [latent,pos="-1.113,1.307"]
depth [latent,pos="-0.838,1.489"]
emerged_land_area [pos="-0.883,1.585"]
hard_coral [outcome,pos="-1.030,-0.171"]
isl_grp [pos="-1.042,1.753"]
island_geomorphology [exposure,pos="-0.970,1.724"]
lagoon_size [pos="-1.086,1.471"]
latitude [pos="-1.104,1.716"]
light [latent,pos="-0.765,1.037"]
longitude [pos="-1.090,1.884"]
nutrient_run_off [latent,pos="-1.076,1.247"]
other_algae [pos="-0.825,0.260"]
pop_dens [pos="-0.711,0.242"]
population_size [pos="-0.720,0.716"]
relief [pos="-1.123,0.674"]
season [pos="-0.720,1.543"]
turbidity [latent,pos="-0.773,0.592"]
wave_exposure [latent,pos="-0.953,1.322"]
ave_npp -> crustose_coraline_algae
ave_npp -> hard_coral
ave_npp -> turbidity
ave_temp -> ave_npp
ave_temp -> crustose_coraline_algae
ave_temp -> hard_coral
ave_temp -> other_algae
bed_shear_stress -> crustose_coraline_algae
bed_shear_stress -> relief
cloud_cover -> light
crustose_coraline_algae -> hard_coral
cyclones -> relief
depth -> bed_shear_stress
depth -> light
depth -> wave_exposure
emerged_land_area -> nutrient_run_off
emerged_land_area -> pop_dens
isl_grp -> island_geomorphology
isl_grp -> light
island_geomorphology -> ave_npp
island_geomorphology -> bed_shear_stress
island_geomorphology -> emerged_land_area
island_geomorphology -> lagoon_size
island_geomorphology -> nutrient_run_off
lagoon_size -> nutrient_run_off
latitude -> cyclones
latitude -> isl_grp
latitude -> light
light -> ave_npp
light -> ave_temp
light -> turbidity
longitude -> isl_grp
nutrient_run_off -> ave_npp
nutrient_run_off -> crustose_coraline_algae
nutrient_run_off -> other_algae
nutrient_run_off -> turbidity
other_algae -> hard_coral
pop_dens -> hard_coral
pop_dens -> nutrient_run_off
population_size -> pop_dens
relief -> hard_coral
season -> ave_temp
season -> cloud_cover
season -> light
turbidity -> crustose_coraline_algae
turbidity -> hard_coral
turbidity -> other_algae
wave_exposure -> bed_shear_stress
}')

names(DAG)

###### IMPORT DATA ######

dat <- read.csv(here("NFF_data", "/ReefWideBRUVUVC.csv"))
str(dat)

# wd <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/"
# setwd(wd)
name_match <- read.csv(here("Nat_resources", "FPDAG_match_table.csv"))
str(name_match)


###### REPLACE NAMES #####
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


###### SUBSET DATA TO MATCH DAG ######
ddat <- dat[, colnames(dat) %in% names(DAG)]
str(ddat)


###### DO ANY VARIABLES NEED TO BE TRANSFORMED ? ######
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


###### TRANSFORM DATA ######

# Function to standardize
stdize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE) * 2)
} # Sometimes do sd*2 in this...because MacNeil does it in his code...no better reason

ddat[, 3:ncol(ddat)] <- lapply(ddat[, 3:ncol(ddat)], stdize)


# check % of data which NAs ####
sapply(ddat, function(x) paste(round(sum(is.na(x)) / length(x), 2) * 100, "%", sep = ""))
# browser 18%. Is marked as latent in dag import.
# Just browser...it will be angry so we remove rows with NA...
ddat <- ddat[complete.cases(ddat), ]
# SD: loses 5 of 28 rows, would be better to lose the whole browser column? Or use dummy data?


# VARIABLES IN DAG BUT NOT DATA ####
InDagNotData <- names(DAG)[!names(DAG) %in% names(ddat)]
InDagNotData
"bed_shear_stress"
"cloud_cover"
"cyclones"
"depth"
"isl_grp"
"light"
"nutrient_run_off"
"turbidity"
"wave_exposure"

# VARIABLES IN DATA BUT NOT DAG ####
names(ddat)[!names(ddat) %in% names(DAG)]


# Reload DAG omitting latent variables & browser ####
DAG <- dagitty('dag {
ave_npp [pos="-0.927,0.696"]
ave_temp [pos="-0.890,1.097"]
crustose_coraline_algae [pos="-0.923,0.349"]
emerged_land_area [pos="-0.883,1.585"]
hard_coral [outcome,pos="-1.030,-0.171"]
island_geomorphology [exposure,pos="-0.970,1.724"]
lagoon_size [pos="-1.086,1.471"]
latitude [pos="-1.104,1.716"]
longitude [pos="-1.090,1.884"]
other_algae [pos="-0.825,0.260"]
pop_dens [pos="-0.711,0.242"]
population_size [pos="-0.720,0.716"]
relief [pos="-1.123,0.674"]
season [pos="-0.720,1.543"]
ave_npp -> crustose_coraline_algae
ave_npp -> hard_coral
ave_temp -> ave_npp
ave_temp -> crustose_coraline_algae
ave_temp -> hard_coral
ave_temp -> other_algae
crustose_coraline_algae -> hard_coral
emerged_land_area -> pop_dens
island_geomorphology -> ave_npp
island_geomorphology -> emerged_land_area
island_geomorphology -> lagoon_size
other_algae -> hard_coral
pop_dens -> hard_coral
population_size -> pop_dens
relief -> hard_coral
season -> ave_temp
}')

# EVALUATE THE D-SEPARATION IMPLICATIONS OF THE DAG ####
test <- dagitty::localTests(x = DAG,
                            data = ddat,
                            abbreviate.names = FALSE)
write.csv(x = test,
          file = here("Nat_resources", "dag_inconsistencies_all_DAG3.csv"),
          row.names = TRUE)


# SUBSET DATA BASED ON CORRELATION VALUE OR P VALUE ####
# can define pass or fail based on thresholds of p-value (meh) or correlation coefficient (sure)
testf2 <- subset(test, estimate >= 0.3 | estimate <= -0.3)
testf2 <- testf2[rev(order(abs(testf2$estimate))), ] # Sort test results by effect size

# SHOW THE INDEPENDENCIES THAT FAILED ####
write.csv(x = testf2,
          file = here("Nat_resources", "dag_inconsistencies_0.3_DAG3.csv"),
          row.names = TRUE)
# Add notes, are connections logical?
# How to interpret these results?

# Plot independencies ####
res <- test[order(abs(test$estimate)), ] # Sort test results by effect size
dev.off()
dagitty::plotLocalTestResults(tail(res, 20)) # Plot 20 results with largest effect size
