library(dplyr)
library(dagitty)
library(base64enc)
library(readr)
library(DataCombine)

rm(list = ls())
wd <- "~/Library/CloudStorage/OneDrive-DalhousieUniversity/Documents/Manuscripts/French Polynesia Reef Sharks/DAG consistency checks"
wd <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/NFF Data code/"
setwd(wd)


# DOWNLOAD THE DAG ####
DAG <- dagitty('dag {
ambush_piscivore [pos="-0.876,-1.268"]
blacktip_reef_shark [pos="-0.970,-1.278"]
browser [latent,pos="-0.771,-0.705"]
coral_spawning [latent,pos="-1.230,0.373"]
crown_of_thorns [latent,pos="-1.137,-0.402"]
crustose_coraline_algae [pos="-0.923,0.349"]
grazer [pos="-0.893,-0.705"]
grey_reef_shark [outcome,pos="-1.206,-1.756"]
hard_coral [exposure,pos="-1.030,-0.171"]
invert [pos="-1.112,-0.074"]
invertivore [pos="-1.108,-0.673"]
offshore_prey [latent,pos="-1.301,-0.762"]
other_algae [pos="-0.825,0.260"]
other_offshore_prey_proxies [latent,pos="-1.283,-0.470"]
planktivore [pos="-1.203,-0.701"]
pop_dens [pos="-0.711,0.242"]
pursuit_piscivore [pos="-0.779,-1.271"]
scraper [pos="-1.003,-0.698"]
sicklefin_lemon_shark [pos="-1.095,-2.097"]
silvertip_shark [latent,pos="-0.753,-1.802"]
tawny_nurse_shark [pos="-1.067,-1.285"]
transient_pelagic_sharks [pos="-0.933,-2.078"]
whitetip_reef_shark [pos="-1.168,-1.385"]
zooplankton [pos="-1.210,0.096"]
ambush_piscivore -> grey_reef_shark
ambush_piscivore -> sicklefin_lemon_shark
ambush_piscivore -> silvertip_shark
ambush_piscivore -> transient_pelagic_sharks
blacktip_reef_shark -> sicklefin_lemon_shark
blacktip_reef_shark -> transient_pelagic_sharks
browser -> ambush_piscivore
browser -> blacktip_reef_shark
browser -> grey_reef_shark
browser -> pursuit_piscivore
browser -> silvertip_shark
browser -> tawny_nurse_shark
browser -> whitetip_reef_shark
coral_spawning -> planktivore
crustose_coraline_algae -> grazer
crustose_coraline_algae -> hard_coral
crustose_coraline_algae -> scraper
grazer -> ambush_piscivore
grazer -> blacktip_reef_shark
grazer -> grey_reef_shark
grazer -> pursuit_piscivore
grazer -> silvertip_shark
grazer -> tawny_nurse_shark
grazer -> whitetip_reef_shark
grey_reef_shark -> sicklefin_lemon_shark
grey_reef_shark -> transient_pelagic_sharks
hard_coral -> coral_spawning
hard_coral -> crown_of_thorns
hard_coral -> scraper
invert -> invertivore
invertivore -> ambush_piscivore
invertivore -> blacktip_reef_shark
invertivore -> grey_reef_shark
invertivore -> pursuit_piscivore
invertivore -> silvertip_shark
invertivore -> tawny_nurse_shark
invertivore -> whitetip_reef_shark
offshore_prey -> grey_reef_shark
offshore_prey -> sicklefin_lemon_shark
offshore_prey -> silvertip_shark
offshore_prey -> transient_pelagic_sharks
other_algae -> browser
other_algae -> grazer
other_algae -> hard_coral
other_algae -> invertivore
other_algae -> scraper
other_offshore_prey_proxies -> offshore_prey
planktivore -> ambush_piscivore
planktivore -> blacktip_reef_shark
planktivore -> grey_reef_shark
planktivore -> pursuit_piscivore
planktivore -> silvertip_shark
planktivore -> tawny_nurse_shark
planktivore -> whitetip_reef_shark
pop_dens -> ambush_piscivore
pop_dens -> browser
pop_dens -> grazer
pop_dens -> hard_coral
pop_dens -> invertivore
pop_dens -> offshore_prey
pop_dens -> planktivore
pop_dens -> pursuit_piscivore
pop_dens -> scraper
pursuit_piscivore -> grey_reef_shark
pursuit_piscivore -> sicklefin_lemon_shark
pursuit_piscivore -> silvertip_shark
pursuit_piscivore -> transient_pelagic_sharks
scraper -> ambush_piscivore
scraper -> blacktip_reef_shark
scraper -> grey_reef_shark
scraper -> pursuit_piscivore
scraper -> silvertip_shark
scraper -> tawny_nurse_shark
scraper -> whitetip_reef_shark
silvertip_shark -> sicklefin_lemon_shark
silvertip_shark -> transient_pelagic_sharks
tawny_nurse_shark -> sicklefin_lemon_shark
tawny_nurse_shark -> transient_pelagic_sharks
whitetip_reef_shark -> sicklefin_lemon_shark
whitetip_reef_shark -> transient_pelagic_sharks
zooplankton -> planktivore
}')

names(DAG)

###### IMPORT DATA ######

dat <- read.csv(paste(wd, "/ReefWideBRUVUVC.csv", sep = ""))
str(dat)

wd <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/"
setwd(wd)
name_match <- read.csv(paste(wd, "/FPDAG_match_table.csv", sep = ""))
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
"coral_spawning"
"crown_of_thorns"
"offshore_prey"
"other_offshore_prey_proxies"
"zooplankton"
# VARIABLES IN DATA BUT NOT DAG ####
names(ddat)[!names(ddat) %in% names(DAG)]


# Reload DAG omitting latent variables & browser ####
DAG <- dagitty('dag {
ambush_piscivore [pos="-0.876,-1.268"]
blacktip_reef_shark [pos="-0.970,-1.278"]
browser [latent,pos="-0.771,-0.705"]
crustose_coraline_algae [pos="-0.923,0.349"]
grazer [pos="-0.893,-0.705"]
grey_reef_shark [exposure,pos="-1.206,-1.756"]
hard_coral [outcome,pos="-1.030,-0.171"]
invert [pos="-1.103,0.007"]
invertivore [pos="-1.108,-0.673"]
other_algae [pos="-0.825,0.260"]
planktivore [pos="-1.203,-0.701"]
pop_dens [pos="-0.711,0.242"]
pursuit_piscivore [pos="-0.779,-1.271"]
scraper [pos="-1.003,-0.698"]
sicklefin_lemon_shark [pos="-1.095,-2.097"]
silvertip_shark [latent,pos="-0.753,-1.802"]
tawny_nurse_shark [pos="-1.067,-1.285"]
transient_pelagic_sharks [pos="-0.933,-2.078"]
whitetip_reef_shark [pos="-1.168,-1.385"]
ambush_piscivore -> browser
ambush_piscivore -> grazer
ambush_piscivore -> invertivore
ambush_piscivore -> planktivore
ambush_piscivore -> scraper
blacktip_reef_shark -> browser
blacktip_reef_shark -> grazer
blacktip_reef_shark -> invertivore
blacktip_reef_shark -> planktivore
blacktip_reef_shark -> scraper
browser -> other_algae
crustose_coraline_algae -> hard_coral
grazer -> crustose_coraline_algae
grazer -> other_algae
grey_reef_shark -> ambush_piscivore
grey_reef_shark -> browser
grey_reef_shark -> grazer
grey_reef_shark -> invertivore
grey_reef_shark -> planktivore
grey_reef_shark -> pursuit_piscivore
grey_reef_shark -> scraper
invertivore -> invert
invertivore -> other_algae
other_algae -> hard_coral
pop_dens -> ambush_piscivore
pop_dens -> browser
pop_dens -> grazer
pop_dens -> hard_coral
pop_dens -> invertivore
pop_dens -> planktivore
pop_dens -> pursuit_piscivore
pop_dens -> scraper
pursuit_piscivore -> browser
pursuit_piscivore -> grazer
pursuit_piscivore -> invertivore
pursuit_piscivore -> planktivore
pursuit_piscivore -> scraper
scraper -> crustose_coraline_algae
scraper -> hard_coral
scraper -> other_algae
sicklefin_lemon_shark -> ambush_piscivore
sicklefin_lemon_shark -> blacktip_reef_shark
sicklefin_lemon_shark -> grey_reef_shark
sicklefin_lemon_shark -> pursuit_piscivore
sicklefin_lemon_shark -> silvertip_shark
sicklefin_lemon_shark -> tawny_nurse_shark
sicklefin_lemon_shark -> whitetip_reef_shark
silvertip_shark -> ambush_piscivore
silvertip_shark -> browser
silvertip_shark -> grazer
silvertip_shark -> invertivore
silvertip_shark -> planktivore
silvertip_shark -> pursuit_piscivore
silvertip_shark -> scraper
tawny_nurse_shark -> browser
tawny_nurse_shark -> grazer
tawny_nurse_shark -> invertivore
tawny_nurse_shark -> planktivore
tawny_nurse_shark -> scraper
transient_pelagic_sharks -> ambush_piscivore
transient_pelagic_sharks -> blacktip_reef_shark
transient_pelagic_sharks -> grey_reef_shark
transient_pelagic_sharks -> pursuit_piscivore
transient_pelagic_sharks -> silvertip_shark
transient_pelagic_sharks -> tawny_nurse_shark
transient_pelagic_sharks -> whitetip_reef_shark
whitetip_reef_shark -> browser
whitetip_reef_shark -> grazer
whitetip_reef_shark -> invertivore
whitetip_reef_shark -> planktivore
whitetip_reef_shark -> scraper
}')

# EVALUATE THE D-SEPARATION IMPLICATIONS OF THE DAG ####
test <- dagitty::localTests(x = DAG,
                            data = ddat,
                            abbreviate.names = FALSE)
write.csv(x = test,
          file = "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/dag_inconsistencies_all_DAG2.csv",
          row.names = TRUE)


# SUBSET DATA BASED ON CORRELATION VALUE OR P VALUE ####
# can define pass or fail based on thresholds of p-value (meh) or correlation coefficient (sure)
testf2 <- subset(test, estimate >= 0.3 | estimate <= -0.3)
testf2 <- testf2[rev(order(abs(testf2$estimate))), ] # Sort test results by effect size

# SHOW THE INDEPENDENCIES THAT FAILED ####
write.csv(x = testf2,
          file = "/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/Nat resources/dag_inconsistencies_0.3_DAG2.csv",
          row.names = TRUE)
# Add notes, are connections logical?
# How to interpret these results?

# Plot independencies ####
res <- test[order(abs(test$estimate)), ] # Sort test results by effect size
dev.off()
dagitty::plotLocalTestResults(tail(res, 20)) # Plot 20 results with largest effect size
