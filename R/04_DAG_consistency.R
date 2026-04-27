# 04_DAG_consistency.R
# d-separation consistency tests for five candidate DAGs of the
# French Polynesia coral reef system.
#
# Author: Simon Dedman <simondedman@gmail.com>
# Created: 2026-04
# Co-authors / development:
#   - Natalie K (initial dagitty consistency-check workflow; archived
#     legacy scripts 05-08 nat_FPDAG_*.R)
#   - Suchinta Arif (Dalhousie; DAG framework; ORCID 0000-0001-8381-3071)
#
# Replaces four legacy scripts that have been moved to R/archive/:
#   05_nat_FPDAG_consistency_check.R                    (full system, both directions)
#   06_nat_FPDAG_consistency_check_DAG1-tophalf-topdown.R     (DAG1)
#   07_nat_FPDAG_consistency_check_DAG2-tophalf-bottomup.R    (DAG2)
#   08_nat_FPDAG_consistency_check_DAG3-bottomhalf-bottomup.R (DAG3)
#
# DAG variants tested:
#   full_TD       - full reef system (sharks + fish + benthos + environment),
#                   sharks as exposure, hard_coral as outcome (top-down)
#   full_BU       - full reef system, ave_temp as exposure, reef_sharks as
#                   outcome (bottom-up; "BU arrows up")
#   tophalf_TD    - upper trophic chain only (species-level sharks ->
#                   ambush/pursuit piscivores -> grazer/scraper/etc. ->
#                   benthos), grey_reef_shark as exposure, hard_coral as
#                   outcome (DAG1)
#   tophalf_BU    - upper trophic chain only, hard_coral as exposure,
#                   grey_reef_shark as outcome (DAG2)
#   bottomhalf_BU - environmental drivers only (latitude/island_geomorphology/
#                   temp/npp -> benthos), island_geomorphology as exposure,
#                   hard_coral as outcome (DAG3)
#
# All five DAGs are systematically related: full_TD/full_BU are the
# unified-system tests in opposing directions; tophalf and bottomhalf are
# subset checks that localise inconsistencies to specific parts of the
# trophic structure. The reef-sharks-vs-temperature pair fails the
# d-separation test in full_BU and is explored further as a supplementary
# linear regression in 08_supplementary_plots.R.
#
# Inputs:
#   NFF_data/ReefWideBRUVUVC.csv     - reef-aggregated input (column
#                                      names already match DAG variable
#                                      names for 17 of 19 columns)
#   NFF_data/FPDAG_match_table.csv   - legacy column-rename lookup; nearly
#                                      identity for current data, retained
#                                      for compatibility
#
# Outputs (written to Results/DAG/consistency/):
#   dag_inconsistencies_<dag>_all.csv  - all d-separation test results
#   dag_inconsistencies_<dag>_0.3.csv  - filtered to |r| >= 0.3, sorted
#                                        by absolute effect size
#
# Standardisation uses Gelman's "centre then divide by 2 SD" form,
# retained from the legacy scripts for comparability.
#
# Packages:
#   dplyr, dagitty, DataCombine, here

library(dplyr)
library(dagitty)
library(DataCombine)
library(here)

# Output directory ####
out_dir <- here("Results", "DAG", "consistency")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# Standardisation function ####
stdize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE) * 2)
}


# Load data and apply legacy column-name mapping ####
dat <- read.csv(here("NFF_data", "ReefWideBRUVUVC.csv"))
name_match <- read.csv(here("NFF_data", "FPDAG_match_table.csv"))
var_names <- data.frame(current_name = colnames(dat))
var_names$corrected_name <- DataCombine::FindReplace(
  var_names,
  "current_name",
  name_match,
  from = "name_in_data",
  to = "name_in_dag",
  exact = TRUE,
  vector = FALSE
)
colnames(dat) <- as.character(var_names$corrected_name$current_name)


# Five candidate DAGs ####
# full_TD: full reef system, top-down (sharks -> coral) ####
full_TD <- dagitty(
  'dag {
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
}'
)

# full_BU: full reef system, bottom-up ("BU arrows up") ####
full_BU <- dagitty(
  'dag {
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
}'
)

# tophalf_TD: upper trophic chain, top-down (DAG1) ####
tophalf_TD <- dagitty(
  'dag {
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
}'
)

# tophalf_BU: upper trophic chain, bottom-up (DAG2) ####
tophalf_BU <- dagitty(
  'dag {
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
}'
)

# bottomhalf_BU: environmental drivers only, bottom-up (DAG3) ####
bottomhalf_BU <- dagitty(
  'dag {
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
}'
)

dags <- list(
  full_TD = full_TD,
  full_BU = full_BU,
  tophalf_TD = tophalf_TD,
  tophalf_BU = tophalf_BU,
  bottomhalf_BU = bottomhalf_BU
)


# Run d-separation tests for each DAG ####
for (dag_name in names(dags)) {
  cat("\n=== Testing DAG:", dag_name, "===\n")
  DAG <- dags[[dag_name]]

  # Subset data to DAG variables (latent vars are simply absent from data)
  ddat <- dat[, colnames(dat) %in% names(DAG), drop = FALSE]

  # Standardise numeric variables (Gelman 2 SD form)
  num_cols <- vapply(ddat, is.numeric, logical(1))
  ddat[num_cols] <- lapply(ddat[num_cols], stdize)

  # Drop rows with any NAs (mostly affects partially-observed variables
  # that the original analyses also dropped row-wise)
  ddat <- ddat[complete.cases(ddat), , drop = FALSE]

  # Run d-separation tests
  test <- dagitty::localTests(
    x = DAG,
    data = ddat,
    abbreviate.names = FALSE
  )

  # Write all results
  write.csv(
    test,
    file = file.path(
      out_dir,
      paste0("dag_inconsistencies_", dag_name, "_all.csv")
    ),
    row.names = TRUE
  )

  # Filter to |estimate| >= 0.3, sort by absolute effect size, write
  testf <- subset(test, estimate >= 0.3 | estimate <= -0.3)
  if (nrow(testf) > 0) {
    testf <- testf[rev(order(abs(testf$estimate))), ]
    write.csv(
      testf,
      file = file.path(
        out_dir,
        paste0("dag_inconsistencies_", dag_name, "_0.3.csv")
      ),
      row.names = TRUE
    )
    cat(
      "  testable independencies:",
      nrow(test),
      "; |r| >= 0.3:",
      nrow(testf),
      "\n"
    )
  } else {
    cat(
      "  testable independencies:",
      nrow(test),
      "; |r| >= 0.3: 0 (no filtered file written)\n"
    )
  }
}

cat("\nAll DAG consistency tests complete. Per-DAG CSVs in:\n  ", out_dir, "\n")
