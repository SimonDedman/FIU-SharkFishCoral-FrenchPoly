# 04_DAG_consistency.R
# d-separation consistency tests for three candidate DAGs of the
# French Polynesia coral reef system.
#
# Author: Simon Dedman <simondedman@gmail.com>
# Created: 2026-04
# Co-authors / development:
#   - Natalie Klinard (initial dagitty consistency-check workflow; archived
#     legacy scripts 05-08 nat_FPDAG_*.R; ORCID 0000-0001-7809-9360)
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
#   bottomhalf_BU - environmental drivers only (latitude/island_geomorphology/
#                   temp/npp -> benthos), island_geomorphology as exposure,
#                   hard_coral as outcome (DAG3)
#
# 2026-10-14: the legacy tophalf_TD / tophalf_BU DAGs (DAG1, DAG2; species-level
# upper trophic chain) were dropped: their species-level nodes have no data
# columns at guild resolution, so almost none of their implications were
# testable, and the paper does not use them. Archived scripts 06/07 keep them.
#
# The DAGs are systematically related: full_TD/full_BU are the
# unified-system tests in opposing directions; bottomhalf is a
# subset check that localises inconsistencies to specific parts of the
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
#   dplyr, dagitty, here
#
# Note: legacy scripts used DataCombine::FindReplace for the column-name
# mapping below; replaced here with base-R `match()` since DataCombine
# was archived from CRAN in 2018 and is no longer installable on
# R >= 4.5. The mapping behaviour is identical.

library(dplyr)
library(dagitty)
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
# For each data column, look up its DAG-equivalent name in the lookup
# table; if not found (NA), keep the original column name.
matched_idx <- match(colnames(dat), name_match$name_in_data)
new_names <- ifelse(
  is.na(matched_idx),
  colnames(dat),
  name_match$name_in_dag[matched_idx]
)
colnames(dat) <- new_names


# Three candidate DAGs ####
# full_TD / full_BU: full reef system, from the shared DAG source ####
# 2026-09-24: the two literal dagitty strings that used to sit here were
# IDENTICAL in their edges (full_BU was a copy of full_TD with only the
# exposure/outcome tags swapped, inherited from the legacy script), so the
# "full_BU" results before this date were TD results under another name.
# Both graphs now come from R/utils/dag_directions.R: TD is the latest
# dagitty DAG (7VHv3H73 + invertivores -> crown_of_thorns), BU reverses the
# 13 overlay (faunal/floral) edges listed in R/dags/overlay_edges_TD-direction.csv.
# Latent nodes stay in the graph; dagitty tests only implications among
# observed variables.
source(here("R", "utils", "dag_directions.R"))
full_TD <- dag_td()
full_BU <- dag_bu(full_TD)

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
  bottomhalf_BU = bottomhalf_BU
)


# Run d-separation tests for each DAG ####
for (dag_name in names(dags)) {
  cat("\n=== Testing DAG:", dag_name, "===\n")
  DAG <- dags[[dag_name]]

  # Any non-latent node with no data column (e.g. zooplankton in the full
  # DAGs) is treated as latent, so dagitty tests only observed implications
  missing_nodes <- setdiff(setdiff(names(DAG), latents(DAG)), colnames(dat))
  if (length(missing_nodes)) {
    cat("  no data column, treated as latent:", missing_nodes, "\n")
    latents(DAG) <- c(latents(DAG), missing_nodes)
  }

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
