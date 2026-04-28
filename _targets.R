# _targets.R
# targets pipeline definition for FIU-SharkFishCoral-FrenchPoly.
#
# Run the full pipeline with:    targets::tar_make()
# Visualise dependencies with:   targets::tar_visnetwork()
# Inspect a target's output:     targets::tar_read(<target_name>)
# Reset the cache:               targets::tar_destroy()
#
# Coarse-grained pipeline: each numbered analysis script in R/ becomes
# one target that sources the script and tracks its main output files
# via format = "file". Reviewers get a single-command entry point
# (tar_make()) and skip-if-unchanged behaviour at script granularity.
# Re-running a downstream target only requires its upstream file
# fingerprints to be unchanged on disk.
#
# This pipeline expects the data archive to have been downloaded from
# FIU Dataverse (DOI: 10.34703/gzx1-9v95/1L510G) into NFF_data/ at the
# project root. See data/README.md for the full file manifest.
#
# Note on script 05 (Bayesian SCM fits): a full tar_make() will run
# the brms fits if the model_list_*_spline.Rds files are absent or
# stale; this takes several hours. To skip the fit, download the
# pre-fitted model lists from the Dataverse archive into
# Results/DAG/, then run tar_make(starts_with("dag_results")) or
# similar to compute only the downstream targets.

library(targets)
library(here)

tar_option_set(
  packages = c("here", "dplyr"),
  format = "rds"
)

list(
  # === Stage 0: raw input files ====================================
  # File-target so any change to the raw data invalidates downstream
  # targets. Listed paths must all exist before tar_make() is run.
  tar_target(
    raw_inputs,
    c(
      here("NFF_data", "fixed_bethic_uvc_final_2023_02_26.csv"),
      here("NFF_data", "fish.spp.list.fn.gps.fixed.csv"),
      here("NFF_data", "wide.df1.ch3.60min.2023.01.csv"),
      here("NFF_data", "wide.df1.teleosts.csv"),
      here("NFF_data", "site_order_df.csv"),
      here("NFF_data", "BRUV.csv"),
      here("NFF_data", "UVC.csv"),
      here("NFF_data", "ReefWideBRUVUVC.csv"),
      here("NFF_data", "FPDAG_match_table.csv"),
      here("NFF_data", "Trophic_Categorisation_Desbiens.csv"),
      here(
        "NFF_data",
        "BenthicSurveyDataSheets",
        "Algae_breakdown_2025_07.xlsx"
      )
    ),
    format = "file"
  ),

  # === Stage 1: data preparation ===================================
  # 01_data_prep.R aggregates raw benthic UVC, fish UVC, and shark
  # BRUV records to per-reef analysis tables. Also generates SM
  # Fig 32 (functional group trophic levels).
  tar_target(
    reef_data,
    {
      # Reference raw_inputs to establish dependency:
      raw_inputs
      source(here("R", "01_data_prep.R"))
      c(
        here("NFF_data", "ch4_reef_wide_df2.RData"),
        here("NFF_data", "ch4_reef_wide_df2.csv"),
        here("NFF_data", "ch4_atoll_reef_wide_df2.RData"),
        here("NFF_data", "ch4_atoll_reef_wide_df2.csv"),
        here("NFF_data", "ch4_island_reef_wide_df2.RData"),
        here("NFF_data", "ch4_island_reef_wide_df2.csv"),
        here("NFF_data", "survey.wide.df2.RData"),
        here("NFF_data", "FnGpTrophicLevels.png")
      )
    },
    format = "file"
  ),

  # === Stage 2: FishBase trophic levels ============================
  # 02_trophic_levels.R queries FishBase for mean trophic levels per
  # functional group. Output is console-only, so we return a sentinel
  # string and rely on the message channel.
  tar_target(
    trophic_levels,
    {
      raw_inputs
      source(here("R", "02_trophic_levels.R"))
      "trophic levels computed; see console output"
    }
  ),

  # === Stage 3: BRT models =========================================
  # 03_BRT_models.R fits gbm.auto BRTs for chi_benthos_percent and
  # the four log-biomass groups, separately for All / Atolls /
  # HighIslands subsets. Outputs many files under Results/BRT/.
  tar_target(
    brt_outputs,
    {
      reef_data
      source(here("R", "03_BRT_models.R"))
      list.files(
        here("Results", "BRT"),
        recursive = TRUE,
        full.names = TRUE
      )
    },
    format = "file"
  ),

  # === Stage 4: DAG consistency ====================================
  # 04_DAG_consistency.R runs d-separation tests on five candidate
  # DAGs (full_TD, full_BU, tophalf_TD, tophalf_BU, bottomhalf_BU)
  # against ReefWideBRUVUVC.csv. Produces 10 inconsistency CSVs in
  # Results/DAG/consistency/.
  tar_target(
    dag_consistency,
    {
      raw_inputs
      source(here("R", "04_DAG_consistency.R"))
      list.files(
        here("Results", "DAG", "consistency"),
        pattern = "dag_inconsistencies_.*\\.csv$",
        full.names = TRUE
      )
    },
    format = "file"
  ),

  # === Stage 5: Bayesian SCM fits (SLOW: ~hours) ===================
  # 05_DAG_bayesian_fits.R fits 44 brms models (4 scenarios x 11
  # models each) with cubic shrinkage splines and Student-t errors.
  # Outputs four model-list .Rds files plus the posteriors list.
  # If the .Rds files were downloaded from the Dataverse archive,
  # this target will be considered up-to-date and skipped.
  tar_target(
    bayesian_fits,
    {
      reef_data
      source(here("R", "05_DAG_bayesian_fits.R"))
      c(
        here(
          "Results",
          "DAG",
          "models_list_Atolls_BottomUp_spline.Rds"
        ),
        here("Results", "DAG", "models_list_Atolls_TopDown_spline.Rds"),
        here(
          "Results",
          "DAG",
          "models_list_HighIslands_BottomUp_spline.Rds"
        ),
        here(
          "Results",
          "DAG",
          "models_list_HighIslands_TopDown_spline.Rds"
        ),
        here("Results", "DAG", "posteriors_list_spline.Rds")
      )
    },
    format = "file"
  ),

  # === Stage 6: SCM results processing & figures ===================
  # 06_DAG_results.R consumes the brms model lists, runs LOO
  # comparisons, and produces ~30 outputs in Results/DAG/ (Bayesian
  # R^2 distributions, posterior intervals, stacking weights, etc.).
  # Includes Main Fig 2 and Fig 3 candidates.
  tar_target(
    dag_results,
    {
      bayesian_fits
      source(here("R", "06_DAG_results.R"))
      list.files(
        here("Results", "DAG"),
        pattern = "_spline\\.(csv|png|Rds|xlsx)$",
        full.names = TRUE
      )
    },
    format = "file"
  ),

  # === Stage 7: DAG network plots ==================================
  # 07_DAG_network_plots.R renders Main Fig 4 (DAG networks for
  # Atolls and HighIslands) using effect-size and posterior-interval
  # CSVs from stage 6.
  tar_target(
    dag_network_plots,
    {
      dag_results
      source(here("R", "07_DAG_network_plots.R"))
      list.files(
        here("Results", "DAG"),
        pattern = "DAG_(Atolls|HighIslands)_spline\\.png$",
        full.names = TRUE
      )
    },
    format = "file"
  ),

  # === Stage 8: supplementary plots ================================
  # 08_supplementary_plots.R produces 12 sections of supplementary
  # diagnostics. Spans column plots, lm scatter plots, focused
  # diagnostics, the predator-teleost cross-check, the Other Algae
  # independence test, and the SST/reef-sharks lm.
  tar_target(
    supplementary_plots,
    {
      reef_data
      raw_inputs
      source(here("R", "08_supplementary_plots.R"))
      c(
        list.files(
          here("Results", "ColumnPlots"),
          full.names = TRUE
        ),
        list.files(
          here("Results", "Boxplots"),
          full.names = TRUE
        ),
        list.files(
          here("Results", "Scatterplots"),
          full.names = TRUE
        ),
        list.files(
          here("Results", "LMplots_DAG"),
          full.names = TRUE
        ),
        list.files(
          here("Results", "LMplots_BRT"),
          recursive = TRUE,
          full.names = TRUE
        ),
        here("NFF_data", "Mean_biomass_g_per_m2_per_reef.csv"),
        here("NFF_data", "scatter_pred_tel_plot1.png")
      )
    },
    format = "file"
  )
)
