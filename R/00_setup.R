# 00_setup.R
# Common setup for the FIU-SharkFishCoral-FrenchPoly analysis pipeline.
#
# Author: Simon Dedman <simondedman@gmail.com>
# Created: 2026-04
#
# Source this script (or have `_targets.R` source it) before running any
# of the numbered analysis scripts 01-08. It:
#
#   1. Anchors `here::here()` to the project root via the .Rproj file.
#   2. Sets a fixed seed for any stochastic step (the Bayesian fits in
#      05 set their own per-model seed = 123 inside `brm()`; this seed
#      governs anything that uses base R's RNG).
#   3. Lists required packages and provides a `check_packages()` helper
#      that errors with a clear message if any are missing. Once `renv`
#      is set up (planned for v1.0), `renv::restore()` replaces this.
#   4. Defines `stdize()`, the Gelman "centre then divide by 2 SD"
#      standardisation function used in 04_DAG_consistency.R. The
#      Bayesian fit scripts use a different (0-1 rescaled) variant
#      defined inside those scripts.

library(here)

# Anchor here() to the project root ####
# Detects the .Rproj file at the top of the project; downstream scripts
# can then use here("NFF_data", "...") regardless of working directory.
here::i_am("FIU-SharkFishCoral-FrenchPoly.Rproj")


# Seed ####
set.seed(123)


# Required packages ####
# Listed by stage so they can be cross-referenced against script imports.
# When this list grows, prefer `renv::snapshot()` to pin versions.
required_packages <- list(
  data_prep   = c("tidyverse", "plotly"),
  trophic     = c("rfishbase"),
  brt         = c("gbm.auto"),
  dag         = c("dagitty", "ggdag", "ggrepel",
                  "igraph", "tidygraph", "ggraph", "reporter"),
  bayesian    = c("brms", "rstan", "loo"),
  supplementary = c("propr", "reshape2", "readxl", "lubridate",
                    "viridis", "ggpubr"),
  pipeline    = c("targets"),
  shared      = c("here", "dplyr", "ggplot2")
)


# Check installed packages ####
check_packages <- function(stages = names(required_packages)) {
  needed <- unique(unlist(required_packages[stages]))
  missing <- needed[!vapply(needed, requireNamespace,
                            logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "The following packages are required but not installed:\n  ",
      paste(missing, collapse = ", "),
      "\n\nInstall via:\n  install.packages(c(",
      paste0("'", missing, "'", collapse = ", "), "))\n",
      "or, once renv is configured, via:\n  renv::restore()",
      call. = FALSE
    )
  }
  invisible(needed)
}


# Standardisation helper (used by 04_DAG_consistency.R) ####
stdize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE) * 2)
}


# Memory ceiling for large brms LOO refits in 05 ####
options(future.globals.maxSize = 1.5 * 1024^3)
