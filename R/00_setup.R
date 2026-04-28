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
#   3. Documents the required packages by pipeline stage and prints a
#      helpful guidance message if any are missing, pointing the user
#      at `renv::restore()` as the canonical install path.
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
# These are the project's top-level dependencies; transitive dependencies
# are pinned in renv.lock.
required_packages <- list(
  data_prep     = c("tidyverse", "plotly"),
  trophic       = c("rfishbase"),
  brt           = c("gbm.auto"),
  dag           = c("dagitty", "ggdag", "ggrepel",
                    "igraph", "tidygraph", "ggraph", "reporter"),
  bayesian      = c("brms", "rstan", "loo"),
  supplementary = c("propr", "reshape2", "readxl", "lubridate",
                    "viridis", "ggpubr"),
  pipeline      = c("targets"),
  shared        = c("here", "dplyr", "ggplot2")
)


# Verify environment ####
# The canonical install path is `renv::restore()`, which reads the
# project's renv.lock and installs every package at its pinned version
# (including the two GitHub-sourced packages, gbm.auto and propr).
# This block prints a one-line OK or a clear restore prompt; it never
# auto-installs and never errors out, so sourcing 00_setup.R is safe
# even on a fresh clone where nothing is installed yet.
local({
  needed <- unique(unlist(required_packages))
  missing <- needed[!vapply(needed, requireNamespace,
                            logical(1), quietly = TRUE)]
  if (length(missing) == 0) {
    message(
      "00_setup.R: ", length(needed),
      " top-level packages present. Pipeline ready."
    )
  } else {
    message(
      "00_setup.R: ", length(missing), " of ", length(needed),
      " required packages are not installed:\n  ",
      paste(missing, collapse = ", "),
      "\n\nRun the following from the project root to install all\n",
      "pinned dependencies (including the two GitHub-sourced packages):\n",
      "\n  renv::restore()\n\n",
      "If renv itself is missing, first:\n",
      "  install.packages(\"renv\"); renv::restore()"
    )
  }
})


# Standardisation helper (used by 04_DAG_consistency.R) ####
stdize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE) * 2)
}


# Memory ceiling for large brms LOO refits in 05 ####
options(future.globals.maxSize = 1.5 * 1024^3)
