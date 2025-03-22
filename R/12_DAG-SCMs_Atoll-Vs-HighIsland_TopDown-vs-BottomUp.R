# loo (leave one out) expected log pointwise predictive density metrics
# For SCMs with 2 topographies and 2 directions from scripts 10_Suchinta-DAG.R & 11_Si-DAG-bottomup.R
# Simon Dedman, 2025-03-20 simondedman@gmail.com

library(here)

# ?loo:
# point estimates and standard errors of the:
# expected log pointwise predictive density (elpd_loo),
# the effective number of parameters (p_loo) and the
# LOO information criterion looic (-2 * elpd_loo, i.e. converted to deviance scale).
#
# Model with lower elpd_loo generally preferred re: predictive accuracy.
# SE elpd_loo can help assess whether diff between 2 models is meaningful.

resultsdf <- data.frame(
  topo = character(),
  direction = character(),
  model = character(),
  elpd_loo = numeric(),
  elpd_loo_SE = numeric(),
  p_loo = numeric(),
  p_loo_SE = numeric(),
  looic = numeric(),
  looic_SE = numeric(),
  stringsAsFactors = FALSE
)

counter <- 1
for (topo in c("Atolls", "HighIslands", "All")) {
  # topo <- "Atolls"
  for (direction in c("TopDown", "BottomUp")) {
    # direction <- "TopDown"
    print(paste0(
      "Model: ",
      counter,
      "/6; Topography: ",
      topo,
      "; Direction: ",
      direction
    ))
    loopmodelslist <- readRDS(here(
      "Results",
      "DAG",
      paste0("models_list_", topo, "_", direction, ".Rds")
    ))
    for (loopmodel in 1:length(loopmodelslist)) {
      # loopmodel <- 5
      tmp <- loo(loopmodelslist[[loopmodel]], k_threshold = 0.7)
      resultsdf <- rbind(
        resultsdf,
        data.frame(
          topo = topo,
          direction = direction,
          model = deparse1(loopmodelslist[[loopmodel]]$formula),
          elpd_loo = tmp$estimates[1, 1],
          elpd_loo_SE = tmp$estimates[1, 2],
          p_loo = tmp$estimates[2, 1],
          p_loo_SE = tmp$estimates[2, 2],
          looic = tmp$estimates[3, 1],
          looic_SE = tmp$estimates[3, 2]
        )
      )
    }
    counter <- counter + 1
  }
}
rm(tmp, counter, topo, direction, loopmodelslist, loopmodel)
readr::write_csv(resultsdf, here("Results", "DAG", "loo_results.csv"))
