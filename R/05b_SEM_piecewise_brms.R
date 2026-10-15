# 05b_SEM_piecewise_brms.R
# Structural equation models of top-down (TD) vs bottom-up (BU) control,
# one equation per endogenous variable, with whole-model fit and comparison.
#
# Author: Simon Dedman <simondedman@gmail.com>
# Created: 2026-09-24, in response to Science review round 1 (aej8649, R1).
# Co-authors / development: Suchinta Arif (DAG framework), Natalie Klinard
#   (d-separation workflow), Reviewer 1 (framing of the requests below).
#
# Purpose:
#   Replaces the per-path LOO-R2 approach of 05_DAG_bayesian_fits.R (kept for
#   the record) with a conventional piecewise SEM. Reviewer points addressed:
#     R1,4   two separate DAGs (TD and BU derived from one file; see
#            R/utils/dag_directions.R), no double-headed arrows
#     R1,6   overall model fit: d-separation basis set + Fisher's C
#     R1,7   the omitted benthos -> predator paths are among the tested
#            independence claims and are reported separately
#     R1,8   sub-model fitting stated explicitly (equations table written out)
#     R1,9   each endogenous variable appears ONCE, with all its DAG parents,
#            so no duplicate models and coefficients are partial
#     R1,10  regression coefficients (standardised) with CIs, frequentist and
#            Bayesian, per path
#     R1,11  linear paths on transformed data; splines kept as a sensitivity
#            check only
#     R1,12  TD vs BU compared as whole models by BIC (Shipley & Douma 2020)
#     R2,16  invertivores -> crown_of_thorns -> hard_coral is in the DAG and
#            enters hard_coral's equation as a latent-mediated path
#
# Equation rule (n = 24 reefs, so parsimony matters):
#   Endogenous set = the 9 biotic nodes (2 shark groups, 4 teleost guilds, 3
#   benthos classes). Each equation's predictors are the node's DIRECT observed
#   DAG parents, plus observed BIOTIC nodes that reach it only through latent
#   nodes (e.g. invertivores -> crown_of_thorns -> hard_coral). Latent-mediated
#   paths from environmental nodes are dropped: their influence is carried by
#   the environmental covariates that are direct parents (NPP, temperature,
#   population density, relief). Environment -> environment equations (e.g.
#   geomorphology -> lagoon size) are not fitted: those links are undisputed and
#   would make geomorphology both a predictor and the grouping variable.
#   Pairs of endogenous nodes sharing a latent parent get correlated errors
#   (%~~%) rather than a tested independence claim.
#
# Stages (each writes to Results/SEM/; brms fits are cached per equation so a
# stopped run resumes):
#   A  data prep: log1p on abundances/biomasses/pop.dens, z-score everything
#   B  piecewiseSEM: pooled psem for TD and BU, d-sep table, Fisher's C,
#      standardised coefficients, multigroup (atoll vs high island), and the
#      Shipley & Douma generalised log-likelihood / BIC comparison. Seconds.
#   C  brms linear: y ~ (parents) * geomorphology, gaussian, normal(0,1) priors
#      on standardised slopes -> posterior slopes per geomorphology per path
#   D  brms spline sensitivity: y ~ s(parent, k=3, bs="cs") ..., student-t,
#      pooled; LOO compared with a matching pooled linear fit
#   E  tables + coefficient plot (labels built from the DAG edge list)
#
# Inputs:
#   NFF_data/ch4_reef_wide_df2.RData
#   R/dags/FP_DAG_TopDown_2026-09-24_from7VHv3H73_plusInvertivoresCOTS.dag
#   R/dags/overlay_edges_TD-direction.csv   (via R/utils/dag_directions.R)
#
# Outputs (Results/SEM/):
#   equations_<TD|BU>.csv, correlated_errors_<TD|BU>.csv
#   dsep_<TD|BU>.csv, dsep_benthos_to_predators.csv, fisherC.csv
#   coefs_pooled_<TD|BU>.csv, multigroup_<TD|BU>_*.csv
#   loglik_bic_comparison.csv
#   brms/<TD|BU>_<response>_<linear|spline|linear_pooled>.Rds
#   brms_slopes_by_geomorphology.csv, loo_linear_vs_spline.csv
#   <date>_SEM_coefficients_TDvsBU.png, progress.txt
#
# Runtime:
#   Stage B < 1 min. Stages C-D: 3 brms fits per equation x 17 equations = 51
#   fits at ~1.5 min each (4 chains on 4 cores, Stan compile included):
#   MEASURED 2026-09-24 on a 12-core desktop: 76 min of fitting (42 + 34 min
#   across two runs; the first was OOM-killed at fit 27 while other jobs ran)
#   plus ~1 min for stage E. Peak memory now ~1 GB (fits are not held in
#   memory). Fully resumable from Results/SEM/brms/.
#
# Packages: dagitty, piecewiseSEM, nlme, brms, loo, here, dplyr, tidyr, ggplot2, readr

suppressPackageStartupMessages({
  library(dagitty)
  library(piecewiseSEM)
  library(nlme) # attached, not nlme::: dSep() rebuilds lme calls by name
  library(brms)
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(ggtext)
})
source(here("R", "utils", "dag_directions.R"))

run_piecewise <- TRUE
run_brms_linear <- TRUE
run_brms_spline <- TRUE
brms_iter <- 2000
brms_seed <- 123
brms_cores <- 4

# Variants (2026-09-25, Simon's Q1/Q4 decisions): 2 DAGs x 3 hard_coral options ####
#   Rscript R/05b_SEM_piecewise_brms.R --variant=<name> [--no-brms]
#   original  = TD DAG 7VHv3H73 + invertivores -> COTS
#   revised   = original + ave_npp/ave_temp -> each prey guild + a latent
#               "unmeasured_guild_driver" -> the three guilds (which gives the
#               guilds correlated errors), after the d-separation failures of
#               the original (Results/SEM/dsep_*.csv)
#   coral9    = hard_coral keeps all 9 DAG-implied predictors
#   coral7lat = drops the two latent-mediated ones (invertivores, planktivores)
#   coral7env = drops pop_dens and relief from hard_coral only
# "original" writes to Results/SEM/ (as before); others to Results/SEM/variants/<name>/.
dag_original <- "FP_DAG_TopDown_2026-09-24_from7VHv3H73_plusInvertivoresCOTS.dag"
dag_revised <- "FP_DAG_TopDown_2026-09-25_revised_npp-temp-guilds_latentGuildDriver.dag"
VARIANTS <- list(
  original = list(dag = dag_original, coral_drop = character(0)),
  original_coral7lat = list(dag = dag_original, coral_drop = c("invertivores", "planktivores")),
  original_coral7env = list(dag = dag_original, coral_drop = c("pop_dens", "relief")),
  revised = list(dag = dag_revised, coral_drop = character(0)),
  revised_coral7lat = list(dag = dag_revised, coral_drop = c("invertivores", "planktivores")),
  revised_coral7env = list(dag = dag_revised, coral_drop = c("pop_dens", "relief")),
  # 2026-10-03, exploratory (stage B only so far): the revised DAG's remaining
  # failing claims added as edges, to show Simon what each would buy.
  #   revised2 = revised + pop_dens -> reef_sharks + ave_temp -> reef_sharks
  #   revised3 = revised2 + ave_npp -> other_algae
  revised2 = list(dag = "FP_DAG_TopDown_2026-10-03_revised2_plus-popdens-temp-to-reefsharks.dag", coral_drop = character(0)),
  revised3 = list(dag = "FP_DAG_TopDown_2026-10-03_revised3_plus-npp-to-otheralgae.dag", coral_drop = character(0))
)
cli_args <- commandArgs(trailingOnly = TRUE)
variant <- sub("^--variant=", "", grep("^--variant=", cli_args, value = TRUE))
if (!length(variant)) variant <- "original"
stopifnot(variant %in% names(VARIANTS))
V <- VARIANTS[[variant]]
if ("--no-brms" %in% cli_args) run_brms_linear <- run_brms_spline <- FALSE

# --island-re (2026-10-14, Simon's decision after the revised2 refute-check):
# every equation gets a random intercept for island. The 24 reefs come from 12
# islands (mostly 2 reefs each; Tahiti 3, Marutea 1), so without it reefs on one
# island count as independent and p-values / CIs are optimistic. Geomorphology
# is an island-level property: 7 atoll islands vs 5 high islands.
# piecewiseSEM side uses nlme::lme (supported by psem/dSep without lmerTest);
# brms side adds (1 | island). Output goes to <variant>_islandRE/.
island_re <- "--island-re" %in% cli_args
run_name <- if (island_re) paste0(variant, "_islandRE") else variant
# MEASURED 2026-10-14: at brms defaults (adapt_delta 0.8, 2000 iter) the island-RE
# fits gave 125 divergences in 24 of 51 fits (vs 80 in 15, splines only, without
# the RE), max Rhat 1.028, min ESS ratio 0.022: the island SD (12 islands, 2 reefs
# each) makes a funnel. Tighter adaptation and longer chains for RE runs only, so
# reef-level runs stay byte-comparable with their caches.
brms_control <- if (island_re) list(adapt_delta = 0.99, max_treedepth = 12) else NULL
if (island_re) brms_iter <- 4000

out_dir <- if (run_name == "original") here("Results", "SEM") else here("Results", "SEM", "variants", run_name)
brms_dir <- file.path(out_dir, "brms")
dir.create(brms_dir, showWarnings = FALSE, recursive = TRUE)
progress_file <- file.path(out_dir, "progress.txt")
progress <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(...))
  cat(msg, "\n")
  cat(msg, "\n", file = progress_file, append = TRUE)
}
cat("", file = progress_file)
progress("START 05b  variant=", variant, "  island_re=", island_re, "  dag=", V$dag,
         "  coral_drop=", paste(V$coral_drop, collapse = ","))
writeLines(c(variant, V$dag, paste(V$coral_drop, collapse = ","), paste0("island_re=", island_re)),
           file.path(out_dir, "variant.txt"))

# Stage A: data ####
# DAG node -> data column (17 observed nodes)
colmap <- c(
  sicklefin_lemon_sharks = "sicklefin_lemon_sharks",
  reef_sharks = "reef_sharks",
  piscivores = "biomass_g_per_m2_Piscivore",
  herbivores = "biomass_g_per_m2_Herbivore",
  invertivores = "biomass_g_per_m2_Invertivore",
  planktivores = "biomass_g_per_m2_Planktivore",
  hard_coral = "Hard.Coral",
  crustose_coraline_algae = "CCA",
  other_algae = "Other.Algae",
  pop_dens = "pop.dens",
  relief = "Relief",
  ave_npp = "ave_npp",
  ave_temp = "ave_temp",
  lagoon_size = "lagoon.size",
  emerged_land_area = "Emerged.land.area",
  island_geomorphology = "topo",
  latitude = "latitude"
)
biotic <- c(
  "sicklefin_lemon_sharks", "reef_sharks", "piscivores", "herbivores",
  "invertivores", "planktivores", "hard_coral", "crustose_coraline_algae",
  "other_algae"
)
log_vars <- c(
  "sicklefin_lemon_sharks", "reef_sharks", "piscivores", "herbivores",
  "invertivores", "planktivores", "pop_dens", "lagoon_size", "emerged_land_area"
)

raw <- readRDS(here("NFF_data", "ch4_reef_wide_df2.RData"))
stopifnot(all(colmap %in% names(raw)))
dat <- as.data.frame(raw[, colmap])
names(dat) <- names(colmap)
dat$reef_name <- raw$reef_name
dat$island <- factor(as.character(raw$site_name)) # 12 islands; grouping for --island-re
stopifnot(nlevels(dat$island) == 12)
dat$geomorphology <- factor(
  ifelse(raw$topo %in% c("open atoll", "closed atoll"), "Atolls", "HighIslands"),
  levels = c("Atolls", "HighIslands")
)
dat$island_geomorphology <- factor(dat$island_geomorphology)
for (v in log_vars) dat[[v]] <- log1p(dat[[v]])
num_vars <- setdiff(names(colmap), "island_geomorphology")
dat_std <- dat
for (v in num_vars) dat_std[[v]] <- as.numeric(scale(dat[[v]]))
stopifnot(nrow(dat_std) == 24, !anyNA(dat_std[, num_vars]))
n_reefs <- nrow(dat_std)
readr::write_csv(dat_std, file.path(out_dir, "sem_data_standardised.csv"))

# Stage A2: equations from the DAG ####
td <- dag_td(V$dag)
bu <- dag_bu(td)
observed <- names(colmap)
latent_nodes <- setdiff(names(td), observed)

# Directed path from x to y whose intermediate nodes are all latent?
latent_mediated <- function(g, x, y) {
  e <- edges(g)
  keep <- c(x, y, latent_nodes)
  e <- e[e$v %in% keep & e$w %in% keep, ]
  if (!nrow(e)) return(FALSE)
  g2 <- dagitty(paste("dag {", paste(e$v, "->", e$w, collapse = "\n"), "}"))
  x %in% names(g2) && y %in% names(g2) && y %in% descendants(g2, x)
}

equations_from_dag <- function(g) {
  e <- edges(g)
  eqs <- list()
  rows <- list()
  for (y in biotic) {
    direct <- intersect(e$v[e$w == y], observed)
    mediated <- setdiff(
      Filter(function(x) latent_mediated(g, x, y), setdiff(biotic, c(y, direct))),
      direct
    )
    preds <- c(direct, mediated)
    if (y == "hard_coral" && length(V$coral_drop)) {
      direct <- setdiff(direct, V$coral_drop); mediated <- setdiff(mediated, V$coral_drop)
      preds <- c(direct, mediated)
    }
    if (!length(preds)) next
    eqs[[y]] <- preds
    rows[[y]] <- data.frame(
      response = y,
      predictor = preds,
      path_type = c(rep("direct", length(direct)), rep("latent-mediated", length(mediated))),
      stringsAsFactors = FALSE
    )
  }
  list(eqs = eqs, table = do.call(rbind, rows))
}

# Endogenous pairs with a shared latent ancestor and no directed path
# between them in the observed projection -> correlated error
correlated_errors <- function(g, eqs) {
  e <- edges(g)
  endo <- names(eqs)
  out <- list()
  for (i in seq_along(endo)) for (j in seq_along(endo)) {
    if (j <= i) next
    a <- endo[i]; b <- endo[j]
    linked <- a %in% eqs[[b]] || b %in% eqs[[a]] ||
      latent_mediated(g, a, b) || latent_mediated(g, b, a)
    if (linked) next
    shared <- Filter(function(l) latent_mediated(g, l, a) && latent_mediated(g, l, b), latent_nodes)
    if (length(shared)) out[[length(out) + 1]] <- data.frame(a = a, b = b, via = paste(shared, collapse = ";"))
  }
  if (length(out)) do.call(rbind, out) else data.frame(a = character(), b = character(), via = character())
}

overlay <- overlay_edges()
overlay_key <- paste(overlay$from, overlay$to)
scenarios <- list(TD = td, BU = bu)
eq_sets <- lapply(scenarios, equations_from_dag)
corr_sets <- Map(function(g, es) correlated_errors(g, es$eqs), scenarios, eq_sets)

for (nm in names(scenarios)) {
  tab <- eq_sets[[nm]]$table
  tab$overlay_path <- paste(tab$predictor, tab$response) %in% overlay_key |
    paste(tab$response, tab$predictor) %in% overlay_key
  tab$n_predictors <- ave(tab$predictor, tab$response, FUN = length)
  readr::write_csv(tab, file.path(out_dir, paste0("equations_", nm, ".csv")))
  readr::write_csv(corr_sets[[nm]], file.path(out_dir, paste0("correlated_errors_", nm, ".csv")))
  progress(nm, ": ", length(eq_sets[[nm]]$eqs), " equations, ",
           nrow(tab), " paths (", sum(tab$overlay_path), " overlay), ",
           nrow(corr_sets[[nm]]), " correlated-error pairs")
}

formula_for <- function(y, preds, interaction = FALSE, re = FALSE) {
  rhs <- paste(preds, collapse = " + ")
  if (interaction) rhs <- paste0("(", rhs, ") * geomorphology")
  if (re) rhs <- paste(rhs, "+ (1 | island)") # brms syntax
  as.formula(paste(y, "~", rhs))
}

# One equation, frequentist: lm, or lme with an island random intercept.
# do.call embeds the data frame in the call: psem re-evaluates model calls,
# and a bare `data = data` there resolves to utils::data().
fit_eq <- function(y, preds, data, interaction = FALSE, method = "REML") {
  if (island_re) {
    do.call("lme", list(fixed = formula_for(y, preds, interaction), random = ~ 1 | island,
                            data = data, method = method))
  } else {
    do.call("lm", list(formula = formula_for(y, preds, interaction), data = data))
  }
}

# Stage B: piecewiseSEM ####
build_psem <- function(eqs, corr, data) {
  parts <- lapply(names(eqs), function(y) fit_eq(y, eqs[[y]], data))
  if (nrow(corr)) {
    for (k in seq_len(nrow(corr))) {
      # %~~% is a piecewiseSEM function (class formula.cerror), not formula syntax
      parts[[length(parts) + 1]] <- eval(parse(text = paste(corr$a[k], "%~~%", corr$b[k])))
    }
  }
  parts$data <- data
  do.call(psem, parts)
}

# Shipley & Douma (2020) generalised log-likelihood: conditional densities of
# endogenous nodes plus marginal normal densities of every other variable in the
# union set, so TD and BU (different endogenous sets) are on the same footing.
generalised_loglik <- function(eqs, data, union_vars) {
  ll <- 0; k <- 0
  for (y in names(eqs)) {
    # ML (not REML) so log-likelihoods are comparable across fixed-effect sets
    f <- fit_eq(y, eqs[[y]], data, method = "ML")
    ll <- ll + as.numeric(logLik(f))
    # parameters: fixed effects + residual variance (+ island variance)
    k <- k + if (island_re) length(nlme::fixef(f)) + 2 else length(coef(f)) + 1
  }
  for (v in setdiff(union_vars, names(eqs))) {
    x <- data[[v]]
    ll <- ll + sum(dnorm(x, mean(x), sd(x) * sqrt((length(x) - 1) / length(x)), log = TRUE))
    k <- k + 2
  }
  c(logLik = ll, k = k, n = nrow(data), BIC = -2 * ll + k * log(nrow(data)), AIC = -2 * ll + 2 * k)
}

if (run_piecewise) {
  fits <- list(); summ <- list(); fisher <- list(); ll_rows <- list()
  union_vars <- unique(unlist(lapply(eq_sets, function(es) c(names(es$eqs), unlist(es$eqs)))))
  for (nm in names(scenarios)) {
    es <- eq_sets[[nm]]
    fits[[nm]] <- build_psem(es$eqs, corr_sets[[nm]], dat_std)
    s <- summary(fits[[nm]], .progressBar = FALSE)
    summ[[nm]] <- s
    fisher[[nm]] <- data.frame(scenario = nm, s$Cstat, n_claims = nrow(s$dTable),
                               AIC_psem = if (is.data.frame(s$AIC)) s$AIC$AIC[1] else NA_real_, row.names = NULL)
    # Export the unrounded dSep() table (summary() rounds p to 4 dp, which
    # made Fisher's C irreproducible from the CSV) with the full claim from
    # basisSet(): "response ~ omitted predictor | conditioning set".
    dt <- dSep(fits[[nm]], .progressBar = FALSE)
    bs <- basisSet(fits[[nm]])
    stopifnot(length(bs) == nrow(dt))
    dt$Full.Claim <- vapply(bs, function(b) {
      paste0(b[2], " ~ ", b[1], if (length(b) > 2) paste0(" | ", paste(b[-(1:2)], collapse = ", ")) else "")
    }, character(1))
    dt$scenario <- nm
    readr::write_csv(dt, file.path(out_dir, paste0("dsep_", nm, ".csv")))
    co <- s$coefficients
    names(co)[names(co) == ""] <- "signif"
    co$scenario <- nm
    co$overlay_path <- paste(co$Predictor, co$Response) %in% overlay_key |
      paste(co$Response, co$Predictor) %in% overlay_key
    readr::write_csv(co, file.path(out_dir, paste0("coefs_pooled_", nm, ".csv")))
    # residual df per equation, pooled and per group
    dfs <- data.frame(
      scenario = nm, response = names(es$eqs),
      n_predictors = lengths(es$eqs),
      resid_df_pooled = n_reefs - lengths(es$eqs) - 1,
      resid_df_atolls = sum(dat_std$geomorphology == "Atolls") - lengths(es$eqs) - 1,
      resid_df_highislands = sum(dat_std$geomorphology == "HighIslands") - lengths(es$eqs) - 1,
      resid_df_interaction = n_reefs - 2 * (lengths(es$eqs) + 1)
    )
    readr::write_csv(dfs, file.path(out_dir, paste0("residual_df_", nm, ".csv")))
    # generalised log-lik / BIC: pooled and per geomorphology
    ll_rows[[paste(nm, "pooled")]] <- data.frame(scenario = nm, subset = "pooled",
                                                 t(generalised_loglik(es$eqs, dat_std, union_vars)))
    for (g in levels(dat_std$geomorphology)) {
      # With --island-re a subset has only 7 (atoll) or 5 (high-island) islands,
      # and pop_dens/temp/NPP are island-level, so equations with more
      # island-level predictors than islands are singular: record NA, not a crash.
      ll <- tryCatch(generalised_loglik(es$eqs, dat_std[dat_std$geomorphology == g, ], union_vars),
                     error = function(e) {
                       progress(nm, " ", g, ": generalised log-lik NOT ESTIMABLE (", conditionMessage(e), ")")
                       c(logLik = NA_real_, k = NA_real_, n = sum(dat_std$geomorphology == g),
                         BIC = NA_real_, AIC = NA_real_)
                     })
      ll_rows[[paste(nm, g)]] <- data.frame(scenario = nm, subset = g, t(ll))
    }
    progress(nm, ": Fisher's C = ", round(s$Cstat$Fisher.C, 2), ", df = ", s$Cstat$df,
             ", p = ", signif(s$Cstat$P.Value, 3), " (", nrow(s$dTable), " claims)")
  }
  readr::write_csv(do.call(rbind, fisher), file.path(out_dir, "fisherC.csv"))
  llt <- do.call(rbind, ll_rows)
  llt <- llt |> group_by(subset) |> mutate(dBIC_vs_best = BIC - min(BIC)) |> ungroup() # NA if either side not estimable
  readr::write_csv(llt, file.path(out_dir, "loglik_bic_comparison.csv"))
  print(as.data.frame(llt))

  # R1,7: the omitted benthos -> predator paths, as tested claims
  benthos <- c("hard_coral", "crustose_coraline_algae", "other_algae")
  predators <- c("sicklefin_lemon_sharks", "reef_sharks", "piscivores")
  claims <- bind_rows(lapply(names(summ), function(nm) {
    dt <- summ[[nm]]$dTable
    dt$scenario <- nm
    dt
  }))
  ind <- claims$Independ.Claim
  pick <- vapply(ind, function(cl) {
    any(vapply(benthos, function(b) grepl(paste0("\\b", b, "\\b"), cl), logical(1))) &&
      any(vapply(predators, function(p) grepl(paste0("\\b", p, "\\b"), cl), logical(1)))
  }, logical(1))
  readr::write_csv(claims[pick, ], file.path(out_dir, "dsep_benthos_to_predators.csv"))

  # Geomorphology test: which paths differ between atolls and high islands ####
  # Per equation, lm(y ~ (parents) * geomorphology); each predictor x
  # geomorphology interaction is F-tested (drop1, marginal). Group slopes are
  # read from the same model (Atolls = main effect; High Islands = main +
  # interaction). This is the test piecewiseSEM::multigroup() performs; it is
  # done here directly because multigroup() fails on the TD model set
  # ("undefined columns selected", 2026-09-24) while succeeding on BU, and
  # both directions must be treated identically. multigroup() is still run
  # as a cross-check and its output saved when it works.
  for (nm in names(scenarios)) {
    es <- eq_sets[[nm]]
    int_rows <- list(); slope_rows_lm <- list()
    for (y in names(es$eqs)) {
      preds <- es$eqs[[y]]
      if (island_re) {
        # lme: likelihood-ratio test (ML fits) of each x:geomorphology term,
        # full interaction model vs the same model without that one term
        m_int <- fit_eq(y, preds, dat_std, interaction = TRUE, method = "ML")
        cf <- nlme::fixef(m_int)
        rdf <- min(m_int$fixDF$X)
      } else {
        m_int <- fit_eq(y, preds, dat_std, interaction = TRUE)
        d1 <- drop1(m_int, test = "F")
        d1 <- d1[grepl(":geomorphology", rownames(d1)), , drop = FALSE]
        cf <- coef(m_int)
        rdf <- m_int$df.residual
      }
      for (x in preds) {
        int_term <- paste0(x, ":geomorphologyHighIslands") # coefficient name
        if (island_re) {
          m_red <- update(m_int, fixed = as.formula(paste(". ~ . -", paste0(x, ":geomorphology"))))
          lr <- anova(m_red, m_int)
          stat <- lr$L.Ratio[2]; tdf <- lr$df[2] - lr$df[1]; pv <- lr$`p-value`[2]
        } else {
          d1_row <- d1[rownames(d1) == paste0(x, ":geomorphology"), , drop = FALSE] # drop1 term label
          stat <- if (nrow(d1_row)) d1_row[["F value"]] else NA_real_
          tdf <- if (nrow(d1_row)) d1_row[["Df"]] else NA_integer_
          pv <- if (nrow(d1_row)) d1_row[["Pr(>F)"]] else NA_real_
        }
        int_rows[[length(int_rows) + 1]] <- data.frame(
          scenario = nm, response = y, predictor = x,
          test = if (island_re) "LRT (ML, lme island RE)" else "F (drop1, lm)",
          statistic = stat, df = tdf, resid_df = rdf,
          # an interaction model with no residual df (island-level df under
          # --island-re) cannot test anything: blank the p-value, never quote it
          estimable = rdf > 0, P_value = if (rdf > 0) pv else NA_real_,
          slope_atolls = unname(cf[x]),
          slope_highislands = unname(cf[x] + cf[int_term]),
          overlay_path = paste(x, y) %in% overlay_key | paste(y, x) %in% overlay_key
        )
      }
    }
    it <- bind_rows(int_rows)
    readr::write_csv(it, file.path(out_dir, paste0("geomorphology_interaction_", nm, ".csv")))
    progress(nm, ": geomorphology interactions NOT estimable (resid df <= 0): ", sum(!it$estimable), " of ", nrow(it))
    progress(nm, ": geomorphology interactions p < 0.05: ", sum(it$P_value < 0.05, na.rm = TRUE),
             " of ", nrow(it), " paths (", sum(it$P_value < 0.05 & it$overlay_path, na.rm = TRUE),
             " overlay); min residual df ", min(it$resid_df))
    mg <- if (island_re) simpleError("skipped: island random effect run") else
      tryCatch(multigroup(fits[[nm]], group = "geomorphology"), error = function(e) e)
    if (inherits(mg, "error")) {
      progress(nm, ": piecewiseSEM::multigroup cross-check failed: ", conditionMessage(mg))
    } else {
      readr::write_csv(as.data.frame(mg$anovaInts), file.path(out_dir, paste0("multigroup_", nm, "_interactions.csv")))
      gc <- bind_rows(lapply(names(mg$group.coefs), function(g) {
        x <- as.data.frame(mg$group.coefs[[g]]); x$group <- g; x
      }))
      readr::write_csv(gc, file.path(out_dir, paste0("multigroup_", nm, "_group_coefs.csv")))
      progress(nm, ": piecewiseSEM::multigroup cross-check ok")
    }
  }
  saveRDS(fits, file.path(out_dir, "psem_fits.Rds"))
}

# Stage C/D: brms ####
# Fit, save, and DROP: fits are reloaded from disk one at a time in stage E.
# Holding all 51 fits in memory grew R to 2.8 GB and got it OOM-killed
# (2026-09-24 16:18) while other jobs were running.
fit_cached <- function(path, ...) {
  if (file.exists(path)) return(invisible(TRUE))
  f <- brm(...)
  saveRDS(f, path, compress = "xz")
  rm(f); invisible(gc())
  invisible(TRUE)
}
load_fit <- function(nm, y, variant) readRDS(file.path(brms_dir, paste0(nm, "_", y, "_", variant, ".Rds")))
n_fits_total <- sum(lengths(lapply(eq_sets, function(es) es$eqs))) * 3
n_done <- 0
if (run_brms_linear || run_brms_spline) {
  t0 <- Sys.time()
  for (nm in names(scenarios)) {
    es <- eq_sets[[nm]]
    for (y in names(es$eqs)) {
      preds <- es$eqs[[y]]
      if (run_brms_linear) {
        # interaction model: group-specific slopes
        p <- file.path(brms_dir, paste0(nm, "_", y, "_linear.Rds"))
        fit_cached(
          p, formula = formula_for(y, preds, interaction = TRUE, re = island_re), data = dat_std,
          family = gaussian(),
          prior = c(set_prior("normal(0, 1)", class = "b"),
                    set_prior("normal(0, 2)", class = "Intercept")),
          chains = 4, iter = brms_iter, seed = brms_seed, cores = brms_cores, refresh = 0, control = brms_control,
          save_pars = save_pars(all = TRUE)
        )
        n_done <- n_done + 1
        # pooled linear: the comparator for the spline sensitivity check
        p <- file.path(brms_dir, paste0(nm, "_", y, "_linear_pooled.Rds"))
        fit_cached(
          p, formula = formula_for(y, preds, re = island_re), data = dat_std, family = student(),
          prior = c(set_prior("normal(0, 1)", class = "b"),
                    set_prior("normal(0, 2)", class = "Intercept")),
          chains = 4, iter = brms_iter, seed = brms_seed, cores = brms_cores, refresh = 0, control = brms_control,
          save_pars = save_pars(all = TRUE)
        )
        n_done <- n_done + 1
      }
      if (run_brms_spline) {
        p <- file.path(brms_dir, paste0(nm, "_", y, "_spline.Rds"))
        rhs <- paste(sprintf('s(%s, k = 3, bs = "cs")', preds), collapse = " + ")
        if (island_re) rhs <- paste(rhs, "+ (1 | island)")
        fit_cached(
          p, formula = as.formula(paste(y, "~", rhs)), data = dat_std, family = student(),
          prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                    set_prior("exponential(1)", class = "sds")),
          chains = 4, iter = brms_iter, seed = brms_seed, cores = brms_cores, refresh = 0, control = brms_control,
          save_pars = save_pars(all = TRUE)
        )
        n_done <- n_done + 1
      }
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      progress("brms ", n_done, "/", n_fits_total, " fits (", nm, " ", y, ") elapsed ",
               round(el, 1), " min; eta ",
               format(Sys.time() + (el / n_done) * (n_fits_total - n_done) * 60, "%H:%M %Z"))
    }
  }
}

# Stage E: tables and figure ####
pretty <- c(
  sicklefin_lemon_sharks = "Sicklefin lemon sharks", reef_sharks = "Reef sharks",
  piscivores = "Piscivores", herbivores = "Herbivores", invertivores = "Invertivores",
  planktivores = "Planktivores", hard_coral = "Hard coral",
  crustose_coraline_algae = "Crustose coralline algae", other_algae = "Other algae",
  pop_dens = "Human population density", relief = "Reef relief", ave_npp = "Mean NPP",
  ave_temp = "Mean temperature"
)

if (run_brms_linear) {
  # posterior slopes per geomorphology for every path
  slope_rows <- list()
  loo_rows <- list()
  for (nm in names(scenarios)) {
    es <- eq_sets[[nm]]
    for (y in names(es$eqs)) {
      f <- load_fit(nm, y, "linear")
      dr <- as_draws_df(f)
      rm(f)
      for (x in es$eqs[[y]]) {
        b <- dr[[paste0("b_", x)]]
        bi <- dr[[paste0("b_", x, ":geomorphologyHighIslands")]]
        for (g in c("Atolls", "HighIslands")) {
          s <- if (g == "Atolls") b else b + bi
          slope_rows[[length(slope_rows) + 1]] <- data.frame(
            scenario = nm, response = y, predictor = x, geomorphology = g,
            estimate = median(s), lower95 = quantile(s, 0.025), upper95 = quantile(s, 0.975),
            prob_direction = max(mean(s > 0), mean(s < 0)),
            overlay_path = paste(x, y) %in% overlay_key | paste(y, x) %in% overlay_key,
            row.names = NULL
          )
        }
      }
      if (run_brms_spline) {
        lp <- loo(load_fit(nm, y, "linear_pooled"))
        ls <- loo(load_fit(nm, y, "spline"))
        cmp <- loo_compare(list(linear = lp, spline = ls))
        loo_rows[[length(loo_rows) + 1]] <- data.frame(
          scenario = nm, response = y,
          elpd_linear = lp$estimates["elpd_loo", 1], elpd_spline = ls$estimates["elpd_loo", 1],
          best = rownames(cmp)[1], elpd_diff = cmp[2, "elpd_diff"], se_diff = cmp[2, "se_diff"],
          pareto_k_gt07_linear = sum(lp$diagnostics$pareto_k > 0.7),
          pareto_k_gt07_spline = sum(ls$diagnostics$pareto_k > 0.7)
        )
      }
    }
  }
  slopes <- bind_rows(slope_rows)
  readr::write_csv(slopes, file.path(out_dir, "brms_slopes_by_geomorphology.csv"))
  if (length(loo_rows)) readr::write_csv(bind_rows(loo_rows), file.path(out_dir, "loo_linear_vs_spline.csv"))

  # Coefficient plot: overlay (trophic) paths, TD vs BU, by geomorphology.
  # One row per trophic PAIR, labelled in TD wording; the TD point is the TD
  # path's slope (predator -> prey) and the BU point the reversed path's slope.
  pairs <- overlay |> filter(from %in% biotic, to %in% biotic)
  pairs$label <- paste(pretty[pairs$from], "→", pretty[pairs$to])
  plot_df <- bind_rows(
    slopes |> filter(scenario == "TD") |> inner_join(pairs, by = c("predictor" = "from", "response" = "to")),
    slopes |> filter(scenario == "BU") |> inner_join(pairs, by = c("predictor" = "to", "response" = "from"))
  ) |>
    mutate(
      scenario = factor(scenario, levels = c("TD", "BU"), labels = c("Top-down", "Bottom-up")),
      label = factor(label, levels = rev(pairs$label)),
      geomorphology = factor(geomorphology, levels = c("HighIslands", "Atolls"),
                             labels = c("High islands", "Atolls"))
    )
  # Per-geomorphology mean rows at the top of each facet (as in Fig. 2A):
  # mean |slope| over the 11 overlay paths, computed per posterior draw
  # (draws from the 11 independent fits paired by index), so the row carries
  # a posterior median and 95% CI like the paths. Absolute values, because
  # signed slopes of opposite sign would cancel.
  mean_rows <- list()
  for (nm in names(scenarios)) {
    es <- eq_sets[[nm]]
    mats <- list(Atolls = list(), HighIslands = list())
    for (k in seq_len(nrow(pairs))) {
      y <- if (nm == "TD") pairs$to[k] else pairs$from[k]
      x <- if (nm == "TD") pairs$from[k] else pairs$to[k]
      if (!(y %in% names(es$eqs)) || !(x %in% es$eqs[[y]])) next
      dr <- as_draws_df(load_fit(nm, y, "linear"))
      b <- dr[[paste0("b_", x)]]; bi <- dr[[paste0("b_", x, ":geomorphologyHighIslands")]]
      mats$Atolls[[k]] <- abs(b); mats$HighIslands[[k]] <- abs(b + bi)
    }
    for (g in names(mats)) {
      m <- rowMeans(do.call(cbind, mats[[g]]))
      mean_rows[[length(mean_rows) + 1]] <- data.frame(
        scenario = nm, geomorphology = g, estimate = median(m),
        lower95 = quantile(m, 0.025), upper95 = quantile(m, 0.975),
        label = paste0("**", ifelse(g == "Atolls", "Atolls", "High islands"), " mean |\u03b2|**"),
        row.names = NULL)
    }
  }
  mean_df <- bind_rows(mean_rows) |>
    mutate(scenario = factor(scenario, levels = c("TD", "BU"), labels = c("Top-down", "Bottom-up")),
           geomorphology = factor(geomorphology, levels = c("HighIslands", "Atolls"),
                                  labels = c("High islands", "Atolls")))
  readr::write_csv(mean_df, file.path(out_dir, "brms_mean_abs_slope_by_geomorphology.csv"))
  lvls <- c(rev(pairs$label), "**Atolls mean |\u03b2|**", "**High islands mean |\u03b2|**")
  plot_df <- bind_rows(plot_df |> mutate(label = as.character(label)), mean_df) |>
    mutate(label = factor(label, levels = lvls))
  pl <- ggplot(plot_df, aes(x = estimate, y = label, colour = scenario)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
    geom_pointrange(aes(xmin = lower95, xmax = upper95),
                    position = position_dodge(width = 0.6), size = 0.4) +
    facet_wrap(~geomorphology, ncol = 1, scales = "free_y") + # free_y: each facet shows only its own mean row
    scale_colour_manual(values = c("Top-down" = "#F8766D", "Bottom-up" = "#00BA38")) +
    labs(
      x = "Standardised partial regression coefficient (posterior median, 95% CI)",
      y = NULL, colour = NULL,
      caption = paste0(
        "Bottom-up points are the reversed path (row label read right to left).\n",
        "Each response has one equation with all DAG parents; slopes by geomorphology from a\n",
        "predictor x geomorphology interaction model, n = 24 reefs",
        if (island_re) " on 12 islands (island random intercept).\n" else ".\n",
        "Mean rows: mean |slope| over the 11 paths, per posterior draw. ", format(Sys.Date())
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top", plot.background = element_rect(fill = "white", colour = NA)) +
    # bold the mean rows (markdown ** **); left-axis sub-theme needed under facets (see 06)
    theme_sub_axis_left(text = ggtext::element_markdown())
  ggsave(file.path(out_dir, paste0(Sys.Date(), "_SEM_coefficients_TDvsBU.png")), pl,
         width = 9, height = 9, dpi = 200)
}
progress("DONE 05b")
