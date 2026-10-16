# 13_shark_vs_piscivore_plot.R
# Draft SM figure for R2,7 (functional redundancy of reef sharks and
# piscivorous teleosts): the top-down slopes of reef sharks and of piscivores
# on each prey guild, side by side, from the island-random-effect SEM
# (05b --variant=revised2 --island-re).
#
# Author: Simon Dedman <simondedman@gmail.com> (+ Claude)
# Created: 2026-10-16
#
# 18 slopes: 2 predators x 3 prey guilds x (pooled piecewiseSEM + atolls +
# high islands from brms). Pooled intervals = estimate +/- t(0.975, df) x SE
# with the equation's residual df from piecewiseSEM; brms intervals = 95% CI.
# Both predators sit in the same equation for each guild, so each slope is
# adjusted for the other predator.
#
# Inputs: Results/SEM/variants/revised2_islandRE/{coefs_pooled_TD,brms_slopes_by_geomorphology}.csv
# Output: Results/SEM/variants/revised2_islandRE/<today>_shark_vs_piscivore_slopes.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

res_dir <- here("Results", "SEM", "variants", "revised2_islandRE")
guilds <- c("herbivores", "invertivores", "planktivores")
preds <- c("reef_sharks", "piscivores")

pooled <- read_csv(file.path(res_dir, "coefs_pooled_TD.csv"), show_col_types = FALSE) |>
  filter(Response %in% guilds, Predictor %in% preds) |>
  mutate(Std.Error = as.numeric(Std.Error), DF = as.numeric(DF)) |>
  transmute(response = Response, predictor = Predictor, model = "Pooled (piecewiseSEM)",
            estimate = Std.Estimate,
            lower95 = Std.Estimate - qt(0.975, DF) * Std.Error,
            upper95 = Std.Estimate + qt(0.975, DF) * Std.Error)
bygeo <- read_csv(file.path(res_dir, "brms_slopes_by_geomorphology.csv"), show_col_types = FALSE) |>
  filter(scenario == "TD", response %in% guilds, predictor %in% preds) |>
  transmute(response, predictor,
            model = ifelse(geomorphology == "Atolls", "Atolls (brms)", "High islands (brms)"),
            estimate, lower95, upper95)
d <- bind_rows(pooled, bygeo) |>
  mutate(predictor = factor(predictor, levels = rev(preds), labels = rev(c("Reef sharks", "Piscivores"))),
         response = factor(response, levels = guilds, labels = str_to_sentence(guilds)),
         model = factor(model, levels = c("High islands (brms)", "Atolls (brms)", "Pooled (piecewiseSEM)")))
stopifnot(nrow(d) == 18, !anyNA(d$model))
write_csv(d, file.path(res_dir, "shark_vs_piscivore_slopes.csv"))

p <- ggplot(d, aes(x = estimate, y = predictor, colour = model)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = lower95, xmax = upper95), position = position_dodge(width = 0.6),
                  size = 0.35, linewidth = 0.8) +
  facet_wrap(~response, ncol = 1) +
  scale_colour_manual(values = c("Pooled (piecewiseSEM)" = "black", "Atolls (brms)" = "#0072B2",
                                 "High islands (brms)" = "#E69F00"),
                      breaks = c("Pooled (piecewiseSEM)", "Atolls (brms)", "High islands (brms)"), name = NULL) +
  labs(x = "Standardised top-down slope on prey guild (95% interval)", y = NULL,
       caption = paste0(Sys.Date(), "; each guild's equation contains both predators, so each slope is adjusted for the other")) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.caption = element_text(size = 7))
f <- file.path(res_dir, paste0(Sys.Date(), "_shark_vs_piscivore_slopes.png"))
ggsave(f, p, width = 6.5, height = 6.5, dpi = 300)
cat("wrote", f, "\n")
print(d |> mutate(across(where(is.numeric), \(x) round(x, 2))), n = 18)
