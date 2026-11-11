# 14_fig2A_coefficients.R
# Main-text Fig. 2A: standardised path slopes, top-down vs bottom-up, by geomorphology,
# redrawn from 05b's saved outputs (no model refits).
# Inputs (Results/SEM/variants/revised2_islandRE/):
#   brms_slopes_by_geomorphology.csv, brms_mean_abs_slope_by_geomorphology.csv
# Output: Results/SEM/variants/revised2_islandRE/Fig2A_SEM_coefficients.png
# Styling matches the submitted Fig. 2A (06_DAG_results.R L640: TopDown #F8766D, BottomUp #1b9e77).
# Explanatory notes live in the figure caption, not in the plot (Simon, 2026-11-09).
# The same plot is also drawn by 05b stage E; keep the two in step (styling patched there 2026-11-09).
# Libraries: dplyr, readr, ggplot2 (>= 4, theme_sub_axis_left), ggtext, here

library(dplyr)
library(readr)
library(ggplot2)
library(here)

in_dir <- here("Results", "SEM", "variants", "revised2_islandRE")
slopes <- read_csv(file.path(in_dir, "brms_slopes_by_geomorphology.csv"), show_col_types = FALSE)
means <- read_csv(file.path(in_dir, "brms_mean_abs_slope_by_geomorphology.csv"), show_col_types = FALSE)

pretty <- c(
  sicklefin_lemon_sharks = "Sicklefin lemon sharks", reef_sharks = "Reef sharks",
  piscivores = "Piscivores", herbivores = "Herbivores", invertivores = "Invertivores",
  planktivores = "Planktivores", hard_coral = "Hard coral",
  crustose_coraline_algae = "Crustose coralline algae", other_algae = "Other algae"
)
# Top-down wording (predictor -> prey); bottom-up point is the same edge reversed
td <- slopes |> filter(scenario == "TD", overlay_path)
stopifnot(nrow(td) == 22) # 11 paths x 2 geomorphologies
pairs <- td |> distinct(from = predictor, to = response) |>
  mutate(label = paste(pretty[from], "→", pretty[to]))
# Row order top-to-bottom as in the submitted Fig. 2A (Simon, 2026-11-09)
top_order <- c(
  "Sicklefin lemon sharks → Reef sharks", "Sicklefin lemon sharks → Piscivores",
  "Reef sharks → Planktivores", "Reef sharks → Invertivores", "Reef sharks → Herbivores",
  "Piscivores → Planktivores", "Piscivores → Invertivores", "Piscivores → Herbivores",
  "Herbivores → Hard coral", "Herbivores → Crustose coralline algae", "Herbivores → Other algae"
)
stopifnot(setequal(top_order, pairs$label))

plot_df <- bind_rows(
  td |> inner_join(pairs, by = c("predictor" = "from", "response" = "to")),
  slopes |> filter(scenario == "BU", overlay_path) |>
    inner_join(pairs, by = c("predictor" = "to", "response" = "from"))
)
stopifnot(nrow(plot_df) == 44)

geo_lab <- c(HighIslands = "i  High islands", Atolls = "ii  Atolls")
mean_lab <- c("High islands" = "**High islands mean |β|**", "Atolls" = "**Atolls mean |β|**")
mean_df <- means |>
  mutate(
    scenario = ifelse(scenario == "Top-down", "TD", "BU"),
    geomorphology = ifelse(geomorphology == "Atolls", "Atolls", "HighIslands"),
    label = unname(mean_lab[ifelse(geomorphology == "Atolls", "Atolls", "High islands")])
  ) |>
  select(scenario, geomorphology, estimate, lower95, upper95, label)
stopifnot(nrow(mean_df) == 4)

lvls <- c(rev(top_order), mean_lab[["Atolls"]], mean_lab[["High islands"]]) # factor levels run bottom-up
plot_df <- bind_rows(plot_df |> select(scenario, geomorphology, estimate, lower95, upper95, label), mean_df) |>
  mutate(
    scenario = factor(scenario, levels = c("TD", "BU"), labels = c("Top-down", "Bottom-up")),
    geomorphology = factor(geo_lab[geomorphology], levels = geo_lab),
    label = factor(label, levels = lvls)
  )
stopifnot(!anyNA(plot_df$label), !anyNA(plot_df$geomorphology))

pl <- ggplot(plot_df, aes(x = estimate, y = label, colour = scenario)) +
  # Cohen (1988) small / medium / large benchmarks (0.1, 0.3, 0.5), both signs (Simon, 2026-11-09)
  geom_vline(xintercept = c(-0.5, -0.3, -0.1, 0.1, 0.3, 0.5), linetype = 3, colour = "grey55", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_pointrange(aes(xmin = lower95, xmax = upper95),
                  position = position_dodge(width = 0.6), size = 0.4) +
  facet_wrap(~geomorphology, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("Top-down" = "#F8766D", "Bottom-up" = "#1b9e77")) +
  # labels every 0.5; limits = data range (CI ends) with a small margin (Simon, 2026-11-09)
  scale_x_continuous(breaks = seq(-2, 2, 0.5), labels = function(x) format(x, drop0trailing = TRUE),
                     limits = range(c(plot_df$lower95, plot_df$upper95)), expand = expansion(mult = 0.02)) +
  labs(x = "Standardised partial regression coefficient\n(posterior median, 95% CI)",
       y = NULL, colour = NULL) +
  theme_minimal(base_size = 10) +
  # legend inside, top-left of the panel area, level with the "i  High islands" strip; strip titles
  # right-aligned as in the submitted Fig. 2A (Simon, 2026-11-09), so the two never collide
  # (top legend pulled down into the first strip's row by a negative bottom margin)
  theme(legend.position = "top", legend.justification = c(0, 1),
        legend.direction = "horizontal", legend.margin = margin(0, 0, -26, 0),
        legend.background = element_blank(), legend.key.width = unit(0.5, "cm"),
        strip.text = element_text(hjust = 1, face = "bold", size = 11, margin = margin(2, 2, 2, 2)),
        axis.title.x = element_text(size = 9),
        plot.background = element_rect(fill = "white", colour = NA)) +
  # size 8.5 so the longest label (Herbivores → Crustose coralline algae) fits at 6 in wide;
  # 06's negative label margin clipped these longer labels, so default margin here
  theme_sub_axis_left(text = ggtext::element_markdown(size = 8.5))

out <- file.path(in_dir, "Fig2A_SEM_coefficients.png")
# same size as the submitted panel A (06 facetTopo: 6 x 8 in), leaving room for panel B
ggsave(out, pl, width = 6, height = 8, dpi = 300)
cat("WROTE", out, "rows plotted:", nrow(plot_df), "\n")
