# setwd("~/Documents/My Documents/FinPrint French Poly/Analysis/Ch 4 Rethink Prelim") ## Change to appropriate working directory ##
# setwd("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/NFF Data code/")
library(tidyverse)
library(MASS)
library(vegan)
library(reshape)
library(doBy)
library(utils)
library(RcmdrMisc)
library(ResourceSelection)
library(boot)
library(ggplot2)
library(gplots)
library(rstatix)
library(ape)
library(ggpubr) # sudo apt install cmake
library(rmarkdown)
library(esquisse)
library(viridis)
library(plotly)
library(here)

# import all data frames ####
## import site order ####
# site.order.df <- data.frame(read.csv("site_order_df.csv", header = TRUE, as.is = TRUE))
site.order.df <- data.frame(read.csv(here("NFF_data", "site_order_df.csv"), header = TRUE, as.is = TRUE)) |>
  mutate(across(.cols = site_name,
                .fns = ~ factor(.x)))

## raw benthic uvc data ####
benthic.raw <- data.frame(read.csv(here("NFF_data", "fixed_bethic_uvc_final_2023_02_26.csv"), header = TRUE, as.is = TRUE)) |>
  mutate(across(.cols = c(site_name, reef_name, UniqueID, Date),
                .fns = ~ factor(.x)))

## raw fish uvc data ####
fish.uvc.raw <- data.frame(read.csv(here("NFF_data", "fixed_fish_uvc_final_2023_02_28.csv"), header = TRUE, as.is = TRUE)) |>
  mutate(across(.cols = c(site_name, reef_name, UniqueID, Date, Species, Family, diet.kulbiki, Feeding.group),
                .fns = ~ factor(.x)))

## import teleost df ####
teleost.bruv.raw <- data.frame(read.csv(here("NFF_data", "wide.df1.teleosts.csv"), header = TRUE, as.is = TRUE)) |>  # importing CSV#
  mutate(across(.cols = c(geo, isl_grp, archi, Season, bait, topo),
                .fns = ~ factor(.x)))

## import shark ####
elasmo.bruv.raw <- data.frame(read.csv(here("NFF_data", "wide.df1.ch3.60min.2023.01.csv"), header = TRUE, as.is = TRUE)) |>  # importing CSV#
  mutate(across(.cols = c(geo, isl_grp, archi, Season, bait_type, topo),
                .fns = ~ factor(.x)))

# reef summaries by UVC pieces ####
## Summary by reef for benthic data ####
benthic.reef.df <- benthic.raw |>
  mutate(chi_benthos_percent = ((CCA + Hard.Coral) / 100)) |>
  group_by(reef_name) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
  mutate((benthic.raw |>
            group_by(reef_name) |>
            summarise(n_survey = n_distinct(UniqueID, na.rm = TRUE)))) |>
  dplyr::select(
    reef_name,
    Relief,
    Sand,
    Rubble,
    Pavement,
    Other.Algae,
    Hard.Coral,
    CCA,
    Soft.Coral,
    Invert,
    chi_benthos_percent,
    n_survey
  )

write_csv(benthic.reef.df , here("NFF_data", "benthic_sum_reef_2023_02_26.csv"))
saveRDS(benthic.reef.df, file = here("NFF_data", "benthic_sum_reef_2023_02_26.RData"))


# need full data frame with survey as the primary sampling unit ####
## first sort out fish
# filter for only pred. teleost species, take out sharks
pred.tel.uvc.survey.sum.df <- fish.uvc.raw |>
  filter(
    Family != "Carcharhinidae",
    Family != "Myliobatidae"
  ) |>
  filter(Family == "Lutjanidae" |
           Family == "Scombridae" |
           Family == "Megalopidae" |
           Family == "Carangidae" |
           Family == "Sphyraenidae" |
           Family == "Serranidae" |
           Family == "Lethrinidae") |>
  group_by(UniqueID) |>
  summarise(
    across(c(site_name, reef_name, Date, Time.start, Lat, Long, Observer), \(x) first(x)),
    across(c(biomass_g, biomass_g_per_m2), \(x) sum(x, na.rm = TRUE))
  ) |>
  dplyr::rename(pred_tel_biomass_g = biomass_g, pred_tel_biomass_g_per_m2 = biomass_g_per_m2) |>
  mutate(chi_pred_tel_percent = (pred_tel_biomass_g_per_m2 / 500))


# for just prey species i.e. non-target teleost families, sharks and rays
# Assign NA Feeding.group rows to a Feeding.group
NA.Feeding.group <- fish.uvc.raw |>
  filter(
    Family != "Lutjanidae",
    Family != "Scombridae",
    Family != "Megalopidae",
    Family != "Carangidae",
    Family != "Sphyraenidae",
    Family != "Serranidae",
    Family != "Lethrinidae",
    Family != "Carcharhinidae",
    Family != "Myliobatidae",
    is.na(Feeding.group)) |>
  distinct(Family, diet.kulbiki) |>
  arrange(diet.kulbiki, Family) |>
  mutate(Feeding.group = "NA") |>
  write_csv(here("NFF_data", "NA.Feeding.group.csv"))

# summarise diet.kulbiki by Family
fish.uvc.raw |>
  filter(
    Family != "Lutjanidae",
    Family != "Scombridae",
    Family != "Megalopidae",
    Family != "Carangidae",
    Family != "Sphyraenidae",
    Family != "Serranidae",
    Family != "Lethrinidae",
    Family != "Carcharhinidae",
    Family != "Myliobatidae",
    !is.na(Feeding.group)
  ) |>
  group_by(diet.kulbiki, Feeding.group) |>
  summarise(n = n())
# AWAIT NFF SIGNOFF ####
# "Feeding groups by teleost species" email, 2025-02-21
# Then populate NA Feeding.group rows with the correct Feeding.group

prey.uvc.survey.sum.df <- fish.uvc.raw |>
  filter(
    Family != "Lutjanidae",
    Family != "Scombridae",
    Family != "Megalopidae",
    Family != "Carangidae",
    Family != "Sphyraenidae",
    Family != "Serranidae",
    Family != "Lethrinidae",
    Family != "Carcharhinidae",
    Family != "Myliobatidae"
  ) |>
  # Split to invertivores planktivores herbivores by Feeding.group
  # pivot.wider to get biomass_g and biomass_g_per_m2 columns for each Feeding.group
  pivot_wider(names_from = Feeding.group,
              values_from = c(biomass_g, biomass_g_per_m2),
              values_fn = ~ mean(.x, na.rm = TRUE)) |>
  group_by(UniqueID) |> # reef
  summarise(
    across(c(site_name, reef_name, Date, Time.start, Lat, Long, Observer), \(x) first(x)),
    across(contains("biomass_g"), \(x) sum(x, na.rm = TRUE)) # contains was c(biomass_g, biomass_g_per_m2)
  ) |>
  # dplyr::rename(prey_fish_biomass_g = biomass_g,
  #               prey_fish_biomass_g_per_m2 = biomass_g_per_m2) |>
  # mutate(chi_prey_fish_percent = (prey_fish_biomass_g_per_m2 / 500))
  # @NFF explain what this is in comment please ####
# this needs to be renamed so it doesn't overwrite the original columns
mutate(across(contains("biomass_g_per_m2"), \(x) x/500, .names = "chi_{.col}_percent")) |>
  # remove the "_biomass_g_per_m2" prefix from column names containing "percent"
  dplyr::rename_with(.fn = ~ stringr::str_replace(., "_biomass_g_per_m2", ""),
                     .cols = contains("percent")) |>
  # remove spaces in Mixed diet column names
  dplyr::rename_with(~ gsub(" ", "_", .x, fixed = TRUE))
# @NFF Has piscivores; ensure they're used somewhere ####


# pare down predators bruvs ####
# @NFF pare down how? ####
# maxN teleosts per site
# merged with elasmo.bruv.raw in next block
# mean maxn per site only for families: lutjanidae scombridae megalopidae carangidae sphyraenidae serranidae lethrinidae
# not for: carangidae sphyraenidae serranidae lethrinidae
# @NFF what is the difference between the two lists? ####
# becomes teleost_maxn in pred.bruv.df1 below
trash.tel.df1 <- teleost.bruv.raw |>
  dplyr::select(reef, total_maxN_a) |>
  dplyr::rename(reef_name = reef, teleost_maxn = total_maxN_a) |>
  group_by(reef_name) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

## make pred.df ####
pred.bruv.df1 <- elasmo.bruv.raw |>
  # create new columns for reef_sharks, transient_pelagic sharks
  mutate(reef_sharks = Whitetip.reef.shark + Grey.reef.shark + Blacktip.reef.shark,
         transient_pelagic_sharks = Scalloped.hammerhead.shark + Common.Blacktip.shark + Tiger.shark + Great.hammerhead.shark) |>
  # rename sicklefin_lemon_sharks
  rename(sicklefin_lemon_sharks = Lemon.shark) |>
  #@NFF better to select than -select, user can see what's being selected ####
# only use MaxN
# dplyr::select(-c(nspp_shark:Tiger.shark, maxn_ray:vid_length)) |>
dplyr::select(site_name:reef_name, maxn_shark, transient_pelagic_sharks, sicklefin_lemon_sharks, reef_sharks, time.no.bait:topo) |>
  group_by(site_name, reef_name) |>
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), first),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  merge(trash.tel.df1, by = c("reef_name"), all.x = TRUE) |>
  mutate(teleost_maxn = ifelse(is.na(teleost_maxn), 0, teleost_maxn)) |>
  ungroup() |>
  # put columns in a nicer order
  select(reef_name:topo, latitude, longitude, lagoon.size, Emerged.land.area,
         Population.size, grav, depth, visibility, ave_temp, ave_npp,
         time.no.bait, transient_pelagic_sharks, sicklefin_lemon_sharks,
         reef_sharks, maxn_shark, teleost_maxn)


## combine into full dataframe ####
survey.wide.df1 <- benthic.raw |>
  mutate(chi_benthos_percent = ((CCA + Hard.Coral) / 100)) |>
  dplyr::rename(Time.start = Time) |>
  merge(prey.uvc.survey.sum.df, by = c(
    "UniqueID",
    "site_name",
    "reef_name",
    "Date",
    "Time.start",
    "Lat",
    "Long"
  )) |>
  # mutate(chi_prey_score = ((chi_benthos_percent + chi_prey_fish_percent) / 2)) |>
  # need to replace chi_prey_fish_percent with multiple columns
  # "chi_NA_percent"               "chi_Herbivore_percent"        "chi_Invertivore_percent"      "chi_Coralivore_percent"       "chi_Mixed_diet_percent"       "chi_Piscivore_percent"
  # mutate across columns with "percent" in the name i.e. "chi_{column_name}_percent", to create new columns called "{column_name}_score", which is ((chi_benthos_percent + {input_column}) / 2))
  mutate(across(contains("percent"), \(x) ((chi_benthos_percent + x) / 2), .names = "{.col}_score")) |>
  # remove the "_percent" suffix from column names containing "percent_score"
  rename_with(~ gsub("percent_score", "score", .x, fixed = TRUE)) |>
  # create columns converting scores to grades
  mutate(across(contains("_score"), ~ case_when(
    . >= 0.80 ~ "Very Healthy",
    . >= 0.60 ~ "Healthy",
    . >= 0.40 ~ "Fair",
    . >= 0.20 ~ "Degraded",
    . >= 0.00 ~ "Very Degraded",
    TRUE ~ "Unknown"
  ), .names = "{.col}_grade")) |>
  # remove the "_score" suffix from column names containing "score_grade"
  rename_with(~ gsub("score_grade", "grade", .x, fixed = TRUE)) |>
  # merge predatory teleosts UVC survey data
  merge(pred.tel.uvc.survey.sum.df, by = c(
    "UniqueID",
    "site_name",
    "reef_name",
    "Date",
    "Time.start",
    "Lat",
    "Long",
    "Observer"
  ), all.x = TRUE) |> # need to have x = true too keep surveys with no pred.
  # replace NAs with zeroes in columns containing "biomass_g" or "biomass_g_per_m2"
  # mutate(
  #   pred_tel_biomass_g = ifelse(is.na(pred_tel_biomass_g), 0, pred_tel_biomass_g),
  #   pred_tel_biomass_g_per_m2 = ifelse(is.na(pred_tel_biomass_g_per_m2), 0, pred_tel_biomass_g_per_m2)
  # ) |>
  mutate(across(contains(c("biomass_g", "biomass_g_per_m2")), ~ ifelse(is.na(.), 0, .))) |>
  # mutate(chi_all_score = ((chi_benthos_percent + chi_pred_tel_percent + chi_prey_fish_percent) / 2)) |>
  # Need to replace chi_prey_fish_percent with multiple columns
  # "chi_benthos_percent"
  # "chi_Piscivore_percent"
  # "chi_Herbivore_percent"
  # "chi_Invertivore_percent"
  # "chi_Planktivore_percent" # doesn't exist yet
  # "chi_NA_percent"
  # "chi_Coralivore_percent"
  # "chi_Mixed_diet_percent"
  # mutate(chi_all_score = ((chi_benthos_percent + chi_Piscivore_percent + chi_prey_fish_percent) / 2)) |>
  mutate(across(
    .cols = contains("_percent") & !any_of(c("chi_benthos_percent", "chi_Piscivore_percent")),
    .fns = ~ (chi_benthos_percent + chi_Piscivore_percent + .) / 2,
    .names = "{.col}_all"
  )) |>
  rename_with(~ gsub("percent_all", "all_score", .x, fixed = TRUE)) |>
  mutate(across(contains("all_score"), ~ case_when(
    . >= 0.80 ~ "Very Healthy",
    . >= 0.60 ~ "Healthy",
    . >= 0.40 ~ "Fair",
    . >= 0.20 ~ "Degraded",
    . >= 0.00 ~ "Very Degraded",
    TRUE ~ "Unknown"
  ), .names = "{.col}_grade")) |>
  rename_with(~ gsub("score_grade", "grade", .x, fixed = TRUE)) |>
  merge(pred.bruv.df1, by = c("site_name", "reef_name")) |>
  # @NFF why remove these?####
dplyr::select(-c(latitude, longitude)) |>
  mutate(
    pop.dens = Population.size / Emerged.land.area,
    across(.cols = ends_with("grade"),
           .fns = ~ factor(.x, levels = c("Very Degraded",
                                          "Degraded",
                                          "Fair",
                                          "Healthy",
                                          "Very Healthy"))),
    isl_grp = factor(isl_grp, levels = c("marquesas",
                                         "west tuamotu",
                                         "east tuamotu",
                                         "windward",
                                         "leeward",
                                         "australes")),
    topo = factor(topo, levels = c("open atoll",
                                   "closed atoll",
                                   "near atoll",
                                   "high barrier",
                                   "high fringing ",
                                   "high rocky"))
  )
write_csv(survey.wide.df1, here("NFF_data", "ch4_survey_wide_df1.csv"))
saveRDS(survey.wide.df1, file = here("NFF_data", "survey.wide.df1.RData"))

## filter for just islands ####
island.survey.wide.df1 <- survey.wide.df1 |>
  filter(geo == "island") |>
  droplevels()

write_csv(island.survey.wide.df1, here("NFF_data", "island.ch4_survey_wide_df1.csv"))
saveRDS(island.survey.wide.df1, file = here("NFF_data", "island.survey.wide.df1.RData"))

## filter for just atolls ####
atoll.survey.wide.df1 <- survey.wide.df1 |>
  filter(geo == "atoll") |>
  droplevels()

write_csv(atoll.survey.wide.df1, here("NFF_data", "atoll.ch4_survey_wide_df1.csv"))
saveRDS(atoll.survey.wide.df1, file = here("NFF_data", "atoll.survey.wide.df1.RData"))

## remove marquesas ####
survey.wide.df2 <- survey.wide.df1 |>
  filter(
    site_name != "Nuka Hiva",
    site_name != "Uapou"
  ) |>
  droplevels()

write_csv(survey.wide.df2, here("NFF_data", "ch4_survey_wide_df2.csv"))
saveRDS(survey.wide.df2, file = here("NFF_data", "survey.wide.df2.RData"))

## just islands ####
island.survey.wide.df2 <- survey.wide.df2 |>
  filter(geo == "island") |>
  droplevels()

write_csv(island.survey.wide.df2, here("NFF_data", "island.ch4_survey_wide_df2.csv"))
saveRDS(island.survey.wide.df2, file = here("NFF_data", "island.survey.wide.df2.RData"))


## just atoll ####
atoll.survey.wide.df2 <- survey.wide.df2 |>
  filter(geo == "atoll") |>
  droplevels()

write_csv(atoll.survey.wide.df2, here("NFF_data", "atoll.ch4_survey_wide_df2.csv"))
saveRDS(atoll.survey.wide.df2, file = here("NFF_data", "atoll.survey.wide.df2.RData"))




# full summary by reef ####
reef.df1 <- survey.wide.df1 |>
  dplyr::select(-c(
    UniqueID:Depth, # includes Lat and Long
    sum,
    contains("_grade")
    # chi_prey_grade,
    # chi_all_grade
  )) |>
  group_by(site_name, reef_name) |>
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), first),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  merge((pred.bruv.df1) |> dplyr::select(c(reef_name, latitude, longitude)),
        by = c("reef_name")
        # @NFF why remove Lat & Long from survey.wide.df1 then merge back in from pred.bruv.df1?####
  ) |>
  mutate(across(ends_with("_score"), ~ case_when(
    . >= 0.80 ~ "Very Healthy",
    . >= 0.60 ~ "Healthy",
    . >= 0.40 ~ "Fair",
    . >= 0.20 ~ "Degraded",
    . >= 0.00 ~ "Very Degraded",
    TRUE ~ "Unknown"
  ), .names = "{.col}_grade")) |>
  rename_with(~ gsub("score_grade", "grade", .x, fixed = TRUE)) |>
  mutate(across(.cols = ends_with("_grade"),
                .fns = ~ factor(.x, levels = c("Very Degraded",
                                               "Degraded",
                                               "Fair",
                                               "Healthy",
                                               "Very Healthy"))))

write_csv(reef.df1, here("NFF_data", "ch4_reef_wide_df1.csv"))
saveRDS(reef.df1, file = here("NFF_data", "ch4_reef_wide_df1.RData"))

## just islands ####
island.reef.df1 <- reef.df1 |>
  filter(geo == "island") |>
  droplevels()

write_csv(island.reef.df1, here("NFF_data", "ch4_island_reef_wide_df1.csv"))
saveRDS(island.reef.df1, file = here("NFF_data", "ch4_island_reef_wide_df1.RData"))

## just atolls ####
atoll.reef.df1 <- reef.df1 |>
  filter(geo == "atoll") |>
  droplevels()

write_csv(atoll.reef.df1, here("NFF_data", "ch4_atoll_reef_wide_df1.csv"))
saveRDS(atoll.reef.df1, file = here("NFF_data", "ch4_atoll_reef_wide_df1.RData"))

## remove marquesas ####
reef.df2 <- reef.df1 |>
  filter(
    site_name != "Nuka Hiva",
    site_name != "Uapou"
  ) |>
  droplevels()

write_csv(reef.df2, here("NFF_data", "ch4_reef_wide_df2.csv"))
saveRDS(reef.df2, file = here("NFF_data", "ch4_reef_wide_df2.RData"))


## just islands ####
island.reef.df2 <- reef.df2 |>
  filter(geo == "island") |>
  droplevels()

write_csv(island.reef.df2, here("NFF_data", "ch4_island_reef_wide_df2.csv"))
saveRDS(island.reef.df2, file = here("NFF_data", "ch4_island_reef_wide_df2.RData"))

## just atolls ####

atoll.reef.df2 <- reef.df2 |>
  filter(geo == "atoll") |>
  droplevels()

write_csv(atoll.reef.df2, here("NFF_data", "ch4_atoll_reef_wide_df2.csv"))
saveRDS(atoll.reef.df2, file = here("NFF_data", "ch4_atoll_reef_wide_df2.RData"))




# Summary by reef for pred teleost UVC data ####
## predatory fish ####
pred.tel.uvc.reef.df <- pred.tel.uvc.survey.sum.df |>
  group_by(reef_name) |>
  summarise(
    across(c(site_name), \(x) first(x)),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  mutate((pred.tel.uvc.survey.sum.df |>
            group_by(reef_name) |>
            summarise(n_survey = n_distinct(UniqueID, na.rm = TRUE))))

write_csv(pred.tel.uvc.reef.df, here("NFF_data", "pred_tel_uvc_sum_reef_2023_02_28.csv"))
saveRDS(pred.tel.uvc.reef.df, file = here("NFF_data", "pred_tel_uvc_sum_reef_2023_02_28.RData"))




# Summary by reef for prey fish UVC data ####
## "prey" species only ####
prey.uvc.reef.df <- prey.uvc.survey.sum.df |>
  group_by(reef_name) |>
  summarise(
    across(c(site_name), first),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  mutate((prey.uvc.survey.sum.df |>
            group_by(reef_name) |>
            summarise(n_survey = n_distinct(UniqueID, na.rm = TRUE))))

write_csv(prey.uvc.reef.df , here("NFF_data", "prey_uvc_sum_reef_2023_02_28.csv"))
saveRDS(prey.uvc.reef.df, file = here("NFF_data", "prey_uvc_sum_reef_2023_02_28.RData"))


## count surveys per reef ####
survey_count <- survey.wide.df2 |>
  group_by(reef_name) |>
  summarise(n_uvc = n_distinct(UniqueID, na.rm = TRUE))

## count bruvs per reef ####
bruv_count <- elasmo.bruv.raw |>
  group_by(reef_name) |>
  summarise(n_bruv = n_distinct(set_code, na.rm = TRUE)) |>
  merge(survey_count, by = c("reef_name"))

## count # of fish observations ####
fish_obs.d1 <- fish.uvc.raw |>
  filter(
    site_name != "Uapou",
    site_name != "Nuka Hiva",
    reef_name != "Marutea 2",
    UniqueID != "RIT1_1"
  )
# see that there are 4 ID to genus so can calculate % from there
# length(unique(fish_obs.d1$UniqueID)) # 119
#@NFF is this what you mean? Else what are you checking for here? ####
# general tip: leave mini test code in comments, with answers & dates,
# so you know what you were testing for, and whether stuff's changed

##### count number of shark unknown sets ####
# 1801 total sets used here
#@NFF: if object never used, don't create it, just spit out result ####
# uk_sets <- as.numeric(
as.numeric(
  elasmo.bruv.raw |>
    filter(
      Unknown.Shark != 0,
      Whitetip.reef.shark == 0,
      Grey.reef.shark == 0,
      Blacktip.reef.shark == 0,
      Tawny.nurse.shark == 0,
      Lemon.shark == 0,
      Great.hammerhead.shark == 0,
      Common.Blacktip.shark == 0,
      Scalloped.hammerhead.shark == 0,
      Silvertip.shark == 0,
      Tiger.shark == 0,
      site_name != "Uapou",
      site_name != "Nuka Hiva",
      reef_name != "Marutea 2"
    ) |>
    summarise(count = n())
  ) # 2025-02-24: 7

### isl_group sums ####
isl_grp_means <- survey.wide.df2 |>
  group_by(isl_grp) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

isl_grp_sd <- survey.wide.df2 |>
  group_by(isl_grp) |>
  summarise(across(where(is.numeric), \(x) sd(x, na.rm = TRUE)))

## 3D scatterpolt ####
d3_chi_plot2 <- (plot_ly(
  data = survey.wide.df2,
  x = ~maxn_shark,
  y = ~pred_tel_biomass_g_per_m2,
  z = ~chi_benthos_percent,
  type = "scatter3d",
  mode = "markers",
  color = ~geo)) |>
  layout(scene = list(
    xaxis = list(title = "Shark MaxN"),
    yaxis = list(title = "Pred. Teleost Biomass (g/m2)"),
    zaxis = list(title = "CCA + Hard Coral % Cover")
  ))
d3_chi_plot2




# Compare Bruvs Pred. Teleost BRUVS vs UVC ####
## make df ####
compare.tel.df1 <- teleost.bruv.raw |>
  dplyr::select(-c(lutjanidae_maxN:lethrinidae_maxN_a, carangidae_maxN_b:total_maxN_b)) |>
  dplyr::rename(teleost_maxn = total_maxN_a, site_name = site, reef_name = reef) |>
  group_by(site_name, reef_name) |>
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), \(x) first(x)),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  # merge(trash.tel.df1, by = c("reef_name")) |> # duplicates teleost_maxn, breaks ggplot call
  merge(survey.wide.df2 |>
          dplyr::select(reef_name, pred_tel_biomass_g_per_m2) |>
          group_by(reef_name) |>
          summarise(pred_tel_biomass_g_per_m2 = mean(pred_tel_biomass_g_per_m2, na.rm = TRUE)),
        by = c("reef_name")) |> # SD fix for missing pred_tel_biomass_g_per_m2 in ggplot call below
  filter(site_name != "Nuka Hiva")

## plot ####
scatter_pred_tel_plot1 <- ggplot(compare.tel.df1,
                                 aes(x = teleost_maxn,
                                     y = pred_tel_biomass_g_per_m2)) +
  geom_point(size = 6, shape = 21, aes(fill = reef_name), colour = "black") +
  scale_fill_viridis(option = "turbo", discrete = TRUE, name = "Site") +
  ggpubr::theme_pubr(base_size = 14) +
  xlab("Pred. Teleost MaxN BRUVS") +
  ylab("Pred. Teleost Biomass (g/m2) UVC") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
scatter_pred_tel_plot1
ggsave(filename = here("NFF_data", "scatter_pred_tel_plot1.png"))



# Extract unique colnames from all saved dfs ####
# Reimport all saved dfs
# list RData files in folder
dflist <- as.list(list.files(path = here("NFF_data"), pattern = "RData"))
# read them all into a list, each is a list element
alldfs <- lapply(dflist, function(x) readRDS(here("NFF_data", x)))
# give them names in the list object
names(alldfs) <- unlist(dflist)
# extract colname from all
colnameslist <- lapply(alldfs, function(x) colnames(x))
# collapse to a single vector
colnameslist <- unlist(colnameslist)
# reorder
colnameslist <- sort(colnameslist)
# strip names
names(colnameslist) <- NULL
# remove dupes
colnameslist <- colnameslist[!(duplicated(colnameslist))]
# save csv
write.csv(x = colnameslist,
          file = here("NFF_data", paste0(Sys.Date(), "_AllDfsColnames.csv")),
          row.names = FALSE)

# 2025-02-18 remake functional groups to match SCM DAG ####
# sicklefin lemon sharks
# reef sharks
# invertivores
# planktivores
# herbivores
# other algae
# crustose coraline algae
# hard coral
