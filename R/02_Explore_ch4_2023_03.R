# use SCRIPT PACKAGE WORKFLOW to analyse script and check dependencies to see if I can simplify this script.
# that's a bigger project than I realised


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
site.order.df <- data.frame(read.csv(here("NFF_data", "site_order_df.csv"), header = TRUE, as.is = TRUE)) |>
  mutate(across(.cols = site_name,
                .fns = ~ factor(.x)))

## raw benthic uvc data ####
benthic.raw <- data.frame(read.csv(here("NFF_data", "fixed_bethic_uvc_final_2023_02_26.csv"), header = TRUE, as.is = TRUE)) |>
  mutate(across(.cols = c(site_name, reef_name, UniqueID, Date),
                .fns = ~ factor(.x)))

## raw fish uvc data ####
# fish.uvc.raw <- data.frame(read.csv(here("NFF_data", "fixed_fish_uvc_final_2023_02_28.csv"), header = TRUE, as.is = TRUE)) |>
# replaced w/ fish.spp.list from teleostfunctionalgroupdiets.qmd
fish.uvc.raw <- data.frame(read.csv(here("NFF_data", "fish.spp.list.fn.gps.fixed.csv"), header = TRUE, as.is = TRUE)) |>
  mutate(across(.cols = c(site_name, reef_name, UniqueID, Date, Species, Family, uvc.diet, Feeding.group),
                .fns = ~ factor(.x))) |>
  # prey.uvc.survey.sum.df has seconds, benthic.raw does not, so merge breaks. Fix above.
  mutate(Time.start = as.character(stringr::str_sub(Time.start, start = 1, end = 5)))

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

# 2025-02-26 FROMHERE ####
# can I use prey.uvc.survey.sum.df code below for both pred & prey the same?

# need full data frame with survey as the primary sampling unit ####
## first sort out fish
# filter for only pred. teleost species, take out sharks
pred.tel.uvc.survey.sum.df <- fish.uvc.raw |>
  filter(
    Family != "Carcharhinidae", # same in prey.uvc.survey.sum.df
    Family != "Myliobatidae" # same in prey.uvc.survey.sum.df
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

prey.uvc.survey.sum.df <- fish.uvc.raw |>
  dplyr::filter( # filter out predator teleosts & sharks & rays
    Family != "Lutjanidae",
    Family != "Scombridae",
    Family != "Megalopidae",
    Family != "Carangidae",
    Family != "Sphyraenidae",
    Family != "Serranidae",
    Family != "Lethrinidae",
    Family != "Carcharhinidae", # sharks
    Family != "Myliobatidae" # rays
    # Feeding.group != "Piscivore"
  ) |>
  # Split to invertivores planktivores herbivores by Feeding.group
  # pivot.wider to get biomass_g and biomass_g_per_m2 columns for each Feeding.group
  tidyr::pivot_wider(names_from = OfficialFnGp,
                     # TODO do this on fish UVC raw ####
                     # not pred & prey
                     values_from = c(biomass_g, biomass_g_per_m2),
                     values_fn = ~ mean(.x, na.rm = TRUE)) |>
  dplyr::group_by(UniqueID) |> # reef
  dplyr::summarise(
    dplyr::across(c(site_name, reef_name, Date, Time.start, Lat, Long, Observer), \(x) first(x)),
    dplyr::across(contains("biomass_g"), \(x) sum(x, na.rm = TRUE)) # contains was c(biomass_g, biomass_g_per_m2)
  ) |>
  # CHI = coral health index, g per m2 is how it was sampled. /500 to make consistent to the health index.
  # Kaufman, L., Sala, E., Sandin, S. A., Obura, D. O., Rohwer, F., & Tschirky, J.
  # (2011). Coral health index (CHI): Measuring coral community health.
  # Conservation International. https://portals.iucn.org/library/node/28851
  # NFF paper: To assess general health of each reef I use a modified two
  # dimensional coral health index as described in Kaufman et al. (2011).
  # This coral health index combines two parameters, percent cover of hard corals
  # + crustose coralline algae and fish biomass as a fraction of 500 g/m2,
  # which are averaged to produce a score which ranges from 0 to 1,
  # which is then graded into five categories: very degraded, degraded, fair,
  # healthy, and very healthy.
  # this needs to be renamed so it doesn't overwrite the original columns
  dplyr::mutate(dplyr::across(contains("biomass_g_per_m2"), \(x) x/500, .names = "chi_{.col}_percent")) |>
  # remove the "_biomass_g_per_m2" prefix from column names containing "percent"
  dplyr::rename_with(.fn = ~ stringr::str_replace(., "_biomass_g_per_m2", ""),
                     .cols = contains("percent")) |>
  # remove spaces in Mixed diet column names
  dplyr::rename_with(~ gsub(" ", "_", .x, fixed = TRUE))
# @NFF Has piscivores; ensure they're used somewhere ####
# retain prey & pred teleosts together in fish uvc raw near top,
# run both through the same processing, then filter as needed


# pare down predators bruvs ####
# maxN teleosts per site
# merged with elasmo.bruv.raw in next block
# mean maxn per site only for families: lutjanidae scombridae megalopidae carangidae sphyraenidae serranidae lethrinidae
# not for: carangidae sphyraenidae serranidae lethrinidae
# maxN a & b: 2 ways of measuring maxN based on unknowns. B is a bad approach, ignore.
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

# which rows of UniqueID in benthic.raw are not in prey.tel.uvc.survey.sum.df?
# benthic.raw |>
#   dplyr::select(UniqueID) |>
#   anti_join(prey.uvc.survey.sum.df |>
#               dplyr::select(UniqueID),
#             by = "UniqueID") |>
#   pull()
# MM1_1  MM1_2  MM1_3  MM1_4  MM1_5  MM2_1  MM2_2  MM2_3  MM2_4  MM2_5  MM3_1  MM3_2  MM3_3  MM3_4  MM3_5  TET1_1 TET1_2 TET1_3 TET1_4 TET1_5 TET2_1 TET2_2 TET2_3

tmp <- survey.wide.df1
# colnames(tmp)[which(!colnames(tmp) %in% colnames(survey.wide.df1))] # "site_name.x" "site_name.y"
#
# class(benthic.raw$Time) # character
# class(prey.uvc.survey.sum.df$Time.start) # character
# benthic.raw$Time[1] # "11:05"
# prey.uvc.survey.sum.df$Time.start[1] # "10:20:00"
# # prey.uvc.survey.sum.df has seconds, benthic.raw does not, so merge breaks. Fix above.

# which rows of tmp aren't present in survey.wide.df1 based on unique combinations of UniqueID, site_name, reef_name, Date, and Time.start?
library(magrittr) # %T>%
survey.wide.df1 |>
  dplyr::rename(Time.start = Time.start.x) |>
  # dplyr::select(UniqueID, site_name, reef_name, Date, Time.start) |>
  anti_join(tmp
            # |>
            #   dplyr::select(UniqueID, site_name, reef_name, Date, Time.start)
            ,
            by = c("UniqueID", "site_name", "reef_name", "Date", "Time.start")) %T>%
  saveRDS(file = here("NFF_data", "fish.uvc.raw-dontMatch-benthic.raw.RData")) ->
  missing

# FROMHERE 2025-02-26 ####
# Time.start mismatch between benthic.raw and prey.uvc.survey.sum.df
# 50 rows lost, perhaps incorrectly

## combine into full dataframe ####
survey.wide.df1 <- benthic.raw |> # 167 x 19
  mutate(chi_benthos_percent = ((CCA + Hard.Coral) / 100)) |> # 167 x 20
  dplyr::rename(Time.start = Time) |> # 167 x 20
  merge(prey.uvc.survey.sum.df, # (145 x 20)
        by = c(
          "UniqueID", # 144 x 39, = 23 rows lost, which?
          "site_name", # 144 x 38, losing 1 col per merge since it merges col.x col.y to col
          "reef_name", # 144 x 37
          "Date"#, # 144 x 36
          # "Time.start", # 94 x 35, 50 rows lost
          # "Lat", # # 94 x 34
          # "Long" # 94 x 33
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
  dplyr::select(-c(latitude, longitude)) |>
  # Removed because we already have lat and lon, don't need 2 sets
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
        # Lat & Long removed from survey.wide.df1 then merged back in from pred.bruv.df1
        # to avoid lat & long being summaries
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
  # TODO fix after removing pred & prey dfs ####
# if we don't have pred & prey dfs then just filter by feeding.group == piscivore?
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
  # TODO fix after removing pred & prey dfs ####
# if we don't have pred & prey dfs then just filter by feeding.group == prey groups?
group_by(reef_name) |>
  summarise(
    across(c(site_name), first),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  mutate((prey.uvc.survey.sum.df |>
            group_by(reef_name) |>
            summarise(n_survey = n_distinct(UniqueID, na.rm = TRUE))))

write_csv(prey.uvc.reef.df, here("NFF_data", "prey_uvc_sum_reef_2023_02_28.csv"))
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
    site_name != "Uapou", # not coral reefs
    site_name != "Nuka Hiva", # not coral reefs
    reef_name != "Marutea 2", # no corresponding UVC surveys here
    UniqueID != "RIT1_1" # no corresponding UVC surveys here
  )
# see that there are 4 ID to genus so can calculate % (of CHI?) from there
# length(unique(fish_obs.d1$UniqueID)) # 119 @ 2025-02-24
# Unknown what this is for.


## count number of shark unknown sets ####
# 1801 total sets used here
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
d3_chi_plot2 # 2025-02-28 doesn't work




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
scatter_pred_tel_plot1 # 2025-02-28 doesn't work
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
write.csv(x = colnameslist, file = here("NFF_data", paste0(Sys.Date(), "_AllDfsColnames.csv")), row.names = FALSE)

# 2025-02-18 remake functional groups to match SCM DAG ####
# sicklefin lemon sharks
# reef sharks
# invertivores
# planktivores
# herbivores
# other algae
# crustose coraline algae
# hard coral
