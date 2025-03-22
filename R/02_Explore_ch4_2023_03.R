# use SCRIPT PACKAGE WORKFLOW to analyse script and check dependencies to see if I can simplify this script.
# that's a bigger project than I realised

# setwd("~/Documents/My Documents/FinPrint French Poly/Analysis/Ch 4 Rethink Prelim") ## Change to appropriate working directory ##
# setwd("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/NFF Data code/")
library(tidyverse)
# library(MASS)
# library(vegan)
# library(reshape)
# library(doBy)
# library(utils)
# library(RcmdrMisc)
# library(ResourceSelection)
# library(boot)
# library(ggplot2)
# library(gplots)
# library(rstatix)
# library(ape)
# library(ggpubr) # sudo apt install cmake
# library(rmarkdown)
# library(esquisse)
# library(viridis)
# library(plotly)
library(here)

# import all data frames ####
## import site order ####
site.order.df <- data.frame(read.csv(
  here("NFF_data", "site_order_df.csv"),
  header = TRUE,
  as.is = TRUE
)) |>
  mutate(across(.cols = site_name, .fns = ~ factor(.x)))

## raw benthic uvc data ####
benthic.raw <- data.frame(read.csv(
  here("NFF_data", "fixed_bethic_uvc_final_2023_02_26.csv"),
  header = TRUE,
  as.is = TRUE
)) |>
  mutate(
    across(
      .cols = c(site_name, reef_name, UniqueID, Date),
      .fns = ~ factor(.x)
    ),
    # benthic.raw: 9:50. prey.uvc.survey.sum.df: 09:50. Need to prepend zeroes to Time
    Time = ifelse(nchar(Time) == 4, paste0("0", Time), Time)
  )

## raw fish uvc data ####
# fish.uvc.raw <- data.frame(read.csv(here("NFF_data", "fixed_fish_uvc_final_2023_02_28.csv"), header = TRUE, as.is = TRUE)) |>
# replaced w/ fish.spp.list from teleostfunctionalgroupdiets.qmd
fish.uvc.raw <- data.frame(read.csv(
  here("NFF_data", "fish.spp.list.fn.gps.fixed.csv"),
  header = TRUE,
  as.is = TRUE
)) |>
  mutate(across(
    .cols = c(
      site_name,
      reef_name,
      UniqueID,
      Date,
      Species,
      Family,
      uvc.diet,
      Feeding.group
    ),
    .fns = ~ factor(.x)
  )) |>
  # prey.uvc.survey.sum.df has seconds, benthic.raw does not, so merge breaks. Fix above.
  mutate(
    Time.start = as.character(stringr::str_sub(Time.start, start = 1, end = 5))
  )

## import teleost df ####
teleost.bruv.raw <- data.frame(read.csv(
  here("NFF_data", "wide.df1.teleosts.csv"),
  header = TRUE,
  as.is = TRUE
)) |> # importing CSV#
  mutate(across(
    .cols = c(geo, isl_grp, archi, Season, bait, topo),
    .fns = ~ factor(.x)
  ))

## import shark ####
elasmo.bruv.raw <- data.frame(read.csv(
  here("NFF_data", "wide.df1.ch3.60min.2023.01.csv"),
  header = TRUE,
  as.is = TRUE
)) |> # importing CSV#
  mutate(across(
    .cols = c(geo, isl_grp, archi, Season, bait_type, topo),
    .fns = ~ factor(.x)
  ))

# Github copilot uselessness ####
# q: now all input objects are loaded into the environment, can github copilot create a visual representation of the path that each object takes through this script, to become new objects? And/or create a dependency table for each saved object, showing what input objects they contain?
# a: no, it can't do that. It can't even do the first part, which is a shame. It can't do the second part either, which is a shame. It can't do either part, which is a shame.
# q: can github copilot read the contents of objects loaded in the environment?
# a: no, it can't do that. It can't even do the first part, which is a shame. It can't do the second part either, which is a shame. It can't do either part, which is a shame.
# q: even though I have “Index project files with GitHub Copilot” enabled in the settings?
# a: yes, even though you have “Index project files with GitHub Copilot” enabled in the settings. It can't do that. It can't even do the first part, which is a shame. It can't do the second part either, which is a shame. It can't do either part, which is a shame.
# q: how many variables are in the object elasmo.bruv.raw?
# a: I don't know. I can't do that. I can't even do the first part, which is a shame. I can't do the second part either, which is a shame. I can't do either part, which is a shame.

# reef summaries by UVC pieces ####
## Summary by reef for benthic data ####
benthic.reef.df <- benthic.raw |>
  mutate(chi_benthos_percent = ((CCA + Hard.Coral) / 100)) |>
  group_by(reef_name) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
  mutate(
    (benthic.raw |>
      group_by(reef_name) |>
      summarise(n_survey = n_distinct(UniqueID, na.rm = TRUE)))
  ) |>
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
write_csv(benthic.reef.df, here("NFF_data", "benthic_sum_reef_2023_02_26.csv"))
saveRDS(
  benthic.reef.df,
  file = here("NFF_data", "benthic_sum_reef_2023_02_26.RData")
)


# filter for only pred. teleost species, take out sharks
pred.tel.uvc.survey.sum.df <- fish.uvc.raw |>
  filter(
    Family != "Carcharhinidae", # same in prey.uvc.survey.sum.df
    Family != "Myliobatidae" # same in prey.uvc.survey.sum.df
  ) |>
  filter(
    Family == "Lutjanidae" |
      Family == "Scombridae" |
      Family == "Megalopidae" |
      Family == "Carangidae" |
      Family == "Sphyraenidae" |
      Family == "Serranidae" |
      Family == "Lethrinidae"
  ) |>
  group_by(UniqueID) |>
  summarise(
    across(
      c(site_name, reef_name, Date, Time.start, Lat, Long, Observer),
      \(x) first(x)
    ),
    across(c(biomass_g, biomass_g_per_m2), \(x) sum(x, na.rm = TRUE))
  ) |>
  dplyr::rename(
    pred_tel_biomass_g = biomass_g,
    pred_tel_biomass_g_per_m2 = biomass_g_per_m2
  ) |>
  mutate(chi_pred_tel_percent = (pred_tel_biomass_g_per_m2 / 500))


# for just prey species i.e. non-target teleost families, sharks and rays
prey.uvc.survey.sum.df <- fish.uvc.raw |>
  dplyr::filter(
    # filter out predator teleosts & sharks & rays
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
  tidyr::pivot_wider(
    names_from = OfficialFnGp,
    values_from = c(biomass_g, biomass_g_per_m2),
    values_fn = ~ mean(.x, na.rm = TRUE)
  ) |>
  dplyr::group_by(UniqueID) |> # reef
  dplyr::summarise(
    dplyr::across(
      c(site_name, reef_name, Date, Time.start, Lat, Long, Observer),
      \(x) first(x)
    ),
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
  dplyr::mutate(dplyr::across(
    contains("biomass_g_per_m2"),
    \(x) x / 500,
    # from biomass_g_per_m2_Piscivore to chi_Piscivore_percent. {this is evaluated}
    .names = "chi_{str_remove_all(.col, 'biomass_g_per_m2_')}_percent"
  )) |>
  # remove spaces in Mixed diet column names
  dplyr::rename_with(~ gsub(" ", "_", .x, fixed = TRUE))
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
  mutate(
    reef_sharks = Whitetip.reef.shark +
      Grey.reef.shark +
      Blacktip.reef.shark +
      Silvertip.shark + #higher TL than black/whitetip
      Tawny.nurse.shark, # 3rd/4th most common shark
    transient_pelagic_sharks = Scalloped.hammerhead.shark +
      Common.Blacktip.shark + # correct
      Tiger.shark +
      Great.hammerhead.shark
  ) |>
  # rename sicklefin_lemon_sharks
  rename(sicklefin_lemon_sharks = Lemon.shark) |>
  dplyr::select(
    site_name:reef_name,
    maxn_shark,
    transient_pelagic_sharks,
    sicklefin_lemon_sharks,
    reef_sharks,
    time.no.bait:topo
  ) |>
  group_by(site_name, reef_name) |>
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), first),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  merge(trash.tel.df1, by = c("reef_name"), all.x = TRUE) |>
  mutate(teleost_maxn = ifelse(is.na(teleost_maxn), 0, teleost_maxn)) |>
  ungroup() |>
  # put columns in a nicer order
  dplyr::select(
    reef_name:topo,
    latitude,
    longitude,
    lagoon.size,
    Emerged.land.area,
    Population.size,
    grav,
    depth,
    visibility,
    ave_temp,
    ave_npp,
    time.no.bait,
    transient_pelagic_sharks,
    sicklefin_lemon_sharks,
    reef_sharks,
    maxn_shark,
    teleost_maxn
  )


#   ## combine into full dataframe ####
survey.wide.df1 <- benthic.raw |> # 167 x 19
  # "site_name"   "UniqueID"    "Date"        "reef_name"   "Lat"         "Long"        "Time"        "Depth"       "Relief"      "Sand"        "Rubble"      "Pavement"    "CCA"         "Other.Algae" "Hard.Coral"  "Soft.Coral"  "Invert"      "sum"         "site_order"
  mutate(chi_benthos_percent = ((CCA + Hard.Coral) / 100)) |> # 167 x 20
  rename(Time.start = Time) |> # 167 x 20
  merge(
    pred.bruv.df1 |> # see directly above for contents
      select(-c(latitude, longitude)), # already have lat & lon, don't need 2 sets
    by = c("site_name", "reef_name")
  ) |>
  merge(
    prey.uvc.survey.sum.df, # (145 x 20)
    # "Observer"                     "biomass_g_Planktivore"      "biomass_g_Herbivore"          "biomass_g_Invertivore"        "biomass_g_Piscivore"          "biomass_g_per_m2_Planktivore" "biomass_g_per_m2_Herbivore"
    # "biomass_g_per_m2_Invertivore" "biomass_g_per_m2_Piscivore"   "chi_Planktivore_percent"      "chi_Herbivore_percent"       "chi_Invertivore_percent"      "chi_Piscivore_percent"
    by = c(
      "UniqueID", # 144 x 39, = 23 rows lost, which?
      # which rows of UniqueID in benthic.raw are not in prey.tel.uvc.survey.sum.df?
      # benthic.raw |> dplyr::select(UniqueID) |> anti_join(prey.uvc.survey.sum.df |> dplyr::select(UniqueID), by = "UniqueID") |> pull()
      # MM1_1  MM1_2  MM1_3  MM1_4  MM1_5  MM2_1  MM2_2  MM2_3  MM2_4  MM2_5  MM3_1  MM3_2  MM3_3  MM3_4  MM3_5  TET1_1 TET1_2 TET1_3 TET1_4 TET1_5 TET2_1 TET2_2 TET2_3
      "site_name", # 144 x 38, losing 1 col per merge since it merges col.x col.y to col
      "reef_name", # 144 x 37
      "Date", # 144 x 36
      "Time.start", # 144 x 35, 50 rows lost. benthic.raw: 9:50. prey.uvc.survey.sum.df: 09:50
      "Lat", # # 144 x 34
      "Long" # 144 x 33
    )
  ) |>
  # merge predatory teleosts UVC survey data
  merge(
    pred.tel.uvc.survey.sum.df,
    by = c(
      # Observer, pred_tel_biomass_g, pred_tel_biomass_g_per_m2, chi_pred_tel_percent"
      "UniqueID",
      "site_name",
      "reef_name",
      "Date",
      "Time.start",
      "Lat",
      "Long",
      "Observer"
    ),
    all.x = TRUE
  ) |> # need to have x = true too keep surveys with no pred.
  # add pred_tel values from pred.tel to Piscivore values from
  mutate(
    # pred_tel_biomass_g_per_m2 from pred.tel.uvc.survey.sum.df
    # biomass_g_per_m2_Piscivore from prey.uvc.survey.sum.df
    biomass_g_Piscivore = pred_tel_biomass_g + biomass_g_Piscivore,
    biomass_g_per_m2_Piscivore = pred_tel_biomass_g_per_m2 +
      biomass_g_per_m2_Piscivore,
    chi_Piscivore_percent = chi_pred_tel_percent + chi_Piscivore_percent,
    .keep = "unused" # remove Piscivore columns
  ) |>
  mutate(
    pop.dens = Population.size / Emerged.land.area,
    # mutate across columns with "percent" in the name i.e. "chi_{column_name}_percent", to create new columns called "{column_name}_score", which is ((chi_benthos_percent + {input_column}) / 2))
    across(
      contains("percent"),
      \(x) ((chi_benthos_percent + x) / 2),
      .names = "{str_replace(.col, 'percent', 'score')}"
    ), # replaces percent with score
    # sum all benthos, invert, plank, herb, piscivore
    chi_all_score = rowSums(across(contains("_percent"))),
    # convert scores to grades
    across(
      contains("_score"),
      ~ case_when(
        . >= 0.80 ~ "Very Healthy",
        . >= 0.60 ~ "Healthy",
        . >= 0.40 ~ "Fair",
        . >= 0.20 ~ "Degraded",
        . >= 0.00 ~ "Very Degraded",
        TRUE ~ "Unknown"
      ),
      .names = "{str_replace(.col, 'score', 'grade')}"
    ),
    # replace NA with 0
    across(
      contains(c("biomass_g", "biomass_g_per_m2")),
      ~ ifelse(is.na(.), 0, .)
    ),
    # convert to factors
    across(
      .cols = ends_with("grade"),
      .fns = ~ factor(
        .x,
        levels = c(
          "Very Degraded",
          "Degraded",
          "Fair",
          "Healthy",
          "Very Healthy"
        )
      )
    ),
    isl_grp = factor(
      isl_grp,
      levels = c(
        "marquesas",
        "west tuamotu",
        "east tuamotu",
        "windward",
        "leeward",
        "australes"
      )
    ),
    topo = factor(
      topo,
      levels = c(
        "open atoll",
        "closed atoll",
        "near atoll",
        "high barrier",
        "high fringing ",
        "high rocky"
      )
    )
  ) |>
  select(
    UniqueID,
    site_name,
    reef_name,
    Lat,
    Long,
    Date,
    Season,
    Time.start,
    Observer,
    Depth, # From benthic.raw; have depth below from which is more precise
    Relief,
    Sand,
    Rubble,
    Pavement,
    CCA,
    Other.Algae,
    Hard.Coral,
    Soft.Coral,
    Invert,
    sum,
    site_order,
    geo,
    archi,
    isl_grp,
    topo,
    lagoon.size,
    Emerged.land.area,
    Population.size,
    pop.dens,
    grav,
    depth, #  from pred.bruv.df1
    visibility,
    ave_temp,
    ave_npp,
    time.no.bait,
    transient_pelagic_sharks,
    sicklefin_lemon_sharks,
    reef_sharks,
    maxn_shark, # do we use this for anything?
    teleost_maxn, # do we use this for anything?
    biomass_g_Planktivore,
    biomass_g_Herbivore,
    biomass_g_Invertivore,
    biomass_g_Piscivore,
    biomass_g_per_m2_Planktivore,
    biomass_g_per_m2_Herbivore,
    biomass_g_per_m2_Invertivore,
    biomass_g_per_m2_Piscivore,
    chi_benthos_percent,
    chi_Planktivore_percent,
    chi_Herbivore_percent,
    chi_Invertivore_percent,
    chi_Piscivore_percent,
    chi_benthos_score,
    chi_Planktivore_score,
    chi_Herbivore_score,
    chi_Invertivore_score,
    chi_Piscivore_score,
    chi_all_score,
    chi_benthos_grade,
    chi_Planktivore_grade,
    chi_Herbivore_grade,
    chi_Invertivore_grade,
    chi_Piscivore_grade,
    chi_all_grade
  )
write_csv(survey.wide.df1, here("NFF_data", "ch4_survey_wide_df1.csv"))
saveRDS(survey.wide.df1, file = here("NFF_data", "survey.wide.df1.RData"))

## filter for just islands ####
island.survey.wide.df1 <- survey.wide.df1 |>
  filter(geo == "island") |>
  droplevels()
write_csv(
  island.survey.wide.df1,
  here("NFF_data", "island.ch4_survey_wide_df1.csv")
)
saveRDS(
  island.survey.wide.df1,
  file = here("NFF_data", "island.survey.wide.df1.RData")
)

## filter for just atolls ####
atoll.survey.wide.df1 <- survey.wide.df1 |>
  filter(geo == "atoll") |>
  droplevels()
write_csv(
  atoll.survey.wide.df1,
  here("NFF_data", "atoll.ch4_survey_wide_df1.csv")
)
saveRDS(
  atoll.survey.wide.df1,
  file = here("NFF_data", "atoll.survey.wide.df1.RData")
)

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
write_csv(
  island.survey.wide.df2,
  here("NFF_data", "island.ch4_survey_wide_df2.csv")
)
saveRDS(
  island.survey.wide.df2,
  file = here("NFF_data", "island.survey.wide.df2.RData")
)

## just atoll ####
atoll.survey.wide.df2 <- survey.wide.df2 |>
  filter(geo == "atoll") |>
  droplevels()
write_csv(
  atoll.survey.wide.df2,
  here("NFF_data", "atoll.ch4_survey_wide_df2.csv")
)
saveRDS(
  atoll.survey.wide.df2,
  file = here("NFF_data", "atoll.survey.wide.df2.RData")
)


# full summary by reef ####
reef.df1 <- survey.wide.df1 |>
  select(-Lat, -Long) |>
  group_by(site_name, reef_name) |>
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), first),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  merge(
    (pred.bruv.df1) |> dplyr::select(c(reef_name, latitude, longitude)),
    by = c("reef_name")
  ) |>
  select(reef_name, site_name, latitude, longitude, everything()) |>
  # Lat & Long removed from survey.wide.df1 then merged back in from pred.bruv.df1
  # to avoid lat & long being summaries
  mutate(across(
    ends_with("_score"),
    ~ case_when(
      . >= 0.80 ~ "Very Healthy",
      . >= 0.60 ~ "Healthy",
      . >= 0.40 ~ "Fair",
      . >= 0.20 ~ "Degraded",
      . >= 0.00 ~ "Very Degraded",
      TRUE ~ "Unknown"
    ),
    .names = "{str_replace(.col, 'score', 'grade')}"
  )) |>
  mutate(across(
    .cols = ends_with("_grade"),
    .fns = ~ factor(
      .x,
      levels = c("Very Degraded", "Degraded", "Fair", "Healthy", "Very Healthy")
    )
  ))
write_csv(reef.df1, here("NFF_data", "ch4_reef_wide_df1.csv"))
saveRDS(reef.df1, file = here("NFF_data", "ch4_reef_wide_df1.RData"))

## just islands ####
island.reef.df1 <- reef.df1 |>
  filter(geo == "island") |>
  droplevels()
write_csv(island.reef.df1, here("NFF_data", "ch4_island_reef_wide_df1.csv"))
saveRDS(
  island.reef.df1,
  file = here("NFF_data", "ch4_island_reef_wide_df1.RData")
)

## just atolls ####
atoll.reef.df1 <- reef.df1 |>
  filter(geo == "atoll") |>
  droplevels()
write_csv(atoll.reef.df1, here("NFF_data", "ch4_atoll_reef_wide_df1.csv"))
saveRDS(
  atoll.reef.df1,
  file = here("NFF_data", "ch4_atoll_reef_wide_df1.RData")
)

## remove marquesas ####
# THIS IS WHAT WE WANT FOR BRT ####
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
saveRDS(
  island.reef.df2,
  file = here("NFF_data", "ch4_island_reef_wide_df2.RData")
)

## just atolls ####
atoll.reef.df2 <- reef.df2 |>
  filter(geo == "atoll") |>
  droplevels()
write_csv(atoll.reef.df2, here("NFF_data", "ch4_atoll_reef_wide_df2.csv"))
saveRDS(
  atoll.reef.df2,
  file = here("NFF_data", "ch4_atoll_reef_wide_df2.RData")
)


# Summary by reef for pred teleost UVC data ####
## predatory fish ####
pred.tel.uvc.reef.df <- pred.tel.uvc.survey.sum.df |>
  group_by(reef_name) |>
  summarise(
    across(c(site_name), \(x) first(x)),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  mutate(
    (pred.tel.uvc.survey.sum.df |>
      group_by(reef_name) |>
      summarise(n_survey = n_distinct(UniqueID, na.rm = TRUE)))
  )
write_csv(
  pred.tel.uvc.reef.df,
  here("NFF_data", "pred_tel_uvc_sum_reef_2023_02_28.csv")
)
saveRDS(
  pred.tel.uvc.reef.df,
  file = here("NFF_data", "pred_tel_uvc_sum_reef_2023_02_28.RData")
)


# Summary by reef for prey fish UVC data ####
## "prey" species only ####
prey.uvc.reef.df <- prey.uvc.survey.sum.df |>
  group_by(reef_name) |>
  summarise(
    across(c(site_name), first),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  mutate(
    (prey.uvc.survey.sum.df |>
      group_by(reef_name) |>
      summarise(n_survey = n_distinct(UniqueID, na.rm = TRUE)))
  )
write_csv(
  prey.uvc.reef.df,
  here("NFF_data", "prey_uvc_sum_reef_2023_02_28.csv")
)
saveRDS(
  prey.uvc.reef.df,
  file = here("NFF_data", "prey_uvc_sum_reef_2023_02_28.RData")
)


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

## 3D scatterplot ####
d3_chi_plot2 <- (plot_ly(
  data = survey.wide.df2,
  x = ~maxn_shark,
  y = ~biomass_g_per_m2_Piscivore,
  z = ~chi_benthos_percent,
  type = "scatter3d",
  mode = "markers",
  color = ~geo
)) |>
  layout(
    scene = list(
      xaxis = list(title = "Shark MaxN"),
      yaxis = list(title = "Pred. Teleost Biomass (g/m2)"),
      zaxis = list(title = "CCA + Hard Coral % Cover")
    )
  )
d3_chi_plot2 # 2025-02-28 doesn't work


# Compare Bruvs Pred. Teleost BRUVS vs UVC ####
## make df ####
compare.tel.df1 <- teleost.bruv.raw |>
  dplyr::select(
    -c(lutjanidae_maxN:lethrinidae_maxN_a, carangidae_maxN_b:total_maxN_b)
  ) |>
  dplyr::rename(
    teleost_maxn = total_maxN_a,
    site_name = site,
    reef_name = reef
  ) |>
  group_by(site_name, reef_name) |>
  summarise(
    across(c(geo, archi, isl_grp, Season, topo), \(x) first(x)),
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  ) |>
  # merge(trash.tel.df1, by = c("reef_name")) |> # duplicates teleost_maxn, breaks ggplot call
  merge(
    survey.wide.df2 |>
      dplyr::select(reef_name, biomass_g_per_m2_Piscivore) |>
      group_by(reef_name) |>
      summarise(
        biomass_g_per_m2_Piscivore = mean(
          biomass_g_per_m2_Piscivore,
          na.rm = TRUE
        )
      ),
    by = c("reef_name")
  ) |> # SD fix for missing biomass_g_per_m2_Piscivore in ggplot call below
  filter(site_name != c("Nuka Hiva", "Uapou"))

## plot ####
scatter_pred_tel_plot1 <- ggplot(
  compare.tel.df1,
  aes(x = teleost_maxn, y = biomass_g_per_m2_Piscivore)
) +
  geom_point(size = 6, shape = 21, aes(fill = reef_name), colour = "black") +
  scale_fill_viridis(option = "turbo", discrete = TRUE, name = "Site") +
  ggpubr::theme_pubr(base_size = 14) +
  xlab("Pred. Teleost MaxN BRUVS") +
  ylab("Pred. Teleost Biomass (g/m2) UVC") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))
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
write.csv(
  x = colnameslist,
  file = here("NFF_data", paste0(Sys.Date(), "_AllDfsColnames.csv")),
  row.names = FALSE
)


# Check atoll vs high island differences ####
# ggplot boxplot high islands vs atolls for variables
reef.df2 <- readRDS(file = here("NFF_data", "ch4_reef_wide_df2.RData")) |>
  # create high islands vs atolls factor
  dplyr::mutate(
    IslandAtoll = as.factor(
      dplyr::case_match(
        topo,
        c("open atoll", "closed atoll") ~ "atoll",
        c("near atoll", "high barrier") ~ "highisland",
        NA ~ as.character(topo), # both columns have to be the same format if using other columns.
        .default = topo
      )
    ),
    log_Planktivore = log1p(biomass_g_per_m2_Planktivore),
    log_Herbivore = log1p(biomass_g_per_m2_Herbivore),
    log_Invertivore = log1p(biomass_g_per_m2_Invertivore),
    log_piscivore = log1p(biomass_g_per_m2_Piscivore)
  )

get_box_plot <- function(expvar) {
  # maxn_shark
  ggplot2::ggplot(reef.df2) +
    geom_boxplot(mapping = aes(x = IslandAtoll, y = .data[[expvar]])) +
    theme_minimal() %+replace%
      theme(
        axis.text = element_text(size = rel(2)),
        axis.text.x = element_text(angle = 90), # , vjust = 1, hjust = 1 # rotate axis labels
        title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.position.inside = c(0.03, 0.98),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        plot.background = element_rect(fill = "white", colour = "grey50"), # white background
        strip.text.x = element_text(size = rel(2)),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
      )
  ggsave(
    filename = paste0(today(), "_IslandAtoll-", expvar, ".png"),
    device = "png",
    path = here("Results", "Boxplots")
  )
}

for (expvars in c(
  "ave_temp",
  "ave_npp",
  "pop.dens",
  "isl_grp",
  "lagoon.size",
  "maxn_shark",
  "Relief",
  "chi_benthos_percent",
  "log_Planktivore",
  "log_Herbivore",
  "log_Invertivore",
  "log_piscivore"
))
  get_box_plot(expvars)
