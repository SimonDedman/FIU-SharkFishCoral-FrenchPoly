# setwd("~/Documents/My Documents/FinPrint French Poly/Analysis/Ch 4 Rethink Prelim") # Change to appropriate working directory
library(tidyverse)
library(here)

# Data files list#

# import and clean raw data ####

## import organism list ####
fish_uvc_OG <- data.frame(read.csv(here("NFF_data", "fixed_fish_uvc_final_2023_02_28.csv"), header = TRUE, as.is = TRUE))

# import desbiens#
des_categories_raw <- data.frame(read.csv(here("NFF_data", "Trophic_Categorisation_Desbiens.csv"),
  header = TRUE, as.is = TRUE
))
# delete species with more than one functional group#
des_categories <- des_categories_raw[!des_categories_raw$Species %in%
  unique(des_categories_raw
  [
    duplicated(des_categories_raw$Species),
    "Species"
  ]), ]


# make a data frame with all the recorded spp of fish and meta data#

fish.spp.list <- fish_uvc_OG %>%
  select(c(Species, Family, diet.kulbiki, Feeding.group)) %>%
  group_by(Species) %>%
  distinct() %>%
  merge(des_categories,
    by = c("Species", "Family"),
    all.x = TRUE, all.y = FALSE
  ) %>%
  dplyr::rename(
    des_group = Main.Diet,
    uvc.feeding_group = Feeding.group,
    uvc.diet = diet.kulbiki
  ) %>%
  mutate(offical_functional_group = des_group) %>%
  mutate(offical_functional_group = ifelse(uvc.feeding_group == "Invertivore" &
    is.na(des_group),
  "Invertivore",
  (ifelse(uvc.feeding_group == "Coralivore" &
    is.na(des_group),
  "Coralivore",
  des_group
  ))
  )) %>%
  mutate(offical_functional_group = ifelse(uvc.diet == "Coral" &
    is.na(offical_functional_group),
  "Coralivore",
  offical_functional_group
  )) %>%
  mutate(offical_functional_group = ifelse(uvc.diet == "Zooplancton" &
    is.na(offical_functional_group),
  "Planktivore",
  offical_functional_group
  )) %>%
  mutate(offical_functional_group = ifelse(uvc.diet == "Plancton" &
    is.na(offical_functional_group),
  "Planktivore",
  offical_functional_group
  )) %>%
  mutate(offical_functional_group = ifelse(uvc.diet == "Macroinvertebrates" &
    is.na(offical_functional_group),
  "Invertivore",
  offical_functional_group
  )) %>%
  mutate(offical_functional_group = ifelse(uvc.diet == "Microinvertebrates" &
    is.na(offical_functional_group),
  "Invertivore",
  offical_functional_group
  ))

### Under the assumption we are separating out:###
###  predatory teleost families counted on BRUVS & Sharks & Rays ###
### make list of just UVC fish spp. for models ###

fish.spp.list.limited <- fish.spp.list %>%
  filter(
    !Family == "Myliobatidae",
    !Family == "Lutjanidae",
    !Family == "Carangidae",
    !Family == "Carcharhinidae",
    !Family == "Serranidae",
    !Family == "Scombridae",
    !Family == "Lethrinidae",
    !Family == "Sphyraenidae"
  )

### Now what unknowns do we still need to sort out?###

tbd.fish <- fish.spp.list.limited %>%
  filter(is.na(offical_functional_group))
