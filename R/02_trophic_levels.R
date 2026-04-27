# 2025-03-31 Simon Dedman; lookup species against TL for different teleost groups
# Piscivores, Invertivores, Herbivores, Planktivores

# remotes::install_github("ropensci/rfishbase")
library(rfishbase)
library(tidyverse)
fb_tables(server = c("fishbase"), version = "latest")
spptmp <- head(fb_tbl("species"))
famtmp <- head(fb_tbl("families"))

# Piscivores ####
# L355
# survey.wide.df1 <- benthic.raw |>
# merge(pred.bruv.df1
# merge(pred.tel.uvc.survey.sum.df
# biomass_g_Piscivore = pred_tel_biomass_g + biomass_g_Piscivore,

# teleost.bruv.raw = 7 pred tel families

# vector of family names
predfams <- c(
  "Lutjanidae",
  "Scombridae",
  "Megalopidae",
  "Carangidae",
  "Sphyraenidae",
  "Serranidae",
  "Lethrinidae"
)

# pull out the family codes for the predfams
predfamsdf <- data.frame(
  Family = predfams,
  FamCode = fb_tbl("families") |>
    filter(Family %in% predfams) |>
    select(FamCode) |>
    pull()
)

# pull out the species for the predfamcodes
predfamsdf <- fb_tbl("species") |>
  filter(FamCode %in% predfamsdf$FamCode) |>
  select(SpecCode, Genus, Species, FamCode) |>
  # join ecology table to get trophic level
  left_join(fb_tbl("ecology")) |> # Joining with `by = join_by(SpecCode)`
  select(Genus, Species, FamCode, DietTroph, FoodTroph) |>
  # summarise TL by family
  group_by(FamCode) |>
  summarise(
    DietTroph = mean(as.numeric(DietTroph), na.rm = TRUE),
    FoodTroph = mean(as.numeric(FoodTroph), na.rm = TRUE)
  ) |>
  # join family names
  left_join(predfamsdf) |>
  select(Family, everything()) |>
  # use mutate to create a new column 'TL' which is the mean of the two trophic levels
  mutate(TL = rowMeans(pick(DietTroph, FoodTroph), na.rm = TRUE)) |>
  arrange(desc(TL))

mean(predfamsdf$TL, na.rm = TRUE) # 3.99 i.e. 4.


# Invertivores, Herbivores, Planktivores ####
prey.uvc.survey.sum.df <- data.frame(read.csv(
  here("NFF_data", "fish.spp.list.fn.gps.fixed.csv"),
  header = TRUE,
  as.is = TRUE
)) |>
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
  ) |>
  mutate(
    across(
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
    ),
    # Make first letter upper case in Species
    Species = str_to_sentence(Species),
    # replace "Chlororus" with "Chlorurus" in Species
    Species = str_replace(Species, "Chlororus", "Chlorurus")
  ) |> # Species = Genus + species
  left_join(
    fb_tbl("species") |>
      mutate(sci_name = paste(Genus, Species)) |>
      select(sci_name, SpecCode),
    by = c("Species" = "sci_name")
  )

# NA species not including "Genus sp." entries
sppnamelookup <- data.frame(
  original = prey.uvc.survey.sum.df |>
    filter(is.na(SpecCode), !str_ends(Species, " sp| sp.")) |>
    pull(Species) |>
    unique()
)
sppnamelookup$lookup = validate_names(sppnamelookup$original)

# replace non-NAs with validated names
prey.uvc.survey.sum.df <- prey.uvc.survey.sum.df |>
  left_join(
    sppnamelookup |>
      filter(!is.na(lookup)),
    by = c("Species" = "original")
  ) |>
  # replace NA Species with validated names
  mutate(Species = coalesce(lookup, Species)) |>
  select(-lookup)

# NAs only
sppnamelookup <- sppnamelookup |>
  filter(is.na(lookup))
# Fishbase lookups
sppnamelookup$lookup <- c(
  "Cheilinus chlorourus",
  "Hemigymnus fasciatus",
  "Pseudobalistes fuscus",
  "Thalassoma trilobatum",
  "Zebrasoma velifer",
  "Zebrasoma scopas"
)

# replace NAs with validated names
prey.uvc.survey.sum.df <- prey.uvc.survey.sum.df |>
  left_join(sppnamelookup, by = c("Species" = "original")) |>
  # replace NA Species with validated names
  mutate(Species = coalesce(lookup, Species)) |>
  select(-lookup) |>
  left_join(
    fb_tbl("species") |>
      mutate(sci_name = paste(Genus, Species)) |>
      rename(SpecCodeNew = SpecCode) |>
      select(sci_name, SpecCodeNew),
    by = c("Species" = "sci_name")
  ) |>
  mutate(SpecCode = coalesce(SpecCodeNew, SpecCode)) |>
  select(-SpecCodeNew)

# "Genus sp." entries
prey.uvc.survey.sum.df |>
  filter(is.na(SpecCode), str_ends(Species, " sp| sp.")) |>
  pull(Species) |>
  unique()
# "Halichoeres sp."  "Myripristis sp."  "Neoniphon sp."    "Sargocentron sp." "Scarus sp"
# n of 6. IGNORE THESE

# Add trophic level
prey.uvc.survey.sum.df <- prey.uvc.survey.sum.df |>
  left_join(
    fb_tbl("ecology") |> # join_by(SpecCode)
      select(SpecCode, DietTroph, FoodTroph)
  ) |>
  mutate(TL = rowMeans(pick(DietTroph, FoodTroph), na.rm = TRUE))

# Get mean TL for each functional group
prey.uvc.survey.sum.df |>
  group_by(OfficialFnGp) |>
  summarise(meanTL = mean(TL, na.rm = TRUE)) |>
  arrange(desc(meanTL))

# OfficialFnGp meanTL
# Piscivore      3.86
# Invertivore    3.39
# Planktivore    2.93
# Herbivore      2.46
