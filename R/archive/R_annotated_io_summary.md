# R_annotated/ Imports/Exports Reference

Captured from auto-generated annotations dated 2025-10-16 before deletion of the R_annotated/ folder.
Useful as a starting point for building a `_targets.R` pipeline: gives the file-level read/write graph
for each script in `R/` at that point in time. Paths are absolute as captured; treat them as guidance, not truth.

## 01_Teleost_FnGp_TrophLev.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 02_Explore_ch4_2023_03.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_reef_wide_df2.RData
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/fixed_bethic_uvc_final_2023_02_26.csv
#' 
#' @section Exports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/benthic_sum_reef_2023_02_26.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_survey_wide_df1.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/survey.wide.df1.RData
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_survey_wide_df2.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/survey.wide.df2.RData
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_reef_wide_df1.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_reef_wide_df1.RData
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_island_reef_wide_df1.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_atoll_reef_wide_df1.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_reef_wide_df2.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_reef_wide_df2.RData
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_island_reef_wide_df2.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_atoll_reef_wide_df2.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/scatter_pred_tel_plot1.png
#' 

```

## 03_BRT_runs.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_reef_wide_df2.RData
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_atoll_reef_wide_df2.RData
#' 
#' @section Exports:
#'   None
#' 

```

## 03_UVC_funtional_group_fixit.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/fixed_fish_uvc_final_2023_02_28.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/Trophic_Categorisation_Desbiens.csv
#' 
#' @section Exports:
#'   None
#' 

```

## 05_nat_FPDAG_consistency_check.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data//ReefWideBRUVUVC.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv
#' 
#' @section Exports:
#'   None
#' 

```

## 06_nat_FPDAG_consistency_check_DAG1-tophalf-topdown.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data//ReefWideBRUVUVC.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv
#' 
#' @section Exports:
#'   None
#' 

```

## 07_nat_FPDAG_consistency_check_DAG2-tophalf-bottomup.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data//ReefWideBRUVUVC.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv
#' 
#' @section Exports:
#'   None
#' 

```

## 08_nat_FPDAG_consistency_check_DAG3-bottomhalf-bottomup.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ReefWideBRUVUVC.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv
#' 
#' @section Exports:
#'   None
#' 

```

## 09_BART-DAG.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ReefWideBRUVUVC-DAGtested.csv
#' 
#' @section Exports:
#'   None
#' 

```

## 10_DAG-TD-All.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 10_DAG-TD-Atolls.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 10_DAG-TDBU-AtollHI-Collated.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 10_DAG-TD-HighIslands.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 10_Suchinta-DAG.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 11_DAG-BU-All.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 11_DAG-BU-Atolls.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 11_DAG-BU-HighIslands.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

## 11_DAG-TDBU-AtollHI-Collated.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ch4_reef_wide_df2.RData
#' 
#' @section Exports:
#'   None
#' 

```

## 12_DAG-SCMs_Atoll-Vs-HighIsland_TopDown-vs-BottomUp.R

```
#' @section Imports:
#'   - model_path
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/TDhighislandplot.Rds
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/TDatollplot.Rds
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/BUhighislandplot.Rds
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/BUatollplot.Rds
#' 
#' @section Exports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/bayesR2.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/bayesR2summary.csv
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/loo_compare_results.csv
#' 

```

## 12_DAG-SCMs_RawData_Review.R

```
#' @section Imports:
#'   - ~/Dropbox/Galway/Analysis/R/MiscScripts/R/lmplot.R
#' 
#' @section Exports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/Mean_biomass_g_per_m2_per_reef.csv
#' 

```

## 13_DAG-network-plot.R

```
#' @section Imports:
#'   - /media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Results/DAG/bayesR2.csv
#' 
#' @section Exports:
#'   None
#' 

```

## Arif2022ecologicalMonographs.R

```
#' @section Imports:
#'   None
#' 
#' @section Exports:
#'   None
#' 

```

