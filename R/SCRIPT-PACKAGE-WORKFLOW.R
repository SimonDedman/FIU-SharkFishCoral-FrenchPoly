# In an R project, consisting of multiple .R scripts in a /R subfolder, is there a way to automatically:
# - catalogue what files are used (imported) by each script?
# - catalogue what files are created (exported) by each script?
# - what order scripts the scripts run in, i.e. the output of script 1 is the input of script 2?
# - create a network map of scripts, and input and output files
# - annotate scripts with this information so users know what the scripts are doing
# - modify these updated scripts to be used in a `targets` package workflow?


# FROMHERE ####
# L170 analyze_imports not working for 01_TeleostFunctionalGroupDiets.qmd
#


# Examples
# Imported files for one script
script_path <- here::here("R", "02_Explore_ch4_2023_03.R")

analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/01_TeleostFunctionalGroupDiets.qmd"))
 # Error in endsWith(imported_files, "/x") : non-character object(s)
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/02_Explore_ch4_2023_03.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/site_order_df.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/fixed_bethic_uvc_final_2023_02_26.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/fixed_fish_uvc_final_2023_02_28.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/fish.spp.list.fn.gps.fixed.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/wide.df1.teleosts.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/wide.df1.ch3.60min.2023.01.csv"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/03_UVC_funtional_group_fixit.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/fixed_fish_uvc_final_2023_02_28.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/Trophic_Categorisation_Desbiens.csv"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/05_nat_FPDAG_consistency_check.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data//ReefWideBRUVUVC.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/06_nat_FPDAG_consistency_check_DAG1-tophalf-topdown.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data//ReefWideBRUVUVC.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/07_nat_FPDAG_consistency_check_DAG2-tophalf-bottomup.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data//ReefWideBRUVUVC.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/08_nat_FPDAG_consistency_check_DAG3-bottomhalf-bottomup.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data//ReefWideBRUVUVC.csv"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/Nat_resources/FPDAG_match_table.csv"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/09_BART-DAG.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/ReefWideBRUVUVC-DAGtested.csv"
"\""
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/Arif2022ecologicalMonographs.R"))
# NULL
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/brt_benthos_2023_05.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/island.survey.wide.df2.RData"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/atoll.survey.wide.df2.RData"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/survey.wide.df2.RData"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/brt_prey_teleost_2023_05.R"))
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/island.survey.wide.df2.RData"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/atoll.survey.wide.df2.RData"
"/media/simon/data/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/NFF_data/survey.wide.df2.RData"
analyze_imports(script_path <- ("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/FIU-SharkFishCoral-FrenchPoly/R/SCRIPT-PACKAGE-WORKFLOW.R"))
"# identify source(), read.csv(), readRDS(), load(), and similar functions that load external files." "\""

# imported_files <- analyze_imports(script_path)
# print(paste(script_path, "imports:", paste(imported_files, collapse = ", ")))
analyze_imports(script_path) # "site_order_df.csv" "NFF_data"

# Exported files for one script
exported_files <- analyze_exports(script_path)
cleaned_files <- remove_trailing_parentheses(exported_files)
cleaned_files_starting <- clean_starting_sections(cleaned_files)
final_cleaned_prefixes <- clean_here_prefixes(cleaned_files_starting)
final_cleaned_here <- remove_here_function(final_cleaned_prefixes)
print(final_cleaned_here)

# Imported & exported files & dependencies for all scripts
script_paths <- find_r_scripts() # Or provide a path: find_r_scripts("path/to/scripts")
script_data <- analyze_all_scripts(script_paths)
# analyze_all_scripts(find_r_scripts())
dependency_graph <- build_dependency_graph(script_data)
visualize_graph(dependency_graph)


# 0. Find R scripts ####
library(codetools)
library(here)
library(igraph)
library(stringr)

find_r_scripts <- function(path = NULL) {
  if (is.null(path)) {
    # Find scripts in the current project (RStudio)
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      project_dir <- rstudioapi::getActiveProject()
      if (!is.null(project_dir)) {
        path <- file.path(project_dir, "R")
      } else {
        path <- getwd()
      }
    } else {
      path <- getwd()
    }
  }

  r_scripts <- list.files(path, pattern = "\\.(R|qmd)$", full.names = TRUE, recursive = TRUE)
  return(r_scripts)
}



# 1. Cataloguing Files Used (Imported) by Each Script ####
# analyze_import_calls <- function(script_content, call_pattern, extract_pattern) {
#   imported_files <- c()
#   calls <- grep(call_pattern, script_content, value = TRUE)
#
#   for (call in calls) {
#     if (grepl("here\\(", call)) {
#       here_args <- gsub(".*here\\(([^\\)]+)\\).*", "\\1", call)
#       here_args_list <- trimws(unlist(strsplit(here_args, ",")))
#       here_args_list <- gsub("[\"']", "", here_args_list)
#
#       tryCatch({
#         file_path <- do.call(here::here, as.list(here_args_list))
#         imported_files <- c(imported_files, file_path)
#       }, error = function(e) {
#         imported_files <- c(imported_files, paste("here(", paste(here_args_list, collapse = ","), ")"))
#       })
#     } else {
#       file_path <- gsub(extract_pattern, "\\1", call)
#       imported_files <- c(imported_files, file_path)
#     }
#   }
#   return(imported_files)
# }
#
# analyze_imports <- function(script_path) {
#   script_content <- readLines(script_path)
#
#   imported_files <- c(
#     analyze_import_calls(script_content, "source\\(", ".*source\\(\"([^\"]+)\".*"),
#     analyze_import_calls(script_content, "load\\(", ".*load\\(\"([^\"]+)\".*"),
#     analyze_import_calls(script_content, "(read\\.(csv|rds|table|delim)\\(|readRDS\\(|load\\()", ".*(read\\.(csv|rds|table|delim)\\(|\"|readRDS\\(\"|load\\(\")([^\"]+)\".*")
#   )
#
#   imported_files <- unique(imported_files)
#
#   # Filter out non-character elements before using endsWith
#   imported_files <- imported_files[sapply(imported_files, is.character)]
#
#   if (!is.null(imported_files)) imported_files <- imported_files[!endsWith(imported_files, "/x")] # Remove if ends with "/x"
#   return(unique(imported_files))
# }


analyze_import_calls <- function(script_content, call_pattern, extract_pattern) {
  imported_files <- c()
  calls <- grep(call_pattern, script_content, value = TRUE)

  for (call in calls) {
    if (grepl("here\\(", call)) {
      here_args <- gsub(".*here\\(([^\\)]+)\\).*", "\\1", call)
      here_args_list <- trimws(unlist(strsplit(here_args, ",")))
      here_args_list <- gsub("[\"']", "", here_args_list)

      tryCatch({
        file_path <- do.call(here::here, as.list(here_args_list))
        imported_files <- c(imported_files, file_path)
      }, error = function(e) {
        imported_files <- c(imported_files, paste("here(", paste(here_args_list, collapse = ","), ")"))
      })
    } else {
      file_path <- gsub(extract_pattern, "\\1", call)
      imported_files <- c(imported_files, file_path)
    }
  }
  return(imported_files)
}

analyze_imports <- function(script_path) {
  script_content <- readLines(script_path, warn = FALSE)

  if (str_detect(script_path, "\\.qmd$")) {
    # Extract code chunks from .qmd files
    code_chunks <- str_extract_all(paste(script_content, collapse = "\n"), "```\\{r.*?\\}\\n(.*?)\\n```", simplify = TRUE)
    script_content <- unlist(lapply(code_chunks, function(chunk) {
      gsub("```\\{r.*?\\}\\n|\\n```", "", chunk)
    }))
    script_content <- script_content[script_content != ""]
  }

  imported_files <- c(
    analyze_import_calls(script_content, "source\\(", ".*source\\(\"([^\"]+)\".*"),
    analyze_import_calls(script_content, "load\\(", ".*load\\(\"([^\"]+)\".*"),
    analyze_import_calls(script_content, "(read\\.(csv|rds|table|delim)\\(|readRDS\\(|load\\()", ".*(read\\.(csv|rds|table|delim)\\(|\"|readRDS\\(\"|load\\(\")([^\"]+)\".*")
  )

  imported_files <- unique(imported_files)

  # Filter out non-character elements before using endsWith
  imported_files <- imported_files[sapply(imported_files, is.character)]

  imported_files <- imported_files[!endsWith(imported_files, "/x")] # Remove if ends with "/x"
  return(unique(imported_files))
}








# 2. Cataloguing Files Created (Exported) by Each Script ####
# Static Code Analysis: Look for functions like write.csv(), writeRDS(), saveRDS(), save(), etc.
analyze_export_calls <- function(script_content, call_pattern) {
  exported_files <- c()
  calls <- grep(call_pattern, script_content, value = TRUE, ignore.case = TRUE)

  for (call in calls) {
    exported_files <- c(exported_files, call)
  }
  return(unique(exported_files))
}

analyze_exports <- function(script_path) {
  script_content <- readLines(script_path) # Corrected line

  exported_files <- c(
    analyze_export_calls(script_content, "write\\.csv\\("),
    analyze_export_calls(script_content, "saveRDS\\("),
    analyze_export_calls(script_content, "save\\(")
  )
  return(unique(exported_files))
}

remove_trailing_parentheses <- function(exported_files) {
  cleaned_files <- gsub("\\)\\)$", "", exported_files)
  return(cleaned_files)
}

clean_starting_sections <- function(cleaned_files) {
  cleaned_starts <- sapply(cleaned_files, function(file_string) {
    # Handle both file= and filename=
    file_string <- gsub("^(write\\.(csv|rds|table|delim)\\(|saveRDS\\(|save\\(|ggsave\\().*(file|filename)\\s*=\\s*", "", file_string, ignore.case = TRUE)
    return(file_string)
  })

  final_cleaned_files <- sapply(cleaned_starts, function(file_string) {
    if (grepl("here\\(", file_string)) {
      tryCatch({
        file_path <- eval(parse(text = file_string))
        return(file_path)
      }, error = function(e) {
        # If evaluation fails, extract and reconstruct the file path
        here_args <- gsub(".*here\\(([^\\)]+)\\).*", "\\1", file_string)
        here_args_list <- trimws(unlist(strsplit(here_args, ",")))
        here_args_list <- gsub('["\']', "", here_args_list)
        file_path <- paste(here_args_list, collapse = "/")

        # Handle paste0() and Sys.Date()
        if (grepl("paste0\\(", file_string) || grepl("Sys.Date\\(", file_string)) {
          # Capture everything after the closing parenthesis of the here() call.
          remaining_part <- regmatches(file_string, regexpr("\\).*$", file_string))
          if (length(remaining_part) > 0) {
            # Remove the first ) from the remaining part
            remaining_part = substring(remaining_part, 2)
            file_path <- paste0(file_path, "/", remaining_part)
          }
        }

        return(file_path)
      })
    } else {
      return(file_string)
    }
  })

  return(unique(final_cleaned_files))
}

clean_here_prefixes <- function(cleaned_files) {
  cleaned_prefixes <- gsub("^here/", "", cleaned_files)
  return(cleaned_prefixes)
}

remove_here_function <- function(cleaned_files){
  cleaned_here <- gsub("here\\(", "", cleaned_files)
  return(cleaned_here)
}




# 3. Analyse all scripts imports and exports ####

analyze_all_scripts <- function(script_paths) {
  script_data <- list()
  for (script_path in script_paths) {
    script_name <- basename(script_path)
    imports <- analyze_imports(script_path)
    exports <- remove_here_function(clean_here_prefixes(clean_starting_sections(remove_trailing_parentheses(analyze_exports(script_path)))))
    script_data[[script_name]] <- list(imports = imports, exports = exports)
  }
  return(script_data)
}




# 4. Determining Script Execution Order ####
# Dependency Analysis: Combine the import/export information to build a dependency graph.
# If script B imports a file exported by script A, then script A must run before script B.
# Use the R packages igraph to help with the generation of the dependency graph.

# After analyzing all scripts, create a data frame of dependencies
dependencies <- data.frame(
  from_script = c("R/script1.R", "R/script2.R"),
  to_script = c("R/script2.R", "R/script3.R")
)

# Use igraph to create a dependency graph
library(igraph)
graph <- graph_from_data_frame(dependencies)
execution_order <- names(topo_sort(graph))
print(execution_order)



# 4. Dependency graph ####
build_dependency_graph <- function(script_data) {
  edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

  for (script_name in names(script_data)) {
    exports <- script_data[[script_name]]$exports
    for (other_script_name in names(script_data)) {
      if (script_name != other_script_name) {
        imports <- script_data[[other_script_name]]$imports
        if (any(exports %in% imports)) {
          edges <- rbind(edges, data.frame(from = script_name, to = other_script_name, stringsAsFactors = FALSE))
        }
      }
    }
  }

  if (nrow(edges) > 0) {
    graph <- igraph::graph_from_data_frame(edges, directed = TRUE)
    return(graph)
  } else {
    return(NULL)
  }
}

visualize_graph <- function(graph) {
  if (!is.null(graph)) {
    igraph::plot.igraph(graph, vertex.label = igraph::V(graph)$name, vertex.color = "lightblue", edge.arrow.size = 0.5)
  } else {
    print("No dependencies found.")
  }
}






















# 4. Creating a Network Map ####
# igraph and DiagrammeR: Use igraph to create a graph object representing the relationships between scripts and files.
# Use DiagrammeR to visualize the graph.

library(igraph)
library(DiagrammeR)

# Create a graph object with nodes representing scripts and files, and edges representing dependencies
# ... (create the graph object) ...

# Visualize the graph
plot(graph) # basic plot
DiagrammeR::grViz("digraph { ... }") # more customizable




# 5. Annotating Scripts ####
# Roxygen2 Comments: Use Roxygen2-style comments to document the inputs and outputs of each script.
# You can automate the insertion of these comments after analyzing the scripts.

#' @title My Script
#' @description This script does some data processing.
#' @imports data/input.csv, R/helper_functions.R
#' @exports data/output.rds
#' @details This script reads input data, performs calculations, and saves the results.




# 6. Modifying for targets Workflow ####
# targets Package: targets is designed for reproducible workflows.
# Replace your script execution with targets targets.
# Use the import/export information to define the dependencies between targets.
# targets automatically manages the execution order based on these dependencies.

library(targets)

list(
  tar_target(input_data, read.csv("data/input.csv")),
  tar_target(processed_data, process_data(input_data)),
  tar_target(output_data, saveRDS(processed_data, "data/output.rds"))
)

# Key Considerations:
# Dynamic Dependencies: Static analysis might not catch dynamic dependencies (e.g., file paths generated at runtime).
# Error Handling: Add robust error handling to your analysis scripts.
# Regular Expressions: Fine-tune your regular expressions to accurately capture file paths and function calls.
# Maintainability: Design your analysis scripts to be easily maintainable as your project evolves.
# File Path Management: Use here::here() to manage file paths relative to your project root.
# Testing: Test your analysis scripts thoroughly to ensure they correctly identify dependencies.
#
# By combining static code analysis, dependency analysis, and visualization tools, you can effectively manage and document your R project's workflow. targets will help you to create a reproducible data pipeline.
