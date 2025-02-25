# In an R project, consisting of multiple .R scripts in a /R subfolder, is there a way to automatically:
# - catalogue what files are used (imported) by each script?
# - catalogue what files are created (exported) by each script?
# - what order scripts the scripts run in, i.e. the output of script 1 is the input of script 2?
# - create a network map of scripts, and input and output files
# - annotate scripts with this information so users know what the scripts are doing
# - modify these updated scripts to be used in a `targets` package workflow?




# 1. Cataloguing Files Used (Imported) by Each Script ####

## Static Code Analysis ####
# You can parse the R scripts using tools like codetools or regular expressions to
# identify source(), read.csv(), readRDS(), load(), and similar functions that load external files.
# For package dependencies, use renv::dependencies() or pak::pkg_deps().

library(codetools)
analyze_imports <- function(script_path) {
  script_content <- readLines(script_path)

  # Find source() calls
  source_files <- grep("source\\(\"", script_content, value = TRUE)
  source_files <- gsub(".*source\\(\"([^\"]+)\".*", "\\1", source_files)

  # Find read.csv(), readRDS(), load(), etc.
  read_files <- grep("read\\.(csv|rds|table|delim)\\(", script_content, value = TRUE, ignore.case = TRUE)
  read_files_rds <- grep("readRDS\\(", script_content, value = TRUE, ignore.case = TRUE)
  load_files <- grep("load\\(", script_content, value = TRUE, ignore.case = TRUE)

  read_files <- c(read_files, read_files_rds, load_files)
  read_files <- gsub(".*\\(\"([^\"]+)\".*", "\\1", read_files)

  imported_files <- c(source_files, read_files)

  return(unique(imported_files))
}

# Example usage
script_path <- "R/my_script.R"
imported_files <- analyze_imports(script_path)
print(paste(script_path, "imports:", paste(imported_files, collapse = ", ")))




# 2. Cataloguing Files Created (Exported) by Each Script ####
# Static Code Analysis: Look for functions like write.csv(), writeRDS(), saveRDS(), save(), etc.

analyze_exports <- function(script_path) {
  script_content <- readLines(script_path)

  write_files <- grep("write\\.(csv|rds|table|delim)\\(", script_content, value = TRUE, ignore.case = TRUE)
  write_files_rds <- grep("saveRDS\\(", script_content, value = TRUE, ignore.case = TRUE)
  save_files <- grep("save\\(", script_content, value = TRUE, ignore.case = TRUE)

  write_files <- c(write_files, write_files_rds, save_files)
  write_files <- gsub(".*\\(\"([^\"]+)\".*", "\\1", write_files)

  exported_files <- unique(write_files)
  return(exported_files)
}

# Example usage
script_path <- "R/my_script.R"
exported_files <- analyze_exports(script_path)
print(paste(script_path, "exports:", paste(exported_files, collapse = ", ")))




# 3. Determining Script Execution Order ####
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
