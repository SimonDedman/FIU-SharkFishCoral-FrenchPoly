# dag_directions.R
# Single source for the top-down (TD) and bottom-up (BU) DAGs.
#
# The TD DAG is read from R/dags/. BU is DERIVED from it by reversing only the
# faunal/floral "overlay" edges listed in R/dags/overlay_edges_TD-direction.csv
# (the thick dual-arrow lines in Fig. S4). Every other edge (environment,
# geomorphology, human, within-benthos, COTS) keeps its TD direction in both.
# Editing the TD file therefore carries through to BU automatically.
#
# Provenance (2026-09-24):
#   TD  = dagitty 7VHv3H73 (2025-09-11) + invertivores -> crown_of_thorns
#         (R2,16; crown_of_thorns latent).
#   overlay = the 20 edges dagitty GBrpiZXW (BU, 2025-03) reversed relative to
#         7bBT4Rqj, minus 3 transient-pelagic-shark edges (node dropped in
#         7VHv3H73) and minus 4 that Simon confirmed are NOT tested as
#         bidirectional: other_algae -> hard_coral, invertivores -> other_algae,
#         crown_of_thorns -> hard_coral, invertivores -> invert. 13 edges.

dag_dir <- function() here::here("R", "dags")

dag_td <- function(file = "FP_DAG_TopDown_2026-09-24_from7VHv3H73_plusInvertivoresCOTS.dag") {
  dagitty::dagitty(paste(readLines(file.path(dag_dir(), file)), collapse = "\n"))
}

overlay_edges <- function() {
  utils::read.csv(file.path(dag_dir(), "overlay_edges_TD-direction.csv"))
}

# Returns the BU dagitty object: TD with the overlay edges reversed.
dag_bu <- function(td = dag_td(), overlay = overlay_edges()) {
  e <- dagitty::edges(td)
  stopifnot(all(e$e == "->"))
  key <- paste(e$v, e$w)
  hit <- key %in% paste(overlay$from, overlay$to)
  missing <- setdiff(paste(overlay$from, overlay$to), key)
  if (length(missing)) stop("Overlay edges not in TD DAG: ", paste(missing, collapse = "; "))
  e[hit, c("v", "w")] <- e[hit, c("w", "v")]
  # rebuild the DAG text keeping node attributes (latent/exposure/outcome, pos)
  txt <- as.character(td)
  node_lines <- grep("->", strsplit(txt, "\n")[[1]], value = TRUE, invert = TRUE)
  node_lines <- node_lines[!grepl("^\\s*(dag \\{|\\})\\s*$", node_lines)]
  q <- function(x) ifelse(grepl("\\s", x), paste0('"', x, '"'), x)
  edge_lines <- paste(q(e$v), "->", q(e$w))
  bu <- dagitty::dagitty(paste(c("dag {", node_lines, edge_lines, "}"), collapse = "\n"))
  if (!dagitty::isAcyclic(bu)) stop("Derived BU graph is not acyclic")
  bu
}
