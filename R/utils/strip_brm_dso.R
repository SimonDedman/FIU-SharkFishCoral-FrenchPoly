# Strip embedded Stan compiled binaries from brms model objects.
#
# On Linux, brms::brm() saves the compiled C++ Stan executable inside each
# fitted model under @stanmodel@dso, adding ~15-20 MB per model on saveRDS().
# This is unnecessary for archival sharing: stripped fits still support
# summary(), plot(), posterior_predict(), loo(), etc. Only update() and
# re-sampling require the binary, and those will trigger a one-time
# Stan recompile on first call.
#
# Reduces models_list_*_spline.Rds from ~200 MB to ~5-10 MB per file,
# matching the Windows-saved size. Originals are copied to
# Results/DAG/oldversions/pre_strip/ before modification.

library(brms)
library(here)

dag_dir <- here("Results", "DAG")
backup_dir <- here("Results", "DAG", "oldversions", "pre_strip")
dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)

strip_dso <- function(brm_fit) {
  if (inherits(brm_fit, "brmsfit") &&
      !is.null(brm_fit$fit) &&
      isS4(brm_fit$fit)) {
    brm_fit$fit@stanmodel@dso <- new("cxxdso")
  }
  brm_fit
}

target_files <- list.files(
  dag_dir,
  pattern = "^models_list_.*_spline\\.Rds$",
  full.names = TRUE
)

message("Found ", length(target_files), " model-list files to strip.")

for (f in target_files) {
  base <- basename(f)
  backup_path <- file.path(backup_dir, base)

  size_before <- file.size(f) / 1e6
  message(sprintf("\n%s: %.1f MB", base, size_before))

  if (!file.exists(backup_path)) {
    message("  backing up to oldversions/pre_strip/ ...")
    file.copy(f, backup_path, overwrite = FALSE)
  } else {
    message("  backup already present, skipping copy")
  }

  message("  loading ...")
  ml <- readRDS(f)

  message(sprintf("  list length: %d, stripping dso slots ...", length(ml)))
  ml <- lapply(ml, strip_dso)

  message("  saving stripped version ...")
  saveRDS(ml, f)

  size_after <- file.size(f) / 1e6
  message(sprintf("  done: %.1f MB -> %.1f MB (%.0f%% reduction)",
                  size_before, size_after,
                  100 * (1 - size_after / size_before)))
}

message("\nAll files stripped.")
