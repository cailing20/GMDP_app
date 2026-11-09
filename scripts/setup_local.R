# Install missing packages into this app's library without changing global libraries.
# Run once from RStudio: source("scripts/setup_local.R")
# Or from a terminal: Rscript scripts/setup_local.R
local({
  source_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  command_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  script_path <- if (!is.null(source_path)) source_path else if (length(command_path)) command_path[[1]] else NULL
  app_dir <- if (is.null(script_path)) getwd() else dirname(dirname(normalizePath(script_path)))
  if (!file.exists(file.path(app_dir, "app.R"))) stop("Cannot find app.R. Run this script from the app project.")
  library_dir <- file.path(app_dir, ".R-library")
  dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(library_dir, .libPaths()))
  options(timeout = max(600, getOption("timeout")))

  cran_packages <- c(
    "shiny", "data.table", "shinyjs", "stringr", "shinycssloaders", "plotly",
    "dplyr", "ggplot2", "tidyr", "purrr", "readr", "tibble", "DT", "reshape2", "nlme", "Hmisc",
    "RColorBrewer", "patchwork", "ggpubr", "ggrepel", "bslib", "htmltools",
    "shinyWidgets", "openxlsx", "ape"
  )
  bioc_packages <- c("pathview", "ComplexHeatmap", "InteractiveComplexHeatmap")
  all_packages <- c(cran_packages, bioc_packages)
  missing <- function(packages) packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  cran_repo <- c(CRAN = "https://cloud.r-project.org")
  # R 4.3 is paired with Bioconductor 3.18. Never upgrade the existing installation.
  r_series <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
  if (r_series != "4.3") {
    stop("This local setup is configured for R 4.3 / Bioconductor 3.18. Use R 4.3, or deliberately update the Bioconductor repositories below for your R release.")
  }
  bioc_repos <- c(
    BioCsoft = "https://bioconductor.org/packages/3.18/bioc",
    BioCann = "https://bioconductor.org/packages/3.18/data/annotation",
    BioCexp = "https://bioconductor.org/packages/3.18/data/experiment",
    cran_repo
  )
  package_type <- if (identical(Sys.info()[["sysname"]], "Darwin")) "binary" else "source"
  install_missing <- function(packages, repos) {
    needed <- missing(packages)
    if (length(needed)) {
      message("Installing into ", library_dir, ": ", paste(needed, collapse = ", "))
      install.packages(needed, lib = library_dir, repos = repos, type = package_type,
                       dependencies = c("Depends", "Imports", "LinkingTo"))
    }
  }
  install_missing(cran_packages, cran_repo)
  install_missing(bioc_packages, bioc_repos)
  still_missing <- missing(all_packages)
  if (length(still_missing)) stop("Packages still unavailable: ", paste(still_missing, collapse = ", "))
  versions <- data.frame(
    package = all_packages,
    version = vapply(all_packages, function(p) as.character(packageVersion(p)), character(1)),
    library = vapply(all_packages, function(p) dirname(find.package(p)), character(1))
  )
  write.csv(versions, file.path(library_dir, "app-package-versions.csv"), row.names = FALSE)
  message("Local setup complete. Run source(\"run_local.R\") to start the app.")
})
