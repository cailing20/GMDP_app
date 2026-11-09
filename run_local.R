# Open GMDP.Rproj in RStudio, then source this file (or run Rscript run_local.R).
# Optional terminal setting: GMDP_PORT=3838 Rscript run_local.R
local({
  source_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  command_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  script_path <- if (!is.null(source_path)) source_path else if (length(command_path)) command_path[[1]] else NULL
  app_dir <- if (is.null(script_path)) getwd() else dirname(normalizePath(script_path))
  if (!file.exists(file.path(app_dir, "app.R"))) stop("Cannot find app.R. Open GMDP.Rproj first.")
  library_dir <- file.path(app_dir, ".R-library")
  if (dir.exists(library_dir)) .libPaths(c(library_dir, .libPaths()))
  cache_dir <- file.path(app_dir, ".cache", "sass")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  options(sass.cache = cache_dir)
  if (!requireNamespace("shiny", quietly = TRUE)) stop("Run source(\"scripts/setup_local.R\") first.")
  requested_port <- trimws(Sys.getenv("GMDP_PORT", ""))
  explicit_port <- nzchar(requested_port)
  if (explicit_port && !grepl("^[0-9]+$", requested_port)) {
    stop("GMDP_PORT must be a whole number from 1 to 65535.", call. = FALSE)
  }
  port <- if (explicit_port) suppressWarnings(as.integer(requested_port)) else 3838L
  if (is.na(port) || port < 1L || port > 65535L) {
    stop("GMDP_PORT must be a whole number from 1 to 65535.", call. = FALSE)
  }
  # Probe quietly before loading the app. Keep explicit port requests strict.
  port <- tryCatch(
    httpuv::randomPort(min = port, max = port, host = "127.0.0.1"),
    httpuv_unavailable_port = function(e) {
      if (explicit_port) {
        stop(paste0("Port ", port, " is already in use. Open http://127.0.0.1:", port,
                    " or run Sys.unsetenv('GMDP_PORT') to select a free port automatically."),
             call. = FALSE)
      }
      free_port <- httpuv::randomPort(min = 3839L, max = 49151L, host = "127.0.0.1")
      message("Port 3838 is already in use; starting this app at http://127.0.0.1:", free_port)
      free_port
    }
  )
  shiny::runApp(appDir = app_dir, host = "127.0.0.1", port = port, launch.browser = interactive())
})
