# Deploy this updated app to the existing lccl/GMDP application.
# Configure lccl with rsconnect::setAccountInfo() separately; never store tokens here.
local({
  source_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  command_path <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))
  script_path <- if (!is.null(source_path)) source_path else if (length(command_path)) command_path[[1]] else NULL
  app_dir <- if (is.null(script_path)) getwd() else dirname(dirname(normalizePath(script_path)))
  stopifnot(file.exists(file.path(app_dir, 'app.R')))
  if (dir.exists(file.path(app_dir, '.R-library'))) .libPaths(c(file.path(app_dir, '.R-library'), .libPaths()))
  if (!requireNamespace('rsconnect', quietly = TRUE)) stop('Install rsconnect before deploying.')
  stopifnot(getRversion() >= '4.3', getRversion() < '4.4')
  options(repos = c(
    BioCsoft = 'https://bioconductor.org/packages/3.18/bioc',
    BioCann = 'https://bioconductor.org/packages/3.18/data/annotation',
    BioCexp = 'https://bioconductor.org/packages/3.18/data/experiment',
    CRAN = 'https://cloud.r-project.org'
  ), timeout = 600)
  app_files <- c('app.R', unlist(lapply(c('R', 'UI', 'Server', 'data', 'www'), function(d) {
    file.path(d, list.files(file.path(app_dir, d), recursive = TRUE, all.files = FALSE))
  }), use.names = FALSE))
  app_files <- sort(app_files[!grepl('(^|/)(\\.DS_Store|Thumbs\\.db)$', app_files)])
  saved <- readRDS(file.path(app_dir, 'data/PFpw_spearman.rds'))
  stopifnot(identical(saved$metadata$input_md5, unname(tools::md5sum(file.path(app_dir, 'data/input.RData')))))
  rsconnect::deployApp(
    appDir = app_dir, appFiles = app_files, appMode = 'shiny',
    appId = '5753342', appName = 'GMDP',
    account = 'lccl', server = 'shinyapps.io',
    appTitle = 'GMDP Metabolomics Data Explorer',
    launch.browser = FALSE, forceUpdate = TRUE
  )
})
