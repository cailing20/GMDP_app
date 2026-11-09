# Rebuild only when the bundled input changes. Does not modify input.RData.
script_path <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE)[1])
root <- dirname(dirname(normalizePath(script_path)))
setwd(root)
if (dir.exists('.R-library')) .libPaths(c(normalizePath('.R-library'), .libPaths()))
source('R/correlation_helpers.R')
source('R/pairwise_cache.R')
data <- new.env()
load('data/input.RData', envir = data)
elapsed <- system.time(table <- build_pairwise_spearman(data$PFpw, data$run.data, data$joint.cor, data$joint.subject))
saved <- list(
  metadata = list(schema_version = 1L, method = 'spearman',
                  input_md5 = unname(tools::md5sum('data/input.RData')),
                  metabolites = data$joint.cor$metabolite,
                  observations = lapply(data$run.data, function(d) nrow(d$mData)),
                  shared_subjects = nrow(data$joint.subject),
                  rounding_digits = 2L, created_utc = format(Sys.time(), tz = 'UTC', usetz = TRUE)),
  table = table)
saveRDS(saved, 'data/PFpw_spearman.rds', compress = 'xz')
cat('Saved', nrow(table), 'ordered pairs to data/PFpw_spearman.rds in', elapsed[['elapsed']], 'seconds.\n')
