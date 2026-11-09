# Saved Spearman values and separate overview-table / scatter Submit lifecycles.
script_path <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE)[1])
root <- dirname(dirname(normalizePath(script_path))); setwd(root)
dir.create(file.path(root, '.cache/sass'), recursive = TRUE, showWarnings = FALSE)
options(sass.cache = file.path(root, '.cache/sass'))
suppressPackageStartupMessages(source('app.R', local = TRUE))
stopifnot(identical(names(PFpw.spearman), names(PFpw)), nrow(PFpw.spearman) == 89700L,
          identical(PFpw.spearman[, 1:3], PFpw[, 1:3]))
saved <- readRDS('data/PFpw_spearman.rds')
stopifnot(saved$metadata$shared_subjects == 42L, saved$metadata$rounding_digits == 2L)
# Check every saved coefficient against an independent stats::cor matrix.
for (tissue in c('plasma', 'fibroblast')) for (scope in c('all', 'shared')) {
  dat <- run.data[[tissue]]$mData[, joint.cor$metabolite, drop = FALSE]
  if (scope == 'shared') dat <- dat[joint.subject[[if (tissue == 'plasma') 'p.ind' else 'f.ind']], , drop = FALSE]
  dat[!is.finite(dat)] <- NA_real_
  expected <- suppressWarnings(stats::cor(dat, use = 'pairwise.complete.obs', method = 'spearman'))
  n <- crossprod(is.finite(dat)); expected[n < 3L] <- NA_real_
  indices <- cbind(match(as.character(PFpw$Metabolite.1), rownames(expected)), match(as.character(PFpw$Metabolite.2), colnames(expected)))
  stopifnot(isTRUE(all.equal(PFpw.spearman[[paste(scope, tissue, 'r', sep = '.')]], round(expected[indices], 2))))
}
cat('PASS: all 358800 saved coefficients match independent Spearman matrices.\n')
elapsed <- system.time(invisible(load_pairwise_spearman()))[['elapsed']]
cat('Saved file load including input fingerprint verification:', elapsed, 'seconds; file size:', file.size('data/PFpw_spearman.rds'), 'bytes.\n')
shiny::testServer(function(input, output, session) {
  source('Server/PFcomparisonCustomCor_Server.R', local = TRUE)
}, {
  session$setInputs(`PFcompareCustomCor-method`='pearson', `PFcompareCustomCor-hideDup`=TRUE,
                   `PFcompareCustomCor-Mx`='4-acetylbutyrate', `PFcompareCustomCor-My`='Hydroxyisocaproic acid',
                   `PFcompareCustomCor-OnlySharedSubj`='no', `PFcompareCustomCor-Centroid`='no')
  stopifnot(is.null(custom.submitted()), nrow(PFcor.df()$x$data) == 44850L)
  session$setInputs(`PFcompareCustomCor-method`='spearman')
  stopifnot(is.null(custom.submitted()), PFcompareCustomCor.tableRequest()$method == 'spearman',
            grepl('(Spearman)', PFcor.df()$x$container, fixed = TRUE))
  session$setInputs(`PFcompareCustomCor-hideDup`=FALSE)
  stopifnot(nrow(PFcor.df()$x$data) == 89700L, is.null(custom.submitted()))
  session$setInputs(`PFcompareCustomCor-Submit`=1)
  spearman <- custom.submitted()
  stopifnot(grepl('Spearman rho', spearman$plots[[1]][[1]]$labels$title, fixed = TRUE))
  session$setInputs(`PFcompareCustomCor-method`='pearson', `PFcompareCustomCor-hideDup`=TRUE)
  stopifnot(identical(custom.submitted(), spearman), nrow(PFcor.df()$x$data) == 44850L,
            grepl('(Pearson)', PFcor.df()$x$container, fixed = TRUE))
  session$setInputs(`PFcompareCustomCor-My`='L-Leucine')
  stopifnot(identical(custom.submitted(), spearman))
  session$setInputs(`PFcompareCustomCor-Submit`=2)
  stopifnot(grepl('Pearson r', custom.submitted()$plots[[1]][[1]]$labels$title, fixed = TRUE),
            custom.submitted()$plots[[1]][[1]]$labels$y == 'L-Leucine')
})
cat('PASS: table method/duplicate switches apply immediately; scatter plots change only on Submit.\n')
