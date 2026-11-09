# Run from the app: Rscript tests/test_correlations.R
# Or from its parent workspace: Rscript <app-folder>/tests/test_correlations.R
# Uses the bundled app data and ordinary assertions; no test framework required.
script_arg <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))
source_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
script_path <- if (!is.null(source_path)) source_path else if (length(script_arg)) script_arg[[1]] else NULL
root <- if (!is.null(script_path)) dirname(dirname(normalizePath(script_path))) else getwd()
if (!file.exists(file.path(root, 'data/input.RData'))) stop('Cannot locate app data from this test script.')
if (dir.exists(file.path(root, '.R-library'))) .libPaths(c(file.path(root, '.R-library'), .libPaths()))
packages <- c('shiny','data.table','ggplot2','plotly','RColorBrewer','DT','htmltools','shinyjs','patchwork','reshape2','ape','ComplexHeatmap','InteractiveComplexHeatmap')
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop('Missing packages: ', paste(missing, collapse = ', '), '. Run scripts/setup_local.R first.')
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
assert_close <- function(actual, expected, tolerance = 1e-12) {
  stopifnot(isTRUE(all.equal(unname(actual), unname(expected), tolerance = tolerance)))
}
source(file.path(root, 'R/correlation_helpers.R'))
load(file.path(root, 'data/input.RData'))
source(file.path(root, 'R/pairwise_cache.R'))
PFpw.spearman <- load_pairwise_spearman(file.path(root, 'data/input.RData'), file.path(root, 'data/PFpw_spearman.rds'))
set.seed(10)
x <- cbind(a = rnorm(30), b = sample(1:5,30,TRUE), c = rep(1,30), d = rnorm(30))
x[c(1,3,6),1] <- NA;x[c(2,5),2] <- NA;x[10,4] <- Inf
for(method in c('pearson','spearman')) {
 m <- correlation_matrix_stats(x,method)
 for(i in 1:4) for(j in 1:4) {
   s <- correlation_summary(x[,i],x[,j],method)
   keep <- is.finite(x[,i]) & is.finite(x[,j])
   if (sum(keep) >= 3L && length(unique(x[keep,i])) > 1L && length(unique(x[keep,j])) > 1L) {
     independent <- if (method == 'spearman') cor.test(x[keep,i], x[keep,j], method = method, exact = FALSE)
                    else cor.test(x[keep,i], x[keep,j], method = method)
     assert_close(s$estimate, independent$estimate)
     assert_close(s$p.value, independent$p.value)
     stopifnot(s$n == sum(keep))
   } else stopifnot(is.na(s$estimate), is.na(s$p.value))
   stopifnot(isTRUE(all.equal(m$r[i,j],s$estimate,tolerance=1e-12)))
   if(i!=j) stopifnot(isTRUE(all.equal(m$P[i,j],s$p.value,tolerance=1e-12)))
 }
}
stopifnot(is.na(correlation_summary(c(1,NA,Inf),c(2,3,4))$estimate))
shiny::testServer(function(input,output,session) {
 source(file.path(root,'Server/PFcomparisonConcordance_Server.R'),local=TRUE)
 source(file.path(root,'Server/PFcomparisonCustomCor_Server.R'),local=TRUE)
 source(file.path(root,'Server/PFcomparisonPathwayCor_Server.R'),local=TRUE)
 source(file.path(root,'Server/PFcomparisonReactionCor_Server.R'),local=TRUE)
}, {
 session$setInputs(`PFcomparisonConcordance-method`='pearson',`PFcomparisonConcordance-stat`='untransformed data',`PFcomparisonConcordance-metabolite`='Glutaric acid',`PFcomparisonConcordance-sample`=joint.subject$Subject[1],`PFcompareCustomCor-method`='pearson',`PFcompareCustomCor-hideDup`=TRUE,`PFcompareCustomCor-Mx`='Ornithine',`PFcompareCustomCor-My`='Citrulline',`PFcompareCustomCor-OnlySharedSubj`='yes',`PFcompareCustomCor-Centroid`='yes',`PFcomparePathwayCor-method`='pearson',`PFcomparePathwayCor-pathway`='Arginine biosynthesis',`PFcomparePathwayCor-samples`='Shared subjects',`PFcomparePathwayCor-show`='all',`PFcompareReactionCor-method`='pearson',`PFcompareReactionCor-pathwayCpd`='pathway',`PFcompareReactionCor-pathway`='Arginine biosynthesis',`PFcompareReactionCor-enzymeGene`='enzyme',`PFcompareReactionCor-reaction`='R00669',`PFcompareReactionCor-samples`='Shared subjects')
 stopifnot(identical(PFcomparisonConcordance.df()$metabolite$r,joint.stat$metabolite$m$r))
 stopifnot(nrow(PFcor.df()$x$data)==44850)
 stopifnot(length(custom.data.plot()[[1]])==2, length(plot.reaction())==1)
 stopifnot(inherits(pathway.corrplot(),'ggplot'))
 session$setInputs(`PFcomparisonConcordance-method`='spearman',`PFcompareCustomCor-method`='spearman',`PFcomparePathwayCor-method`='spearman',`PFcompareReactionCor-method`='spearman')
 # The custom overview table updates immediately; other tables and plots wait.
 stopifnot(identical(PFcomparisonConcordance.df()$metabolite$r,joint.stat$metabolite$m$r),
           PFcompareCustomCor.tableRequest()$method == 'spearman',
           is.null(custom.submitted()), is.null(pathway.submitted()), is.null(reaction.submitted()),
           is.null(PFcomparisonConcordance.metaboliteResult()), is.null(PFcomparisonConcordance.sampleResult()))
 session$setInputs(`PFcomparisonConcordance-statSubmit`=1, `PFcompareCustomCor-Submit`=1,
                   `PFcomparePathwayCor-submit`=1, `PFcompareReactionCor-submit`=1)
 stopifnot(is.null(PFcomparisonConcordance.metaboliteResult()), is.null(PFcomparisonConcordance.sampleResult()))
 session$setInputs(`PFcomparisonConcordance-metaboliteSubmit`=1, `PFcomparisonConcordance-sampleSubmit`=1)
 s <- correlation_summary(run.data$plasma$mData[joint.subject$p.ind,'Glutaric acid'],run.data$fibroblast$mData[joint.subject$f.ind,'Glutaric acid'],'spearman')
 stopifnot(PFcomparisonConcordance.df()$metabolite$rho[1]==round(s$estimate,2))
 stopifnot(!'method' %in% names(PFcomparisonConcordance.df()$metabolite),
           !'method' %in% names(PFcomparisonConcordance.df()$sample),
           grepl('(Spearman)', output$`PFcomparisonConcordance-metabolitesTitle`, fixed=TRUE),
           grepl('(Spearman)', PFcor.df()$x$container, fixed=TRUE))
 metabolite_plot <- PFcomparisonConcordance.metaboliteResult()
 subject_plot <- PFcomparisonConcordance.sampleResult()
 stopifnot(!grepl('\n', metabolite_plot$x$layout$title$text),
           grepl('Glutaric acid, Spearman rho', metabolite_plot$x$layout$title$text, fixed=TRUE))
 submitted_custom <- custom.submitted()
 submitted_pathway <- pathway.submitted()
 submitted_reaction <- reaction.submitted()
 # Pending method, data and subject edits leave every submitted result unchanged.
 pending_metabolite <- joint.cor$metabolite[2]
 session$setInputs(`PFcomparisonConcordance-method`='pearson',`PFcomparisonConcordance-stat`='robust z-score',
                   `PFcomparisonConcordance-metabolite`=pending_metabolite,`PFcomparisonConcordance-sample`=joint.subject$Subject[2],
                   `PFcompareCustomCor-method`='pearson',`PFcomparePathwayCor-method`='pearson',`PFcompareReactionCor-method`='pearson')
 stopifnot(identical(custom.submitted(),submitted_custom), identical(pathway.submitted(),submitted_pathway),
           identical(reaction.submitted(),submitted_reaction), identical(PFcomparisonConcordance.metaboliteResult(),metabolite_plot),
           identical(PFcomparisonConcordance.sampleResult(),subject_plot), PFcomparisonConcordance.tableRequest()$method=='spearman',
           PFcomparisonConcordance.tableRequest()$stat=='untransformed data', PFcompareCustomCor.tableRequest()$method=='pearson')
 # Submitting just one scatter plot must leave the other scatter and tables alone.
 session$setInputs(`PFcomparisonConcordance-metaboliteSubmit`=2)
 stopifnot(grepl(paste0(pending_metabolite,', Pearson r'),PFcomparisonConcordance.metaboliteResult()$x$layout$title$text,fixed=TRUE),
           identical(PFcomparisonConcordance.sampleResult(),subject_plot), PFcomparisonConcordance.tableRequest()$method=='spearman')
 session$setInputs(`PFcomparisonConcordance-method`='spearman',`PFcompareCustomCor-method`='spearman',
                   `PFcomparePathwayCor-method`='spearman',`PFcompareReactionCor-method`='spearman')
 pw <- PFcompareCustomCor.spearman()
 a <- as.character(pw$Metabolite.1[1]);b <- as.character(pw$Metabolite.2[1])
 stopifnot(pw$all.plasma.r[1]==round(cor(run.data$plasma$mData[,a],run.data$plasma$mData[,b],method='spearman',use='pairwise.complete.obs'),2))
 stopifnot(grepl('Spearman',pathway.corrplot()$labels$fill))
 stopifnot(all(grepl('Spearman',custom.data.plot()[[1]][[1]]$data$stat)),
           !grepl('\n',custom.data.plot()[[1]][[1]]$labels$title))
 stopifnot(all(grepl('Spearman',plot.reaction()[[1]]$data$r)))
 session$setInputs(`PFcompareReactionCor-samples`='All subjects',`PFcompareCustomCor-OnlySharedSubj`='no')
 stopifnot(length(custom.data.plot()[[2]])==2, length(plot.reaction())==1)
 session$setInputs(`PFcomparisonConcordance-stat`='robust z-score',`PFcomparePathwayCor-show`='statistically significant')
 stopifnot(inherits(pathway.corrplot(),'ggplot'),nrow(PFcomparisonConcordance.df()$sample)==42)
})
cat('PASS: helpers and four plasma/fibroblast correlation pages.\n')

# Neighbor correlations must use the requested orientation and replicate filter.
original_sample_names <- lapply(run.data, function(d) names(d$sample.info))
shiny::testServer(function(input, output, session) {
  source(file.path(root, 'Server/Neighbor_Server.R'), local = TRUE)
}, {
  session$setInputs(`neighbor-matrix` = 'fibroblast', `neighbor-dimension` = 'sample',
                    `neighbor-centroid` = 'yes', `neighbor-dx` = c('diagnosed','unknown','unaffected'),
                    `neighbor-distance` = 'correlation', `neighbor-method` = 'pearson')
  for (tissue in c('fibroblast', 'plasma')) {
    for (centroid in c('yes', 'no')) {
      d <- run.data[[tissue]]
      selected <- if (centroid == 'yes') which(d$sample.info$centroid == 'yes') else seq_len(nrow(d$mData))
      raw <- d$mData[selected, , drop = FALSE]
      info <- d$sample.info[selected]
      for (dimension in c('sample', 'metabolite')) {
        observations <- if (dimension == 'sample') raw else t(raw)
        ids <- if (dimension == 'sample') info$ID else colnames(raw)
        pairs <- list(c(1L, 2L), c(2L, nrow(observations)), c(3L, 7L))
        for (method in c('pearson', 'spearman')) {
          session$setInputs(`neighbor-matrix` = tissue, `neighbor-centroid` = centroid,
                            `neighbor-dimension` = dimension, `neighbor-method` = method)
          actual <- dat.dist()
          stopifnot(nrow(actual) == length(ids) * (length(ids) - 1L),
                    !'method' %in% names(actual))
          coefficient <- correlation_coefficient_label(method)
          for (pair in pairs) {
            a <- pair[1]; b <- pair[2]
            row <- if (dimension == 'sample') actual[sample_ID == ids[a] & neighbor_ID == ids[b]]
                   else actual[metabolite == ids[a] & metabolite_neighbor == ids[b]]
            keep <- is.finite(observations[a, ]) & is.finite(observations[b, ])
            reference <- if (method == 'spearman') cor.test(observations[a, keep], observations[b, keep], method = method, exact = FALSE)
                         else cor.test(observations[a, keep], observations[b, keep], method = method)
            stopifnot(nrow(row) == 1L, row$n == sum(keep))
            assert_close(row[[coefficient]], round(reference$estimate, 2))
            assert_close(row$pv, signif(reference$p.value, 2))
            assert_close(row$distance, round(1 - abs(reference$estimate), 2))
          }
        }
        # Changing correlation method must leave Euclidean distances unchanged.
        session$setInputs(`neighbor-distance` = 'euclidean', `neighbor-method` = 'pearson')
        euclidean <- copy(dat.dist())
        session$setInputs(`neighbor-method` = 'spearman')
        stopifnot(identical(euclidean, dat.dist()), !'method' %in% names(euclidean))
        pair <- pairs[[1]]
        row <- if (dimension == 'sample') euclidean[sample_ID == ids[pair[1]] & neighbor_ID == ids[pair[2]]]
               else euclidean[metabolite == ids[pair[1]] & metabolite_neighbor == ids[pair[2]]]
        assert_close(row$distance, round(as.numeric(dist(observations[pair, , drop = FALSE])), 2))
        session$setInputs(`neighbor-distance` = 'correlation')
      }
    }
  }
  # The table and its title remain tied to Submit, including after method edits.
  stopifnot(identical(neighbor.submitted()$data, initial.eu.d))
  session$setInputs(`neighbor-submit`=1)
  submitted_neighbor <- neighbor.submitted()
  stopifnot(grepl('(Spearman correlation)',submitted_neighbor$title,fixed=TRUE))
  session$setInputs(`neighbor-method`='pearson')
  stopifnot(identical(neighbor.submitted(),submitted_neighbor))
  session$setInputs(`neighbor-submit`=2)
  stopifnot(grepl('(Pearson correlation)',neighbor.submitted()$title,fixed=TRUE))
  # Confirm diagnostic filters select rows before computing correlations.
  session$setInputs(`neighbor-matrix` = 'fibroblast', `neighbor-centroid` = 'yes',
                    `neighbor-dimension` = 'sample', `neighbor-method` = 'spearman',
                    `neighbor-dx` = 'unaffected')
  filtered <- dat.dist()
  stopifnot(all(filtered$sample_Dx == 'unaffected'), all(filtered$neighbor_Dx == 'unaffected'))
})
stopifnot(identical(original_sample_names, lapply(run.data, function(d) names(d$sample.info))))
cat('PASS: neighbor coefficients, p values, pair counts and distances across tissues, dimensions and replicate filters; Euclidean regression and sample filtering.\n')

# Use a temporary device so full.cluster() cannot create Rplots.pdf in the project.
cluster_plot_file <- tempfile('gmdp-cluster-tests-', fileext = '.pdf')
pdf(cluster_plot_file, width = 12, height = 12)
tryCatch({
  shiny::testServer(function(input, output, session) {
    # Register the statically mounted interactive heatmap before its server.
    source(file.path(root, 'UI/MetabCluster_UI.R'), local = TRUE)
    source(file.path(root, 'Server/MetabCluster_Server.R'), local = TRUE)
  }, {
    session$setInputs(`metCluster-matrix` = 'fibroblast', `metCluster-method` = 'pearson',
                      `metCluster-all` = 'all', `metCluster-metabolite` = 'Ornithine',
                      `metCluster-cutoff` = 0.6)
    stopifnot(is.null(metCluster.applied()))
    submit_count <- 0L
    submit_cluster <- function() {
      submit_count <<- submit_count + 1L
      session$setInputs(`metCluster-submit`=submit_count)
    }
    for (tissue in c('fibroblast', 'plasma')) {
      d <- run.data[[tissue]]
      raw <- d$mData[d$sample.info$centroid == 'yes', , drop = FALSE]
      for (method in c('pearson', 'spearman')) {
        session$setInputs(`metCluster-matrix` = tissue, `metCluster-method` = method)
        submit_cluster()
        actual <- x.dat()
        expected <- cor(raw, method = method, use = 'pairwise.complete.obs')
        assert_close(actual$m, expected)
        stopifnot(nrow(actual$df) == choose(ncol(raw), 2))
        if (method == 'pearson') stopifnot(identical(actual, cor.list[[tissue]]))
        else assert_close(actual$df$value, as.vector(expected)[as.vector(lower.tri(expected))])
        selected <- actual$df[abs(value) > 0.6]
        stopifnot(identical(r.sub(), selected))
        # Preserve the original positive-correlation selection in the full tree.
        full_selected <- with(actual$df[Var1 != Var2 & value > 0.6], unique(c(as.character(Var1), as.character(Var2))))
        stopifnot(identical(full.metab.list()[[5]], full_selected), length(full_selected) >= 2L)
        full.cluster()
      }
      center <- as.character(x.dat()$df$Var1[which.max(abs(x.dat()$df$value))])
      session$setInputs(`metCluster-all` = 'one', `metCluster-metabolite` = center)
      submit_cluster()
      selected <- x.dat()$df[(Var1 == center | Var2 == center) & abs(value) > 0.6]
      stopifnot(identical(r.sub(), selected), nrow(selected) > 0L)
      # Draw both heatmaps to verify that the method-selected metabolites work.
      stopifnot(inherits(sub.cluster(), 'HeatmapList'))
      session$setInputs(`metCluster-all` = 'all')
    }
    stopifnot(all(c('fibroblast:spearman', 'plasma:spearman') %in% ls(cluster.cor.cache)))
    # Pending method changes keep the old result; Submit clears an empty result.
    session$setInputs(`metCluster-matrix` = 'fibroblast', `metCluster-all` = 'one',
                      `metCluster-metabolite` = 'Hydroquinone', `metCluster-cutoff` = 0.7,
                      `metCluster-method` = 'pearson')
    submit_cluster()
    stopifnot(nrow(r.sub()) > 0L, !is.null(output$`metCluster-plot`))
    applied_cluster <- metCluster.applied()
    session$setInputs(`metCluster-method` = 'spearman')
    stopifnot(identical(metCluster.applied(), applied_cluster), nrow(r.sub()) > 0L)
    submit_cluster()
    stopifnot(nrow(r.sub()) == 0L, grepl('No correlated metabolite', output$`metCluster-hint`))
    stopifnot(inherits(tryCatch(output$`metCluster-plot`, error = identity), 'shiny.silent.error'))
    # The plasma widget now stays mounted in the UI and is hidden on empty results.
  })
}, finally = {
  dev.off()
  unlink(cluster_plot_file)
})
cat('PASS: clustering Pearson cache regression, Spearman matrices, pair selection, full trees and selected-metabolite heatmaps.\n')
cat('All correlation regression checks passed.\n')
