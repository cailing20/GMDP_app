# Focused interactive heatmap lifecycle regression. Browser brush QA complements it.
script_arg <- sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))
root <- dirname(dirname(normalizePath(script_arg[[1]])))
if (dir.exists(file.path(root, '.R-library'))) .libPaths(c(file.path(root, '.R-library'), .libPaths()))
packages <- c('shiny', 'shinyjs', 'data.table', 'reshape2', 'ComplexHeatmap', 'InteractiveComplexHeatmap')
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
source(file.path(root, 'R/correlation_helpers.R'))
load(file.path(root, 'data/input.RData'))
shiny::testServer(function(input, output, session) {
  # The UI must register the stable widget before any server-side rendering.
  source(file.path(root, 'UI/MetabCluster_UI.R'), local = TRUE)
  source(file.path(root, 'Server/MetabCluster_Server.R'), local = TRUE)
}, {
  session$setInputs(`metCluster-matrix` = 'plasma', `metCluster-method` = 'spearman',
                    `metCluster-all` = 'one', `metCluster-metabolite` = 'L-Leucine',
                    `metCluster-cutoff` = 0.7)
  stopifnot(is.null(metCluster.applied()))
  session$setInputs(`metCluster-submit` = 1)
  # Mirror the browser's acknowledgement that widget controls are initialized.
  session$setInputs(metClusterPlasma_reset_ui_done = 1,
                    .clientdata_output_metClusterPlasma_heatmap_width = 550,
                    .clientdata_output_metClusterPlasma_heatmap_height = 1000,
                    .clientdata_output_metClusterPlasma_sub_heatmap_width = 550,
                    .clientdata_output_metClusterPlasma_sub_heatmap_height = 1000)
  plot <- output$metClusterPlasma_heatmap
  stopifnot(is.list(plot), grepl('^data:image/png;base64,', plot$src), nrow(r.sub()) == 2L)
  selected <- sub.cluster()@ht_list[[1]]@matrix
  stopifnot(setequal(colnames(selected), c('L-Leucine', 'L-Isoleucine', '5-Aminopentanoic acid')))
  # Pending settings and plot redraws must continue to use the submitted heatmap.
  session$setInputs(`metCluster-method` = 'pearson', `metCluster-metabolite` = 'Pimelic acid')
  stopifnot(metCluster.applied()$`metCluster-method` == 'spearman',
            identical(selected, sub.cluster()@ht_list[[1]]@matrix))
  # Reusing the mounted widget must replace its observers and render fresh data.
  session$setInputs(`metCluster-submit` = 2)
  session$setInputs(metClusterPlasma_reset_ui_done = 2)
  stopifnot(metCluster.applied()$`metCluster-method` == 'pearson',
            !identical(selected, sub.cluster()@ht_list[[1]]@matrix),
            grepl('^data:image/png;base64,', output$metClusterPlasma_heatmap$src))
  # A no-neighbor submission must provide the hint, with no new widget creation.
  session$setInputs(`metCluster-matrix` = 'fibroblast', `metCluster-method` = 'spearman',
                    `metCluster-metabolite` = 'Hydroquinone', `metCluster-cutoff` = 0.7,
                    `metCluster-submit` = 3)
  stopifnot(nrow(r.sub()) == 0L, grepl('No correlated metabolite', output$`metCluster-hint`))
  # Return to the same plasma configuration after an empty result.
  session$setInputs(`metCluster-matrix` = 'plasma', `metCluster-method` = 'spearman',
                    `metCluster-metabolite` = 'L-Leucine', `metCluster-submit` = 4)
  session$setInputs(metClusterPlasma_reset_ui_done = 3)
  stopifnot(identical(selected, sub.cluster()@ht_list[[1]]@matrix),
            grepl('^data:image/png;base64,', output$metClusterPlasma_heatmap$src))
})
cat('PASS: mounted interactive heatmap initialization, reported L-Leucine configuration, Submit gating, repeated submissions and recovery after no neighbors.\n')
