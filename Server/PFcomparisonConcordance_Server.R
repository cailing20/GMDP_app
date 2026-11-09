PFcomparisonConcordance.method <- reactive({correlation_method(input$`PFcomparisonConcordance-method`)})
# Tables and downloads use the last confirmed method/data selection.
PFcomparisonConcordance.tableRequest <- reactiveVal(list(method = 'pearson', stat = 'untransformed data'))
observeEvent(input$`PFcomparisonConcordance-statSubmit`, {
  PFcomparisonConcordance.tableRequest(list(
    method = PFcomparisonConcordance.method(), stat = input$`PFcomparisonConcordance-stat`))
})
PFcomparisonConcordance.df <- reactive({
  request <- PFcomparisonConcordance.tableRequest()
  method <- request$method
  stat <- if (request$stat == 'untransformed data') 'm' else 'z'
  if (method == 'pearson') {
    # Retain the original, rounded Pearson statistics supplied with the app.
    metabolite <- copy(joint.stat$metabolite[[stat]])
    sample <- copy(joint.stat$sample[[stat]])
  } else {
    key <- if (stat == 'm') 'mData' else 'robustZ'
    p <- run.data$plasma[[key]][joint.subject$p.ind, joint.cor$metabolite, drop = FALSE]
    f <- run.data$fibroblast[[key]][joint.subject$f.ind, joint.cor$metabolite, drop = FALSE]
    make_stats <- function(indices, by_metabolite) {
      results <- lapply(indices, function(i) {
        if (by_metabolite) correlation_summary(p[, i], f[, i], method)
        else correlation_summary(p[i, ], f[i, ], method)
      })
      pv <- vapply(results, function(x) x$p.value, numeric(1))
      data.table(rho = round(vapply(results, function(x) x$estimate, numeric(1)), 2),
                 pv = signif(pv, 2), padj = signif(p.adjust(pv, method = 'BH'), 2),
                 n = vapply(results, function(x) x$n, integer(1)))
    }
    metabolite <- make_stats(seq_len(ncol(p)), TRUE)
    sample <- make_stats(seq_len(nrow(p)), FALSE)
  }
  list(metabolite = cbind(joint.cor[, 1, with = FALSE], metabolite),
       sample = cbind(joint.subject[, 1:2, with = FALSE], sample))
})
output$`PFcomparisonConcordance-metabolitesTitle` <- renderText({
  paste0('Metabolite correlation by shared subjects (',
         correlation_method_label(PFcomparisonConcordance.tableRequest()$method), ')')
})
output$`PFcomparisonConcordance-subjectsTitle` <- renderText({
  paste0('Subject correlation by shared metabolites (',
         correlation_method_label(PFcomparisonConcordance.tableRequest()$method), ')')
})
output$`PFcomparisonConcordance-metabolites` <- DT::renderDataTable({
  DT::datatable(PFcomparisonConcordance.df()$metabolite, rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE), filter = 'top')
}, server = FALSE)
output$`PFcomparisonConcordance-subjects` <- DT::renderDataTable({
  DT::datatable(PFcomparisonConcordance.df()$sample, rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE), filter = 'top')
}, server = FALSE)
output$`PFcomparisonConcordance-mStatDl` <- downloadHandler(
  filename = function() paste0('metabolite_correlation_by_shared_subjects_', PFcomparisonConcordance.tableRequest()$method, '.csv'),
  content = function(con) write.csv(PFcomparisonConcordance.df()$metabolite, con, row.names = FALSE)
)
output$`PFcomparisonConcordance-sStatDl` <- downloadHandler(
  filename = function() paste0('subject_correlation_by_shared_metabolites_', PFcomparisonConcordance.tableRequest()$method, '.csv'),
  content = function(con) write.csv(PFcomparisonConcordance.df()$sample, con, row.names = FALSE)
)
PFcomparisonConcordance.metabDat <- reactive({
  method <- PFcomparisonConcordance.method()
  key <- if (input$`PFcomparisonConcordance-stat` == 'untransformed data') 'mData' else 'robustZ'
  df <- data.table(Subject = with(joint.subject, paste(Subject, Dx, sep = '\n')),
                   Dx = joint.subject$Dx,
                   status = ifelse(joint.subject$Dx %in% c('unknown', 'unaffected'), joint.subject$Dx, 'diagnosed'),
                   plasma = run.data$plasma[[key]][joint.subject$p.ind, input$`PFcomparisonConcordance-metabolite`],
                   fibroblast = run.data$fibroblast[[key]][joint.subject$f.ind, input$`PFcomparisonConcordance-metabolite`])
  # Preserve historical Pearson plots; rank full-precision values for Spearman.
  if (method == 'pearson') df[, c('plasma', 'fibroblast') := lapply(.SD, round, 3), .SDcols = c('plasma', 'fibroblast')]
  label <- correlation_label(df$plasma, df$fibroblast, method)
  correlation_plotly(ggplot(df, aes(x = plasma, y = fibroblast, color = status, label = Subject)) +
             geom_point(alpha = .5) +
             ggtitle(paste0(input$`PFcomparisonConcordance-metabolite`, ', ', label)) +
             theme_bw(), height = 300)
})
PFcomparisonConcordance.metaboliteResult <- reactiveVal(NULL)
observeEvent(input$`PFcomparisonConcordance-metaboliteSubmit`, {
  PFcomparisonConcordance.metaboliteResult(PFcomparisonConcordance.metabDat())
})
output$`PFcomparisonConcordance-metabolitePlot` <- renderPlotly({
  req(PFcomparisonConcordance.metaboliteResult())
})
PFcomparisonConcordance.sampleDat <- reactive({
  method <- PFcomparisonConcordance.method()
  key <- if (input$`PFcomparisonConcordance-stat` == 'untransformed data') 'mData' else 'robustZ'
  subject <- input$`PFcomparisonConcordance-sample`
  p.ind <- which(run.data$plasma$sample.info$Subject == subject & run.data$plasma$sample.info$centroid == 'yes')
  f.ind <- which(run.data$fibroblast$sample.info$Subject == subject & run.data$fibroblast$sample.info$centroid == 'yes')
  df <- data.table(metabolite = joint.cor$metabolite,
                   plasma = as.numeric(run.data$plasma[[key]][p.ind, joint.cor$metabolite]),
                   fibroblast = as.numeric(run.data$fibroblast[[key]][f.ind, joint.cor$metabolite]))
  if (method == 'pearson') df[, c('plasma', 'fibroblast') := lapply(.SD, round, 3), .SDcols = c('plasma', 'fibroblast')]
  correlation_plotly(ggplot(df, aes(x = plasma, y = fibroblast, label = metabolite)) +
             geom_point(alpha = .5, color = '#E95420') +
             ggtitle(paste0(subject, ', ', correlation_label(df$plasma, df$fibroblast, method))) +
             theme_bw(), height = 300)
})
PFcomparisonConcordance.sampleResult <- reactiveVal(NULL)
observeEvent(input$`PFcomparisonConcordance-sampleSubmit`, {
  PFcomparisonConcordance.sampleResult(PFcomparisonConcordance.sampleDat())
})
output$`PFcomparisonConcordance-samplePlot` <- renderPlotly({
  req(PFcomparisonConcordance.sampleResult())
})
