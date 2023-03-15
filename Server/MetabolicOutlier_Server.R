# by default, load the plasma data
output$`outlierMetabolic-info`<-DT::renderDataTable(DT::datatable({initial.outlier},options = list(pageLength=15),filter='top',rownames = F))
metab.outlier.df<-reactive({
  sample.info<-run.data[[input$`outlierMetabolic-matrix`]]$sample.info
  if(input$`outlierMetabolic-centroid`=='yes') sample.info<-sample.info[centroid=='yes']
  outlier.df<-as.data.table(reshape2::melt(data.table(sample.info[,c('ID','Dx')],run.data[[input$`outlierMetabolic-matrix`]][[ifelse(input$`outlierMetabolic-centroid`=='yes','robustCZ','robustZ')]])))
  colnames(outlier.df)[3:4]<-c('metabolite','z-score')
  outlier.df[,`z-score`:=round(`z-score`,2)]
  outlier.df<-outlier.df[abs(`z-score`)>3][order(`z-score`,decreasing = T)][!Dx%in%(input$`outlierMetabolic-excludeSample`)]
  outlier.df
})
output$`outlierMetabolic-info`<-DT::renderDataTable({
  input$`outlierMetabolic-submit`
  DT::datatable({isolate(metab.outlier.df())},options = list(pageLength=15),filter='top',rownames = F)
})