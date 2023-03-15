# by default, load the plasma data
output$`outlierPathway-info`<-DT::renderDataTable(DT::datatable({initial.outlier},options = list(pageLength=15),filter='top',rownames = F))
pw.outlier.df<-reactive({
  sample.info<-run.data[[input$`outlierPathway-matrix`]][['sample.info']]
  nes.df<-fread(paste0('data/',input$`outlierPathway-matrix`,'_mseaOnetailed.csv'))
  if(input$`outlierPathway-centroid`=='yes') {sample.info<-sample.info[centroid=='yes'];nes.df<-nes.df[ID%in%sample.info[['ID']]]}
  nes.df<-data.table(sample.info[match(nes.df$ID,ID),c('ID','Dx'),with=F],nes.df[,-1,with=F])[sig!='non-significant']
  nes.df[,NES:=round(NES,2)]
  nes.df[order(NES,decreasing = T)][!Dx%in%(input$`outlierPathway-excludeSample`)]
})
output$`outlierPathway-info`<-DT::renderDataTable({
  input$`outlierPathway-submit`
  DT::datatable({isolate(pw.outlier.df())},options = list(pageLength=15),filter='top',rownames = F)
})