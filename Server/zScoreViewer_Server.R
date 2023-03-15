zscore.sample.info<-reactive({
  sample.info<-run.data[[input$`zscore-matrix`]]$sample.info
  if(input$`zscore-centroid`=='yes') sample.info<-sample.info[centroid=='yes']
  sample.info
})
zscore.dat<-reactive({
  run.data[[input$`zscore-matrix`]][[ifelse(input$`zscore-centroid`=='yes','robustCZ','robustZ')]]
})
zscore.sample.dat<-reactive({
  ind<-match(input$`zscore-sample`,zscore.sample.info()$ID)
  tmp<-data.table(metabolite=colnames(zscore.dat()),value=as.numeric(round(zscore.dat()[ind,],3)))
  tmp<-tmp[which(!is.na(value)&!is.infinite(value))]
  tmp<-tmp[order(value,decreasing = T)]
  tmp$index<-1:nrow(tmp)
  tmp
})

zscore.metabolite.dat<-reactive({
  tmp<-data.table(Sample=paste(zscore.sample.info()$ID,zscore.sample.info()$Dx,sep = "\n"),
                  value=round(zscore.dat()[,input$`zscore-metabolite`],3),
                  zscore.sample.info())
  tmp<-tmp[which(!is.na(value)&!is.infinite(value))]
  colnames(tmp)[colnames(tmp)=='ID']<-'sample'
  tmp[,status:=Dx]
  tmp[!Dx%in%c('unaffected','unknown'),status:='diagnosed']
  tmp<-tmp[order(tmp$value,decreasing = T),]
  tmp$index<-1:nrow(tmp)
  tmp
})
zscore.sample.plot<-reactive({
  ggplot(zscore.sample.dat(),aes(x=index,y=value,label=metabolite))+geom_point(color='#E95420',alpha=0.5)+xlab('')+ylab('robust z-score')+theme_bw()+
    ggtitle(paste('z-score distribution for',input$`zscore-sample`))
})
zscore.metabolite.plot<-reactive({
  ggplot(zscore.metabolite.dat(),aes(x=index,y=value,label=Sample,color=status))+geom_point(alpha=0.5)+xlab('')+ylab('robust z-score')+theme_bw()+
    ggtitle(paste('z-score distribution for',input$`zscore-metabolite`))
})

observeEvent(input$`zscore-submit`,{
  updateSelectInput(session = session,inputId = 'zscore-metabolite',choices = isolate(sort(colnames(zscore.dat()))))
  updateSelectInput(session = session,inputId = 'zscore-sample',choices = isolate(sort(zscore.sample.info()$ID)))
  updateSelectInput(session = session,inputId = 'zscore-sampleDx',choices = isolate(c('all','unknown','unaffected',sort(setdiff(zscore.sample.info()$Dx,c('unknown','unaffected'))))))
})
observeEvent(input$`zscore-sampleDx`,{
  updateSelectInput(session = session,inputId = 'zscore-sample',choices = isolate(sort(if(input$`zscore-sampleDx`=='all') zscore.sample.info()$ID else zscore.sample.info()[Dx==input$`zscore-sampleDx`][['ID']])))
})
observeEvent(input$`zscore-sample`,{
  output$`zscore-samplePlot`<-renderPlotly({input$`zscore-submit`;expr={isolate(zscore.sample.plot())}})
  output$`zscore-sampleTable`<-DT::renderDataTable({input$`zscore-submit`;isolate(DT::datatable(zscore.sample.dat()[,c('metabolite','value'),with=F],rownames = F))})
})
observeEvent(input$`zscore-metabolite`,{
  output$`zscore-metabolitePlot`<-renderPlotly({input$`zscore-submit`;expr={isolate(zscore.metabolite.plot())}})
  output$`zscore-metaboliteTable`<-DT::renderDataTable({input$`zscore-submit`;DT::datatable({isolate(zscore.metabolite.dat()[,c('sample','Dx','value'),with=F])},rownames = F)})
})
output$`zscore-MetDl` <- downloadHandler(
  filename = function() {
    paste0(input$`zscore-matrix`,'_',paste0(input$`zscore-metabolite`,'_robustZ.csv'))
  },
  content = function(con) {
    write.csv(isolate(zscore.metabolite.dat()[,c('sample','Dx','value'),with=F]), con)
  }
)
output$`zscore-SamDl` <- downloadHandler(
  filename = function() {
    paste0(input$`zscore-matrix`,'_',paste0(input$`zscore-sample`,'_robustZ.csv'))
  },
  content = function(con) {
    write.csv(isolate(zscore.sample.dat()[,c('sample','Dx','value'),with=F]), con)
  }
)