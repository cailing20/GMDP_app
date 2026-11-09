# Matrix/replicate changes are applied with the associated Update button.
zscore.applied <- reactiveVal(list(matrix = 'plasma', centroid = 'yes'))
zscore.sample.info<-reactive({
  sample.info<-run.data[[zscore.applied()$matrix]]$sample.info
  if(zscore.applied()$centroid=='yes') sample.info<-sample.info[centroid=='yes']
  sample.info
})
zscore.dat<-reactive({
  run.data[[zscore.applied()$matrix]][[ifelse(zscore.applied()$centroid=='yes','robustCZ','robustZ')]]
})
zscore.sample.dat<-reactive({
  ind<-match(input$`zscore-sample`,zscore.sample.info()$ID)
  req(length(ind) == 1L, !is.na(ind))
  tmp<-data.table(metabolite=colnames(zscore.dat()),value=as.numeric(round(zscore.dat()[ind,],3)))
  tmp<-tmp[which(!is.na(value)&!is.infinite(value))]
  tmp<-tmp[order(value,decreasing = T)]
  tmp$index<-seq_len(nrow(tmp))
  tmp
})

zscore.metabolite.dat<-reactive({
  req(input$`zscore-metabolite` %in% colnames(zscore.dat()))
  tmp<-data.table(Sample=paste(zscore.sample.info()$ID,zscore.sample.info()$Dx,sep = "\n"),
                  value=round(zscore.dat()[,input$`zscore-metabolite`],3),
                  zscore.sample.info())
  tmp<-tmp[which(!is.na(value)&!is.infinite(value))]
  colnames(tmp)[colnames(tmp)=='ID']<-'sample'
  tmp[,status:=Dx]
  tmp[!Dx%in%c('unaffected','unknown'),status:='diagnosed']
  tmp<-tmp[order(tmp$value,decreasing = T),]
  tmp$index<-seq_len(nrow(tmp))
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
  zscore.applied(list(matrix = input$`zscore-matrix`, centroid = input$`zscore-centroid`))
  updateSelectInput(session = session,inputId = 'zscore-metabolite',choices = isolate(sort(colnames(zscore.dat()))))
  updateSelectInput(session = session,inputId = 'zscore-sample',choices = isolate(sort(zscore.sample.info()$ID)))
  updateSelectInput(session = session,inputId = 'zscore-sampleDx',choices = isolate(c('all','unknown','unaffected',sort(setdiff(zscore.sample.info()$Dx,c('unknown','unaffected'))))))
})
observeEvent(input$`zscore-sampleDx`,{
  updateSelectInput(session = session,inputId = 'zscore-sample',choices = isolate(sort(if(input$`zscore-sampleDx`=='all') zscore.sample.info()$ID else zscore.sample.info()[Dx==input$`zscore-sampleDx`][['ID']])))
})
output$`zscore-samplePlot` <- renderPlotly({ggplotly(zscore.sample.plot())})
output$`zscore-sampleTable` <- DT::renderDataTable({
  DT::datatable(zscore.sample.dat()[,c('metabolite','value'),with=FALSE], rownames=FALSE)
})
output$`zscore-metabolitePlot` <- renderPlotly({ggplotly(zscore.metabolite.plot())})
output$`zscore-metaboliteTable` <- DT::renderDataTable({
  DT::datatable(zscore.metabolite.dat()[,c('sample','Dx','value'),with=FALSE], rownames=FALSE)
})
output$`zscore-MetDl` <- downloadHandler(
  filename = function() {
    paste0(zscore.applied()$matrix,'_',paste0(input$`zscore-metabolite`,'_robustZ.csv'))
  },
  content = function(con) {
    write.csv(isolate(zscore.metabolite.dat()[,c('sample','Dx','value'),with=F]), con)
  }
)
output$`zscore-SamDl` <- downloadHandler(
  filename = function() {
    paste0(zscore.applied()$matrix,'_',paste0(input$`zscore-sample`,'_robustZ.csv'))
  },
  content = function(con) {
    write.csv(isolate(zscore.sample.dat()[,c('metabolite','value'),with=F]), con)
  }
)