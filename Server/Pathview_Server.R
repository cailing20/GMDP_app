shinyjs::hide("pathview-sample")
pathview.sample.info<-reactive({
  sample.info<-run.data[[input$`pathview-matrix`]]$sample.info
  sample.info
})
pathview.metab.info<-reactive({
  run.data[[input$`pathview-matrix`]]$metabolite.info
})
pathview.dat<-reactive({
  run.data[[input$`pathview-matrix`]][['robustZ']]
})
pathview.sample.dat<-reactive({
  ind<-match(input$`pathview-sample`,pathview.sample.info()$ID)
  tmp<-data.table(z=pathview.dat()[ind,],KEGG=pathview.metab.info()[['KEGG']])[!is.na(KEGG)][KEGG!=''][order(z)]
  with(tmp,structure(z,names=KEGG))
})
pathview.Dx.dat<-reactive({
  tmp<-data.table(DxFC[[input$`pathview-matrix`]][Dx==input$`pathview-sampleDx`],KEGG=pathview.metab.info()[['KEGG']])[!is.na(KEGG)]
  with(tmp,structure(Log2FC,names=KEGG))
})
pathview.mnames<-reactive({
  tmp<-m.names[which(m.avail[[input$`pathview-matrix`]])]
  c('Metabolic pathway panorama'='hsa01100',tmp[order(names(tmp))])
})

observeEvent(input$`pathview-specify`,{
  shinyjs::show("pathview-Step2Div")
  updateSelectInput(session = session,inputId = 'pathview-pathway',choices = isolate(pathview.mnames()))
  if(input$`pathview-measure`=='grp'){
    shinyjs::hide("pathview-sample")
    updateSelectInput(session = session,inputId = 'pathview-sampleDx',choices = isolate(sort(setdiff(pathview.sample.info()$Dx,c('unknown','unaffected')))))
  }else{
    shinyjs::show("pathview-sample")
    updateSelectInput(session = session,inputId = 'pathview-sample',choices = isolate(sort(pathview.sample.info()$ID)))
    updateSelectInput(session = session,inputId = 'pathview-sampleDx',choices = isolate(c('all','unknown','unaffected',sort(setdiff(pathview.sample.info()$Dx,c('unknown','unaffected')))))) 
  }
})
observeEvent(input$`pathview-sampleDx`,{
  updateSelectInput(session = session,inputId = 'pathview-sample',choices = isolate(sort(if(input$`pathview-sampleDx`=='all') pathview.sample.info()$ID else pathview.sample.info()[Dx==input$`pathview-sampleDx`][['ID']])))
})
observeEvent(input$`pathview-submit`,{
  shinyjs::show("pathview-PlotDiv")
  output$`pathview-plot`<-renderImage({isolate({
    random.seed<-floor(runif(1,0,10^5))
    outfile <- paste0('./', input$`pathview-pathway`,'.',random.seed, ".png")
    pathview(cpd.data = (if(input$`pathview-measure`=='sample') pathview.sample.dat() else pathview.Dx.dat()),pathway.id = gsub('hsa','',input$`pathview-pathway`),kegg.dir = 'data/pathview/',
             out.suffix=random.seed,kegg.native = T,
             limit = list(gene=1,cpd=match(input$`pathview-measure`,c('grp','sample'))),same.layer=F)
    if(input$`pathview-zoom`){list(src = outfile,width=1300)}else{list(src = outfile)}
  })}, deleteFile = T)
})
