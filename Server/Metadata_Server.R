meta.tb<-reactive({
  tmp<-run.data[[input$`metadata-matrixChoice`]][[paste0(input$`metadata-infoChoice`,'.info')]]
  if(input$`metadata-infoChoice`=='sample') tmp<-tmp[order(Dx)] else tmp<-tmp[order(metabolite)]
  tmp
})
meta.tb.link<-reactive({
  if(input$`metadata-infoChoice`=='sample'){
    meta.tb()%>%mutate(OMIM_ID = ifelse(is.na(OMIM_ID),'',paste0("<a href=https://www.omim.org/entry/", OMIM_ID," target='_blank'>", OMIM_ID,"</a>")))
  }else{
    meta.tb()%>%mutate(HMDB = ifelse(!grepl('HMDB',HMDB),'',paste0("<a href=https://hmdb.ca/metabolites/", HMDB," target='_blank'>", HMDB,"</a>")))%>%
      mutate(KEGG = ifelse(!grepl("^C",KEGG),'',paste0("<a href=https://www.genome.jp/dbget-bin/www_bget?", KEGG," target='_blank'>", KEGG,"</a>")))%>%
      mutate(PubChem = ifelse(is.na(PubChem),'',paste0("<a href=https://pubchem.ncbi.nlm.nih.gov/compound/", PubChem," target='_blank'>", PubChem,"</a>")))
  }
})

output$`metadata-info`<-DT::renderDataTable(DT::datatable({meta.tb.link()},escape = FALSE,options = list(pageLength=15),filter='top',rownames = F))
output$`metadata-tb` <- downloadHandler(
  filename = function() {
    paste0(input$`metadata-matrixChoice`,'_',paste0(input$`metadata-infoChoice`,'_metadata.csv'))
  },
  content = function(con) {
    write.csv(meta.tb(), con)
  }
)