ProcDl.data<-reactive({
  wb<-createWorkbook()
  s <- createStyle(fgFill = "#E95420")
  options("openxlsx.numFmt" = "0.000")
  addWorksheet(wb,'data');writeDataTable(wb,'data',data.table(SampleID=run.data[[input$`ProcDl-matrix`]][['sample.info']][['ID']],run.data[[input$`ProcDl-matrix`]][['mData']]))
  addStyle(wb,'data', s, cols=1:(1+ncol(run.data[[input$`ProcDl-matrix`]][['mData']])), rows=1)
  addWorksheet(wb,'samples');writeDataTable(wb,'samples',run.data[[input$`ProcDl-matrix`]][['sample.info']])
  addStyle(wb,'samples', s, cols=1:ncol(run.data[[input$`ProcDl-matrix`]][['sample.info']]), rows=1)
  addWorksheet(wb,'metabolites');writeDataTable(wb,'metabolites',run.data[[input$`ProcDl-matrix`]][['metabolite.info']])
  addStyle(wb,'metabolites', s, cols=1:ncol(run.data[[input$`ProcDl-matrix`]][['metabolite.info']]), rows=1)
  wb
})

output$`ProcDl-button` <- downloadHandler(
  filename = function() {
    paste0(input$`ProcDl-matrix`,'_data.xlsx')
  },
  content = function(file) {
    saveWorkbook(wb = isolate(ProcDl.data()), file = file,overwrite = T)
  }
)
