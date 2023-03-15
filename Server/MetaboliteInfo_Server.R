# by default, load the merged data
output$`metabolite-info`<-DT::renderDataTable(DT::datatable({run.data$metabolite.info[,c('mode','Compound Name',
                                                                                      grep("SD|diff|between",colnames(run.data$metabolite.info),value = T)),
                                                                                   with=F]},options = list(pageLength=15),filter='top',rownames = F))
observeEvent(input$`metabolite-var`,{
  output$`metabolite-info`<-DT::renderDataTable(DT::datatable({run.data$metabolite.info[,input$`metabolite-var`,with=F]},options = list(pageLength=15),filter='top',rownames = F))
})
