output$`PFcompareM-subjects`<-DT::renderDataTable({DT::datatable(joint.subject[,1:2,with=F],rownames = F,options = list(pageLength=15),filter='top',)})
output$`PFcompareM-metabolites`<-DT::renderDataTable({DT::datatable(joint.cor,rownames = F,options = list(pageLength=15),filter='top')})
PFcompareM.dat<-reactive({
  df<-data.table(Subject=with(joint.subject,paste(Subject,Dx,sep = "\n")),
                 Dx=joint.subject$Dx,
                 status=ifelse(joint.subject$Dx%in%c('unknown','unaffected'),joint.subject$Dx,'diagnosed'),
                 plasma=run.data[['plasma']][['mData']][,input$`PFcompareM-metabolite`][joint.subject[['p.ind']]],
                 fibroblast=run.data[['fibroblast']][['mData']][,input$`PFcompareM-metabolite`][joint.subject[['f.ind']]])
  ggplot(df,aes(x=plasma,y=fibroblast,color=status,label=Dx))+geom_point(alpha=.5)+ggtitle(input$`PFcompareM-metabolite`)+theme_bw()
})
observeEvent(input$`PFcompareM-submit`,{
  output$`PFcompareM-plot`<-renderPlotly(isolate(PFcompareM.dat()))
})
