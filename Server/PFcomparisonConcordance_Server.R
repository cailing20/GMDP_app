output$`PFcomparisonConcordance-metabolites`<-DT::renderDataTable({DT::datatable(cbind(joint.cor[,1,with=F],joint.stat$metabolite$m[,c('r','pv'),with=F]),rownames = F,options = list(pageLength=10),filter='top')})
output$`PFcomparisonConcordance-subjects`<-DT::renderDataTable({DT::datatable(cbind(joint.subject[,1:2,with=F],joint.stat$sample$m[,c('r','pv'),with=F]),rownames = F,options = list(pageLength=10),filter='top',)})
output$`PFcomparisonConcordance-mStatDl` <- downloadHandler(
  filename = 'metabolite_correlation_by_shared_subjects.csv',
  content = function(con) {write.csv(PFcomparisonConcordance.df()[['metabolite']], con)}
)
output$`PFcomparisonConcordance-sStatDl` <- downloadHandler(
  filename = 'subject_correlation_by_shared_metabolites.csv',
  content = function(con) {write.csv(PFcomparisonConcordance.df()[['sample']], con)}
)
PFcomparisonConcordance.metabDat<-reactive({
  df<-data.table(Subject=with(joint.subject,paste(Subject,Dx,sep = "\n")),
                 Dx=joint.subject$Dx,
                 status=ifelse(joint.subject$Dx%in%c('unknown','unaffected'),joint.subject$Dx,'diagnosed'),
                 plasma=round(run.data[['plasma']][[ifelse(input$`PFcomparisonConcordance-stat`=='untransformed data','mData','robustZ')]][,input$`PFcomparisonConcordance-metabolite`][joint.subject[['p.ind']]],3),
                 fibroblast=round(run.data[['fibroblast']][[ifelse(input$`PFcomparisonConcordance-stat`=='untransformed data','mData','robustZ')]][,input$`PFcomparisonConcordance-metabolite`][joint.subject[['f.ind']]],3))
  ggplotly(ggplot(df,aes(x=plasma,y=fibroblast,color=status,label=Subject))+geom_point(alpha=.5)+
             ggtitle(paste0(with(cor.test(df$plasma,df$fibroblast),paste0(input$`PFcomparisonConcordance-metabolite`,ifelse(nchar(input$`PFcomparisonConcordance-metabolite`)<20,", ","\n"),"r=",round(estimate,2),ifelse(p.value<.05,'*','')))))+
             theme_bw(),height = 300, width = 340)
  
})
observeEvent(input$`PFcomparisonConcordance-metaboliteSubmit`,{
  output$`PFcomparisonConcordance-metabolitePlot`<-renderPlotly(isolate(PFcomparisonConcordance.metabDat()))
})


PFcomparisonConcordance.sampleDat<-reactive({
  df<-data.table(metabolite=joint.cor$metabolite,
                 plasma=round(run.data[['plasma']][[ifelse(input$`PFcomparisonConcordance-stat`=='untransformed data','mData','robustZ')]][
                   with(run.data[['plasma']][['sample.info']],which(Subject==input$`PFcomparisonConcordance-sample` & centroid=='yes')),joint.cor$metabolite],3),
                 fibroblast=round(run.data[['fibroblast']][[ifelse(input$`PFcomparisonConcordance-stat`=='untransformed data','mData','robustZ')]][
                   with(run.data[['fibroblast']][['sample.info']],which(Subject==input$`PFcomparisonConcordance-sample` & centroid=='yes')),joint.cor$metabolite],3))
  ggplotly(ggplot(df,aes(x=plasma,y=fibroblast,label=metabolite))+geom_point(alpha=.5,color='#E95420')+
             ggtitle(paste0(with(cor.test(df$plasma,df$fibroblast),paste0(input$`PFcomparisonConcordance-sample`,", r=",round(estimate,2),ifelse(p.value<.05,'*','')))))+
             theme_bw(),height = 300, width = 300)
})
observeEvent(input$`PFcomparisonConcordance-sampleSubmit`,{
  output$`PFcomparisonConcordance-samplePlot`<-renderPlotly(isolate(PFcomparisonConcordance.sampleDat()))
})
PFcomparisonConcordance.df<-reactive({
  list(metabolite=cbind(joint.cor[,1,with=F],joint.stat$metabolite[[ifelse(input$`PFcomparisonConcordance-stat`=='untransformed data','m','z')]][,c('r','pv','padj'),with=F]),
       sample=cbind(joint.subject[,1:2,with=F],joint.stat$sample[[ifelse(input$`PFcomparisonConcordance-stat`=='untransformed data','m','z')]][,c('r','pv','padj'),with=F]))
})
observeEvent(input$`PFcomparisonConcordance-statSubmit`,{
  output$`PFcomparisonConcordance-metabolites`<-DT::renderDataTable({DT::datatable(isolate(PFcomparisonConcordance.df()[['metabolite']][,1:3,with=F]),rownames = F,options = list(pageLength=10),filter='top')})
  output$`PFcomparisonConcordance-subjects`<-DT::renderDataTable({DT::datatable(isolate(PFcomparisonConcordance.df()[['sample']][,1:4,with=F]),rownames = F,options = list(pageLength=10),filter='top')})
})