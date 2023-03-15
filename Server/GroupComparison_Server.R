observeEvent(input$`gp1-select`,output$`compare-group1` <- renderTree({tree.list()[[input$`gp1-select`]]}))
observeEvent(input$`gp2-select`,output$`compare-group2` <- renderTree({tree.list()[[input$`gp2-select`]]}))
gp.data<-reactive({
  gp1<-intersect(run.data$sample.info$ID,unlist(get_selected(input$`compare-group1`, format = "names")))
  gp2<-intersect(run.data$sample.info$ID,unlist(get_selected(input$`compare-group2`, format = "names")))
  tmp<-data.table(sample=c(gp1,gp2),group=rep(c('group 1','group 2'),c(length(gp1),length(gp2))),run.data$mData[match(c(gp1,gp2),run.data$sample.info$ID),])
  tmp
})
gp.compare<-reactive({
  result<-data.table(metabolite=colnames(run.data$mData),
                     gpDiff=as.numeric(gp.data()[,lapply(.SD,FUN=function(x) mean(x,na.rm = T)),by=group,.SDcols=-c(1:2)][,lapply(.SD,FUN=function(x) -diff(x)),.SDcols=-1]),
                     pv=as.numeric(gp.data()[,lapply(.SD,FUN=function(x) tryCatch({
                       if(input$`gp-method`=='parametric') t.test(x~group)$p.value else wilcox.test(x~group)$p.value
                     },error=function(e) -1.0)),.SDcols=-c(1:2)]))
  result<-result[pv>=0]
  result$q<-signif(p.adjust(result$pv,'BH'),2)
  result$gpDiff<-round(as.numeric(result$gpDiff),3)
  result<-result[order(pv,decreasing = F),]
  result$pv<-signif(result$pv,2)
  result
})
observeEvent(input$`gp-submit`,{
  output$volcano<-renderPlotly(print(ggplotly(ggplot(isolate(gp.compare()),aes(x=gpDiff,y=signif(-log10(pv),2),label=metabolite))+
                                                geom_point()+theme_bw()+ylab('-log10(pv)')+xlab('group difference (group 1-group2)'))))
  output$`gp-results`<-DT::renderDataTable(DT::datatable({isolate(gp.compare())},options = list(pageLength=5),rownames = F))
  updateSelectInput(session,inputId = 'gp-metabolite',choices = sort(colnames(run.data$mData)))
})
observeEvent(input$`bp-submit`,
             output$`gp-boxplot`<-renderPlotly({
               p<-ggplot(setNames(as.data.frame(isolate(gp.data())[,c('sample','group',isolate(input$`gp-metabolite`)),with=F]),c('sample','group','feature'))%>%mutate(feature=round(feature,3)),
               aes(x=group,y=feature,label=sample))+geom_boxplot(outlier.shape = NA)+geom_jitter(width=0.3)+theme_bw()
             p <- plotly_build(p,registerFrames=T)
             if(p$x$layout$xaxis$ticktext[1]=='NA') p$x$layout$xaxis$ticktext<-p$x$layout$xaxis$ticktext[-1]
             p$x$data[1] <- lapply(p$x$data[1], FUN = function(x){
               x$marker = list(opacity = 0)
               return(x)
             })
             isolate(p)
             })
)