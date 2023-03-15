scatter.plot<-reactive({
  tmp<-setNames(as.data.frame(run.data$mData[,c(input$`scatter-x-metabolite`,input$`scatter-y-metabolite`)]),c('x','y'))
  tmp$x<-round(tmp$x,3);tmp$y<-round(tmp$y,3)
  tmp<-data.table(tmp,sample=with(run.data$sample.info,paste(ID,Dx,sep = "\n")))
  tmp<-tmp[complete.cases(tmp[,1:2,with=F]),]
  ggplot(tmp,aes(x=x,y=y,label=sample))+geom_point(color='navy',alpha=0.5)+
    xlab(input$`scatter-x-metabolite`)+ylab(input$`scatter-y-metabolite`)+theme_bw()
})
scatter.listen<-reactive({
  list(input$`scatter-x-metabolite`,input$`scatter-y-metabolite`)
})
observeEvent(scatter.listen(),
             output$`scatter-plot`<-renderPlotly(expr={scatter.plot()}))