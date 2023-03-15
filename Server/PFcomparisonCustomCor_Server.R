observeEvent(input$`PFcompareCustomCor-OnlySharedSubj`,{
  if(input$`PFcompareCustomCor-OnlySharedSubj`=='yes')  shinyjs::hide("PFcompareCustomCor-Centroid") else shinyjs::show("PFcompareCustomCor-Centroid")
})

custom.data.plot<-reactive({
  p.dat<-data.table(source='plasma',round(run.data[['plasma']][['mData']][,c(input$`PFcompareCustomCor-Mx`,input$`PFcompareCustomCor-My`)],3),run.data[['plasma']][['sample.info']][,c('ID','Subject','centroid','Dx'),with=F])
  f.dat<-data.table(source='fibroblast',round(run.data[['fibroblast']][['mData']][,c(input$`PFcompareCustomCor-Mx`,input$`PFcompareCustomCor-My`)],3),run.data[['fibroblast']][['sample.info']][,c('ID','Subject','centroid','Dx'),with=F])
  if(input$`PFcompareCustomCor-OnlySharedSubj`=='yes') {p.dat<-p.dat[joint.subject[['p.ind']]];f.dat<-f.dat[joint.subject[['f.ind']]]}
  custom.df<-rbind(p.dat,f.dat);colnames(custom.df)[2:3]<-c('x','y')
  if(input$`PFcompareCustomCor-Centroid`=='yes') custom.df<-custom.df[centroid=='yes']
  custom.df[,status:=ifelse(Dx%in%c('unknown','unaffected'),Dx,'diagnosed')]
  custom.df[,Subject:=paste(Subject,Dx,sep = "\n")]
  get.stat<-function(x,y){tmp<-cor.test(x,y);paste0('r = ',round(tmp$estimate,2),', pv = ',signif(tmp$p.value,2))}
  custom.df[,stat:=get.stat(x,y),by=source]
  if(input$`PFcompareCustomCor-OnlySharedSubj`!='yes') custom.df[,stat2:=get.stat(x,y),by=.(source,status)]
  list(lapply(c('plasma','fibroblast'),function(tissue) ggplot(custom.df[source==tissue],aes(x=x,y=y,color=status,label=Subject))+geom_point(alpha=.5)+xlab(input$`PFcompareCustomCor-Mx`)+ylab(input$`PFcompareCustomCor-My`)+facet_grid(source~stat)+theme_bw()+
                theme(legend.position = 'none')),
       (if(input$`PFcompareCustomCor-OnlySharedSubj`=='no') lapply(c('plasma','fibroblast'),function(tissue) lapply(c('diagnosed','unaffected','unknown'),function(dx){
         ggplot(custom.df[source==tissue][status==dx],aes(x=x,y=y,label=Subject))+geom_point(alpha=.5,color=gg_color_hue(3)[match(dx,c('diagnosed','unaffected','unknown'))])+xlab(input$`PFcompareCustomCor-Mx`)+ylab(input$`PFcompareCustomCor-My`)+
           facet_grid(source~stat2)+theme_classic()+theme(legend.position = 'none')
       }))))
})

observeEvent(input$`PFcompareCustomCor-Submit`,{
  output$`PFcompareCustomCor-PlotPlasma`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[1]][[1]])%>% layout(height = 500,width=500)))
  output$`PFcompareCustomCor-PlotFibroblast`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[1]][[2]])%>% layout(height = 500,width=500)))
  if(input$`PFcompareCustomCor-OnlySharedSubj`!='yes'){
    output$`PFcompareCustomCor-PlotPlasma.1`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[2]][[1]][[1]])%>% layout(height = 220,width=220)))
    output$`PFcompareCustomCor-PlotPlasma.2`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[2]][[1]][[2]])%>% layout(height = 220,width=220)))
    output$`PFcompareCustomCor-PlotPlasma.3`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[2]][[1]][[3]])%>% layout(height = 220,width=220)))
    output$`PFcompareCustomCor-PlotFibroblast.1`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[2]][[2]][[1]])%>% layout(height = 220,width=220)))
    output$`PFcompareCustomCor-PlotFibroblast.2`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[2]][[2]][[2]])%>% layout(height = 220,width=220)))
    output$`PFcompareCustomCor-PlotFibroblast.3`<-renderPlotly(isolate(ggplotly(custom.data.plot()[[2]][[2]][[3]])%>% layout(height = 220,width=220)))
  }else{
    output$`PFcompareCustomCor-PlotPlasma.1`<-NULL;output$`PFcompareCustomCor-PlotPlasma.2`<-NULL;output$`PFcompareCustomCor-PlotPlasma.3`<-NULL
    output$`PFcompareCustomCor-PlotFibroblast.1`<-NULL;output$`PFcompareCustomCor-PlotFibroblast.2`<-NULL;output$`PFcompareCustomCor-PlotFibroblast.3`<-NULL
  }
})