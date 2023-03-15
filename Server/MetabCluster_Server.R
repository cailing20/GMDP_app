# clear the plots
shinyjs::hide('metCluster-iHM');shinyjs::hide('metCluster-plot')
# choose input correlation df
x.dat<-reactive({cor.list[[input$`metCluster-matrix`]]})
r.sub<-reactive({
  if(input$`metCluster-all`=='all'){
    x.dat()$df[abs(value)>input$`metCluster-cutoff`]
  }else{
    x.dat()$df[(Var1==input$`metCluster-metabolite`|Var2==input$`metCluster-metabolite`) & abs(value)>input$`metCluster-cutoff`]
  }
})
observeEvent(input$`metCluster-matrix`,{
  updateSelectInput(session = session,inputId = 'metCluster-metabolite',choices = sort(run.data[[input$`metCluster-matrix`]]$metabolite.info$metabolite))
})
# update visualization option based on all or one metabolite option
observeEvent(input$`metCluster-all`,{
  if(input$`metCluster-all`=='one'){
    shinyjs::show('metCluster-metabolite')
  }else{
    shinyjs::hide('metCluster-metabolite')
  }
})
full.metab.list<-reactive({
  lapply(2:9,function(i) with(cor.list[[input$`metCluster-matrix`]][['df']][Var1!=Var2 & value>(i/10)],unique(c(as.character(Var1),as.character(Var2)))))
})
dend.param<-reactive({sqrt(unlist(lapply(full.metab.list(),length)))[input$`metCluster-cutoff`*10-1]})
# visualization reactives
full.cluster<-reactive({
  d<-as.dist(1-abs(x.dat()[['m']]))
  d.long<-melt(as.matrix(d))
  setDT(d.long)
  metab.col<-table(unlist(full.metab.list()))
  select.metab<-full.metab.list()[[input$`metCluster-cutoff`*10-1]];d<-as.dist(1-abs(cor.list[[input$`metCluster-matrix`]][['m']][select.metab,select.metab]));h<-hclust(d,method = 'average')
  par(mar=c(rep(5-(25-dend.param())/20,4)),xpd=T,cex=1.65-dend.param()/20);plot(as.phylo(h), type = "fan",tip.color=colorRampPalette(brewer.pal(11,'Spectral')[4:1])(8)[metab.col[select.metab]])
})
full.network<-reactive({
  all.nodes<-as.character(unique(c(r.sub()$Var1,r.sub()$Var2)))
  nodes<-data.frame(id=all.nodes,label=all.nodes)
  edges <- r.sub()[,1:3]
  colnames(edges) <- c("from", "to", "width")
  edges[,label:=round(width,2)][,color:=ifelse(width>0,'red','blue')][,width:=abs(width)*3]
  visNetwork(nodes, edges) %>% 
    visNodes(color = list(background = "#ffd6c7", border = '#E95420'))%>%
    visOptions(highlightNearest = list(enabled = T,hover = T))%>%
    visPhysics(stabilization = FALSE)%>%visEdges(smooth = FALSE)
})
sub.cluster<-reactive({
  sel.metab<-intersect(with(r.sub(),unique(c(Var1,Var2))),colnames(run.data[[input$`metCluster-matrix`]][['robustCZ']]))
  if(length(sel.metab)>1){
    sel.dat<-run.data[[input$`metCluster-matrix`]][['robustCZ']][,sel.metab]
    sel.sam<-run.data[[input$`metCluster-matrix`]][['sample.info']][centroid=='yes']
    rownames(sel.dat)<-with(sel.sam,paste0(Subject,' (',Dx,')'))
    draw(Heatmap(sel.dat, name = "robust Z score",clustering_method_rows = 'ward.D2',clustering_method_columns = 'ward.D2',
                 show_row_names = input$`metCluster-matrix`=='fibroblast', row_names_gp = gpar(fontsize=10),
                 heatmap_legend_param = list(legend_direction = "horizontal")),heatmap_legend_side = "bottom",padding = unit(c(10, 2, 2, 15), "mm"))
  }
})


observeEvent(input$`metCluster-submit`,{
  output$`metCluster-hint`<-NULL
  switch(match(input$`metCluster-all`,c('all','one')),
         {           
           output$`metCluster-plot`<-renderPlot(isolate(full.cluster()),height = dend.param()*20+950,width=dend.param()*20+950)
           shinyjs::hide('metCluster-iHM');shinyjs::show('metCluster-plot')
         },
         {
           if(nrow(r.sub())==0){
             output$`metCluster-hint`<-renderText("No correlated metabolite at this cut-off, please select a lower cut-off value and try again.")
           }else{
             if(input$`metCluster-matrix`=='plasma'){
               isolate({
                 InteractiveComplexHeatmapWidget(input, output, session, ht_list = sub.cluster(),output_id = 'metCluster-iHM',close_button = F,output_ui = NULL,output_ui_float = TRUE,
                                                 height1 = 1000,height2 = 1000,width1 = 550,width2 = 550)
                 shinyjs::hide('metCluster-plot');shinyjs::show('metCluster-iHM')
               })
             }else{
               output$`metCluster-plot`<-renderPlot(isolate(sub.cluster()),height = 1000,width=nrow(r.sub())*15+300) 
               shinyjs::hide('metCluster-iHM');shinyjs::show('metCluster-plot')
             }
           }
         })
})

