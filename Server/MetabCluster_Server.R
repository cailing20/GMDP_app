# clear the plots
shinyjs::hide('metCluster-iHM');shinyjs::hide('metCluster-plot')
# Only submitted settings may affect plots, including plot redraws on resize.
metCluster.applied <- reactiveVal(NULL)
cluster.settings <- reactive({req(metCluster.applied())})
# choose input correlation df
cluster.cor.cache <- new.env(parent=emptyenv())
x.dat<-reactive({
  tissue <- cluster.settings()$`metCluster-matrix`
  method <- correlation_method(cluster.settings()$`metCluster-method`)
  if(method=='pearson') return(cor.list[[tissue]])
  key <- paste(tissue,method,sep=':')
  if(!exists(key,envir=cluster.cor.cache,inherits=FALSE)) {
    dat <- run.data[[tissue]]
    m <- stats::cor(dat$mData[dat$sample.info$centroid=='yes',,drop=FALSE],
                    use='pairwise.complete.obs',method=method)
    m[!is.finite(m)] <- NA_real_
    df <- as.data.table(reshape2::melt(m))
    df <- df[as.vector(lower.tri(m))]
    assign(key,list(m=m,df=df),envir=cluster.cor.cache)
  }
  get(key,envir=cluster.cor.cache,inherits=FALSE)
})
r.sub<-reactive({
  if(cluster.settings()$`metCluster-all`=='all'){
    x.dat()$df[abs(value)>cluster.settings()$`metCluster-cutoff`]
  }else{
    x.dat()$df[(Var1==cluster.settings()$`metCluster-metabolite`|Var2==cluster.settings()$`metCluster-metabolite`) & abs(value)>cluster.settings()$`metCluster-cutoff`]
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
  lapply(2:9,function(i) with(x.dat()[['df']][Var1!=Var2 & value>(i/10)],unique(c(as.character(Var1),as.character(Var2)))))
})
dend.param<-reactive({sqrt(unlist(lapply(full.metab.list(),length)))[cluster.settings()$`metCluster-cutoff`*10-1]})
# visualization reactives
full.cluster<-reactive({
  d<-as.dist(1-abs(x.dat()[['m']]))
  d.long<-melt(as.matrix(d))
  setDT(d.long)
  metab.col<-table(unlist(full.metab.list()))
  select.metab<-full.metab.list()[[cluster.settings()$`metCluster-cutoff`*10-1]]
  validate(need(length(select.metab)>=2,'Fewer than two metabolites meet this cutoff; select a lower cutoff.'))
  d<-as.dist(1-abs(x.dat()[['m']][select.metab,select.metab]));h<-hclust(d,method = 'average')
  par(mar=c(rep(5-(25-dend.param())/20,4)),xpd=T,cex=1.65-dend.param()/20);plot(as.phylo(h), type = "fan",tip.color=colorRampPalette(brewer.pal(11,'Spectral')[4:1])(8)[metab.col[select.metab]])
  title(main=paste(correlation_method_label(cluster.settings()$`metCluster-method`),'correlation'))
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
  sel.metab<-intersect(with(r.sub(),unique(c(Var1,Var2))),colnames(run.data[[cluster.settings()$`metCluster-matrix`]][['robustCZ']]))
  if(length(sel.metab)>1){
    sel.dat<-run.data[[cluster.settings()$`metCluster-matrix`]][['robustCZ']][,sel.metab]
    sel.sam<-run.data[[cluster.settings()$`metCluster-matrix`]][['sample.info']][centroid=='yes']
    rownames(sel.dat)<-with(sel.sam,paste0(Subject,' (',Dx,')'))
    draw(Heatmap(sel.dat, name = "robust Z score",clustering_method_rows = 'ward.D2',clustering_method_columns = 'ward.D2',
                 show_row_names = cluster.settings()$`metCluster-matrix`=='fibroblast', row_names_gp = gpar(fontsize=10),
                 heatmap_legend_param = list(legend_direction = "horizontal")),heatmap_legend_side = "bottom",padding = unit(c(10, 2, 2, 15), "mm"))
  }
})


observeEvent(input$`metCluster-submit`,{
  metCluster.applied(list(
    `metCluster-matrix` = input$`metCluster-matrix`,
    `metCluster-method` = correlation_method(input$`metCluster-method`),
    `metCluster-all` = input$`metCluster-all`,
    `metCluster-metabolite` = input$`metCluster-metabolite`,
    `metCluster-cutoff` = input$`metCluster-cutoff`))
  output$`metCluster-hint`<-NULL
  switch(match(input$`metCluster-all`,c('all','one')),
         {           
           output$`metCluster-plot`<-renderPlot(isolate(full.cluster()),height = dend.param()*20+950,width=dend.param()*20+950)
           shinyjs::hide('metCluster-iHM');shinyjs::show('metCluster-plot')
         },
         {
           if(nrow(r.sub())==0){
             output$`metCluster-plot` <- NULL
             shinyjs::hide('metCluster-plot')
             shinyjs::hide('metCluster-iHM')
             output$`metCluster-hint`<-renderText("No correlated metabolite at this cut-off, please select a lower cut-off value and try again.")
           }else{
             if(input$`metCluster-matrix`=='plasma'){
               isolate({
                 # Use the mounted widget; its browser controls are already ready.
                 # Draw the submitted heatmap on a temporary device, then hand the
                 # complete object to the interactive renderer.
                 heatmap <- local({
                   grDevices::pdf(file = NULL)
                   on.exit(grDevices::dev.off())
                   sub.cluster()
                 })
                 makeInteractiveComplexHeatmap(input, output, session,
                                               ht_list = heatmap, heatmap_id = 'metClusterPlasma')
                 shinyjs::hide('metCluster-plot');shinyjs::show('metCluster-iHM')
               })
             }else{
               output$`metCluster-plot`<-renderPlot(isolate(sub.cluster()),height = 1000,width=nrow(r.sub())*15+300) 
               shinyjs::hide('metCluster-iHM');shinyjs::show('metCluster-plot')
             }
           }
         })
})

