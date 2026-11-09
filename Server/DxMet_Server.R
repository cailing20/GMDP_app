shinyjs::hide('DxMet-statsDl')
observeEvent(input$`DxMet-matrix`,{
  updateSelectInput(session = session,inputId = 'DxMet-Dx',choices = sort(setdiff(run.data[[input$`DxMet-matrix`]][['sample.info']][['Dx']],c('unknown','unaffected'))))
})
calc.DxMet<-reactive({
  sample.info<-copy(run.data[[input$`DxMet-matrix`]][['sample.info']])
  z.dat<-run.data[[input$`DxMet-matrix`]][[ifelse(input$`DxMet-centroid`=='yes','robustCZ','robustZ')]]
  # The manuscript (Figure 4 legend) specifies all replicates for statistics.
  # The replicate selector only changes which robust Z-score points are shown.
  stat.sample.info <- copy(sample.info)
  m.dat<-run.data[[input$`DxMet-matrix`]][['mData']]
  if(input$`DxMet-centroid`=='yes') sample.info<-sample.info[centroid=='yes']
  sample.info[,diagnosis:=Dx]
  sample.info[!Dx%in%c('unaffected',input$`DxMet-Dx`),diagnosis:='other affected']
  sample.info[Dx%in%input$`DxMet-Dx`,diagnosis:='affected']
  if(input$`DxMet-other`=='unaffected') {z.dat<-z.dat[which(sample.info$diagnosis!='other affected'),];sample.info<-sample.info[diagnosis!='other affected']}
  # this is to calculate the statistical significance with data from the original (logged) scale
  stat.sample.info[,diagnosis:=ifelse(Dx%in%input$`DxMet-Dx`,'affected',ifelse(Dx=='unaffected','unaffected','other affected'))]
  if(input$`DxMet-other`=='unaffected') {
    keep <- stat.sample.info$diagnosis!='other affected'
    m.dat <- m.dat[keep,,drop=FALSE]
    stat.sample.info <- stat.sample.info[keep]
  }
  tmp.df<-data.table(stat.sample.info[,c('Subject','diagnosis'),with=F],m.dat)
  tmp.df[diagnosis%in%c('unaffected','other affected'),diagnosis:='other']
  tmp.df[,diagnosis:=factor(diagnosis,levels=c('other','affected'))]
  stat.df<-tmp.df[,lapply(.SD,function(x){
    tryCatch({fit<-lme(x ~ diagnosis,random = ~1|Subject,data=data.table(x,diagnosis,Subject)[!is.na(x)]);c(fit$coefficients$fixed[2],anova(fit)$`p-value`[2])},error=function(e) rep(NA,2))
  }),.SDcols=-(1:2)]
  stat.df<-data.table(colnames(stat.df),t(stat.df));setnames(stat.df,c('metabolite','log2FC','pval'));stat.df[,padj:=p.adjust(pval,'BH')][,metab.sig:=paste0(metabolite,ifelse(padj<.05,'*',''))]
  stat.df<-stat.df[order(pval)]
  stat.df2<-copy(stat.df)
  stat.df2[is.finite(pval) & pval<2.2e-16,pval:=2.2e-16]
  stat.df2[,`-log10(pv)`:=round(-log10(pval),2)][,log2FC:=round(log2FC,2)]
  g.volcano<-ggplot(stat.df2,aes(x=log2FC,y=`-log10(pv)`,label=metabolite))+geom_point(alpha=.5,col='#E95420')+
    theme_bw()+scale_y_continuous(expand = expand_scale(mult = c(0.05, 0)))+
    ggtitle('selected Dx - reference')+xlab('log2FC')+ylab('-log10 (p-value)')
  # Keep BH adjustment on the original P values, then apply the manuscript's
  # reporting threshold to the summary only. The volcano uses the capped copy.
  stat.df[,pval:=ifelse(pval<2.2e-16,0,signif(pval,2))][,padj:=ifelse(padj<2.2e-16,0,signif(padj,2))][,log2FC:=round(log2FC,2)]
  colnames(z.dat)<-stat.df[match(colnames(z.dat),metabolite)][['metab.sig']]
  z.df<-as.data.table(reshape2::melt(data.table(sample.info[,c('Dx','diagnosis','Subject')],z.dat)))[!is.na(value)]
  z.median<-z.df[diagnosis=='affected',list(value=median(value)),by=variable]
  ordered.metabolites<-as.character(z.median[order(value,decreasing = T)][['variable']])
  if(input$`DxMet-representation`=='Top & bottom 25'){
    ordered.metabolites<-c(ordered.metabolites[1:25],rev(ordered.metabolites)[25:1]) 
  }else{
    ordered.metabolites<-grep("\\*$",ordered.metabolites,value = T)
  }
  z.median<-z.median[variable%in%ordered.metabolites]
  z.df<-z.df[variable%in%ordered.metabolites]
  if(input$`DxMet-representation`=='Top & bottom 25') z.df[,rank:=ifelse(variable%in%ordered.metabolites[1:25],'increased top 25','decreased top 25')]
  z.df[,variable:=factor(variable,levels=ordered.metabolites)]
  if(input$`DxMet-other`!='unaffected') z.df[diagnosis=='other affected',Dx:='other affected']
  z.df[,Dx:=factor(Dx,levels=c('unaffected',(if(input$`DxMet-other`!='unaffected') 'other affected'),input$`DxMet-Dx`))]
  z.df[,sample:=Subject];z.df[!Dx%in%input$`DxMet-Dx`,sample:=' ']
  if(length(input$`DxMet-Dx`)>1) z.df[sample!=' ',sample:=paste0(sample,' (',Dx,')')]
  z.df[,sample:=factor(sample,levels = c(unlist(lapply(input$`DxMet-Dx`,function(x) sort(unique(z.df[Dx==x][['sample']])))),' '))]
  
  Dx.cols<-c(unlist(lapply(1:length(input$`DxMet-Dx`),function(i){
    subs<-as.character(sort(unique(z.df[Dx==input$`DxMet-Dx`[i]][['sample']])))
    structure(colorRampPalette(brewer.pal(9,c('YlOrRd','Purples','Blues','BuGn','RdPu')[i])[c(7,3)])(length(subs)),names=subs)
  })),' '='white')
  # please do not select more than 5 Dx
  g<-ggplot(z.df%>%arrange(Dx),aes(x=value,y=variable,color=Dx))+geom_point(alpha=.5,aes(shape=Dx,size=Dx,fill=sample))+theme_bw()+
    xlab('Robust z-score')+#ggtitle(paste(input$`DxMet-Dx`,collapse = " + "))+
    scale_shape_manual(values = structure(c(1,(if(input$`DxMet-other`!='unaffected') 1),rep(21,length(input$`DxMet-Dx`))),names=levels(z.df)))+
    scale_size_manual(values = structure(c(1,(if(input$`DxMet-other`!='unaffected') 1),rep(2,length(input$`DxMet-Dx`))),names=levels(z.df)))+
    scale_color_manual(values = structure(c('darkgray',(if(input$`DxMet-other`!='unaffected') 'lightblue'),rep('red',length(input$`DxMet-Dx`))),names=levels(z.df)))+
    scale_fill_manual(values = Dx.cols)+guides(fill = guide_legend(override.aes = list(shape = 21,size=3,alpha=.7,color='white')),color = guide_legend(override.aes = list(alpha=1,size=2)))+
    theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),strip.background = element_rect(fill=NA),legend.position = 'top',
          legend.title = element_blank(),legend.direction = 'vertical')
  if(input$`DxMet-representation`=='Top & bottom 25') g<-g+facet_grid(rank~.,space = 'free',scales = 'free_y')
  list(dotplot=g,volcano=g.volcano,stats=stat.df)
})
# Keep plots, tables and downloads tied to the last successful Submit.
DxMet.submitted <- reactiveVal(NULL)
output$`DxMet-DlBtn` <- renderUI({
  req(DxMet.submitted())
  downloadButton("DxMet-statsDl", label = 'Download')
})
output$`DxMet-plot` <- renderPlot({
  req(DxMet.submitted())$result$dotplot
}, width = 600, height = function() {
  submitted <- req(DxMet.submitted())
  400 + 8 * if (submitted$representation == 'Top & bottom 25') 50 else
    nrow(submitted$result$stats[grep("\\*$", metab.sig)])
})
output$`DxMet-volcano` <- renderPlotly({
  ggplotly(req(DxMet.submitted())$result$volcano)
})
output$`DxMet-stats` <- DT::renderDataTable({
  DT::datatable(req(DxMet.submitted())$result$stats[,c(1,2,4),with=FALSE],
                rownames=FALSE, options=list(pageLength=5))
})
output$`DxMet-statsDl` <- downloadHandler(
  filename = function() paste0(paste(req(DxMet.submitted())$dx,collapse=', '),' - reference.csv'),
  content = function(con) write.csv(req(DxMet.submitted())$result$stats[,1:4,with=FALSE],con,row.names=FALSE)
)
observeEvent(input$`DxMet-submit`, {
  if (is.null(input$`DxMet-Dx`)) {
    showNotification('Please select at least one Dx')
  } else if (length(input$`DxMet-Dx`) > 5) {
    showNotification('Please select no more than 5 Dx')
  } else {
    result <- calc.DxMet()
    DxMet.submitted(list(result=result, dx=input$`DxMet-Dx`,
                         representation=input$`DxMet-representation`))
    shinyjs::show('DxMet-PlotDiv')
    shinyjs::show('DxMet-statsDl')
  }
})
