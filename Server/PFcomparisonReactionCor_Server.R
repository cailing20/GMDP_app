observeEvent(input$`PFcompareReactionCor-pathwayCpd`,{
  if(input$`PFcompareReactionCor-pathwayCpd`=='pathway'){shinyjs::show('PFcompareReactionCor-pathway');shinyjs::hide('PFcompareReactionCor-cpd')}else{shinyjs::hide('PFcompareReactionCor-pathway');shinyjs::show('PFcompareReactionCor-cpd')}
})
rn.df.sub<-reactive({
  if(input$`PFcompareReactionCor-pathwayCpd`=='pathway'){
    rn.df.sub<-rn.df[pathway==input$`PFcompareReactionCor-pathway`&covered]
  }else{
    rn.df.sub<-rn.df[(metab1%in%input$`PFcompareReactionCor-cpd`|metab2%in%input$`PFcompareReactionCor-cpd`)&covered]
  }
  rn.df.sub
})
reaction.choices<-reactive(structure(rn.df.sub()[['reaction']],names=paste0(rn.df.sub()[[input$`PFcompareReactionCor-enzymeGene`]],' (',rn.df.sub()[['reaction']],')')))
observeEvent(input$`PFcompareReactionCor-pathwayCpd`,{
  updateSelectInput(session = session,inputId = 'PFcompareReactionCor-reaction',choices = reaction.choices()[order(reaction.choices())])
})
observeEvent(input$`PFcompareReactionCor-enzymeGene`,{
  updateSelectInput(session = session,inputId = 'PFcompareReactionCor-reaction',choices = reaction.choices()[order(reaction.choices())])
})
observeEvent(input$`PFcompareReactionCor-pathway`,{
  updateSelectInput(session = session,inputId = 'PFcompareReactionCor-reaction',choices = reaction.choices()[order(reaction.choices())])
})
observeEvent(input$`PFcompareReactionCor-cpd`,{
  updateSelectInput(session = session,inputId = 'PFcompareReactionCor-reaction',choices = reaction.choices()[order(reaction.choices())])
})

plot.reaction<-reactive({
  method <- correlation_method(input$`PFcompareReactionCor-method`)
  # Compound/pathway selection narrows the reaction choices, not the pairs
  # belonging to the selected reaction. Include each measured pair once.
  cpds.m<-unique(rn.df[reaction==input$`PFcompareReactionCor-reaction` & covered, .(metab1, metab2)])
  validate(need(nrow(cpds.m) > 0, 'No covered metabolite pair is available for this reaction.'))
  p.dat<-data.table(Dx=run.data[['plasma']][['sample.info']][['Dx']],run.data[['plasma']][['mData']])
  f.dat<-data.table(Dx=run.data[['fibroblast']][['sample.info']][['Dx']],run.data[['fibroblast']][['mData']])
  if(input$`PFcompareReactionCor-samples`=='Shared subjects'){
    p.dat<-p.dat[joint.subject[['p.ind']],];f.dat<-f.dat[joint.subject[['f.ind']],]
  }else{
    p.dat<-p.dat[with(run.data[['plasma']][['sample.info']],which(centroid=='yes')),]
    f.dat<-f.dat[with(run.data[['fibroblast']][['sample.info']],which(centroid=='yes')),]
  }
  g.list<-list()
  for(i in seq_len(nrow(cpds.m))){
    cpds<-with(cpds.m[i],c(metab1,metab2))
    if(any(!cpds%in%colnames(p.dat))) p.dat[,setdiff(cpds,colnames(p.dat)):=NA]
    if(any(!cpds%in%colnames(f.dat))) f.dat[,setdiff(cpds,colnames(f.dat)):=NA]
    df<-rbind(data.table(source='plasma',p.dat[,c(cpds,'Dx'),with=F]),data.table(source='fibroblast',f.dat[,c(cpds,'Dx'),with=F]));colnames(df)[2:3]<-c('x','y')
    df[,status:=ifelse(Dx%in%c('unknown','unaffected'),Dx,'diagnosed')]
    if(input$`PFcompareReactionCor-samples`=='Shared subjects'){
      df[, r := correlation_label(x, y, method, digits = 3), by = source]
      g<-ggplot(df,aes(x=x,y=y))+geom_point(alpha=.6,aes(color=status))+geom_text(df[!duplicated(paste(source))],mapping = aes(x=-Inf,y=Inf,label=r),hjust=-0.1,vjust=c(1.2))
    }else{
      df[, r := correlation_label(x, y, method, digits = 3), by = .(source, status)]
      df.stat <- unique(df[, .(source, status, r)])[order(source, status)]
      df.stat[, label_row := seq_len(.N), by = source]
      g<-ggplot(df,aes(x=x,y=y,color=status))+geom_point(alpha=.6)+geom_text(df.stat,mapping = aes(x=-Inf,y=Inf,label=r),hjust=-0.1,vjust=df.stat$label_row * 1.2)
    }
    g.list[[i]]<-g+facet_wrap(~source,scales = 'free')+xlab(cpds[1])+ylab(cpds[2])+
      theme_bw()+theme(axis.text = element_blank(),axis.ticks = element_blank(),strip.background = element_rect(fill = NA),legend.position = 'none')
  }
  g.list#+plot_annotation(title=enzyme.name)
})
table.reaction<-reactive({
  table.reaction<-with(rn.df[reaction==input$`PFcompareReactionCor-reaction`],data.table(KEGG=unique(c(cpd1,cpd2))))
  table.reaction[,metabolite:=KEGG.metabolite.ref[match(table.reaction$KEGG,KEGG.metabolite.ref$KEGG)][['metabolite']]]
  table.reaction[,plasma:=KEGG%in%run.data$plasma$metabolite.info$KEGG]
  table.reaction[,fibroblast:=KEGG%in%run.data$fibroblast$metabolite.info$KEGG]
  table.reaction[order(plasma|fibroblast,decreasing = T)]
})

reaction.submitted <- reactiveVal(NULL)
observeEvent(input$`PFcompareReactionCor-submit`,{
  selected_reaction <- input$`PFcompareReactionCor-reaction`
  plots <- plot.reaction()
  metabolites <- table.reaction()
  reaction.submitted(list(plots = plots, method = correlation_method(input$`PFcompareReactionCor-method`)))
  output$`PFcompareReactionCor-reactionPlot`<-renderPlot(if(length(plots)==1) plots[[1]] else wrap_plots(plots,ncol=1),width=700,height = 200+250*length(plots))
  output$`PFcompareReactionCor-reactionTable`<-renderDataTable(isolate(DT::datatable(metabolites,rownames = F,caption = 'Availability of reaction-related metabolites',options = list(dom = 't', scrollX = TRUE))))
  if(selected_reaction%in%reaction.ref$reaction){
    output$`PFcompareReactionCor-reactionRefName`<-renderText(reaction.ref[reaction==selected_reaction][['reaction.name']])
    output$`PFcompareReactionCor-reactionRefMformula`<-renderText(reaction.ref[reaction==selected_reaction][['reaction.metabolite.formula']])
    output$`PFcompareReactionCor-reactionRefCformula`<-renderText(reaction.ref[reaction==selected_reaction][['reaction.cpd.formula']])
  }else{
    output$`PFcompareReactionCor-reactionRefName`<-NULL;output$`PFcompareReactionCor-reactionRefMformula`<-NULL;output$`PFcompareReactionCor-reactionRefCformula`<-NULL
  }
  # the following works on BioHPC but not commercial shiny server
  # output$`PFcompareReactionCor-reactionKEGG`<-renderText({isolate(paste0('<img src="http://rest.kegg.jp/get/',input$`PFcompareReactionCor-reaction`,'/image">'))})
  # output$`PFcompareReactionCor-reactionKEGG` <- renderUI({tags$img(src = isolate(paste0('http://rest.kegg.jp/get/',input$`PFcompareReactionCor-reaction`,'/image')))})
  illustration <- paste0(selected_reaction, '.gif')
  illustration_available <- file.exists(file.path('www', illustration)) &&
    isTRUE(file.size(file.path('www', illustration)) > 0)
  output$`PFcompareReactionCor-reactionKEGG` <- renderUI({
    if (illustration_available) tags$img(src = illustration, width = 700,
                                       alt = paste('Reaction', selected_reaction))
    else tags$p(paste('Reaction illustration is unavailable in this local archive',
                      paste0('(', selected_reaction, ').')))
  })
})
