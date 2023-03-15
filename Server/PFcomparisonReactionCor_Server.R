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
  cpds.m<-rn.df.sub()[reaction==input$`PFcompareReactionCor-reaction` & covered]
  p.dat<-data.table(Dx=run.data[['plasma']][['sample.info']][['Dx']],run.data[['plasma']][['mData']])
  f.dat<-data.table(Dx=run.data[['fibroblast']][['sample.info']][['Dx']],run.data[['fibroblast']][['mData']])
  if(input$`PFcompareReactionCor-samples`=='Shared subjects'){
    p.dat<-p.dat[joint.subject[['p.ind']],];f.dat<-f.dat[joint.subject[['f.ind']],]
  }else{
    p.dat<-p.dat[with(run.data[['plasma']][['sample.info']],which(centroid=='yes')),]
    f.dat<-f.dat[with(run.data[['fibroblast']][['sample.info']],which(centroid=='yes')),]
  }
  g.list<-list()
  for(i in 1:nrow(cpds.m)){
    cpds<-with(cpds.m[i],c(metab1,metab2))
    if(any(!cpds%in%colnames(p.dat))) p.dat[,setdiff(cpds,colnames(p.dat)):=NA]
    if(any(!cpds%in%colnames(f.dat))) f.dat[,setdiff(cpds,colnames(f.dat)):=NA]
    df<-rbind(data.table(source='plasma',p.dat[,c(cpds,'Dx'),with=F]),data.table(source='fibroblast',f.dat[,c(cpds,'Dx'),with=F]));colnames(df)[2:3]<-c('x','y')
    df[,status:=ifelse(Dx%in%c('unknown','unaffected'),Dx,'diagnosed')]
    if(input$`PFcompareReactionCor-samples`=='Shared subjects'){
      df[,r:=tryCatch({paste0('r = ',round(cor.test(x,y)$estimate,3),ifelse(cor.test(x,y)$p.value<.05,'*',''))},error=function(e) ""),by=.(source)]
      g<-ggplot(df,aes(x=x,y=y))+geom_point(alpha=.6,aes(color=status))+geom_text(df[!duplicated(paste(source))],mapping = aes(x=-Inf,y=Inf,label=r),hjust=-0.1,vjust=c(1.2))
    }else{
      df[,r:=tryCatch({paste0('r = ',round(cor.test(x,y)$estimate,3),ifelse(cor.test(x,y)$p.value<.05,'*',''))},error=function(e) ""),by=.(source,status)]
      df.stat<-df[!duplicated(paste(source,status))][order(status)][!is.na(x)][!is.na(y)];df.stat[source=='plasma',r:=paste0(r,' ')];df.stat[,r:=factor(r,levels=r)]
      g<-ggplot(df,aes(x=x,y=y,color=status))+geom_point(alpha=.6)+geom_text(df.stat,mapping = aes(x=-Inf,y=Inf,label=r),hjust=-0.1,vjust=rep(c(1.2,2.4,3.6),each=nrow(df.stat)/3))
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

observeEvent(input$`PFcompareReactionCor-submit`,{
  output$`PFcompareReactionCor-reactionPlot`<-renderPlot(isolate(if(length(plot.reaction())==1) plot.reaction()[[1]] else wrap_plots(plot.reaction(),ncol=1)),width=700,height = 200+250*length(plot.reaction()))
  output$`PFcompareReactionCor-reactionTable`<-renderDataTable(isolate(DT::datatable(table.reaction(),rownames = F,caption = 'Availability of reaction-related metabolites',options = list(dom = 't'))))
  if(input$`PFcompareReactionCor-reaction`%in%reaction.ref$reaction){
    output$`PFcompareReactionCor-reactionRefName`<-renderText(reaction.ref[reaction==input$`PFcompareReactionCor-reaction`][['reaction.name']])
    output$`PFcompareReactionCor-reactionRefMformula`<-renderText(reaction.ref[reaction==input$`PFcompareReactionCor-reaction`][['reaction.metabolite.formula']])
    output$`PFcompareReactionCor-reactionRefCformula`<-renderText(reaction.ref[reaction==input$`PFcompareReactionCor-reaction`][['reaction.cpd.formula']])
  }else{
    output$`PFcompareReactionCor-reactionRefName`<-NULL;output$`PFcompareReactionCor-reactionRefMformula`<-NULL;output$`PFcompareReactionCor-reactionRefCformula`<-NULL
  }
  # the following works on BioHPC but not commercial shiny server
  # output$`PFcompareReactionCor-reactionKEGG`<-renderText({isolate(paste0('<img src="http://rest.kegg.jp/get/',input$`PFcompareReactionCor-reaction`,'/image">'))})
  # output$`PFcompareReactionCor-reactionKEGG` <- renderUI({tags$img(src = isolate(paste0('http://rest.kegg.jp/get/',input$`PFcompareReactionCor-reaction`,'/image')))})
  output$`PFcompareReactionCor-reactionKEGG` <- renderUI({tags$img(src = isolate(paste0(input$`PFcompareReactionCor-reaction`,'.gif')),width=700)})
})