

pathway.corrplot<-reactive({
  method <- correlation_method(input$`PFcomparePathwayCor-method`)
  p.m<-intersect(pw.list[[input$`PFcomparePathwayCor-pathway`]],run.data[['plasma']][['metabolite.info']][['metabolite']])
  f.m<-intersect(pw.list[[input$`PFcomparePathwayCor-pathway`]],run.data[['fibroblast']][['metabolite.info']][['metabolite']])
  metabolites <- unname(pw.list[[input$`PFcomparePathwayCor-pathway`]])
  p.dat <- matrix(NA_real_, nrow(run.data$plasma$mData), length(metabolites), dimnames = list(NULL, metabolites))
  f.dat <- matrix(NA_real_, nrow(run.data$fibroblast$mData), length(metabolites), dimnames = list(NULL, metabolites))
  if (length(p.m)) p.dat[, p.m] <- run.data$plasma$mData[, p.m, drop = FALSE]
  if (length(f.m)) f.dat[, f.m] <- run.data$fibroblast$mData[, f.m, drop = FALSE]
  if(input$`PFcomparePathwayCor-samples`=='Shared subjects'){
    p.dat<-p.dat[joint.subject[['p.ind']],,drop=FALSE];f.dat<-f.dat[joint.subject[['f.ind']],,drop=FALSE]
  }else{
    p.dat<-p.dat[with(run.data[['plasma']][['sample.info']],which(centroid=='yes')),,drop=FALSE]
    f.dat<-f.dat[with(run.data[['fibroblast']][['sample.info']],which(centroid=='yes')),,drop=FALSE]
  }
  p.cor <- correlation_matrix_stats(p.dat, method); f.cor <- correlation_matrix_stats(f.dat, method)
  cor.df<-rbind(data.table(source='plasma',reshape2::melt(p.cor$r)),data.table(source='fibroblast',reshape2::melt(f.cor$r)))
  cor.df[Var1==Var2,value:=NA]
  if(input$`PFcomparePathwayCor-show`=='statistically significant') cor.df[which(c(reshape2::melt(p.cor$P)[,3],reshape2::melt(f.cor$P)[,3])>.05),value:=NA]
  make.ind <- function(x, y) {
    reference <- pw.list[[input$`PFcomparePathwayCor-pathway`]]
    map_names <- function(z) {
      z <- as.character(z)
      matched <- match(z, names(reference))
      z[!is.na(matched)] <- unname(reference[matched[!is.na(matched)]])
      z
    }
    x <- map_names(x); y <- map_names(y)
    vapply(seq_along(x), function(i) paste(sort(c(x[i], y[i])), collapse = '_'), character(1))
  }
  if(input$`PFcomparePathwayCor-show`=='linked by reaction'){
    rn.df.sub<-rn.df[grep(input$`PFcomparePathwayCor-pathway`,pathway,fixed = T)][cpd1%in%names(pw.list[[input$`PFcomparePathwayCor-pathway`]]) & cpd2%in%names(pw.list[[input$`PFcomparePathwayCor-pathway`]])]
    rn.df.sub[,identifier:=make.ind(cpd1,cpd2)]
    cor.df[,identifier2:=make.ind(Var1,Var2)]
    cor.df[!identifier2%in%rn.df.sub$identifier,value:=NA]
  }
  ggplot(cor.df,aes(x=Var1,y=Var2,fill=value))+geom_tile()+facet_wrap(~source,ncol=1)+scale_fill_gradientn(colours = brewer.pal(5,'RdBu')[c(5,3,1)], limits=c(-1,1))+
    theme_minimal()+theme(title = element_text(size = 15),strip.text = element_text(size = 14),legend.title = element_text(size = 12),
                          axis.title = element_blank(),legend.position = 'top',axis.text.x = element_text(angle = 45,hjust = 1,size = 12),axis.text.y = element_text(size = 12))+
    ggtitle(input$`PFcomparePathwayCor-pathway`)+
    labs(fill=paste(correlation_method_label(method), "correlation coefficient"))
})
pathway.submitted <- reactiveVal(NULL)
observeEvent(input$`PFcomparePathwayCor-submit`, {
  pathway.submitted(pathway.corrplot())
})
output$`PFcomparePathwayCor-Plot` <- renderPlot({
  req(pathway.submitted())
}, height = 1000)
