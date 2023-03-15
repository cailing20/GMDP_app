

pathway.corrplot<-reactive({
  p.m<-intersect(pw.list[[input$`PFcomparePathwayCor-pathway`]],run.data[['plasma']][['metabolite.info']][['metabolite']])
  f.m<-intersect(pw.list[[input$`PFcomparePathwayCor-pathway`]],run.data[['fibroblast']][['metabolite.info']][['metabolite']])
  p.dat<-as.data.table(run.data[['plasma']][['mData']][,p.m,drop=F])
  p.dat<-as.matrix(p.dat[,setdiff(pw.list[[input$`PFcomparePathwayCor-pathway`]],p.m):= NA][,pw.list[[input$`PFcomparePathwayCor-pathway`]],with=F])
  f.dat<-as.data.table(run.data[['fibroblast']][['mData']][,f.m,drop=F])
  f.dat<-as.matrix(f.dat[,setdiff(pw.list[[input$`PFcomparePathwayCor-pathway`]],f.m):= NA][,pw.list[[input$`PFcomparePathwayCor-pathway`]],with=F])
  if(input$`PFcomparePathwayCor-samples`=='Shared subjects'){
    p.dat<-p.dat[joint.subject[['p.ind']],];f.dat<-f.dat[joint.subject[['f.ind']],]
  }else{
    p.dat<-p.dat[with(run.data[['plasma']][['sample.info']],which(centroid=='yes')),]
    f.dat<-f.dat[with(run.data[['fibroblast']][['sample.info']],which(centroid=='yes')),]
  }
  p.cor<-rcorr(p.dat);f.cor<-rcorr(f.dat)
  cor.df<-rbind(data.table(source='plasma',reshape2::melt(p.cor$r)),data.table(source='fibroblast',reshape2::melt(f.cor$r)))
  cor.df[Var1==Var2,value:=NA]
  if(input$`PFcomparePathwayCor-show`=='statistically significant') cor.df[which(c(reshape2::melt(p.cor$P)[,3],reshape2::melt(f.cor$P)[,3])>.05),value:=NA]
  make.ind<-function(x,y) {x<-pw.list[[input$`PFcomparePathwayCor-pathway`]][x];y<-pw.list[[input$`PFcomparePathwayCor-pathway`]][y];sapply(1:length(x), function(i) paste(sort(c(x[i],y[i])),collapse = '_'))}
  if(input$`PFcomparePathwayCor-show`=='linked by reaction'){
    rn.df.sub<-rn.df[grep(input$`PFcomparePathwayCor-pathway`,pathway)][cpd1%in%names(pw.list[[input$`PFcomparePathwayCor-pathway`]]) & cpd2%in%names(pw.list[[input$`PFcomparePathwayCor-pathway`]])]
    rn.df.sub[,identifier:=make.ind(cpd1,cpd2)]
    cor.df[,identifier2:=make.ind(Var1,Var2)]
    cor.df[!identifier2%in%rn.df.sub$identifier,value:=NA]
  }
  ggplot(cor.df,aes(x=Var1,y=Var2,fill=value))+geom_tile()+facet_wrap(~source,ncol=1)+scale_fill_gradientn(colours = brewer.pal(5,'RdBu')[c(5,3,1)], limits=c(-1,1))+
    theme_minimal()+theme(axis.title = element_blank(),legend.position = 'top',axis.text.x = element_text(angle = 45,hjust = 1))+ggtitle(input$`PFcomparePathwayCor-pathway`)+
    labs(fill="Pearson correlation coefficient")
})
observeEvent(input$`PFcomparePathwayCor-submit`,{
  output$`PFcomparePathwayCor-Plot`<-renderPlot(isolate(pathway.corrplot()),height = 1000)
})

