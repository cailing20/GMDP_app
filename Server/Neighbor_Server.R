# by default, load the merged data
output$`neighbor-table`<-DT::renderDataTable(DT::datatable({initial.eu.d},options = list(pageLength=15),filter='top',rownames = F))
dat.dist<-reactive({
  mData<-run.data[[input$`neighbor-matrix`]][['mData']]
  sample.info<-run.data[[input$`neighbor-matrix`]][['sample.info']]
  sample.info[,status:=Dx];sample.info[!Dx%in%c('unknown','unaffected'),status:='diagnosed']
  ind<-with(sample.info,which(status%in%input$`neighbor-dx`))
  mData<-mData[ind,];sample.info<-sample.info[ind]
  if(input$`neighbor-centroid`=='yes'){
    mData<-mData[with(sample.info,which(centroid=='yes')),]
    sample.info<-sample.info[centroid=='yes']
  }
  if(input$`neighbor-dimension`=='metabolite') mData<-t(mData)
  if(input$`neighbor-distance`=='euclidean'){
    dat.dist<-reshape2::melt(as.matrix(dist(mData)))
    # dat.dist<-reshape2::melt(as.matrix(dist(prcomp(mData)$x[,1:10])))
    if(input$`neighbor-dimension`=='sample'){
      dat.dist<-data.table(sample.info[dat.dist[,'Var1'],c('ID','Dx')],sample.info[dat.dist[,'Var2'],c('ID','Dx')],round(dat.dist[,'value'],2))[with(dat.dist,Var1!=Var2)]
      setnames(dat.dist,c('sample_ID','sample_Dx','neighbor_ID','neighbor_Dx','distance'))
    }else{
      setDT(dat.dist);setnames(dat.dist,c('metabolite','metabolite_neighbor','distance'));dat.dist[,distance:=round(distance,2)];dat.dist<-dat.dist[metabolite!=metabolite_neighbor]
    }
  }else{
    mData<-t(mData)
    dat.cor<-rcorr(mData)
    dat.dist<-reshape2::melt(1-abs(dat.cor$r))
    cor.stat<-cbind(round(reshape2::melt(dat.cor$r)[,3],2),signif(reshape2::melt(dat.cor$P)[,3],2),reshape2::melt(dat.cor$n)[,3])
    if(input$`neighbor-dimension`=='sample'){
      dat.dist<-data.table(sample.info[dat.dist[,'Var1'],c('ID','Dx')],sample.info[dat.dist[,'Var2'],c('ID','Dx')],round(dat.dist[,'value'],2),cor.stat)[with(dat.dist,Var1!=Var2)]
      setnames(dat.dist,c('sample_ID','sample_Dx','neighbor_ID','neighbor_Dx','distance','r','pv','n'))
    }else{
      dat.dist<-data.table(dat.dist,cor.stat)[with(dat.dist,Var1!=Var2)]
      setDT(dat.dist);setnames(dat.dist,c('metabolite','metabolite_neighbor','distance','r','pv','n'));dat.dist[,distance:=round(distance,2)]
    }
  }
  dat.dist
})
observeEvent(input$`neighbor-submit`,{
  if(is.null(input$`neighbor-dx`)) showNotification("Please select at least one type of samples to include") else  output$`neighbor-table`<-DT::renderDataTable(DT::datatable({isolate(dat.dist())},options = list(pageLength=15),filter='top',rownames = F))
})

