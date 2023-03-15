# by default, load the merged data
sample.info<-reactive({fread(paste0('data/',input$`dataset-run`,'/',input$`dataset-run`,'_sample_info.csv'))[,-1]})
metabolite.info<-reactive({
  m.info<-fread(paste0('data/',input$`dataset-run`,'/',input$`dataset-run`,'_metabolite_info.csv'))
  if('mode'%in%colnames(m.info)){
    m.info[mode%in%switch(match(input$`dataset-mode`,c('positive','negative','combined')),'pos','neg',c('pos','neg')),-1]
  }else m.info
  })
zScore<-reactive({tmp<-as.matrix(fread(paste0('data/',input$`dataset-run`,'/',input$`dataset-run`,'_',input$`dataset-mode`,'_robustZ.csv'))[,-1]);tmp[is.infinite(tmp)]<-NA;tmp})
outlier.df<-reactive({fread(paste0('data/',input$`dataset-run`,'/',input$`dataset-run`,'_',input$`dataset-mode`,'_outlier.csv'))})
mData<-reactive({tmp<-as.matrix(fread(paste0('data/',input$`dataset-run`,'/',input$`dataset-run`,'_processed_',input$`dataset-mode`,'.csv'))[,-1]);class(tmp)<-'numeric';tmp[is.infinite(tmp)]<-NA;tmp})
observeEvent(input$`dataset-run`,{
  if(grepl('fibroblast',input$`dataset-run`)){
    updateRadioButtons(session,inputId = 'dataset-mode',label = "Choose mode:",choices=c('positive','negative','combined'),selected='combined')
    shinyjs::hide('dataset-mode')
  }else{
    shinyjs::show('dataset-mode')
  }
})
tree.list<-reactive({
  family<-sapply(run.data$sample.info[['ID']],FUN=function(x) paste(unlist(strsplit(unlist(strsplit(x,split = '_',fixed = T))[1],split = '-',fixed = T))[1:3],collapse = '-'))
  by.cohort.list<-lapply(setdiff(unique(run.data$sample.info[['Cohort']]),NA),FUN=function(x) unique(family[run.data$sample.info[['Cohort']]==x]))
  by.cohort.list<-structure(lapply(by.cohort.list,FUN=function(x){
    # x is family
    x<-x[!is.na(x)];
    structure(lapply(sort(unique(x)),function(y){
      structure(lapply(sort(unique(run.data$sample.info$Subject[family==y])),FUN=function(z){
        structure(as.list(sort(run.data$sample.info$ID[which(run.data$sample.info$Subject==z)])),names=sort(run.data$sample.info$ID[which(run.data$sample.info$Subject==z)]))
      }),names=sort(unique(run.data$sample.info$Subject[family==y])))}),
      names=sort(unique(x)))}),
    names=setdiff(unique(run.data$sample.info$Cohort),NA))
  Dx<-sort(setdiff(unique(run.data$sample.info$Dx),NA));Dx<-c(intersect(c('control','PhaseB_fam','PhaseC_fam'),Dx),setdiff(Dx,c('control','PhaseB_fam','PhaseC_fam')))
  by.Dx.list<-lapply(Dx,FUN=function(x) unique(run.data$sample.info$Subject[run.data$sample.info$Dx==x]))
  by.Dx.list<-structure(lapply(by.Dx.list,FUN=function(x){
    x<-sort(x[!is.na(x)]);
    structure(lapply(x,function(y){
      run.data$sample.info$ID[which(run.data$sample.info$Subject==y)]
      structure(as.list(run.data$sample.info$ID[which(run.data$sample.info$Subject==y)]),names=run.data$sample.info$ID[which(run.data$sample.info$Subject==y)])
    }),names=x)
  }),names=Dx)
  list('Diagnosis'=by.Dx.list,'Cohort'=by.cohort.list)
})


observeEvent(input$`dataset-update`,{
  run.data$sample.info<-sample.info()
  run.data$metabolite.info<-metabolite.info()
  run.data$robustZ<-zScore()
  run.data$mData<-mData()
  run.data$outlier.df<-outlier.df()
  
  output$`sample-info`<-DT::renderDataTable(DT::datatable({sample.info()[,c('Subject','Dx', 'Gender','age'),with=F]},options = list(pageLength=15),filter='top',rownames = F))
  output$`metabolite-info`<-DT::renderDataTable(DT::datatable({metabolite.info()[,c('mode','Compound Name',grep('SD|diff|between',colnames(metabolite.info()),value=T)),with=F]},options = list(pageLength=15),filter='top',rownames = F))
  
  updateCheckboxGroupInput(session,inputId = "outlier-var", choices = colnames(outlier.df()), selected = c('ID','Dx','metabolite','robustZ'))
  updateCheckboxGroupInput(session,inputId = "sample-var", choices = colnames(sample.info()), selected = c('Subject','Dx', 'Gender','age'))
  updateCheckboxGroupInput(session,inputId = "metabolite-var", choices = setdiff(colnames(metabolite.info()),'V1'),
                           selected = c('mode','Compound Name',grep('SD|diff|between',colnames(metabolite.info()),value=T)))
  updateSelectizeInput(session,inputId = "scatter-x-metabolite",choices = colnames(run.data$mData))
  updateSelectizeInput(session,inputId = "scatter-y-metabolite",choices = colnames(run.data$mData))
  updateSelectizeInput(session,inputId = "zscore-metabolite",choices = colnames(mData()))
  updateSelectizeInput(session,inputId = "zscore-sample",choices = sample.info()[['ID']])
})