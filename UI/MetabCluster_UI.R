fluidPage(
  fluidRow(
    column(2,  
           useShinyjs(),
           selectInput(inputId = 'metCluster-matrix',label = 'Select matrix',choices = c('plasma','fibroblast'),selected = 'plasma'),
           radioButtons(inputId = 'metCluster-all',label='Select metabolite(s)',choices=c('all metabolites'='all','specific metabolite'='one'),selected = 'all'),
           selectInput(inputId = "metCluster-metabolite",label='Specify center metabolite',choices = sort(run.data$plasma$metabolite.info$metabolite),width = '95%'),
           sliderInput(inputId = 'metCluster-cutoff',label = 'Select minimal correlation strength',min = 0.2,max = 0.9,value = 0.6,step = 0.1),
           actionButton(inputId = 'metCluster-submit',label = "Submit"),
           textOutput('metCluster-hint')),
    column(10,useShinyjs(),plotOutput("metCluster-plot"),
           htmlOutput('metCluster-iHM')
           # InteractiveComplexHeatmapOutput(heatmap_id = 'metCluster-iHM',title3 = '',height1 = 1200,height2 = 1200,width1 = 600,width2 = 800,output_ui = NULL)
           )
  )
)