fluidPage(
  fluidRow(
    column(2,  
           useShinyjs(),
           selectInput(inputId = 'metCluster-matrix',label = 'Select matrix',choices = c('plasma','fibroblast'),selected = 'plasma'),
           correlation_method_input('metCluster-method'),
           radioButtons(inputId = 'metCluster-all',label='Select metabolite(s)',choices=c('all metabolites'='all','specific metabolite'='one'),selected = 'all'),
           selectInput(inputId = "metCluster-metabolite",label='Specify center metabolite',choices = sort(run.data$plasma$metabolite.info$metabolite),width = '95%'),
           sliderInput(inputId = 'metCluster-cutoff',label = 'Select minimal correlation strength',min = 0.2,max = 0.9,value = 0.6,step = 0.1),
           actionButton(inputId = 'metCluster-submit',label = "Submit"),
           textOutput('metCluster-hint')),
    column(10,useShinyjs(),plotOutput("metCluster-plot"),
           # Mount the widget before document-ready scripts initialize its controls.
           # Loading it through renderUI can run those scripts before the DOM exists.
           hidden(div(id = 'metCluster-iHM',
             InteractiveComplexHeatmapOutput(
               heatmap_id = 'metClusterPlasma', title3 = '',
               height1 = 1000, height2 = 1000, width1 = 550, width2 = 550,
               output_ui = NULL, output_ui_float = TRUE)))
           )
  )
)