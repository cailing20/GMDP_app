fluidPage(fluidRow(
  column(3,                            
         selectInput(inputId = 'neighbor-matrix',label = "Specify matrix",choices = c('plasma','fibroblast')),
         selectInput(inputId = 'neighbor-dimension',label = "Specify dimension",choices = c('sample','metabolite')),
         selectInput(inputId = 'neighbor-centroid',label = 'Exclude replicates',choices = c('yes','no'),selected = 'yes'),
         checkboxGroupInput(inputId = 'neighbor-dx',label = 'Samples to include',choices = c('diagnosed','unknown','unaffected'),selected = c('diagnosed','unknown','unaffected')),
         radioButtons(inputId = 'neighbor-distance',label = 'Choose ranking distance',choices = c('Euclidean distance'='euclidean','Absolute correlation distance'='correlation'),selected = 'euclidean'),
         actionButton(inputId = 'neighbor-submit',label = 'Submit')),
  column(9,mainPanel(DT::dataTableOutput("neighbor-table",width = '1000px')))
))