fluidPage(fluidRow(
  useShinyjs(),
  column(2,                            
         selectInput(inputId = 'DxMet-matrix',label = "Specify matrix",choices = c('plasma','fibroblast')),
         selectInput(inputId = 'DxMet-Dx',label = "Specify disease",choices = sort(setdiff(run.data$plasma$sample.info$Dx,c('unknown','unaffected'))),multiple = T),
         selectInput(inputId = 'DxMet-centroid',label = 'Exclude replicates',choices = c('yes','no'),selected = 'yes'),
         selectInput(inputId = 'DxMet-other',label = 'Select reference',choices = c('unaffected only'='unaffected','unaffected and other diagnosed'='all_other'),selected = 'unaffected'),
         selectInput(inputId = 'DxMet-representation',label = 'What to show', choices = c('Top & bottom 25','Statistically significant results')),
         actionButton(inputId = 'DxMet-submit',label = 'Submit')),
  column(6,useShinyjs(),hidden(div(id = 'DxMet-PlotDiv', shinycssloaders::withSpinner(plotOutput("DxMet-plot"),color = '#e95420')))),
  column(4,plotlyOutput("DxMet-volcano",height = "300px"),
         dataTableOutput("DxMet-stats"),
         uiOutput("DxMet-DlBtn"))
))
