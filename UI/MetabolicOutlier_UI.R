fluidPage(
  fluidRow(
    column(2,                            
           selectInput(inputId = 'outlierMetabolic-matrix',label = 'Select matrix',choices = c('plasma','fibroblast'),selected = 'plasma'),
           selectInput(inputId = 'outlierMetabolic-centroid',label = 'Exclude replicates',choices = c('yes','no'),selected = 'yes'),
           checkboxGroupInput(inputId = 'outlierMetabolic-excludeSample',label = 'Exclude samples with Dx',choices = c('unaffected','unknown')),
           actionButton(inputId = 'outlierMetabolic-submit',label = "Submit")),
    column(10,mainPanel(DT::dataTableOutput("outlierMetabolic-info",width = '1000px')))
  )
)