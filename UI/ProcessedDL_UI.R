fluidPage(
  fluidRow(
    shinyjs::useShinyjs(),  
    radioButtons(inputId = 'ProcDl-matrix',label = "Choose data matrix",choices = c('plasma','fibroblast'),inline = T),
    downloadButton("ProcDl-button",label = 'Download')
  )
)