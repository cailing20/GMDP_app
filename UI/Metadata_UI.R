fluidPage(fluidRow(
  column(2,radioButtons(inputId = "metadata-matrixChoice", label = "Select matrix:",choices = c('plasma','fibroblast'),selected = 'plasma'),
         radioButtons(inputId = "metadata-infoChoice", label = "Select dimension:",choices = c('sample','metabolite'),selected = 'sample')),
  column(10,mainPanel(DT::dataTableOutput("metadata-info",width = '1150px'),
         downloadButton(outputId = 'metadata-tb', label = "Download")))
))

