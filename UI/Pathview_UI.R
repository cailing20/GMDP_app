fluidPage(
  useShinyjs(),
  fluidRow(
    column(3,radioButtons(inputId = 'pathview-matrix',label = 'Select matrix',choices = c('plasma','fibroblast'),selected = 'plasma',inline = T)),  
    column(7,radioButtons(inputId = 'pathview-measure',label = 'Select input',choices = c('Dx - unaffected group comparison log2FC'='grp','Individual sample robust Z-score'='sample'),selected = 'grp',inline = T)),
    column(2,actionButton(inputId = 'pathview-specify',label = "Confirm selection"))
  ),
  hidden(div(id = 'pathview-Step2Div',   
             hr(),
             fluidRow(
               column(3,selectInput(inputId = "pathview-sampleDx",label='Select Dx',choices = sort(setdiff(run.data$plasma$sample.info$Dx,c('unknown','unaffected'))))),
               column(2,selectInput(inputId = "pathview-sample",label='Select sample',choices = run.data$plasma$sample.info$ID)),
               column(4,selectInput(inputId = "pathview-pathway",label='Select pathway',
                                    choices = c('Metabolic pathway panorama'='hsa01100',m.names[which(m.avail$plasma)][order(names(m.names[which(m.avail$plasma)]))]))),
               column(2,materialSwitch(inputId = "pathview-zoom", label = "Fix image width")),
               column(1,actionButton(inputId = 'pathview-submit',label = "Submit"))
             ))
  ),
  fluidRow(hidden(div(id = 'pathview-PlotDiv', shinycssloaders::withSpinner(plotOutput("pathview-plot"),color = '#e95420'))))
)