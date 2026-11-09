
fluidPage(
  card(
    card_header("Choose data and replicate specifications"),
    card_body(fluidRow(
      column(4,radioButtons(inputId = 'zscore-matrix',label = 'Select matrix',choices = c('plasma','fibroblast'),selected = 'plasma',inline = T)),
      column(4,radioButtons(inputId = 'zscore-centroid',label = 'Exclude replicates',choices = c('yes','no'),selected = 'yes',inline = T)),
      column(4,actionButton(inputId = 'zscore-submit',label = "Update matrix and replicate specifications"))
    ))
  ),
  fluidRow(
    column(6,card(
      card_header('View by metabolite'),
      card_body(selectInput(inputId = "zscore-metabolite",label='Select metabolite',choices = sort(run.data$plasma$metabolite.info$metabolite),width = '90%'),
                plotlyOutput("zscore-metabolitePlot",height = "400px"),
                dataTableOutput("zscore-metaboliteTable"),
                downloadButton(outputId = 'zscore-MetDl', label = "Download"), fillable = FALSE)
    )),
    column(6,card(
      card_header('View by sample'),
      card_body(
        fluidRow(column(6,selectInput(inputId = "zscore-sampleDx",label='Select Dx',choices = c('all','unknown','unaffected',sort(setdiff(run.data$plasma$sample.info$Dx,c('unknown','unaffected')))))),
                 column(6,selectInput(inputId = "zscore-sample",label='Select sample',choices = run.data$plasma$sample.info$ID))),
        plotlyOutput("zscore-samplePlot",height = "400px"),
        dataTableOutput("zscore-sampleTable"),
        downloadButton(outputId = 'zscore-SamDl', label = "Download"), fillable = FALSE)
    ))
  )
)

