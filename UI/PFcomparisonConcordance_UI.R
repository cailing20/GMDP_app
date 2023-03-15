fluidPage(
  fluidRow(           
    card(card_header("Select input data for concordance assessment by Pearson Correlations"),card_body(
      fluidRow(column(9,radioButtons(inputId = 'PFcomparisonConcordance-stat',label = "Display correlation statistics in shared subjects and metabolites by",choices = c('untransformed data','robust z-score'),inline = T)),
               column(3,actionButton(inputId = 'PFcomparisonConcordance-statSubmit',label = 'Confirm correlation input')))
    ))),
  fluidRow(
    column(9,
           fluidRow(
             column(5,card(card_header('Metabolite correlation by shared subjects'),
                           card_body(dataTableOutput("PFcomparisonConcordance-metabolites"),downloadButton("PFcomparisonConcordance-mStatDl",label = 'Download')))),
             column(7,card(card_header('Subject correlation by shared metabolites'),
                           card_body(dataTableOutput("PFcomparisonConcordance-subjects"),downloadButton("PFcomparisonConcordance-sStatDl",label = 'Download'))))
           )),
    column(3,
           card(card_header('Scatter plots'),card_body(
             fluidRow(column(8,selectInput(inputId = 'PFcomparisonConcordance-metabolite',label = "Select metabolite to plot",choices = joint.cor[['metabolite']])),
                      column(4,actionButton(inputId = 'PFcomparisonConcordance-metaboliteSubmit',label = "Submit"))),
             fluidRow(div(style = "height:310px;",plotlyOutput('PFcomparisonConcordance-metabolitePlot'))),
             fluidRow(column(8,selectInput(inputId = 'PFcomparisonConcordance-sample',label = "Select subject to plot",choices = joint.subject[['Subject']])),
                      column(4,actionButton(inputId = 'PFcomparisonConcordance-sampleSubmit',label = "Submit"))),
             fluidRow(div(style = "height:310px;",plotlyOutput('PFcomparisonConcordance-samplePlot'))))
           )),
    
  )
)