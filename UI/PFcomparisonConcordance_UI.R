fluidPage(
  fluidRow(           
    card(card_header("Select input data for concordance assessment"),card_body(
      correlation_method_input("PFcomparisonConcordance-method"),
      fluidRow(column(9,radioButtons(inputId = 'PFcomparisonConcordance-stat',label = "Display correlation statistics in shared subjects and metabolites by",choices = c('untransformed data','robust z-score'),inline = T)),
               column(3,actionButton(inputId = 'PFcomparisonConcordance-statSubmit',label = 'Confirm correlation input')))
    ))),
  fluidRow(
    column(9,
           fluidRow(
             column(5,card(card_header(textOutput('PFcomparisonConcordance-metabolitesTitle')),
                           card_body(dataTableOutput("PFcomparisonConcordance-metabolites"),downloadButton("PFcomparisonConcordance-mStatDl",label = 'Download'), fillable = FALSE))),
             column(7,card(card_header(textOutput('PFcomparisonConcordance-subjectsTitle')),
                           card_body(dataTableOutput("PFcomparisonConcordance-subjects"),downloadButton("PFcomparisonConcordance-sStatDl",label = 'Download'), fillable = FALSE)))
           )),
    column(3,
           card(card_header('Scatter plots'),card_body(
             div(style = "display:flex; flex-wrap:wrap; gap:8px; align-items:flex-end;",
                 div(style = "flex:1 1 160px; min-width:0;",
                     selectInput(inputId = 'PFcomparisonConcordance-metabolite', label = "Select metabolite to plot", choices = joint.cor[['metabolite']], width = "100%")),
                 div(style = "margin-bottom:15px;",
                     actionButton(inputId = 'PFcomparisonConcordance-metaboliteSubmit', label = "Submit"))),
             div(style = "height:310px;",plotlyOutput('PFcomparisonConcordance-metabolitePlot', height = '300px')),
             div(style = "display:flex; flex-wrap:wrap; gap:8px; align-items:flex-end;",
                 div(style = "flex:1 1 160px; min-width:0;",
                     selectInput(inputId = 'PFcomparisonConcordance-sample', label = "Select subject to plot", choices = joint.subject[['Subject']], width = "100%")),
                 div(style = "margin-bottom:15px;",
                     actionButton(inputId = 'PFcomparisonConcordance-sampleSubmit', label = "Submit"))),
             div(style = "height:310px;",plotlyOutput('PFcomparisonConcordance-samplePlot', height = '300px')), fillable = FALSE)
           )),
    
  )
)