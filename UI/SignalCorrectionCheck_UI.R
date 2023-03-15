fluidPage(
  sidebarPanel(
    radioButtons(inputId = 'correctionCheck-run',label = 'choose run',choices = c('run 3'='r3','run 4'='r4')),
    radioButtons(inputId = 'correctionCheck-mode',label = 'choose mode',choices = c('positive'='pos','negative'='neg')),
    useShinyjs(),
    div(hidden(
      selectInput(inputId = "correctionCheck-metabolite",label='choose metabolite',choices = NULL)
    )),
    radioButtons(inputId = 'correctionCheck-color',label = 'colory by',choices = c('type'='type','batch'='batch')),
    radioButtons(inputId = 'correctionCheck-transformation',label = 'transformation',choices = c('log'='log','none'='none'))),
  mainPanel(title="check correction",plotlyOutput("correctionCheck-plot"))
)