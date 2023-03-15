fluidPage(
  sidebarPanel(
    selectInput(inputId = "scatter-x-metabolite",label='choose metabolite for x:',choices = colnames(initial.mData)),
    selectInput(inputId = "scatter-y-metabolite",label='choose metabolite for y:',choices = colnames(initial.mData))
  ),
  mainPanel(plotlyOutput('scatter-plot'))
)