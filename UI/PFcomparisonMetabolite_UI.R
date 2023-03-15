fluidPage(
  column(3,h4('Concordance between matrix'),selectInput(inputId = 'PFcompareM-metabolite',label = "Select metabolite to plot",choices = joint.cor[['metabolite']]),actionButton(inputId = 'PFcompareM-submit',label = "Submit"),plotlyOutput('PFcompareM-plot')),
  column(3,h4('Shared subjects'),dataTableOutput("PFcompareM-subjects")),
  column(6,h4('Shared metabolites'),dataTableOutput("PFcompareM-metabolites"))
)