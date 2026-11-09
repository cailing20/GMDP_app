fluidPage(
  fluidRow(
    column(4,
           correlation_method_input("PFcomparePathwayCor-method"),
           h4('Pathway-specific pairwise correlations'),
           selectInput(inputId = 'PFcomparePathwayCor-pathway',label = "Select pathway",choices = names(pw.list),selected = 'Arginine biosynthesis'),
           radioButtons(inputId = 'PFcomparePathwayCor-samples',label = "Samples to include",choices = c('Shared subjects','All subjects'),inline = T),
           radioButtons(inputId = 'PFcomparePathwayCor-show',label = "Correlation to display",choices = c("all","statistically significant","linked by reaction")),
           actionButton(inputId = 'PFcomparePathwayCor-submit',label = 'Plot correlation matrix')),
    column(8,plotOutput('PFcomparePathwayCor-Plot',width = "600px",height = "1500px"))
  )
)
