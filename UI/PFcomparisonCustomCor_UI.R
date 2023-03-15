fluidPage(
  fluidRow(
    column(2,
           h4('Custom pairwise correlation'),
           selectInput(inputId = 'PFcompareCustomCor-Mx',label = "Select metabolite x",choices = joint.cor$metabolite),
           selectInput(inputId = 'PFcompareCustomCor-My',label = "Select metabolite y",choices = joint.cor$metabolite),
           radioButtons(inputId = 'PFcompareCustomCor-OnlySharedSubj',label = "Include only shared subjects",choices = c('yes','no'),selected = 'yes',inline = T),
           useShinyjs(),
           div(hidden(radioButtons(inputId = 'PFcompareCustomCor-Centroid',label = "Exclude replicates",choices = c('yes','no'),selected = 'yes',inline = T))),
           actionButton(inputId = 'PFcompareCustomCor-Submit',label = 'Submit')),
    column(10,
           fluidRow(column(6,div(style = "height:500px",plotlyOutput('PFcompareCustomCor-PlotPlasma'))),column(6,div(style = "height:500px",plotlyOutput('PFcompareCustomCor-PlotFibroblast')))))
           
  ),
  fluidRow(column(2,plotlyOutput('PFcompareCustomCor-PlotPlasma.1')),column(2,plotlyOutput('PFcompareCustomCor-PlotPlasma.2')),column(2,plotlyOutput('PFcompareCustomCor-PlotPlasma.3')),
           column(2,plotlyOutput('PFcompareCustomCor-PlotFibroblast.1')),column(2,plotlyOutput('PFcompareCustomCor-PlotFibroblast.2')),column(2,plotlyOutput('PFcompareCustomCor-PlotFibroblast.3')))
    
)