fluidPage(
  fluidRow(
    column(6,
           correlation_method_input("PFcompareCustomCor-method"),
           div(materialSwitch(inputId = "PFcompareCustomCor-hideDup", label = "Hide duplicated pairs",value = T),
               dataTableOutput('PFcompareCustomCor-Table'),style = "font-size: 85%; width: 100%"),
           h4('Custom pairwise correlation'),
           fluidRow(
             selectInput(inputId = 'PFcompareCustomCor-Mx',label = "Select metabolite x",choices = joint.cor$metabolite),
             selectInput(inputId = 'PFcompareCustomCor-My',label = "Select metabolite y",choices = joint.cor$metabolite),
           ),
           fluidRow(
             column(6,radioButtons(inputId = 'PFcompareCustomCor-OnlySharedSubj',label = "Include only shared subjects",choices = c('yes','no'),selected = 'yes',inline = T)),
             column(6,useShinyjs(),div(hidden(radioButtons(inputId = 'PFcompareCustomCor-Centroid',label = "Exclude replicates",choices = c('yes','no'),selected = 'yes',inline = T))))
           ),
           actionButton(inputId = 'PFcompareCustomCor-Submit',label = 'Submit')),
    column(6,
           fluidRow(column(6,plotlyOutput('PFcompareCustomCor-PlotPlasma',height = '345px')),column(6,plotlyOutput('PFcompareCustomCor-PlotFibroblast',height = '345px'))),
           fluidRow(column(4,plotlyOutput('PFcompareCustomCor-PlotPlasma.1',height = '225px')),column(4,plotlyOutput('PFcompareCustomCor-PlotPlasma.2',height = '225px')),column(4,plotlyOutput('PFcompareCustomCor-PlotPlasma.3',height = '225px'))),
           fluidRow(column(4,plotlyOutput('PFcompareCustomCor-PlotFibroblast.1',height = '225px')),column(4,plotlyOutput('PFcompareCustomCor-PlotFibroblast.2',height = '225px')),column(4,plotlyOutput('PFcompareCustomCor-PlotFibroblast.3',height = '225px'))))
    
  ),
)
