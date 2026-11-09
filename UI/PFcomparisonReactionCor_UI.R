fluidPage(
  fluidRow(
    column(4,
           correlation_method_input("PFcompareReactionCor-method"),
           h4('Reaction-specific pathway correlation(s)'),
           useShinyjs(),
           radioButtons(inputId = 'PFcompareReactionCor-samples',label = "Samples to include",choices = c('Shared subjects','All subjects'),inline = T),
           radioButtons(inputId = 'PFcompareReactionCor-pathwayCpd',label = "Select reaction by",choices = c('pathway','compound'),inline = T),
           selectInput(inputId = 'PFcompareReactionCor-pathway',label = "Select pathway",choices = c(setdiff(unique(rn.df[which(covered)]$pathway),'other'),'other'),selected = 'Arginine biosynthesis'),
           selectInput(inputId = 'PFcompareReactionCor-cpd',label = "Select compound",choices = unique(with(rn.df[which(covered)],c(metab1,metab2))),selected = 'Ornithine'),
           radioButtons(inputId = 'PFcompareReactionCor-enzymeGene',label = "Specify reaction by",choices = c('Enzyme name'='enzyme','Gene name'='gene'),inline = T),
           selectInput(inputId = 'PFcompareReactionCor-reaction',label = "Select reaction",choices = NULL),
           textOutput('PFcompareReactionCor-reactionRefName'),textOutput('PFcompareReactionCor-reactionRefMformula'),textOutput('PFcompareReactionCor-reactionRefCformula'),
           actionButton(inputId = 'PFcompareReactionCor-submit',label = 'Submit'),
           br(),
           dataTableOutput(outputId = 'PFcompareReactionCor-reactionTable')),
    column(8,uiOutput("PFcompareReactionCor-reactionKEGG"),#htmlOutput('PFcompareReactionCor-reactionKEGG',width = "400px"),
           plotOutput('PFcompareReactionCor-reactionPlot'))
  )
)