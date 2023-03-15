fluidPage(
  column(5,
         fluidRow(
           hr(),
           radioButtons(inputId = 'gp-method',label="Method:",choices = c('t-test'='parametric','Mann Whitney U'='nonparametric'),inline = T),
           actionButton(inputId = 'gp-submit',label = "Submit")
         ),
         hr(),
         fluidRow(
           column(6,
                  h4('Specify group 1'),
                  radioButtons(inputId = 'gp1-select',label = "Select group 1 by:",choices = c('Diagnosis'='Diagnosis','Cohort'='Cohort')),
                  shinyTree("compare-group1", checkbox = TRUE)),
           column(6,
                  h4('Specify group 2'),
                  radioButtons(inputId = 'gp2-select',label = "Select group 2 by:",choices = c('Diagnosis'='Diagnosis','Cohort'='Cohort')),
                  shinyTree("compare-group2", checkbox = TRUE))
         )),
  column(4,
         h4('results'),
         plotlyOutput('volcano'),
         DT::dataTableOutput("gp-results")),
  column(3,
         selectInput(inputId = 'gp-metabolite',label = "choose metabolite",choices = NULL),
         actionButton(inputId = 'bp-submit',label = "Submit"),
         plotlyOutput('gp-boxplot'))
)