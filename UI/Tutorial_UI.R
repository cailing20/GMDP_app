fluidPage(
  navlistPanel(
    widths = c(3, 9),id = "tutorial-tabs",
               tabPanel("All functionalities",uiOutput("tutorial-1"),value = 1),
               br(),
               "Data overview",
               tabPanel("Dashboard",uiOutput("tutorial-2"),value = 2),
               tabPanel("Metadata",uiOutput("tutorial-3"),value = 3),
               tabPanel("Download processed data",uiOutput("tutorial-4"),value = 4),
               br(),
               "Metabolic deviation",
               tabPanel("Z-score viewer",uiOutput("tutorial-5"),value = 5),
               tabPanel("Dx vs. reference comparison",uiOutput("tutorial-6"),value = 6),
               tabPanel("Pathway view",uiOutput("tutorial-7"),value = 7),
               tabPanel("Review all outliers",uiOutput("tutorial-8"),value = 8),
               br(),
               "Metabolic neighbors",
               tabPanel("Metabolite clustering",uiOutput("tutorial-9"),value = 9),
               tabPanel("Neighbor distance table",uiOutput("tutorial-10"),value = 10),
               br(),
               "Plasma-vs-fibroblast comparison",
               tabPanel("Plasma-fibroblast concordance",uiOutput("tutorial-11"),value = 11),
               tabPanel("Custom metabolite pairwise correlations",uiOutput("tutorial-12"),value = 12),
               tabPanel("Pathway-specific metabolite pairwise correlations",uiOutput("tutorial-13"),value = 13),
               tabPanel("Reaction-specific metabolite pairwise correlations",uiOutput("tutorial-14"),value = 14)
               
  )
  
)
  