# preload
# Use the project library when dependencies were installed with scripts/setup_local.R.
if (dir.exists('.R-library')) .libPaths(c(normalizePath('.R-library'), .libPaths()))
# setwd('/project/CRI/DeBerardinis_lab/lcai/GMDP/metabolomics_manuscript_2022/app/v2/')
library(shiny)
library(data.table)
library(shinyjs)
library(stringr)
library(shinycssloaders)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(DT)
library(reshape2)
library(nlme)
library(Hmisc)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(ggrepel)
library(pathview)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(openxlsx)
library(ape)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
source(file.path('R', 'correlation_helpers.R'), local=TRUE)
load('data/input.RData')
source(file.path('R', 'pairwise_cache.R'), local=TRUE)
PFpw.spearman <- load_pairwise_spearman()

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "gmdp.css")),
  navbarPage(title = "GMDP database",
             collapsible = TRUE,
             # theme = shinytheme("united"),
             theme = bs_theme(bg = "#FFFFFF",fg = "#E95420",primary = "#E95420"),
             navbarMenu(title="Data overview",
                        tabPanel(title="Dashboard",
                                 source(file.path("UI","Dashboard_UI.R"), local=TRUE)$value),
                        tabPanel(title="Metadata",
                                 source(file.path("UI", "Metadata_UI.R"),  local = TRUE)$value),
                        tabPanel(title="Download processed data",
                                 source(file.path("UI", "ProcessedDL_UI.R"),  local = TRUE)$value)
             ),
             navbarMenu(title="Metabolic deviation",
                        tabPanel(title="Z-score viewer",
                                 source(file.path("UI","zScoreViewer_UI.R"), local=TRUE)$value),
                        tabPanel(title="Dx vs. reference comparison",
                                 source(file.path("UI","DxMet_UI.R"), local=TRUE)$value),
                        tabPanel(title="Pathway view",
                                 source(file.path("UI","Pathview_UI.R"), local=TRUE)$value),
                        tabPanel(title="Review all outliers",
                                 source(file.path("UI","MetabolicOutlier_UI.R"), local=TRUE)$value)
             ),
             navbarMenu(title = "Metabolic neighbors",
                        tabPanel(title="Metabolite clustering",
                                 source(file.path("UI","MetabCluster_UI.R"), local=TRUE)$value),
                        tabPanel(title="Neighbor distance table",
                                 source(file.path("UI","Neighbor_UI.R"), local=TRUE)$value)
             ),
             navbarMenu(title="Plasma-vs-fibroblast comparison",
                        tabPanel(title="Plasma-fibroblast concordance",
                                 source(file.path("UI","PFcomparisonConcordance_UI.R"), local=TRUE)$value),
                        tabPanel(title="Custom metabolite pairwise correlations",
                                 source(file.path("UI", "PFcomparisonCustomCor_UI.R"),  local = TRUE)$value),
                        tabPanel(title="Pathway-specific metabolite pairwise correlations",
                                 source(file.path("UI", "PFcomparisonPathwayCor_UI.R"),  local = TRUE)$value),
                        tabPanel(title="Reaction-specific metabolite pairwise correlations",
                                 source(file.path("UI", "PFcomparisonReactionCor_UI.R"),  local = TRUE)$value)
             ),
             tabPanel(title="Tutorials",
                      source(file.path("UI","Tutorial_UI.R"), local=TRUE)$value
             )
  ),
  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {color:#631600 !important;background-color: #FDEEE9 !important;}'),
                        type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #E95420;
                                                  border-top: 1px solid #a63811 ;
                                                  border-bottom: 1px solid #a63811 ;}

                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #E95420 }'
             )))
)
server <- function(input, output,session) {
  source(file.path("Server", "Metadata_Server.R"),  local = TRUE)$value
  source(file.path("Server", "ProcessedDL_Server.R"),  local = TRUE)$value
  source(file.path("Server", "zScoreViewer_Server.R"),  local = TRUE)$value
  source(file.path("Server", "Pathview_Server.R"),  local = TRUE)$value
  source(file.path("Server", "MetabolicOutlier_Server.R"),  local = TRUE)$value
  source(file.path("Server", "Neighbor_Server.R"),  local = TRUE)$value
  source(file.path("Server", "DxMet_Server.R"),  local = TRUE)$value
  source(file.path("Server", "MetabCluster_Server.R"),  local = TRUE)$value
  source(file.path("Server", "PFcomparisonConcordance_Server.R"),  local = TRUE)$value
  source(file.path("Server", "PFcomparisonCustomCor_Server.R"),  local = TRUE)$value
  source(file.path("Server", "PFcomparisonPathwayCor_Server.R"),  local = TRUE)$value
  source(file.path("Server", "PFcomparisonReactionCor_Server.R"),  local = TRUE)$value
  source(file.path("Server", "Tutorial_Server.R"),  local = TRUE)$value
}

shinyApp(server = server, ui = ui)
