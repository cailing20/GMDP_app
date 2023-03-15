fluidPage(fluidRow(
  radioButtons('dataset-run',label = "Choose the dataset that you want to work with in this session:",
               choices = structure(c("GMDP_run3","GMDP_collaborator_run4","GMDP_run4","collaborator_run4","GMDP_run34","GMDP_colalborator_run34",'fibroblasts','fibroblasts_median'),
                                   names=c("run 3 GMDP samples", "run 4 GMDP + collaborator samples","run 4 GMDP samples","run 4 collaborator samples",
                                           "run 3&4 merged GMDP samples","run 3&4 merged GMDP + collaborator samples",
                                           "fibroblasts cultured in DMEM (triplicates)","fibroblasts cultured in DMEM (collapsed to median)")),width = "100%",selected = 'GMDP_run34'),
  useShinyjs(),
  radioButtons('dataset-mode',label = "Choose mode:",choices=c('positive','negative','combined'),selected='combined'),
  actionButton('dataset-update',label = "Update Session")
))