fluidPage(fluidRow(
  column(2,checkboxGroupInput(inputId = "metabolite-var", label = "Columns in table to show:",selected = c('mode','Compound Name','run3_replicate.SD','run4_Heparin.repSD','run4_EDTA.repSD','run4_plasma.diff.pv','between_run_r','between_run_pv'),
                              choices = c('mode','Compound Name','Formula','run3_Mass','run3_RT','run3_SN','run3_replicate.SD','run4_Mass','run4_RT','run4_SN','run4_Heparin.repSD','run4_EDTA.repSD','run4_plasma.diff.pv','between_run_r','between_run_pv'))),
  column(10,DT::dataTableOutput("metabolite-info"))
))