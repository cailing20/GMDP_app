blue<-"#a6cee3"
pink<-brewer.pal(9,'Set3')[8]
brks <- seq(-1,1,length.out=100)
clrs <- colorRampPalette(c(blue,'white',pink))(101)
PFcompareCustomCor.method <- reactive({correlation_method(input$`PFcompareCustomCor-method`)})
PFcompareCustomCor.spearman <- reactive({PFpw.spearman})
# The overall table is independent of the custom scatter-plot Submit button.
PFcompareCustomCor.tableRequest <- reactive({
  list(method = PFcompareCustomCor.method(),
       hide_duplicates = if (is.null(input$`PFcompareCustomCor-hideDup`)) TRUE else isTRUE(input$`PFcompareCustomCor-hideDup`))
})
PFcor.df <- reactive({
  request <- PFcompareCustomCor.tableRequest()
  method <- request$method
  sketch <- htmltools::withTags(table(
    class = 'display',
    thead(tr(th(rowspan = 2, colspan = 2, 'Metabolite'),
             th(colspan = 4, paste0('Pairwise correlation (', correlation_method_label(method), ')'))),
          tr(th(colspan = 2, 'all samples'), th(colspan = 2, 'shared samples')),
          tr(lapply(c('1', '2', rep(c('plasma', 'fibroblast'), times = 2)), th)))
  ))
  dat <- if (method == 'pearson') PFpw else PFcompareCustomCor.spearman()
  widget <- DT::datatable(dat[part %in% c('A', if (!request$hide_duplicates) 'B')][order(all.plasma.r, decreasing = TRUE)][, -1, with = FALSE],
                rownames = FALSE, filter = 'top', container = sketch,
                options = list(pageLength = 5, scrollX = TRUE, headerCallback = JS("function( thead, data, start, end, display ) {$(thead).closest('thead').find('th').css('border', '1px solid #f7c0ad');}"))) %>%
    formatStyle(3:6, 3:6, backgroundColor = styleInterval(brks, clrs))
  # scrollX duplicates the header. Refresh the visible copy from current
  # settings, preserving the hidden sizing header used by DataTables.
  widget$x$correlationMethod <- correlation_method_label(method)
  htmlwidgets::onRender(widget, "function(el, x) {
    function updateMethodHeading() {
      var headers = el.querySelectorAll('.dataTables_scrollHead thead th[colspan=\"4\"]');
      if (!headers.length) headers = el.querySelectorAll('thead th[colspan=\"4\"]');
      headers.forEach(function(th) {
        th.textContent = 'Pairwise correlation (' + x.correlationMethod + ')';
      });
    }
    updateMethodHeading();
    $(el).find('table').off('draw.dt.gmdpMethod').on('draw.dt.gmdpMethod', updateMethodHeading);
  }")
})
output$`PFcompareCustomCor-Table` <- DT::renderDataTable({PFcor.df()})


observeEvent(input$`PFcompareCustomCor-OnlySharedSubj`,{
  if(input$`PFcompareCustomCor-OnlySharedSubj`=='yes')  shinyjs::hide("PFcompareCustomCor-Centroid") else shinyjs::show("PFcompareCustomCor-Centroid")
})

custom.data.plot<-reactive({
  method <- PFcompareCustomCor.method()
  selected <- c(input$`PFcompareCustomCor-Mx`, input$`PFcompareCustomCor-My`)
  p.values <- run.data$plasma$mData[, selected, drop = FALSE]
  f.values <- run.data$fibroblast$mData[, selected, drop = FALSE]
  # Preserve historical Pearson rounding; rank full precision for Spearman.
  if (method == 'pearson') {p.values <- round(p.values, 3); f.values <- round(f.values, 3)}
  p.dat <- data.table(source = 'plasma', p.values, run.data$plasma$sample.info[, c('ID','Subject','centroid','Dx'), with = FALSE])
  f.dat <- data.table(source = 'fibroblast', f.values, run.data$fibroblast$sample.info[, c('ID','Subject','centroid','Dx'), with = FALSE])
  if(input$`PFcompareCustomCor-OnlySharedSubj`=='yes') {p.dat<-p.dat[joint.subject[['p.ind']]];f.dat<-f.dat[joint.subject[['f.ind']]]}
  custom.df<-rbind(p.dat,f.dat);colnames(custom.df)[2:3]<-c('x','y')
  if(input$`PFcompareCustomCor-Centroid`=='yes') custom.df<-custom.df[centroid=='yes']
  custom.df[,status:=ifelse(Dx%in%c('unknown','unaffected'),Dx,'diagnosed')]
  custom.df[,Subject:=paste(Subject,Dx,sep = "\n")]
  get.stat <- function(x, y) correlation_label(x, y, method, include_p = TRUE)
  custom.df[,stat:=get.stat(x,y),by=source]
  if(input$`PFcompareCustomCor-OnlySharedSubj`!='yes') custom.df[,stat2:=get.stat(x,y),by=.(source,status)]
  list(lapply(c('plasma','fibroblast'),function(tissue) ggplot(custom.df[source==tissue],aes(x=x,y=y,color=status,label=Subject))+geom_point(alpha=.5)+xlab(input$`PFcompareCustomCor-Mx`)+ylab(input$`PFcompareCustomCor-My`)+facet_grid(source~.)+ggtitle(unique(custom.df[source==tissue]$stat))+theme_bw()+
                theme(legend.position = 'none')),
       (if(input$`PFcompareCustomCor-OnlySharedSubj`=='no') lapply(c('plasma','fibroblast'),function(tissue) lapply(c('diagnosed','unaffected','unknown'),function(dx){
         ggplot(custom.df[source==tissue][status==dx],aes(x=x,y=y,label=Subject))+geom_point(alpha=.5,color=gg_color_hue(3)[match(dx,c('diagnosed','unaffected','unknown'))])+xlab(input$`PFcompareCustomCor-Mx`)+ylab(input$`PFcompareCustomCor-My`)+
           facet_grid(source~.)+ggtitle(unique(custom.df[source==tissue & status==dx]$stat2))+theme_classic()+theme(legend.position = 'none')
       }))))
})

# Capture data and settings eagerly on Submit; pending edits cannot leak into
# outputs when a hidden plot is revealed or a widget is resized.
custom.submitted <- reactiveVal(NULL)
observeEvent(input$`PFcompareCustomCor-Submit`, {
  plots <- custom.data.plot()
  custom.submitted(list(plots = plots, only_shared = input$`PFcompareCustomCor-OnlySharedSubj` == 'yes'))
})
for (tissue_index in seq_along(c('Plasma', 'Fibroblast'))) local({
  tissue <- tissue_index
  output_id <- paste0('PFcompareCustomCor-Plot', c('Plasma', 'Fibroblast')[tissue])
  output[[output_id]] <- renderPlotly({
    submitted <- req(custom.submitted())
    correlation_plotly(submitted$plots[[1]][[tissue]], height = 345)
  })
  for (group_index in 1:3) local({
    group <- group_index
    output[[paste0(output_id, '.', group)]] <- renderPlotly({
      submitted <- req(custom.submitted())
      req(!submitted$only_shared)
      correlation_plotly(submitted$plots[[2]][[tissue]][[group]], height = 225)
    })
  })
})
