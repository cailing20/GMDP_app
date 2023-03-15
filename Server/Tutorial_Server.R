tutorial.list<-list(1,2:3,4:6,7:8,9:10,11:15,16:19,20:22,23:27,28:30,31:32,33:34,35:38,39:43)
observeEvent(input$`tutorial-tabs`,{
  ids<-tutorial.list[[as.numeric(input$`tutorial-tabs`)]]
  if(as.numeric(input$`tutorial-tabs`)!=1) ids<-ids[-1]
  output[[paste0("tutorial-",input$`tutorial-tabs`)]] <- renderUI({
    image_output_list <- lapply(ids,function(i){
      shinycssloaders::withSpinner(imageOutput(outputId = paste0('img',i),width = '1040px',height = '780px'),color = '#e95420')})
    do.call(tagList, image_output_list)
  })
  lapply(ids,function(i){
    output[[paste0('img',i)]] <-renderImage({list(src = paste0('www/tutorial/GMDP2_tutorial.',str_pad(i,width = 3,pad = '0',side = 'left'),'.jpeg'), 
                                                  width = "1000px",height='750px', alt = "Image failed to render")}, deleteFile = FALSE)
  })
})
