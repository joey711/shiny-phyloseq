################################################################################
# UI
################################################################################
output$heat_sample_label <- renderUI({
  uivar("sample.label", "Sample Labels:", vars("samples"))
})
output$heat_taxa_label <- renderUI({
  uivar("taxa.label", "Taxa Labels:", vars("taxa"))
})
output$heat_sample_order <- renderUI({
  uivar("sample.order", "Sample Ordering:", vars("samples"))
})
output$heat_taxa_order <- renderUI({
  uivar("taxa.order", "Taxa Ordering:", vars("taxa"))
})
################################################################################
# heatmap plot definition
################################################################################
physeq_heat = reactive({
  return(switch(input$uicttype_heat, Counts=physeq(), Proportions=physeqProp()))
})
make_heatmap = reactive({
  if(input$actionb_heat < 1){
    return(NULL)
  }
  p3 = NULL
  isolate({
    try(p3 <- plot_heatmap(
      physeq_heat(), 
      method=input$ord_method_heat,
      distance=input$dist_heat,
      sample.label=av(input$sample.label),
      taxa.label=av(input$taxa.label),
      sample.order=av(input$sample.order),
      taxa.order=av(input$taxa.order),
      low = input$locolor_heat,
      high = input$hicolor_heat,
      na.value = input$NAcolor_heat),
      silent=TRUE)
  })
  return(p3)
})
# Render plot in panel and in downloadable file with format specified by user selection
output$heatmap <- renderPlot({
  shiny_phyloseq_print(make_heatmap())
}, width=function(){72*input$width_heat}, height=function(){72*input$height_heat})
output$downloadHeat <- downloadHandler(
  filename = function(){paste0("Heatmap_", simpletime(), ".", input$downtype_heat)},
  content = function(file){
    ggsave2(filename=file,
            plot=make_heatmap(),
            device=input$downtype_heat,
            width=input$width_heat, height=input$height_heat, dpi=300L, units="in")
  }
)