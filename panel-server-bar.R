################################################################################
# UI
################################################################################
output$bar_uix_xvar <- renderUI({
  selectInput("x_bar", "Horizontal ('x') Variable:", 
              vars("both", TRUE, TRUE), "Sample")
})
output$bar_uix_colvar <- renderUI({
  selectInput("color_bar", "Color Fill Variable:", vars("both"))
})
################################################################################
# bar plot definition
################################################################################
physeq_bar = reactive({
  return(switch({input$uicttype_bar}, Counts=physeq(), Proportions=physeqProp()))
})
get_facet <- reactive({
  if(is.null(input$facform_bar) | input$facform_bar=="NULL"){
    return(NULL)
  } else {
    return(as.formula(input$facform_bar))
  }
})
make_bar_plot = reactive({
  p0 = NULL
  # Try with facet argument included first. If fails, retry without it.
  try(p0 <- plot_bar(physeq_bar(), x=input$x_bar, y="Abundance", fill=av(input$color_bar), 
                     facet_grid=get_facet()),
      silent=TRUE)
  if(!inherits(p0, "ggplot")){
    warning("Could not render bar plot, attempting without faceting...")
    try(p0 <- plot_bar(physeq_bar(), x=xvar, y="Abundance", fill=av(input$color_bar)),
        silent=TRUE)
  }
  return(p0)
})
finalize_bar_plot = reactive({
  p0 = make_bar_plot()
  p0 <- p0 + scale_fill_brewer(palette=input$pal_bar) 
  return(p0)
})
# Render plot in panel and in downloadable file with format specified by user selection
output$bar <- renderPlot({
  shiny_phyloseq_print(finalize_bar_plot())
}, width=function(){72*input$width_bar}, height=function(){72*input$height_bar})
output$downloadBar <- downloadHandler(
  filename = function(){paste0("Barplot_", simpletime(), ".", input$downtype_bar)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_bar_plot(),
            device=input$downtype_bar,
            width=input$width_bar, height=input$height_bar, dpi=300L, units="in")
  }
)