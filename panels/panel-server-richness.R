################################################################################
# UI
################################################################################
output$richness_uix_x <- renderUI({
  uivar("x_alpha", "Horizontal (x) Variable:", vars("samples"))
})
output$richness_uix_color <- renderUI({
  uivar("color_alpha", "Color Variable:", vars("samples"))
})
output$richness_uix_shape <- renderUI({
  uivar("shape_alpha", "Shape Variable:", vars("samples"))
})
################################################################################
# Alpha Diversity plot definition
################################################################################
make_richness_plot = reactive({
  if(input$actionb_rich == 0){
    return(NULL)
  }
  isolate({
    p4 = NULL
    try(p4 <- plot_richness(physeq(), x=av(input$x_alpha),
                            color=av(input$color_alpha),
                            shape=av(input$shape_alpha),
                            measures=input$measures_alpha),
        silent=TRUE)
    return(p4)
  })
})
finalize_richness_plot = reactive({
  p4 = make_richness_plot()
  if(inherits(p4, "ggplot")){
    # Adjust size/alpha of points, but not error bars
    p4$layers[[1]]$geom_params$size <- input$size_rich
    p4$layers[[1]]$geom_params$alpha <- input$alpha_rich
    p4 <- p4 + scale_colour_brewer(palette = input$pal_rich)
    return(p4)
  } else {
    # If for any reason p4 is not a ggplot at this point,
    # render fail-plot rather than tinker with innards.
    return(fail_gen())
  }
})
# Render plot in panel and in downloadable file with format specified by user selection
output$richness <- renderPlot({
  shiny_phyloseq_print(finalize_richness_plot())
}, width=function(){72*input$width_rich}, height=function(){72*input$height_rich})
output$downloadRichness <- downloadHandler(
  filename = function(){paste0("Richness_", simpletime(), ".", input$downtype_rich)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_richness_plot(),
            device=input$downtype_rich,
            width=input$width_rich, height=input$height_rich, dpi=300L, units="in")
  }
)