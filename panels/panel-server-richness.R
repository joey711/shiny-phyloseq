################################################################################
# UI
################################################################################
output$richness_uix_x <- renderUI({
  selectInput("x_alpha",
        label = "Horizontal (x) Variable:",
        choices = c(list("samples"), vars("samples")),
        selected = "samples")
})
output$richness_uix_color <- renderUI({
  selectInput("color_alpha",
              label = "Color Variable:",
              choices = c(list("samples"), vars("samples")),
              selected = "NULL")
})
output$richness_uix_shape <- renderUI({
  selectInput("shape_alpha",
              label = "Shape Variable:",
              choices = c(list("samples"), vars("samples")),
              selected = "NULL")
})
################################################################################
# Alpha Diversity plot definition
################################################################################
physeq_rich = reactive({
  return(switch(input$uicttype_rich, Original=get_phyloseq_data(), Filtered=physeq()))
})
make_richness_plot = reactive({
  if(input$actionb_rich == 0){
    return(NULL)
  }
  isolate({
    p4 = NULL
    try(p4 <- plot_richness(physeq_rich(),
                            x = av(input$x_alpha),
                            color = av(input$color_alpha),
                            shape = av(input$shape_alpha),
                            measures = input$measures_alpha),
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
    if(length(p4$layers) >= 2){
      p4$layers[[2]]$geom_params$alpha <- input$alpha_rich 
    }
    if(!is.null(av(input$color_rich))){
      if(plyr::is.discrete(p4$data[[input$color_rich]])){
        # Discrete brewer palette mapping
        p4 <- p4 + scale_colour_brewer(palette=input$pal_rich)
      } else {
        # Continuous brewer palette mapping
        p4 <- p4 + scale_colour_distiller(palette=input$pal_rich) 
      }    
    }
    p4 <- p4 + shiny_phyloseq_ggtheme_list[[input$theme_rich]]
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