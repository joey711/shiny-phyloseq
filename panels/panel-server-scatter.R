################################################################################
# UI
################################################################################
output$scat_uix_x <- renderUI({
  selectInput(inputId="x_scat", label="Horizontal ('x') Variable:", 
              choices=c(list(Abundance="Abundance"), vars()),
              selected="Sample")
})
output$scat_uix_y <- renderUI({
  selectInput(inputId="y_scat", label="Vertical ('y') Variable:", 
              choices=c(list(Abundance="Abundance"), vars()),
              selected="Abundance")
})
output$scat_uix_color <- renderUI({
  uivar("color_scat", "Color Variable:", vars())
})
output$scat_uix_shape <- renderUI({
  uivar("shape_scat", "Shape Variable:", vars())
})
################################################################################
# Flexible Scatter plot
################################################################################
physeq_scat = reactive({
  return(switch(input$uicttype_scat, Counts=physeq(), Proportions=physeqProp()))
})
make_scatter_plot = reactive({
  pscat = NULL
  try({
    scatmap = aes_string(x=av(input$x_scat), y=av(input$y_scat),
                         color=av(input$color_scat), shape=av(input$shape_scat))
    scatdf = psmelt(physeq_scat())
    pscat = ggplot(data=scatdf, mapping=scatmap) + geom_point()
    pscat = pscat + theme(axis.text.x=element_text(angle=-90, vjust=0.5, hjust=0)) 
    if(!is.null(av(input$facform_scat))){
      # Add facet_grid layer if user-provided one
      # # Need to add a user-toggle for free_x and free_y panels.
      pscat <- pscat + facet_grid(facets=as.formula(av(input$facform_scat)))
    }
  }, silent=TRUE)
  return(pscat)
})
finalize_scatter_plot = reactive({
  pscat = make_scatter_plot()
  if(inherits(pscat, "ggplot")){
    # Adjust size/alpha of points, but not error bars
    pscat$layers[[1]]$geom_params$size <- input$size_scat
    pscat$layers[[1]]$geom_params$alpha <- input$alpha_scat
    pscat <- pscat + scale_colour_brewer(palette = input$pal_scat)
    return(pscat)
  } else {
    # If for any reason pscat is not a ggplot at this point,
    # render fail-plot rather than tinker with innards.
    return(failp)
  }
})
# Render plot in panel and in downloadable file with format specified by user selection
output$scatter <- renderPlot({
  shiny_phyloseq_print(finalize_scatter_plot())
}, width=function(){72*input$width_scat}, height=function(){72*input$height_scat})
output$downloadScatter <- downloadHandler(
  filename = function(){paste0("Scatter_", simpletime(), ".", input$downtype_scat)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_scatter_plot(),
            device=input$downtype_scat,
            width=input$width_scat, height=input$height_scat, dpi=300L, units="in")
  }
)