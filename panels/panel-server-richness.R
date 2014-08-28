################################################################################
# UI
################################################################################
output$rich_uix_x <- renderUI({
  selectInput("x_rich", 
        label = "X",
        choices = c(list("samples"), vars("samples")),
        selected = "samples")
})
output$rich_uix_color <- renderUI({
  selectInput("color_rich",
              label = "Color",
              choices = c(list("samples"), vars("samples")),
              selected = "NULL")
})
output$rich_uix_shape <- renderUI({
  selectInput("shape_rich", 
              label = "Shape",
              choices = c(list("samples"), vars("samples")),
              selected = "NULL")
})
output$rich_uix_label <- renderUI({
  selectInput("label_rich", "Label", vars("samples"), "NULL")
})
################################################################################
# Alpha Diversity plot definition
################################################################################
physeq_rich = reactive({
  return(switch(input$uicttype_rich, Original=get_phyloseq_data(), Filtered=physeq()))
})
make_richness_plot = reactive({
  p4 = NULL
  try(p4 <- plot_richness(physeq_rich(),
                          measures = input$measures_rich),
      silent=TRUE)
  return(p4)
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
    # x (horizontal) axis mapping
    if(!is.null(av(input$x_rich))){
      p4$mapping$x <- as.symbol(input$x_rich)
      p4 <- update_labels(p4, list(x = input$x_rich))
    }
    # Shape mapping.
    if(!is.null(av(input$shape_rich))){
      p4$mapping$shape <- as.symbol(input$shape_rich)
      p4 <- update_labels(p4, list(shape = input$shape_rich))
    }
    # Color mapping/palette.
    if(!is.null(av(input$color_rich))){
      p4$mapping$colour <- as.symbol(input$color_rich)
      p4 <- update_labels(p4, list(colour = input$color_rich))
      if(plyr::is.discrete(p4$data[[input$color_rich]])){
        # Discrete brewer palette mapping
        p4 <- p4 + scale_colour_brewer(palette=input$pal_rich)
      } else {
        # Continuous brewer palette mapping
        p4 <- p4 + scale_colour_distiller(palette=input$pal_rich) 
      }    
    }
    # Point-label mapping
    if(!is.null(av(input$label_rich))){
      p4 <- p4 + geom_text(aes_string(label=input$label_rich), 
                           size = input$label_size_rich,
                           vjust = input$label_vjust_rich)
    }
    p4 <- p4 + shiny_phyloseq_ggtheme_list[[input$theme_rich]]
    # Add the x-axis label rotation as specified by user
    p4 <- p4 + theme(axis.text.x = element_text(
      angle = input$x_axis_angle_rich,
      vjust = 0.5)
    )
    # Protect against too-many x-axis labels
    # Should be based on the aesthetic in p4 (more general)
    if(!is.null(av(input$x_rich))){
      if(plyr::is.discrete(p4$data[[input$x_rich]])){
        # Check the number of discrete classes (e.g. factor levels)
        if(length(unique(p4$data[[input$x_rich]])) > input$label_max_rich){
          # If number of classes is above max, set x-axis theme to blank
          p4 <- p4 + theme(axis.text.x = element_blank(),
                           axis.ticks.x = element_blank())
        } 
      }      
    }
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
output$download_rich <- downloadHandler(
  filename = function(){paste0("Richness_", simpletime(), ".", input$downtype_rich)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_richness_plot(),
            device=input$downtype_rich,
            width=input$width_rich, height=input$height_rich, dpi=300L, units="in")
  }
)