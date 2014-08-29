################################################################################
# UI
################################################################################
physeq_scat = reactive({
  return(
    switch({input$transform_scat},
           Counts = physeq(),
           Prop = physeqProp(),
           RLog = physeqRLog(),
           CLR = physeqCLR(),
           physeq()
    )
  )
})
scatdf = reactive({
  return(psmelt(physeq_scat()))
})
output$scat_uix_x <- renderUI({
  selectInput(inputId="x_scat", label="X", 
              choices = as.list(c("NULL", names(scatdf()))),
              selected="Sample")
})
output$scat_uix_y <- renderUI({
  selectInput(inputId="y_scat", label="Y", 
              choices = as.list(c("NULL", names(scatdf()))),
              selected="Abundance")
})
output$scat_uix_color <- renderUI({
  selectInput("color_scat", "Color",
              choices = as.list(c("NULL", names(scatdf()))))
})
output$scat_uix_shape <- renderUI({
  selectInput("shape_scat", "Shape",
              choices = as.list(c("NULL", names(scatdf()))))
})
output$scat_uix_facetrow <- renderUI({
  selectInput("facetrow_scat", "Facet Row", vars("both"), multiple = TRUE)
})
output$scat_uix_facetcol <- renderUI({
  selectInput("facetcol_scat", "Facet Col", vars("both"), multiple = TRUE)
})
output$scat_uix_label <- renderUI({
  selectInput("label_scat", "Label", vars("both"), "NULL")
})
################################################################################
# Flexible Scatter plot
################################################################################
make_scatter_plot = reactive({
  pscat = NULL
  try({
    scatmap = aes_string(x=av(input$x_scat), y=av(input$y_scat),
                         color=av(input$color_scat), shape=av(input$shape_scat))
    pscat = ggplot(data=scatdf(), mapping=scatmap) + geom_point()
    pscat = pscat + theme(axis.text.x=element_text(angle=-90, vjust=0.5, hjust=0)) 
    pscat_facet_form = get_facet_grid(input$facetrow_scat, input$facetcol_scat)
    if(!is.null(pscat_facet_form)){
      # Add facet_grid layer if user-provided one
      # # Maybe add a user-toggle for free_x and free_y panels.
      pscat <- pscat + facet_grid(pscat_facet_form)
    }
  }, silent=TRUE)
  return(pscat)
})
finalize_scatter_plot = reactive({
  pscat = make_scatter_plot()
  if(inherits(pscat, "ggplot")){
    # Add labels if non-NULL
    if(!is.null(av(input$label_scat))){
      pscat <- pscat + geom_text(aes_string(label=input$label_scat), 
                           size = input$label_size_scat,
                           vjust = input$label_vjust_scat)
    }
    # Adjust size/alpha of points, but not error bars
    pscat$layers[[1]]$geom_params$size <- input$size_scat
    pscat$layers[[1]]$geom_params$alpha <- input$alpha_scat
    if(!is.null(av(input$color_scat))){
      if(plyr::is.discrete(pscat$data[[input$color_scat]])){
        # Discrete brewer palette mapping
        pscat <- pscat + scale_colour_brewer(palette=input$pal_scat)
      } else {
        # Continuous brewer palette mapping
        pscat <- pscat + scale_colour_distiller(palette=input$pal_scat) 
      }    
    }
    pscat <- pscat + shiny_phyloseq_ggtheme_list[[input$theme_scat]]
    return(pscat)
  } else {
    # If for any reason pscat is not a ggplot at this point,
    # render fail-plot rather than tinker with innards.
    return(fail_gen())
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