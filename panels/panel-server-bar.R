################################################################################
# UI
################################################################################
output$bar_uix_xvar <- renderUI({
  selectInput("x_bar", "X-Axis", 
              vars("both", TRUE, TRUE), "Sample")
})
output$bar_uix_colvar <- renderUI({
  selectInput("color_bar", "Color", vars("both"))
})
output$bar_uix_facetrow <- renderUI({
  selectInput("facetrow_bar", "Facet Row", vars("both"), multiple = TRUE)
})
output$bar_uix_facetcol <- renderUI({
  selectInput("facetcol_bar", "Facet Col", vars("both"), multiple = TRUE)
})
################################################################################
# bar plot definition
################################################################################
physeq_bar = reactive({
  return(switch({input$uicttype_bar}, Counts=physeq(), Prop=physeqProp()))
})
make_bar_plot = reactive({
  p0 = NULL
  # Try with facet argument included first. If fails, retry without it.
  try(p0 <- plot_bar(physeq_bar(),
                     x=input$x_bar,
                     y="Abundance",
                     fill=av(input$color_bar), 
                     facet_grid=get_facet_grid(input$facetrow_bar, input$facetcol_bar)),
      silent=TRUE)
  if(!inherits(p0, "ggplot")){
    warning("Could not render bar plot, attempting without faceting...")
    try(p0 <- plot_bar(physeq_bar(), x=xvar, y="Abundance", fill=av(input$color_bar)),
        silent=TRUE)
  }
  return(p0)
})
finalize_bar_plot = reactive({
  if(input$actionb_bar < 1){
    p0 = fail_gen("Change settings and/or click '(Re)Build Graphic' Button")
  }
  isolate({
    p0 <- make_bar_plot()
  })
  p0 <- p0 + scale_fill_brewer(palette=input$pal_bar) + 
    shiny_phyloseq_ggtheme_list[[input$theme_bar]]
  # Add the x-axis label rotation as specified by user
  p0 <- p0 + 
    theme(axis.text.x = element_text(angle = input$x_axis_angle_bar,
                                     vjust = 0.5, hjust = 1)
  )
  return(p0)
})
# Render plot in panel and in downloadable file with format specified by user selection
output$bar <- renderPlot({
  shiny_phyloseq_print(finalize_bar_plot())
}, width=function(){72*input$width_bar}, height=function(){72*input$height_bar})
output$download_bar <- downloadHandler(
  filename = function(){paste0("Barplot_", simpletime(), ".", input$downtype_bar)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_bar_plot(),
            device=input$downtype_bar,
            width=input$width_bar, height=input$height_bar, dpi=300L, units="in")
  }
)