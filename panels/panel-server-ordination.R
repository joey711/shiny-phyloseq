################################################################################
# UI ordination
################################################################################
output$ord_uix_color <- renderUI({
  selectInput("color_ord", "Color", vars(input$type_ord), "NULL")
})
output$ord_uix_shape <- renderUI({
  selectInput("shape_ord", "Shape", vars(input$type_ord), "NULL")
})
output$ord_uix_constraint <- renderUI({
  selectInput("constraint_ord", "Constraint", vars(input$type_ord), "NULL", multiple = TRUE)
})
output$ord_uix_facetrow <- renderUI({
  selectInput("facetrow_ord", "Facet Row", vars(input$type_ord), multiple = TRUE)
})
output$ord_uix_facetcol <- renderUI({
  selectInput("facetcol_ord", "Facet Col", vars(input$type_ord), multiple = TRUE)
})
output$ord_uix_label <- renderUI({
  selectInput("label_ord", "Label", vars(input$type_ord), "NULL")
})
################################################################################
# Ordination functions
################################################################################
get_formula_ord <- reactive({
  if(is.null(av(input$constraint_ord))){
    return(NULL)
  } else {
    formstring = paste("~", paste(input$constraint_ord, collapse = "+"))
    return(as.formula(formstring))
  }
})
# Define global reactive distance matrix. Will re-calc if method or plot-type change.
gdist <- reactive({
  if(input$dist_ord %in% distance("list")$vegdist){
    return(input$dist_ord)
  } else {
    idist = NULL
    try({idist <- distance(physeq(), method=input$dist_ord, type=input$type_ord)}, silent=TRUE)
    if(is.null(idist)){warning("gdist: Could not calculate distance matrix with these settings.")}
    return(idist)
  }
})
# Define reactive ordination access
get_ord = reactive({
  ordinate(physeq(), method=input$ord_method, distance=gdist(), formula=get_formula_ord())
})
make_ord_plot = reactive({
  p1 = NULL
  try(p1 <- plot_ordination(physeq(), get_ord(), type=input$ord_plot_type), silent=TRUE)
  return(p1)
})
# Finalize Ordination Plot (for download and panel)
finalize_ordination_plot = reactive({
  p1 = make_ord_plot()
  if(inherits(p1, "ggplot")){
    p1$layers[[1]]$geom_params$size <- av(input$size_ord)
    p1$layers[[1]]$geom_params$alpha <- av(input$alpha_ord)
    if(!is.null(av(input$color_ord))){
      p1$mapping$colour <- as.symbol(input$color_ord)
      p1 <- update_labels(p1, list(colour = input$color_ord))
      if(plyr::is.discrete(p1$data[[input$color_ord]])){
        # Discrete brewer palette mapping
        p1 <- p1 + scale_colour_brewer(palette=input$pal_ord)
      } else {
        # Continuous brewer palette mapping
        p1 <- p1 + scale_colour_distiller(palette=input$pal_ord) 
      }    
    }
    if(!is.null(av(input$shape_ord))){
      p1$mapping$shape  <- as.symbol(av(input$shape_ord))
      p1 <- update_labels(p1, list(shape = input$shape_ord))
    }
    if(!is.null(av(input$label_ord))){
      p1 <- p1 + geom_text(aes_string(label=input$label_ord), 
                           size = input$label_size_ord,
                           vjust = input$label_vjust_ord)
    }
    p1ord_facet_form = get_facet_grid(input$facetrow_ord, input$facetcol_ord)
    if(!is.null(p1ord_facet_form)){
      # Add facet_grid layer if user-provided one
      # # Maybe add a user-toggle for free_x and free_y panels.
      p1 <- p1 + facet_grid(p1ord_facet_form)
    }
    p1 <- p1 + shiny_phyloseq_ggtheme_list[[input$theme_ord]]
  }
  return(p1)
})
# Render plot in panel and in downloadable file with format specified by user selection
output$ordination <- renderPlot({
  shiny_phyloseq_print(finalize_ordination_plot())
}, width=function(){72*input$width_ord}, height=function(){72*input$height_ord})
output$download_ord <- downloadHandler(
  filename = function(){paste0("Ordination_", simpletime(), ".", input$downtype_ord)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_ordination_plot(),
            device=input$downtype_ord,
            width=input$width_ord, height=input$height_ord, dpi=300L, units="in")
  }
)
# Always add a 'supplemental' scree plot, if supported, below the ordination plot itself
output$scree_ord <- renderPlot({
  pscree = NULL
  try(pscree <- plot_ordination(physeq(), get_ord(), type="scree", title = "Scree Plot"), silent=TRUE)
  return(shiny_phyloseq_print(pscree))
}, width=400, height=250)
