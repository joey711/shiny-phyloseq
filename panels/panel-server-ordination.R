########################################
# Compute ordination UI
########################################
# Define the variables category that should be shown in uix
get_type_vars = reactive({
  switch({input$ord_plot_type},
         sites = "samples",
         species = "taxa",
         biplot = "both",
         split = "both",
         "samples") 
})
output$ord_uix_color <- renderUI({
  selectInput("color_ord", "Color", vars(get_type_vars()), "NULL")
})
output$ord_uix_shape <- renderUI({
  selectInput("shape_ord", "Shape", vars(get_type_vars()), "NULL")
})
output$ord_uix_constraint <- renderUI({
  selectInput("constraint_ord", "Constraint", vars(get_type_vars()), "NULL", multiple = TRUE)
})
output$ord_uix_facetrow <- renderUI({
  selectInput("facetrow_ord", "Facet Row", vars(get_type_vars()), multiple = TRUE)
})
output$ord_uix_facetcol <- renderUI({
  selectInput("facetcol_ord", "Facet Col", vars(get_type_vars()), multiple = TRUE)
})
output$ord_uix_label <- renderUI({
  selectInput("label_ord", "Label", vars(get_type_vars()), "NULL")
})
# Define tooltip mapping options
tooltip_opts_ord = reactive({
  optlist = c("x", "y")
  if(!is.null(input$color_ord)){
    optlist <- c(optlist, "colour")
  }
  if(!is.null(input$shape_ord)){
    optlist <- c(optlist, "shape")
  }
  if(!is.null(input$label_ord)){
    optlist <- c(optlist, "label")
  }
  return(optlist)
})
# Define tooltip UI
output$ord_uix_tooltip <- renderUI({
  # Define vector of non-NULL mapping vars
  selectInput(inputId = "tooltip_ord",
              label = "Tooltip",
              choices = tooltip_opts_ord(),
              selected = tooltip_opts_ord(),
              multiple = TRUE)
})
output$ord_uix_dist <- renderUI({
  if(input$ord_method %in% c("DCA", "CCA", "RDA", "DPCoA")){
    return(selectInput("dist_ord", tags$del("Distance"), "NULL"))
  }
  return(selectInput("dist_ord", "Distance", distlist, selected="bray"))
})
########################################
# Compute Ordination object
########################################
physeq_ord = reactive({
  return(
    switch({input$transform_ord},
           Counts = physeq(),
           Prop = physeqProp(),
           RLog = physeqRLog(),
           CLR = physeqCLR(),
           physeq()
    )
  )
})
get_formula_ord <- reactive({
  if(is.null(av(input$constraint_ord))){
    return(NULL)
  } else {
    formstring = paste("~", paste(input$constraint_ord, collapse = "+"))
    return(as.formula(formstring))
  }
})
# Define the `type` argument passed to ordinate function, then to distance function, if applicable
get_type_ord = reactive({
  # Pass "samples" unless explicitly species-only plot.
  switch(input$ord_plot_type, species = "taxa", "samples")
})
get_ord = reactive({
  # Attempt ordination. If fails because `type` unused argument, try again without it
  ord = try(ordinate(physeq_ord(),
                      method = input$ord_method, 
                      distance = input$dist_ord, 
                      formula = get_formula_ord(), 
                      type = get_type_ord()), silent=TRUE)
  if(inherits(ord, "try-error")){
    # Try again, without type specified
    ord = try(ordinate(physeq_ord(),
                       method = input$ord_method, 
                       distance = input$dist_ord, 
                       formula = get_formula_ord()), silent=TRUE)
  }
  if(inherits(ord, "try-error")){
    # Try again, without formula
    ord = try(ordinate(physeq_ord(),
                       method = input$ord_method, 
                       distance = input$dist_ord), silent=TRUE)
  }  
  if(inherits(ord, "try-error")){
    warning(ord)
    ord = NULL
  }
  return(ord)
})
make_ord_plot = reactive({
  p1 = NULL
  try(p1 <- plot_ordination(physeq = physeq_ord(), 
                            ordination = get_ord(),
                            type=input$ord_plot_type,
                            axes = c(as.integer(input$axis_x_ord),
                                     as.integer(input$axis_y_ord)),
                            color = av(input$color_ord),
                            shape = av(input$shape_ord)
                            ),
      silent=TRUE)
  return(p1)
})
# Finalize Ordination Plot (for download and panel)
finalize_ordination_plot = reactive({
  p1 = make_ord_plot()
  if(inherits(p1, "ggplot")){
    p1$layers[[1]] <- NULL
    p1 <- p1 + geom_point(size = av(input$size_ord),
                          alpha = av(input$alpha_ord))
    if(!is.null(av(input$color_ord))){
      if(plyr::is.discrete(p1$data[[input$color_ord]])){
        # Discrete brewer palette mapping
        p1 <- p1 + scale_colour_brewer(palette=input$pal_ord)
      } else {
        # Continuous brewer palette mapping
        p1 <- p1 + scale_colour_distiller(palette=input$pal_ord) 
      }    
    }
    if(!is.null(av(input$label_ord))){
      p1 <- p1 + geom_text(aes_string(label=input$label_ord), 
                           size = input$label_size_ord,
                           vjust = input$label_vjust_ord)
    }
    p1ord_facet_form = get_facet_grid(input$facetrow_ord, input$facetcol_ord)
    if(!is.null(p1ord_facet_form)){
      # Add facet_grid layer if user-provided one.
      p1 <- p1 + facet_grid(p1ord_facet_form)
    }
    p1 <- p1 + shiny_phyloseq_ggtheme_list[[input$theme_ord]]
  }
  return(p1)
})
########################################
# Render Ordination
########################################
output$ordination_plotly <- renderPlotly({
  ggplotly(p = finalize_ordination_plot(),
           tooltip = input$tooltip_ord,
           width = 72*input$width_ord,
           height = 72*input$height_ord)
})
########################################
# Scree plot, if supported
########################################
finalize_scree = reactive({
  pscree = NULL
  try({
    pscree <- plot_scree(ordination = get_ord(),
                         title = "Scree Plot")
    # Truncate N discrete elements in scree plot
    pscree$data <- pscree$data[1:10, ]
  }, silent=TRUE)
  return(pscree)
})
output$scree_plotly <- renderPlotly({
  fcr1 = finalize_scree()
  ggplotly(p = shiny_phyloseq_check_plotly(fcr1),
           width = 72*input$width_ord)
})
########################################
# Download Handling
########################################
output$download_ord <- downloadHandler(
  filename = function(){paste0("Ordination_", 
                               simpletime(), ".", 
                               input$downtype_ord)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_ordination_plot(),
            device=input$downtype_ord,
            width=input$width_ord, 
            height=input$height_ord, 
            dpi=300L, 
            units="in")
  }
)
########################################
