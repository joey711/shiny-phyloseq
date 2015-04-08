################################################################################
# UI
################################################################################
output$tree_uix_color <- renderUI({
  uivar("color_tree", "Color", vars("both", TRUE, TRUE))
})
output$tree_uix_shape <- renderUI({
  uivar("shape_tree", "Shape", vars("both", TRUE, TRUE))
})
output$tree_uix_tiplabs <- renderUI({
  uivar("label_tip_tree", "Labels", vars("taxa", TRUE, TRUE))
})
output$tree_uix_point_thresh <- renderUI({
  numericInputRow("abundance_threshold_tree", "Min",
              value=0.1*median(as(otu_table(physeq()), "matrix"), na.rm=TRUE),
              max=max(as(otu_table(physeq()), "matrix"), na.rm=TRUE),
              min=min(as(otu_table(physeq()), "matrix"), na.rm=TRUE),
              class="col-md-12"
  )
})
################################################################################
# phylogenetic tree plot definition
################################################################################
filter_phyloseq = reactive({
  filterPhyseq = physeq()
  # observe({print(paste0("tree obs min threshold: ", input$abundance_threshold_tree))})
  # observe({print(paste0("Class of tree obs min threshold: ", class(input$abundance_threshold_tree)))})
  otu_table(filterPhyseq)[otu_table(filterPhyseq) < input$abundance_threshold_tree] <- 0
  return(filterPhyseq)
})
make_tree = reactive({     
  p2 = NULL
  try(p2 <- plot_tree(filter_phyloseq(), input$method_tree,
                      justify=input$justify_tree,
                      nodelabf=nodeplotblank, 
                      ladderize=av(input$ladderize_tree),
                      label.tips=av(input$label_tip_tree),
                      color=av(input$color_tree), shape=av(input$shape_tree), 
                      text.size=av(input$size_tree),
                      plot.margin=input$margin_tree),
      silent=TRUE)
  if(input$plot_tree_radial=="radial"){
    p2 <- p2 + coord_polar(theta="y")
  }
  return(p2)
})
finalize_tree_plot = reactive({
  if(ntaxa(physeq()) <= 500 & !is.null(phy_tree(physeq(), errorIfNULL=FALSE))){
    p2 = make_tree()
  } else if(!is.null(phy_tree(physeq(), errorIfNULL=FALSE))){
    # Notify user that there are too many taxa for a tree graphic
    p2 = fail_gen("Too Many OTUs For a Tree Graphic",
                  "Please Filter or Merge OTUs, and Try Again")
  } else {
    # Remind user that tree is missing from data
    p2 = fail_gen("No Tree in Input Data",
                  "Cannot Make Tree Graphic without Tree")        
  }
  # Reactive Color section
  if( !is.null(av(input$color_tree)) & input$method_tree == "sampledodge"){
    # Find point-layer
    treeptlay = which(sapply(p2$layers, function(x) x$geom$objname == "point"))[1]
    # Test whether first point-layer color variable is discrete
    if(plyr::is.discrete(p2$layers[[treeptlay]]$data[[input$color_tree]])){
      # Discrete brewer palette mapping
      p2 <- p2 + scale_colour_brewer(palette=input$pal_tree) + 
        scale_fill_brewer(palette=input$pal_tree)
    } else {
      # Continuous brewer palette mapping
      p2 <- p2 + scale_colour_distiller(palette=input$pal_tree) +
        scale_fill_distiller(palette=input$pal_tree)
    }    
  }
  p2 <- p2 + shiny_phyloseq_ggtheme_list[[input$theme_tree]]
  return(p2)
})
# Render plot in panel and in downloadable file with format specified by user selection
output$tree <- renderPlot({
  shiny_phyloseq_print(finalize_tree_plot())
}, width=function(){72*input$width_tree}, height=function(){72*input$height_tree})
output$download_tree <- downloadHandler(
  filename = function(){paste0("Tree_", simpletime(), ".", input$downtype_tree)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_tree_plot(),
            device=input$downtype_tree,
            width=input$width_tree, height=input$height_tree, dpi=300L, units="in")
  }
)