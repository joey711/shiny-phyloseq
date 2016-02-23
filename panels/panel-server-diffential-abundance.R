################################################################################
# UI
################################################################################
output$dfabund_uix_xvar <- renderUI({
  selectInput("x_dfabund", "X-Axis", 
              vars("both", TRUE, TRUE), "Sample")
})
output$dfabund_uix_colvar <- renderUI({
  selectInput("color_dfabund", "Color", vars("both"))
})
output$dfabund_uix_facetrow <- renderUI({
  selectInput("facetrow_dfabund", "Facet Row", vars("both"), multiple = TRUE)
})
output$dfabund_uix_facetcol <- renderUI({
  selectInput("facetcol_dfabund", "Facet Col", vars("both"), multiple = TRUE)
})
output$dfabund_uix_constraint <- renderUI({
  selectInput("constraint_dfabund", "Constraint", vars(get_type_vars()), "NULL", multiple = TRUE)
})

################################################################################
# dfabund plot definition
################################################################################
physeq_dfabund = reactive({
  return(switch({input$uicttype_dfabund}, Counts=physeq(), Prop=physeqProp()))
})
get_formula_dfabund <- reactive({
  if(is.null(av(input$constraint_dfabund))){
    return(NULL)
  } else {
    formstring = paste("~", paste(input$constraint_dfabund, collapse = "+"))
    return(as.formula(formstring))
  }
})

make_dfabund_plot = reactive({
  p5 = NULL
  mydata <- physeq_dfabund()
  
  # Correct taxonomy
  if (colnames(tax_table(mydata))[1] == "Rank1") {
    taxlength = length(colnames(tax_table(mydata)))
    if (taxlength == 6) {
      ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
    } else if (taxlength == 7) {
      ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  } else if (taxlength == 11) {
    ranks <- c("Kingdom", "Cladus", "Supergroup", "Kingdom", "Subkingdom", "Phylum", "Subphylum", "Class", "Order", "Family", "Genus")
  }
  colnames(tax_table(mydata)) <- ranks
  }
  try(p5 <-amp_test_species(mydata,
                           group= input$constraint_dfabund, 
                           tax.add=c("Phylum","Genus"),
                           sig = 0.001, 
                           fold = 0.5, 
                           plot.type = "boxplot", 
                           plot.show = 10,
                           plot.theme = "clean"
                          ),
      silent=FALSE)
  return(p5$plot_sig)
})

finalize_dfabund_plot = reactive({
  if(input$actionb_dfabund < 1){
    p5 = fail_gen("Change settings and/or click '(Re)Build Graphic' Button")
  }
  isolate({
    p5 <- make_dfabund_plot()
  })
  
  return(p5)
})
# Render plot in panel and in downloadable file with format specified by user selection
output$dfabund <- renderPlot({
  shiny_phyloseq_print(finalize_dfabund_plot())
}, width=function(){72*input$width_dfabund}, height=function(){72*input$height_dfabund})
output$download_dfabund <- downloadHandler(
  filename = function(){paste0("DifferentialAbundancePlot_", simpletime(), ".", input$downtype_dfabund)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_dfabund_plot(),
            device=input$downtype_dfabund,
            width=input$width_dfabund, height=input$height_dfabund, dpi=300L, units="in")
  }
)