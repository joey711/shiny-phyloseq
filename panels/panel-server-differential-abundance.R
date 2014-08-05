################################################################################
# UI
################################################################################
output$diffabund_uix_testvars <- renderUI({
  selectInput("diffabund_testvars", "Test Variable:",
              choices = vars("samples"), 
              selected = NULL,
              multiple = TRUE)
})
output$diffabund_uix_x <- renderUI({
  selectInput("x_diffabund", "Horizontal (x) Variable:",
              choices = vars("taxa"), 
              selected = rank_names(physeq(), errorIfNULL = FALSE)[1],
              multiple = FALSE)
})
output$diffabund_uix_color <- renderUI({
  selectInput("color_diffabund", "Color Variable:",
              choices = vars("taxa"), 
              selected = rank_names(physeq(), errorIfNULL = FALSE)[1],
              multiple = FALSE)
})
output$diffabund_uix_shape <- renderUI({
  selectInput("shape_diffabund", "Shape Variable:",
              choices = vars("taxa"), 
              selected = rank_names(physeq(), errorIfNULL = FALSE)[1],
              multiple = FALSE)
})
################################################################################

diffabund_test_formula = reactive({
  if(length(av(input$diffabund_testvars)) > 0){
    #observe({print(as.formula(paste("~", paste(input$diffabund_testvars, collapse = " + "))))})
    return(as.formula(paste("~", paste(input$diffabund_testvars, collapse = " + "))))
  }
  return(NULL)
})
 
# DESeq2 section - test with `observe`
diffabund_results_DESeq2 = reactive({
  res = NULL
  if(!is.null(diffabund_test_formula()) & !is.null(physeq())){
    # Conversion
    dds = phyloseq_to_deseq2(physeq(), design = diffabund_test_formula())
    # Multiple Testing
    dds = DESeq2::DESeq(dds, test="Wald", fitType="parametric")
    # Organize results table for plotting
    res = DESeq2::results(dds, cooksCutoff = FALSE)
    res <- res[which(res$padj <= input$diffabund_sig_threshold), ]
    if(nrow(res) > 0){
      if(!is.null(tax_table(physeq(), errorIfNULL = TRUE))){
        res <- cbind(
          as(res, "data.frame"), 
          as(tax_table(physeq())[rownames(res), ], "matrix")
        )
      }
    }
  }
  return(res)
})

diffabund_calc = reactive({
  # Initialize Results List
  resList = list() 
  if("DESeq2" %in% av(input$diffabund_types)){
    resList$DESeq2 <- diffabund_results_DESeq2()
  }
  return(resList)
})

make_diffabund_plot = reactive({
  deseqres = diffabund_calc()$DESeq2
  if(!is.null(deseqres)){
    if( nrow(deseqres) >= 1L ){
      p = ggplot(data = deseqres, 
                 mapping = aes_string(
                   x=av(input$x_diffabund), 
                   y="stat",
                   color=av(input$color_diffabund),
                   shape=av(input$shape_diffabund))) 
      p = p + geom_point(size=input$size_diffabund, alpha=input$alpha_diffabund)
      p = p + theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5))
      p = p + ggtitle(label = "DESeq2")
      if(!is.null(av(input$pal_diffabund))){
        p <- p + scale_colour_brewer(palette = av(input$pal_diffabund)) 
      }
      if(!is.null(av(input$theme_diffabund))){
        p <- p + shiny_phyloseq_ggtheme_list[[av(input$theme_diffabund)]]
      }
      return(p)
    } else {
      return(NULL)
    }
  }
})

finalize_diffabund_plot = reactive({
  pda = NULL
  trash <- try({pda <- make_diffabund_plot()}, silent = TRUE)
  if(inherits(pda, "ggplot")){
    return(pda)
  } else {
    # If for any reason pda is not a ggplot at this point,
    # render fail-plot rather than add layers
    return(fail_gen())
  }
})

#   # Pretend you had other methods included
#   daPlotList = list(pda, pda, pda, NULL, pda)
#   DAggplotindices = sapply(daPlotList, inherits, "ggplot")
#   if( any(DAggplotindices) ){
#     # If any legit ggplots
#     # Remove all but ggplots
#     daPlotList = daPlotList[DAggplotindices]
#     return(daPlotList)
#   p = finalize_diffabund_plot()
#   if(length(p) > 1){
#     do.call(gridExtra::grid.arrange, p)
#   } else {
#     shiny_phyloseq_print(p)
#   }
# Render plot in panel and in downloadable file with format specified by user selection
output$diffabund <- renderPlot({
  #shiny_phyloseq_print(finalize_diffabund_plot())
  p = finalize_diffabund_plot()
  p = list(p, p, p)
  if(length(p) > 1){
    do.call(gridExtra::grid.arrange, c(p, list(ncol=input$diffabund_ncol)))
  } else {
    shiny_phyloseq_print(p)
  }  
}, width=function(){72*input$width_diffabund},
   height=function(){72*input$height_diffabund}
)
output$downloaddiffabund <- downloadHandler(
  filename = function(){paste0("diffabund_", simpletime(), ".", input$downtype_diffabund)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_diffabund_plot(),
            device=input$downtype_diffabund,
            width=input$width_diffabund,
            height=input$height_diffabund, dpi=300L, units="in")
  }
)
