# The main reactive data object. Returns a phyloseq-class instance.
physeq = reactive({
  ps0 = get_phyloseq_data()
  if(input$actionb_filter == 0){
    # Don't execute filter if filter-button has never been clicked.
    if(inherits(ps0, "phyloseq")){
      return(ps0)
    } else {
      return(NULL)
    }
  }
  # Isolate all filter code so that button click is required for update
  isolate({
    if(inherits(ps0, "phyloseq")){
      # Expression filters
      if( !is.null(av(input$filter_subset_taxa_expr)) ){
        ps0 = eval(parse(text=paste0("subset_taxa(ps0, ", input$filter_subset_taxa_expr, ")")))
      }
      if( !is.null(av(input$filter_subset_samp_expr)) ){
        ps0 = eval(parse(text=paste0("subset_samples(ps0, ", input$filter_subset_samp_expr, ")")))
      }
      if( input$filter_taxa_sums_threshold > 0 ){
        # OTU sums filter
        ps0 <- prune_taxa({taxa_sums(ps0) > input$filter_taxa_sums_threshold}, ps0)
      }
      if( input$filter_sample_sums_threshold > 0 ){
        # Sample sums filtering
        ps0 <- prune_samples({sample_sums(ps0) > input$filter_sample_sums_threshold}, ps0)
      }
      if(inherits(input$filter_kOverA_sample_threshold, "numeric")){
        if(input$filter_kOverA_sample_threshold > 1){
          # kOverA OTU Filtering
          flist = genefilter::filterfun(
            genefilter::kOverA(input$filter_kOverA_sample_threshold,
                               input$filter_kOverA_count_threshold, na.rm=TRUE)
          )
          ps0 <- filter_taxa(ps0, flist, prune=TRUE)
        }
      }
      return(ps0)
    } else {
      return(NULL)
    }
  })
})
# Define a proportions-only version of input phyloseq object
physeqProp = reactive({
  if(is.null(physeq())){
    return(NULL)
  }
  return(transform_sample_counts(physeq(), function(x){x / sum(x)}))
})
# kOverA `k` Filter UI
maxSamples = reactive({
  # Create logical indicated the samples to keep, or dummy logical if nonsense input
  if(inherits(get_phyloseq_data(), "phyloseq")){
    return(nsamples(get_phyloseq_data()))
  } else {
    # Dummy response.
    return(NULL)
  }
})
output$filter_ui_kOverA_k <- renderUI({
  numericInput("filter_kOverA_sample_threshold",
               "`k` - Number of Samples that Must Exceed `A`",
               min=0, max=maxSamples(), value=kovera_k, step=1)    
})  
output_phyloseq_print_html <- reactive({
  HTML(paste0(capture.output(print(get_phyloseq_data())), collapse=" <br/> "))
})
output$contents <- renderUI({
  output_phyloseq_print_html()
})
output$filtered_contents0 <- renderUI({
  output_phyloseq_print_html()
})
output$filtered_contents <- renderUI({
  HTML(paste0(capture.output(print(physeq())), collapse=" <br/> "))
})
# Generic Function for plotting marginal histograms
sums_hist = function(thesums=NULL, xlab="", ylab=""){
  if(is.null(thesums)){
    p = qplot(0)
  } else {
    p = ggplot(data.frame(sums=thesums), aes(x=sums))
    p = p + geom_histogram()
    p = p + xlab(xlab) + ylab(ylab) 
    p = p + scale_x_log10(labels = scales::comma)
  }
  return(p)
}
lib_size_hist = reactive({
  xlab = "Number of Reads (Counts)"
  ylab = "Number of Libraries"
  return(sums_hist(sample_sums(get_phyloseq_data()), xlab, ylab))
})
otu_sum_hist = reactive({
  xlab = "Number of Reads (Counts)"
  ylab = "Number of OTUs"
  return(sums_hist(taxa_sums(get_phyloseq_data()), xlab, ylab))    
})
output$sample_variables <- renderText({return(
  paste0(sample_variables(get_phyloseq_data(), errorIfNULL=FALSE), collapse=", ")
)})
output$rank_names <- renderText({return(
  paste0(rank_names(get_phyloseq_data(), errorIfNULL=FALSE), collapse=", ")
)})
output$filter_summary_plot <- renderPlot({
  plib0 = lib_size_hist() + ggtitle("Original Data")
  potu0 = otu_sum_hist() + ggtitle("Original Data")
  if(inherits(physeq(), "phyloseq")){
    potu1 = sums_hist(taxa_sums(physeq()), xlab = "Number of Reads (Counts)",
                      ylab = "Number of OTUs"
    ) + 
      ggtitle("Filtered Data")
    plib1 = sums_hist(sample_sums(physeq()), xlab = "Number of Reads (Counts)",
                      ylab = "Number of Libraries"
    ) + 
      ggtitle("Filtered Data")
  } else {
    potu1 = plib1 = fail_gen()
  }
  gridExtra::grid.arrange(plib0, potu0, plib1, potu1, ncol=2, 
                          main="Histograms: Before and After Filtering")
})