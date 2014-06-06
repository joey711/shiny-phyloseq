# The main reactive data object. Returns a phyloseq-class instance.
physeq = reactive({
  ps0 = get_phyloseq_data()
  if(inherits(ps0, "phyloseq")){
    observe({print(paste("filter_kOverA_count_threshold:", input$filter_kOverA_count_threshold))})
    observe({print(paste("filter_kOverA_sample_threshold:", input$filter_kOverA_sample_threshold))})
    observe({print(paste("filter_sample_sums_threshold:", input$filter_sample_sums_threshold))})
    observe({print(paste("filter_taxa_sums_threshold:", input$filter_taxa_sums_threshold))})
    observe({print(paste("filter_subset_taxa_expr:", input$filter_subset_taxa_expr))})
    observe({print(paste("filter_subset_samp_expr:", input$filter_subset_samp_expr))})
    # Expression filters
    if( !is.null(av(input$filter_subset_taxa_expr)) ){
      ps0 = eval(parse(text=paste0("subset_taxa(ps0, ", input$filter_subset_taxa_expr, ")")))
      observe({print("subset_taxa...")})
      observe({print(ps0)})
    }
    if( !is.null(av(input$filter_subset_samp_expr)) ){
      ps0 = eval(parse(text=paste0("subset_samples(ps0, ", input$filter_subset_samp_expr, ")")))
      observe({print("subset_samples...")})
      observe({print(ps0)})
    }
    if( input$filter_taxa_sums_threshold > 0 ){
      # OTU sums filter
      ps0 <- prune_taxa({taxa_sums(ps0) > input$filter_taxa_sums_threshold}, ps0)
      observe({print("prune OTUs...")})
      observe({print(ps0)})
    }
    if( input$filter_sample_sums_threshold > 0 ){
      # Sample sums filtering
      ps0 <- prune_samples({sample_sums(ps0) > input$filter_sample_sums_threshold}, ps0)
      observe({print("prune samples...")})
      observe({print(ps0)})
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
# Define a proportions-only version of input phyloseq object
physeqProp = reactive({transform_sample_counts(physeq(), function(x){x / sum(x)})})
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
output$library_sizes <- renderPlot({
  if(inherits(get_phyloseq_data(), "phyloseq")){
    libtitle = "Histogram of Library Sizes in Selected Data"
    p1 = lib_size_hist() + ggtitle(libtitle)
    otusumtitle = "Histogram of OTU total counts in Selected Data"
    p2 = otu_sum_hist() + ggtitle(otusumtitle)
    gridExtra::grid.arrange(p1, p2, ncol=2)
  } else {
    libfailtext = "Press the `Load Selection` button \n to load/refresh data."
    print(qplot(x=0, y=0, main="") + xlim(-1, 1) + ylim(-1, 1) + 
            geom_segment(aes(x=0, y=0, xend=-0.5, yend=0.15), size=3,
                         arrow=grid::arrow(length=grid::unit(0.5, "cm"))) +
            annotate("text", 0, 0, label=libfailtext, size=12, hjust=0.5, vjust=-1) +
            theme(line=element_blank(), text=element_blank(), panel.border=element_blank())
    )
  }
})
output$OTU_count_thresh_hist <- renderPlot({
  ps0 = get_phyloseq_data()
  if(inherits(get_phyloseq_data(), "phyloseq")){
    mx = as(otu_table(ps0), "matrix")
    if(!taxa_are_rows(ps0)){mx <- t(mx)}
    thresh = input$dataset_count_threshold
    df = data.frame(x=apply(mx, 1, function(x, thresh){sum(x>thresh)}, thresh))
    p = ggplot(df, aes(x=x)) + geom_histogram()
    p = p + xlab("Number of Samples with Count Above Threshold") + ylab("Number of OTUs")
    p = p + ggtitle(paste("Histogram of OTUs Observed More Than", thresh, "Times"))
    return(shiny_phyloseq_print(p))
  } else {
    return(qplot(0))
  }
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
    potu1 = plib1 = failp
  }
  gridExtra::grid.arrange(plib0, potu0, plib1, potu1, ncol=2, 
                          main="Histograms: Before and After Filtering")
})