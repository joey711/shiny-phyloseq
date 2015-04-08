################################################################################
# UI subset_taxa expression cascade
# filter_subset_taxa_expr
################################################################################
output$filter_uix_subset_taxa_ranks <- renderUI({
  rankNames = list("NULL"="NULL")
  rankNames <- c(rankNames, as.list(rank_names(get_phyloseq_data(), errorIfNULL=FALSE)))
  rankNames <- c(rankNames, list(OTU="OTU"))
  return(
    selectInput("filter_rank", "Taxonomic Ranks", rankNames, "NULL", multiple = FALSE)
  )
})
output$filter_uix_subset_taxa_select <- renderUI({
  rank_list = list("NULL" = "NULL")
  if(!is.null(av(input$filter_rank))){
    # If a filter rank is specified, use it, and provide the multi-select widget for these rank classes
    if(input$filter_rank == "OTU"){
      rank_list <- c(rank_list, as.list(taxa_names(get_phyloseq_data())))
    } else {
      rank_list <- c(rank_list, as.list(get_taxa_unique(get_phyloseq_data(), input$filter_rank)))
    }
  }
  return(
    selectInput(inputId = "filter_rank_selection", label = "Select Taxa",
                choices = rank_list, selected = "NULL", multiple = TRUE)
  )
})
################################################################################
# UI subset_samples expression cascade
# filter_subset_samp_expr
################################################################################
output$filter_uix_subset_sample_vars <- renderUI({
  sampVars = list("NULL"="NULL")
  sampVars <- c(sampVars, as.list(sample_variables(get_phyloseq_data(), errorIfNULL=FALSE)))
  sampVars <- c(sampVars, list(Sample="Sample"))
  return(
    selectInput("filter_samvars", "Sample Variables", sampVars, "NULL", multiple = FALSE)
  )
})
output$filter_uix_subset_sample_select <- renderUI({
  varLevels = list("NULL"="NULL")
  if(!is.null(av(input$filter_samvars))){
    if(input$filter_samvars == "Sample"){
      varLevels <- c(varLevels, as.list(sample_names(get_phyloseq_data())))
    } else {
      if(!is.null(sample_variables(get_phyloseq_data(), FALSE))){
        varvec = get_variable(get_phyloseq_data(), input$filter_samvars)
        if(plyr::is.discrete(varvec)){
          varLevels <- c(varLevels, as.list(unique(as(varvec, "character"))))
        }
      } 
    }
  }
  return(
    selectInput(inputId = "filter_samvars_selection", label = "Variable Classes",
                choices = varLevels, selected = "NULL", multiple = TRUE)
  )  
})
################################################################################
# The main reactive data object. Returns a phyloseq-class instance.
# This is considered the "filtered" data, used by all downstream panels,
# And generally the input to any transformation options as well
################################################################################
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
      # Cascading selection filters
      if( !is.null(av(input$filter_rank_selection)) ){
        keepTaxa = NULL
        if(!is.null(tax_table(ps0, FALSE))){
          if(input$filter_rank == "OTU"){
            # OTU IDs directly
            keepTaxa = input$filter_rank_selection
          } else {
            TT = as(tax_table(ps0), "matrix")
            keepTaxa = TT[, input$filter_rank] %in% input$filter_rank_selection 
          }
          if(length(keepTaxa) > 1){
            ps0 <- prune_taxa(keepTaxa, ps0)
          } else {
            warning("Bad subset_taxa specification. ntaxa(ps0) one or fewer OTUs")
          }
        }
      }
      if( !is.null(av(input$filter_samvars_selection)) ){
        keepSamples = NULL
        if(!is.null(sample_data(ps0, FALSE))){
          if(input$filter_samvars == "Sample"){
            # Samples IDs directly
            keepSamples = input$filter_samvars_selection
          } else {
            varvec = as(get_variable(ps0, input$filter_samvars), "character")
            keepSamples = varvec %in% input$filter_samvars_selection 
          }
          if(length(keepSamples) > 1){
            ps0 <- prune_samples(keepSamples, ps0)
          } else {
            warning("Bad subset_taxa specification. ntaxa(ps0) one or fewer OTUs")
          }
        }
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
          koatry = try(ps0 <- filter_taxa(ps0, flist, prune=TRUE), silent = TRUE)
          if(inherits(koatry, "try-error")){
            warning("kOverA parameters resulted in an error, kOverA filtering skipped.")
          }
        }
      }
      return(ps0)
    } else {
      return(NULL)
    }
  })
})
################################################################################
################################################################################
# TRANSFORMATIONS
################################################################################
################################################################################
# Proportional transformation
################################################################################
physeqProp = reactive({
  if(is.null(physeq())){
    return(NULL)
  }
  return(
    transform_sample_counts(physeq(), function(x){x / sum(x)})
  )
})
################################################################################
# Regularized Log Transformation (blind, fast)
################################################################################
physeqRLog = reactive({
  if(is.null(physeq())){
    return(NULL)
  }
  ps0RLog = physeq()
  # Demo a "blind" transformation, with design formula 
  # only containing an intercept term.
  # This will often throw a warning. DESeq2 usually handles the condition fine
  dds = phyloseq_to_deseq2(ps0RLog, ~ 1)
  rld <- DESeq2::rlog(dds, blind = TRUE, fast = TRUE)
  rlogMat <- GenomicRanges::assay(rld)
  otu_table(ps0RLog) <- otu_table(rlogMat, taxa_are_rows = TRUE)
  return(ps0RLog)
})
################################################################################
# Centered Log-Ratio (CLR) Transformation
################################################################################
gm_mean = function(x, na.rm=TRUE){
  # The geometric mean, with some error-protection bits.
  exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
}
clr = function(x, base=2){
  x <- log((x / gm_mean(x)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}
physeqCLR = reactive({
  if(is.null(physeq())){
    return(NULL)
  }
  return(
    transform_sample_counts(physeq(), fun = clr)
  )
})
################################################################################
# Misc Filter-tab server code
################################################################################
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
  numericInputRow("filter_kOverA_sample_threshold", "k",
               min=0, max=maxSamples(), value=kovera_k, step=1, class="col-md-12")
})
output$contents <- renderUI({
  output_phyloseq_print_html(get_phyloseq_data())
})
output$filtered_contents0 <- renderUI({
  output_phyloseq_print_html(get_phyloseq_data())
})
output$filtered_contents <- renderUI({
  output_phyloseq_print_html(physeq())
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
  gridExtra::grid.arrange(plib0, potu0, plib1, potu1, ncol=2) #, main="Histograms: Before and After Filtering")
})
################################################################################
# Component Table
################################################################################
output$uix_available_components_filt <- renderUI({
  selectInput("available_components_filt", "Available Components",
              choices = component_options(physeq()))
})
# Render the user-selected data component using DataTables
output$physeqComponentTable <- renderDataTable({
  if(is.null(av(input$available_components_filt))){
    return(NULL)
  }
  component = do.call(what = input$available_components_filt, args = list(physeq()))
  return(tablify_phyloseq_component(component, input$component_table_colmax_filt))
}, options = list(
  pageLength = 5 
))
################################################################################