################################################################################
# Options, default settings, and load packages
################################################################################
# load packages
library("shiny"); packageVersion("shiny")
library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")
library("data.table"); packageVersion("data.table")
library("d3Network"); packageVersion("d3Network")
# Default options for app startup
source("default-parameters.R", local=FALSE)
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)
theme_set(theme_bw())
pal = "Set1"
scale_colour_discrete <- function(palname = pal, ...) {
  scale_colour_brewer(palette = palname, ...)
}
scale_fill_discrete <- function(palname = pal, ...) {
  scale_fill_brewer(palette = palname, ...)
}
################################################################################
# Included Data
# Define the named list of datasets to choose from
################################################################################
includedDatasets = c("GlobalPatterns", "enterotype", "esophagus", "soilrep")
data(list=includedDatasets)
datalist = list(GlobalPatterns=GlobalPatterns, 
                enterotype=enterotype,
                esophagus=esophagus,
                soilrep=soilrep)

filepath = system.file("extdata", "study_1457_split_library_seqs_and_mapping.zip", package="phyloseq")
kostic = microbio_me_qiime(filepath)
if(inherits(kostic, "phyloseq")){
  datalist <- c(list(study_1457_Kostic=kostic), datalist)
}
# filepath <- system.file("extdata", "study_816_split_library_seqs_and_mapping.tar.gz", package="phyloseq")
# study_816 = microbio_me_qiime(filepath)
# if(inherits(study_816, "phyloseq")){
#   datalist <- c(list(study_816=study_816), datalist)
# }
################################################################################
# Begin Shiny Server definition.
################################################################################
shinyServer(function(input, output){
  ################################################################################
  # Define the available phyloseq datasets for plotting.
  ################################################################################
  get_qiime_data = reactive({
    qiime_data = NULL
    if(!is.null(av(input$qiime_server_ID))){
      if( !is.na(as.integer(input$qiime_server_ID)) ){
        observe({print(paste0("Attempting integer ID import: ", input$qiime_server_ID))})
        zipftp = as(isolate({input$qiime_server_ID}), "integer")
        studyname = input$qiime_server_ID
      } else {
        observe({print(paste0("Attempting character ID import: ", input$qiime_server_ID))})
        zipftp = as(isolate({input$qiime_server_ID}), "character")
        studyname = gsub("\\_split\\_.+$", "", basename(zipftp))
      }
      observe({print(paste0("Extension Chosen: ", input$qiime_server_ext))})
      trash = try({qiime_data <- microbio_me_qiime(zipftp, ext=input$qiime_server_ext)}, silent=TRUE)
    }
    if(inherits(qiime_data, "phyloseq")){
      qiime_data <- list(qiime_data)
      names(qiime_data) <- studyname
      datalist <<- c(qiime_data, datalist)
    } else {
      observe({print("Attempt made to access qiime server data, but didn't work this pass...")})
    }
    return(NULL)
  })
  get_loaded_data = reactive({
    if(!is.null(input$file1$name)){
      # Added uploaded data, if provided, and it is phyloseq-class.
      objectNames = load(input$file1$datapath)
      loadedObjects = mget(objectNames)
      arePhyloseq = sapply(loadedObjects, inherits, "phyloseq")
      if(any(arePhyloseq)){
        loadedObjects <- loadedObjects[which(arePhyloseq)]
      } else {
        loadedObjects <- NULL
      }
      datalist <<- c(loadedObjects, datalist)
      observe({print(paste("Available objects in datalist:", names(datalist), collapse=", "))})
    }
    return(NULL)
  })
  output$phyloseqDataset <- renderUI({
    # Expect the side-effect of these two functions to be to add
    # elements to the datalist, if appropriate
    get_loaded_data()
    get_qiime_data()
    return(radioButtons("physeqSelect", "Available Datasets:", names(datalist)))
  })
  get_phyloseq_data = reactive({
    ps0 = NULL
    if(!is.null(input$physeqSelect)){
      if(input$physeqSelect %in% names(datalist)){
        ps0 <- datalist[[input$physeqSelect]]
      }
    }
    observe({print(ps0)})
    if(inherits(ps0, "phyloseq")){
      return(ps0)
    } else {
      observe({print("ps0 is NULL in get_phyloseq_data()")})
      return(NULL)
    }
  })
  ################################################################################
  # Filtering
  ################################################################################
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
                min=0, max=maxSamples(), value=0, step=1)    
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
              theme(panel.border=element_blank(), axis.line=element_blank(),
                    axis.text=element_blank(), axis.ticks=element_blank())
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
  ################################################################################
  # Data-Reactive UI Definitions.
  ################################################################################
  # Define data-reactive variable lists
  rankNames = reactive({
    rankNames = as.list(rank_names(physeq(), errorIfNULL=FALSE))
    names(rankNames) <- rankNames
    return(rankNames)
  })
  variNames = reactive({
    variNames = as.list(sample_variables(physeq(), errorIfNULL=FALSE))
    names(variNames) <- variNames
    return(variNames)
  })
  vars = function(type="both", withnull=TRUE, singles=FALSE){
    if(!type %in% c("both", "taxa", "samples")){
      stop("incorrect `type` specification when accessing variables for UI.")
    }
    returnvars = NULL
    if(type=="samples"){
      if(singles){
        returnvars <- c(list(Sample="Sample"), variNames())
      } else {
        returnvars <- variNames()
      }
    }
    if(type=="taxa"){
      if(singles){
        returnvars <- c(rankNames(), list(OTU="OTU"))
      } else {
        returnvars <- rankNames()
      }
    } 
    if(type=="both"){
      # Include all variables
      if(singles){
        returnvars <- c(rankNames(), variNames(), list(OTU="OTU", Sample="Sample"))
      } else {
        returnvars <- c(rankNames(), variNames())
      }
    }
    if(withnull){
      # Put NULL first so that it is default when `select` not specified
      returnvars <- c(list("NULL"="NULL"), returnvars)
    }
    return(returnvars)
  }
  # A generic selectInput UI. Plan is to pass a reactive argument to `choices`.
  uivar = function(id, label="Variable:", choices, selected="NULL"){
    selectInput(inputId=id, label=label, choices=choices, selected=selected)
  }
  ################################################################################
  # plot_ordination() ui
  ################################################################################
  output$ord_uix_color <- renderUI({
    selectInput("color_ord", "Color Variable:", vars(input$type_ord), "NULL")
  })
  output$ord_uix_shape <- renderUI({
    selectInput("shape_ord", "Shape Variable:", vars(input$type_ord), "NULL")
  })
  ################################################################################
  # plot_richness() ui
  ################################################################################
  output$richness_uix_x <- renderUI({
    uivar("x_alpha", "Horizontal (x) Variable:", vars("samples"))
  })
  output$richness_uix_color <- renderUI({
    uivar("color_alpha", "Color Variable:", vars("samples"))
  })
  output$richness_uix_shape <- renderUI({
    uivar("shape_alpha", "Shape Variable:", vars("samples"))
  })
  ################################################################################
  # plot_network() ui
  ################################################################################
  output$network_uix_color <- renderUI({
    uivar("color_net", "Color Variable:", vars(input$type_net))
  })
  output$network_uix_shape <- renderUI({
    uivar("shape_net", "Shape Variable:", vars(input$type_net))
  })
  ################################################################################
  # plot_bar() uix
  ################################################################################
  output$bar_uix_xvar <- renderUI({
    selectInput("x_bar", "Horizontal ('x') Variable:", 
                vars("both", TRUE, TRUE), "Sample")
  })
  output$bar_uix_colvar <- renderUI({
    selectInput("color_bar", "Color Fill Variable:", vars("both"))
  })
  ################################################################################
  # plot_tree() ui
  ################################################################################
  output$tree_uix_color <- renderUI({
    uivar("color_tree", "Color Variable:", vars("both", TRUE, TRUE))
  })
  output$tree_uix_shape <- renderUI({
    uivar("shape_tree", "Shape Variable:", vars("both", TRUE, TRUE))
  })
  output$tree_uix_tiplabs <- renderUI({
    uivar("label_tip_tree", "Tip Labels", choices=vars("taxa", TRUE, TRUE))
  })
  output$tree_uix_point_thresh <- renderUI({
    sliderInput("abundance_threshold_tree",
                "Count Minimum Threshold for Points",
                value=0.1*median(as(otu_table(physeq()), "matrix"), na.rm=TRUE),
                max=max(as(otu_table(physeq()), "matrix"), na.rm=TRUE),
                min=min(as(otu_table(physeq()), "matrix"), na.rm=TRUE)
    )
  })
  ################################################################################
  # plot_heatmap() ui
  ################################################################################
  output$heat_sample_label <- renderUI({
    uivar("sample.label", "Sample Labels:", vars("samples"))
  })
  output$heat_taxa_label <- renderUI({
    uivar("taxa.label", "Taxa Labels:", vars("taxa"))
  })
  output$heat_sample_order <- renderUI({
    uivar("sample.order", "Sample Ordering:", vars("samples"))
  })
  output$heat_taxa_order <- renderUI({
    uivar("taxa.order", "Taxa Ordering:", vars("taxa"))
  })
  ################################################################################
  # scatterplot ui
  ################################################################################
  output$scat_uix_x <- renderUI({
    selectInput(inputId="x_scat", label="Horizontal ('x') Variable:", 
                choices=c(list(Abundance="Abundance"), vars()),
                selected="Sample")
  })
  output$scat_uix_y <- renderUI({
    selectInput(inputId="y_scat", label="Vertical ('y') Variable:", 
                choices=c(list(Abundance="Abundance"), vars()),
                selected="Abundance")
  })
  output$scat_uix_color <- renderUI({
    uivar("color_scat", "Color Variable:", vars())
  })
  output$scat_uix_shape <- renderUI({
    uivar("shape_scat", "Shape Variable:", vars())
  })
  ################################################################################
  # d3network uix
  ################################################################################
  output$d3_uix_color <- renderUI({
    selectInput("color_d3", "Color Variable:",
                choices = vars(input$type_d3, TRUE, TRUE),
                selected = d3NetworkColorVar)
  })
  output$d3_uix_node_label <- renderUI({
    selectInput("d3_node_label", "Node Label (hover):",
                choices = vars(input$type_d3, TRUE, TRUE),
                selected = d3NodeLabelVar,
                multiple = TRUE)
  }) 
  ################################################################################
  # Plot Rendering Stuff.
  ################################################################################ 
  # Define a proportions-only version of input phyloseq object
  physeqProp = reactive({transform_sample_counts(physeq(), function(x){x / sum(x)})})
  # Define a dummy "failed plot" to return if render section cannot build valid plot.
  fail_gen = function(main="Graphic Fail.", 
                      subtext="Please change parameters and try again."){
    qplot(x=0, y=0, main=main) + 
      annotate("text", 0, 0, label=":-(",
               size=75, angle=270, hjust=0.5, vjust=0.35) +
      annotate("text", 0, 0, label=subtext, size=10, hjust=0.5, vjust=-7) +
      theme_bw() + 
      theme(panel.border=element_blank(), axis.line=element_blank(),
            axis.text=element_blank(), axis.ticks=element_blank())
  }
  failp = fail_gen()
  # Define a default controlled ggplot printing check for all print rendering
  shiny_phyloseq_print = function(p, f=failp){
    if(inherits(p, "ggplot")){
      # Check that rendering will work
      printout = NULL
      try(printout <- print(p), silent=TRUE)
      if(is.null(printout)){
        # If still NULL, the print-render failed,
        # otherwise print() would have returned a 'list'
        # Nothing was printed. Print the fail graphic in its place.
        print(f)
      }
    } else {
      print(f)
    }
  }    
  # Define generic function to access/clean variables
  # This especially converts "NULL" to NULL
  av = function(x){
    if( isTRUE(all.equal(x, "")) | isTRUE(all.equal(x, "NULL")) ){
      return(NULL)
    }
    return(x)
  }
  ################################################################################
  # Ordination section
  ################################################################################
  get_formula <- reactive({
    if(is.null(av(input$formula)) | input$formula=="NULL"){
      return(NULL)
    } else {
      return(as.formula(input$formula))
    }
  })
  #observe({print(paste0("formula argument: ", get_formula()))})
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
    ordinate(physeq(), method=input$ord_method, distance=gdist(), formula=get_formula())
  })
  make_ord_plot = reactive({
    p1 = NULL
    try(p1 <- plot_ordination(physeq(), get_ord(), type=input$ord_plot_type), silent=TRUE)
    return(p1)
  })
  # ordination plot definition
  output$ordination <- renderPlot({
    p1 = make_ord_plot()
    if(inherits(p1, "ggplot")){
      p1$layers[[1]]$geom_params$size <- av(input$size_ord)
      p1$layers[[1]]$geom_params$alpha <- av(input$alpha_ord)
      if(!is.null(av(input$color_ord))){
        p1$mapping$colour <- as.symbol(av(input$color_ord))
      }
      if(!is.null(av(input$shape_ord))){
        p1$mapping$shape  <- as.symbol(av(input$shape_ord))
      }
    }
    shiny_phyloseq_print(p1)
  }, width=700, height=700)    
  ################################################################################
  # bar plot definition
  ################################################################################
  physeq_bar = reactive({
    return(switch({input$uicttype_bar}, Counts=physeq(), Proportions=physeqProp()))
  })
  get_facet <- reactive({
    if(is.null(input$facform_bar) | input$facform_bar=="NULL"){
      return(NULL)
    } else {
      return(as.formula(input$facform_bar))
    }
  })
  make_bar_plot = reactive({
    psb = physeq_bar()
    p0 = NULL
    try(p0 <- plot_bar(psb, x=input$x_bar, y="Abundance", fill=av(input$color_bar), 
                       facet_grid=get_facet()),
        silent=TRUE)
    if(!inherits(p0, "ggplot")){
      warning("Could not render bar plot, attempting without faceting...")
      try(p0 <- plot_bar(psb, x=xvar, y="Abundance", fill=av(input$color_bar)),
          silent=TRUE)
    }
    return(p0)
  })
  output$bar <- renderPlot({
    shiny_phyloseq_print(make_bar_plot())
  }, width=700, height=400)
  ################################################################################
  # phylogenetic tree plot definition
  ################################################################################
  filter_phyloseq = reactive({
    filterPhyseq = physeq()
    observe({print(paste0("tree obs min threshold: ", input$abundance_threshold_tree))})
    observe({print(paste0("Class of tree obs min threshold: ", class(input$abundance_threshold_tree)))})
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
  output$tree <- renderPlot({
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
    shiny_phyloseq_print(p2)
  }, width=700, height=700)
  ################################################################################
  # heatmap plot definition
  ################################################################################
  physeq_heat = reactive({
    return(switch(input$uicttype_heat, Counts=physeq(), Proportions=physeqProp()))
  })
  make_heatmap = reactive({
    p3 = NULL
    try(p3 <- plot_heatmap(physeq_heat(), method=input$ord_method_heat, distance=input$dist_heat,
                           sample.label=av(input$sample.label),
                           taxa.label=av(input$taxa.label),
                           sample.order=av(input$sample.order),
                           taxa.order=av(input$taxa.order),
                           low = input$locolor_heat,
                           high = input$hicolor_heat,
                           na.value = input$NAcolor_heat),
        silent=TRUE)
    return(p3)
  })
  output$heatmap <- renderPlot({
    shiny_phyloseq_print(make_heatmap())
  }, width=700, height=700)
  ################################################################################
  # Alpha Diversity plot definition
  ################################################################################
  make_richness_plot = reactive({
    p4 = NULL
    try(p4 <- plot_richness(physeq(), x=av(input$x_alpha),
                            color=av(input$color_alpha),
                            shape=av(input$shape_alpha),
                            measures=input$measures_alpha),
        silent=TRUE)
    return(p4)
  })
  output$richness <- renderPlot({
    p4 = make_richness_plot()
    if(inherits(p4, "ggplot")){
      # Adjust size/alpha of points, but not error bars
      p4$layers[[1]]$geom_params$size <- input$size_alpha
      p4$layers[[1]]$geom_params$alpha <- input$alpha_alpha
      shiny_phyloseq_print(p4)
    } else {
      # If for any reason p4 is not a ggplot at this point,
      # render fail-plot rather than tinker with innards.
      print(failp)
    }
  }, width=700, height=500)
  ################################################################################
  # Generate a network plot 
  ################################################################################
  # The reactive value version of the network.
  # Only returns distance matrix, regardless of distance-method argument
  distonly <- reactive({
    idist = NULL
    try({idist <- distance(physeq(), method=input$dist_net, type=input$type_net)}, silent=TRUE)
    if(is.null(idist)){warning("distonly: Could not calculate distance matrix with these settings.")}
    # rescale the distance matrix to be [0, 1]
    idist <- idist / max(idist, na.rm=TRUE)
    idist <- idist - min(idist, na.rm=TRUE)
    return(idist)
  })
  ## Changes only if input$uinetdistmax changes, or the reactive distance matrix, gdist()
  #observe({print(paste0("Network Max Distance (input$uinetdistmax): ", input$uinetdistmax))})
  #observe({print(paste0("Network Min Distance (input$uinetdispdist): ", input$uinetdispdist))})
  ig <- reactive({
    make_network(physeq(),
                 type=input$type_net,
                 distance=distonly(),
                 max.dist=input$uinetdistmax,
                 keep.isolates=FALSE)
  })
  #print(paste0("Class of ig(): ", class(isolate(ig()))))
  initial_plot_network = reactive({
    plot_network(ig(), physeq(),
                 type=input$type_net,
                 color=isolate(input$color_net),
                 shape=isolate(input$shape_net),
                 point_size=isolate(input$size_net),
                 alpha=isolate(input$alpha_net)
    )
  })
  get_edge_df = reactive({
    p = initial_plot_network()
    whichEdge = which(sapply(p$layers, function(x){x$geom$objname=="line"}))
    edgeDF0 = p$layers[[whichEdge]]$data
    # Add the distance associated with each edge entry
    edgeDF0 = plyr::ddply(edgeDF0, "id", function(df, dmat){
      df$dist <- dmat[df$value[1], df$value[2]]
      return(df)
    }, dmat = as.matrix(distonly()))
    return(edgeDF0)
  })
  update_plot_network = reactive({
    p = initial_plot_network()
    # New edge layer
    # Define the layer that contains the edges and vertices
    whichEdge = which(sapply(p$layers, function(x){x$geom$objname=="line"}))
    # Define the edge data frame
    edgeDF0 = get_edge_df()
    # Subset newEdgeDF according to max allowed distance
    newEdgeDF = edgeDF0[edgeDF0$dist <= input$uinetdispdist, ]
    newEdgeMap = aes_string(x="x", y="y", group="id", colour=input$color_net)
    newEdgeLayer = geom_line(mapping=newEdgeMap, data=newEdgeDF, alpha=input$alpha_net)
    p$layers[[whichEdge]] <- newEdgeLayer
    # New vertex layer
    whichVert = which(sapply(p$layers, function(x){x$geom$objname=="point"}))
    # Subset
    newVertDF = p$data[as.character(p$data$value) %in% as.character(newEdgeDF$value), ]
    newVertMap = aes_string(x="x", y="y", colour=input$color_net, shape=input$shape_net)
    newVertLayer = geom_point(mapping=newVertMap, data=newVertDF,
                              size=input$size_net, alpha=input$alpha_net)
    p$layers[[whichVert]] <- newVertLayer
    # New label layer
    # Re-define the subset 
    whichLabel = which(sapply(p$layers, function(x){x$geom$objname=="text"}))
    p$layers[[whichLabel]]$data <- newVertDF
    # Updates legend labels.
    p = update_labels(p, list(colour=input$color_net))
    p = update_labels(p, list(shape=input$shape_net))
    # Fix the coordinate ranges based on the original.
    p = p + xlim(I(range(edgeDF0$x, na.rm=TRUE, finite=TRUE)))
    p = p + ylim(I(range(edgeDF0$y, na.rm=TRUE, finite=TRUE)))
    return(p)
  })
  output$network <- renderPlot({
    shiny_phyloseq_print(update_plot_network())
  }, width=700, height=700)
  ################################################################################
  # Flexible Scatter plot
  ################################################################################
  physeq_scat = reactive({
    return(switch(input$uicttype_scat, Counts=physeq(), Proportions=physeqProp()))
  })
  make_scatter_plot = reactive({
    pscat = NULL
    try({
      scatmap = aes_string(x=av(input$x_scat), y=av(input$y_scat),
                           color=av(input$color_scat), shape=av(input$shape_scat))
      scatdf = psmelt(physeq_scat())
      pscat = ggplot(data=scatdf, mapping=scatmap) + geom_point()
      pscat = pscat + theme(axis.text.x=element_text(angle=-90, vjust=0.5, hjust=0)) 
      if(!is.null(av(input$facform_scat))){
        # Add facet_grid layer if user-provided one
        # # Need to add a user-toggle for free_x and free_y panels.
        pscat <- pscat + facet_grid(facets=as.formula(av(input$facform_scat)))
      }
    }, silent=TRUE)
    return(pscat)
  })
  output$scatter <- renderPlot({
    pscat = make_scatter_plot()
    if(inherits(pscat, "ggplot")){
      # Adjust size/alpha of points, but not error bars
      pscat$layers[[1]]$geom_params$size <- input$size_scat
      pscat$layers[[1]]$geom_params$alpha <- input$alpha_scat
      shiny_phyloseq_print(pscat)
    } else {
      # If for any reason pscat is not a ggplot at this point,
      # render fail-plot rather than tinker with innards.
      print(failp)
    }
  }, width=700, height=700)
  ################################################################################
  # d3 interactive network graphic 
  ################################################################################
  # Define global reactive distance matrix. 
  # Re-calc only if method or plot-type change.
  d3distReact <- reactive({
    try({idist <- distance(physeq(), method=input$dist_d3, type=input$type_d3)}, silent=TRUE)
    if(is.null(idist)){warning("d3dist: Could not calculate distance matrix with these settings.")}
    return(idist)
  })  
  calculate_links_data = reactive({
    d3dist <- as.matrix(d3distReact())
    # Set duplicate entries and self-links to Inf
    d3dist[upper.tri(d3dist, diag = TRUE)] <- Inf
    # Create data.table.
    d3LinkNames = c("Source", "target")
    LinksData = data.table(reshape2::melt(d3dist, varnames=d3LinkNames, as.is = TRUE))
    # Remove entries above the threshold
    # (This will also remove self-links and duplicate links)
    LinksData <- LinksData[value < input$dist_d3_threshold, ]
    # Rescale remaining links
    LinksData[, value:=(0.1+input$d3_link_scale*(value-min(value))/max(value))]
    # Don't sort yet, instead create mapping variable from Source ID to link node ID
    # d3link nodes are numbered from 0.
    nodeUnion = union(LinksData$Source, LinksData$target)
    d3lookup = (0:(length(nodeUnion)-1))
    names(d3lookup) <- nodeUnion
    # In-place replacement.
    LinksData[, Source:=d3lookup[Source]]
    LinksData[, target:=d3lookup[target]]
    # Order by the `d3lookup` node ID, in this case, the Source label
    setkey(LinksData, Source)
    # Create covariates table (taxa in this case)
    if(input$type_d3 == "taxa"){
      NodeData = data.frame(OTU=nodeUnion, tax_table(physeq())[nodeUnion, ], stringsAsFactors = FALSE)
    } else {
      NodeData = data.frame(Sample=nodeUnion, sample_data(physeq())[nodeUnion, ], stringsAsFactors = FALSE)      
    }
    NodeData$ShowLabels <- apply(NodeData[, input$d3_node_label, drop=FALSE], 1, paste0, collapse="; ")
    return(list(link=data.frame(LinksData), node=NodeData))
  })  
  default_Source = function(x){
    if(is.null(av(x))){
      if(input$type_d3=="taxa"){
        return("OTU")
      } else {
        return("Sample")
      }
    } else {
      return(x)
    }
  }
  # The d3Network output definition.
  output$networkPlot <- renderPrint({
    d3Network::d3ForceNetwork(
      Links = calculate_links_data()$link, 
      Nodes = calculate_links_data()$node,
      Source = "Source", Target = "target",
      Value = "value",
      NodeID = "ShowLabels",
      Group = default_Source(input$color_d3),
      linkColour = input$d3_link_color,
      opacity = input$d3_opacity,
      zoom = FALSE, 
      standAlone = FALSE, 
      width = input$width_d3, height = input$height_d3,
      parentElement = "#networkPlot")
    #zoom = as.logical(input$d3_zoom),
  })
})
