# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)

shinyServer(function(input, output){
  # require packages
  require("shiny")
  require("phyloseq")
  require("ggplot2")
  require("plyr")
  ################################################################################
  # Define the available phyloseq datasets for plotting.
  ################################################################################
  includedDatasets = c("GlobalPatterns", "enterotype", "esophagus", "soilrep")
  data(list=includedDatasets)
  observe({print(paste0("top of server, available variables: ", ls()))})
  output$phyloseqDataset <- renderUI({
    # Always include the included datasets.
    observe({print(paste0("uploaded temporary file path: ", input$file1$datapath))})
    observe({print(paste0("uploaded file original name: ", input$file1$name))})
    datasets = includedDatasets
    if(!is.null(input$file1$name)){
      objectNames = load(input$file1$datapath)
      datasets <- c(objectNames, datasets)
      observe({print(paste("Object Names in File:", objectNames))})
    }
    observe({print(paste0("output$physeqDataset(): Available Variables:", ls(), collapse=" "))})
    return(radioButtons("physeqSelect", "Choose Dataset:", datasets))
  })
  physeq = reactive({
    physeq = NULL
    observe({print(paste0("physeq() input$physeqSelect:", input$physeqSelect))})
    if(!is.null(input$physeqSelect)){
      if(!input$physeqSelect %in% includedDatasets){
        observe({print("physeq(): Loading data...")})
        load(file=input$file1$datapath)
      } else {
        data(list=includedDatasets)
      }
      observe({print(paste0("physeq(): Available Variables:", ls(), collapse=" "))})
      trash = try(physeq <- get(input$physeqSelect), silent=TRUE)
      observe({print(paste0("physeq(): class(trash):", class(trash), collapse=" "))})
      observe({print("physeq(): physeq print...")})
      observe({print(physeq)})
      if(inherits(trash, "try-error")){
        warning("physeq(): Could not `get` selected object from server environment... \n")
      }
    }
    observe({print("physeq(): physeq-print")})
    observe({print(physeq)})
    if(inherits(physeq, "phyloseq")){
      return(physeq)
    } else {
      return(NULL)
    }
  })
  observe({print(paste0("prior to output$contents: Available Variables:", ls(), collapse=" "))})
  output$contents <- renderText({
    cat("Made it to output$contents... \n")
    observe({print(physeq())})
    return(paste0(capture.output(print(physeq())), collapse="; "))
  })
  ################################################################################
  # Data-Reactive UI Definitions.
  ################################################################################
  ################################################################################
  # Define the ordination list.
  ################################################################################
  ordlist = as.list(ordinate("list"))
  names(ordlist) <- ordlist
  ordlist = ordlist[-which(ordlist %in% c("MDS", "PCoA"))]
  ordlist = c(list("MDS/PCoA"="MDS"), ordlist)
  ################################################################################
  # User Interface, sidebar panel(s) `sbp`, etc.
  ################################################################################
  # ui submit button for input changes
  uibutton = submitButton("Build/Rebuild Plot", icon("refresh"))
  # Type for distance/network/etc. Samples or Taxa
  uitype = function(id="type", selected="taxa"){
    selectInput(inputId=id, label="Calculation: Samples or Taxa?",
                selected=selected,
                choices=list("Taxa"="taxa", "Samples"="samples"))
  }
  # Whether to use proportions or counts
  uicttype = function(id="uicttype"){
    radioButtons(inputId=id, label="Count Type",
                 choices=c("Counts", "Proportions"),
                 selected="Counts")
  } 
  ################################################################################
  # Define data-reactive variable lists
  ################################################################################
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
  sampvarlist = reactive({c(variNames(), list("NULL"="NULL"))})
  observe({print(paste0("Variables (`sampvarlist()`): ", sampvarlist(), collapse=" "))})
  specvarlist = reactive({c(rankNames(), list("NULL"="NULL"))})
  observe({print(paste0("Variables (`specvarlist()`): ", specvarlist(), collapse=" "))})
  vars = reactive({c(rankNames(), variNames(), list("NULL"="NULL"))})
  observe({print(paste0("Variables (`vars()`): ", vars(), collapse=" "))})
  ################################################################################
  #   ui for distance method
  #   NOTE: not all distance methods are supported if "taxa" selected for type. 
  #   For example, the UniFrac distance and DPCoA cannot be calculated for taxa-wise 
  #   distances, because they use a taxa-wise tree as part of their calculation 
  #   between samples, and there is no transpose-equivalent for this tree
  uidist = function(id="dist", selected="bray"){
    distlist = as.list(unlist(distance("list")))
    names(distlist) <- distlist
    selectInput(id, "Distance Method:", distlist, selected=selected)
  }
  # ui for point size slider
  uiptsz = function(id="size"){
    sliderInput(inputId=id, label="Point Size:", min=1, max=10, value=5, step=1)
  }
  # ui for point opacity slider
  uialpha = function(id="alpha"){
    sliderInput(inputId=id, label="Opacity:", min=0, max=1, value=1, step=0.1)
  }
  # A generic selectInput UI. Plan is to pass a reactive argument to `choices`.
  uivar = function(id, label="Variable:", choices, selected="NULL"){
    selectInput(inputId=id, label=label, choices=choices, selected=selected)
  }
  ################################################################################
  # Define reactive portions of sidebar panels.
  # They are reactively passed back to ui,
  # because they change based on the data contents.
  ################################################################################
  # plot_ordination() ui
  ################################################################################
  output$ord_uix_color <- renderUI({uivar("color_ord", "Color Variable:", vars())})
  output$ord_uix_shape <- renderUI({uivar("shape_ord", "Shape Variable:", vars())})
  ################################################################################
  # plot_richness() ui
  ################################################################################
  output$richness_uix_x <- renderUI({
    uivar("x_alpha", "Horizontal (x) Variable:", sampvarlist())
  })
  output$richness_uix_color <- renderUI({
    uivar("color_alpha", "Color Variable:", sampvarlist())
  })
  output$richness_uix_shape <- renderUI({
    uivar("shape_alpha", "Shape Variable:", sampvarlist())
  })
  ################################################################################
  # plot_network() ui
  ################################################################################
  # New
  output$network_uix_color <- renderUI({
    uivar("color_net", "Color Variable:", vars())
  })
  output$network_uix_shape <- renderUI({
    uivar("shape_net", "Shape Variable:", vars())
  })
#   # Old
#   # ui for max distance to consider in initializing plot calculations
#   uinetdistmax = numericInput(inputId="uinetdistmax", label="Network - Build Distance Threshold:",
#                               step=0.1, value=0.9)
#   # ui for distance to display
#   uinetdispdist = numericInput(inputId="uinetdispdist",
#                                label="Network - Edge Distance Display Threshold:",
#                                step=0.1, value=0.3)
#   output$sbp_net   = renderUI({
#     sidebarPanel(uibutton, br(), uitype("type_net", "samples"),
#                  uidist("dist_net"),
#                  uivar("color_net", "Color Variable:", vars()),
#                  uivar("shape_net", "Shape Variable:", vars()),
#                  uinetdistmax, uinetdispdist,
#                  uiptsz("size_net"), uialpha("alpha_net")
#     )
#   })
  ################################################################################
  # plot_bar() uix
  ################################################################################
  output$bar_uix_xvar <- renderUI({
    selectInput(inputId="x_bar", 
                label="Horizontal ('x') Variable:", 
                choices=c(list("Sample"="Sample"), rankNames(), variNames()))
  })
  output$bar_uix_colvar <- renderUI({
    uivar("color_bar", "Color Fill Variable:", vars())
  })
  ################################################################################
  # plot_tree() ui
  ################################################################################
  output$sbp_tree  = renderUI({
    sidebarPanel(uibutton,
                 selectInput(inputId="method_tree", label="Tree Method", 
                             choices=list(`No Points`="treeonly", `Dodged Points`="sampledodge"),
                             selected="treeonly"),
                 selectInput(inputId="justify_tree", label="Justify",
                             choices=list(Jagged="jagged", Left="left"),
                             selected="left"),
                 selectInput(inputId="ladderize_tree", label="Ladderize",
                             choices=list(Right="right", Left="left", `NULL`="NULL"),
                             selected="left"),
                 uivar("color_tree", "Color Variable:", vars()),
                 uivar("shape_tree", "Shape Variable:", vars()),
                 uiptsz("size_tree"), 
                 uivar("label_tip_tree", "Tip Labels", 
                       choices=c(OTU="OTU", specvarlist()),
                       selected="OTU"),
                 sliderInput("abundance_threshold_tree",
                             "Count Minimum Threshold for Points",
                             value=0.1*median(as(otu_table(physeq()), "matrix"), na.rm=TRUE),
                             max=max(as(otu_table(physeq()), "matrix"), na.rm=TRUE),
                             min=min(as(otu_table(physeq()), "matrix"), na.rm=TRUE)
                 ),
                 numericInput("margin_tree", "Margin", value=0.2, min=0, step=0.1)
    )
  })
  ################################################################################
  # plot_heatmap() ui
  ################################################################################
  uiord_heat = selectInput("ord_method_heat", "Ordination Method (axis ordering):", 
                           ordlist, selected="NMDS")
  uicttype_heat = uicttype("uicttype_heat")  
  output$sbp_heat  = renderUI({
    sidebarPanel(uibutton, br(), uiord_heat, uidist("dist_heat"),
                           uivar("sample.label", "Sample Labels:", sampvarlist()),
                           uivar("taxa.label", "Taxa Labels:", specvarlist()),
                           uivar("sample.order", "Sample Ordering:", sampvarlist()),
                           uivar("taxa.order", "Taxa Ordering:", specvarlist()),
                           textInput("locolor_heat", "Low Color", "#000033"),
                           textInput("hicolor_heat", "High Color", "#66CCFF"),
                           textInput("NAcolor_heat", "Missing Value Color", "black"),
                           uicttype_heat)
  })
  ################################################################################
  # scatterplot ui
  ################################################################################
  output$sbp_scat   = renderUI({
    uicttype_scat = uicttype("uicttype_scat")
    scatvars = c(OTU="OTU", Sample="Sample", Abundance="Abundance",
                 rankNames(), variNames())
    uix_scat = selectInput(inputId="x_scat", label="Horizontal ('x') Variable:", 
                           choices=scatvars, selected="Sample")
    uiy_scat = selectInput(inputId="y_scat", label="Vertical ('y') Variable:", 
                           choices=scatvars, selected="Abundance")
    sidebarPanel(uibutton, br(), 
                 uix_scat, uiy_scat,
                 uivar("color_scat", "Color Variable:", vars()),
                 uivar("shape_scat", "Shape Variable:", vars()),
                 textInput("facform_scat", "Facet Grid Formula:", value="NULL"),
                 uiptsz("size_scat"), uialpha("alpha_scat"),
                 uicttype_scat)
  })
  ################################################################################
  # Plot Rendering Stuff.
  ################################################################################
  observe({print(paste0("wayyy before: input$uicttype_bar: ", input$uicttype_bar, collapse=""))})
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
    if(is.null(input$formula) | input$formula=="NULL"){
      return(NULL)
    } else {
      return(as.formula(input$formula))
    }
  })
  #observe({print(paste0("formula argument: ", get_formula()))})
  # Define global reactive distance matrix. Will re-calc if method or plot-type change.
  gdist <- reactive({
    if(input$dist_ord %in% distance("list")$vegdist){
      return(input$dist)
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
    try(p1 <- plot_ordination(physeq(), get_ord(), type=input$ord_plot_type,
                              color=av(input$color_ord), shape=av(input$shape_ord)), silent=TRUE)
    return(p1)
  })
  # ordination plot definition
  output$ordination <- renderPlot({
    p1 = make_ord_plot()
    if(inherits(p1, "ggplot")){
      p1$layers[[1]]$geom_params$size <- input$size_ord
      p1$layers[[1]]$geom_params$alpha <- input$alpha_ord
      shiny_phyloseq_print(p1)
    }
    if(!inherits(p1, "ggplot")){
      # If for any reason p1 is not a ggplot at this point,
      # render fail-plot rather than tinker with ggplot innards
      print(failp)
    }
  })    
  ################################################################################
  # bar plot definition
  ################################################################################
  observe({print(paste0("before physeq_bar def input$uicttype_bar: ", input$uicttype_bar, collapse=""))})
  physeq_bar = reactive({
    observe({print(paste0("input$uicttype_bar: ", input$uicttype_bar, collapse=""))})
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
  })
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
    # Troubleshoot:
    #       observe({print(physeq)})
    #       observe({print(paste0("Tree Method (input$method_tree): ", input$method_tree))})
    #       observe({print(paste0("Tree Method Class (class(input$method_tree)): ", class(input$method_tree)))})
    #       observe({print(paste0("Color: ", av(input$color_tree)))})      
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
  })
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
  })
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
  })
  ################################################################################
  # Generate a network plot 
  ################################################################################
  # The reactive value version of the network.
  # Only returns distance matrix, regardless of distance-method argument
  distonly <- reactive({
    idist = NULL
    try({idist <- distance(physeq(), method=input$dist_net, type=input$type_net)}, silent=TRUE)
    if(is.null(idist)){warning("distonly: Could not calculate distance matrix with these settings.")}
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
  update_plot_network = reactive({
    p = initial_plot_network()
    # New edge layer
    # Define the layer that contains the edges and vertices
    whichEdge = which(sapply(p$layers, function(x){x$geom$objname=="line"}))
    # Define the edge data frame
    edgeDF0 = p$layers[[whichEdge]]$data
    # Add the distance associated with each edge entry
    edgeDF0 = ddply(edgeDF0, "id", function(df, dmat){
      df$dist <- dmat[df$value[1], df$value[2]]
      return(df)
    }, dmat = as.matrix(distonly()))
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
    return(p)
  })
  output$network <- renderPlot({
    shiny_phyloseq_print(update_plot_network())
  }, width=800, height=400)
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
  })
})
