################################################################################
# Define the available phyloseq datasets for plotting.
################################################################################
get_loaded_data = reactive({
  if(!is.null(input$file1$name)){
    # Add uploaded data, if provided, and it is phyloseq-class.
    # Load user-data into a new environment (essentially sandbox)
    env_userdata = new.env()
    objectNames = load(input$file1$datapath, envir = env_userdata)
    loadedObjects = mget(objectNames, envir = env_userdata)
    arePhyloseq = sapply(loadedObjects, inherits, "phyloseq")
    if(any(arePhyloseq)){
      loadedObjects <- loadedObjects[which(arePhyloseq)]
    } else {
      loadedObjects <- NULL
    }
    datalist <<- c(loadedObjects, datalist)
  }
  return(NULL)
})
get_biom_data = reactive({
  if(!is.null(input$filebiom$name)){
    # Loop through each uploaded file
    # Added uploaded data, if provided, and it is phyloseq-class.
    importedBiom = NULL
    importedBiom <- sapply(input$filebiom$name, function(i, x){
      ib = NULL
      junk = try(ib <- import_biom(x$datapath[x$name==i]), silent = TRUE)
      return(ib)
    }, x=input$filebiom, simplify = FALSE, USE.NAMES = TRUE)
    arePhyloseq = sapply(importedBiom, inherits, "phyloseq")
    if(any(arePhyloseq)){
      importedBiom <- importedBiom[which(arePhyloseq)]
    } else {
      importedBiom <- NULL
    }
    datalist <<- c(importedBiom, datalist)
  }
  return(NULL)
})  
process_uploaded_tree = reactive({
  if(!is.null(av(input$treefile$name))){
    if(!is.null(isolate(input$physeqSelect))){
      # This is what you'll merge with.
      b4treeMerge = datalist[[isolate(input$physeqSelect)]]
      treeResult = read_tree(input$treefile$datapath)
      if(is.null(treeResult)){
        # Initial tree result failed, attempt read_tree_greengenes
        treeResult <- read_tree_greengenes(input$treefile$datapath)
      }
      if(is.null(treeResult)){
        warning("Tree import failed. Please check tree file and try again.")
      }
      if(inherits(b4treeMerge, "phyloseq") & inherits(treeResult, "phylo")){
        # Make sure both are expected class
        if(length(intersect(
          taxa_names(b4treeMerge),
          taxa_names(treeResult))) > 0){
          # Only attempt merge if there are OTUs in common.
          # Remove original tree component.
          b4treeMerge@phy_tree <- NULL
          aftreeMerge = merge_phyloseq(b4treeMerge, treeResult)
          # Finally, if the result is a phyloseq object, replace in the current datalist
          if(inherits(aftreeMerge, "phyloseq")){
            #datalist[[isolate(input$physeqSelect)]] <- aftreeMerge
            aftreeMergeL = list(aftreeMerge)
            names(aftreeMergeL) <- paste0(isolate(input$physeqSelect), "_AddTree")
            datalist <<- c(aftreeMergeL, datalist)
          }
        }
      }
    }
  }
  return(NULL)
})
output$phyloseqDataset <- renderUI({
  # Expect the side-effect of these functions to be to add
  # elements to the datalist, if appropriate
  get_loaded_data()
  get_biom_data()
  #get_qiime_data()
  # Process tree (and in principle, other components) last
  process_uploaded_tree()
  return(
    selectInput("physeqSelect", "Select Dataset", names(datalist))
  )
})
get_phyloseq_data = reactive({
  ps0 = NULL
  if(!is.null(input$physeqSelect)){
    if(input$physeqSelect %in% names(datalist)){
      ps0 <- datalist[[input$physeqSelect]]
    }
  }
  if(inherits(ps0, "phyloseq")){
    return(ps0)
  } else {
    return(NULL)
  }
})
output$library_sizes <- renderPlot({
  if(inherits(get_phyloseq_data(), "phyloseq")){
    libtitle = "Library Sizes"
    p1 = lib_size_hist() + ggtitle(libtitle)
    otusumtitle = "OTU Totals"
    p2 = otu_sum_hist() + ggtitle(otusumtitle)
    gridExtra::grid.arrange(p1, p2, ncol=2)
  } else {
    fail_gen("")
  }
})
output$uix_available_components_orig <- renderUI({
  selectInput("available_components_orig", "Available Components",
              choices = component_options(get_phyloseq_data()))
})
# Render the user-selected data component using DataTables
output$ps0ComponentTable <- renderDataTable({
  if(is.null(av(input$available_components_orig))){
    return(NULL)
  }
  component = do.call(what = input$available_components_orig, args = list(get_phyloseq_data()))
  return(tablify_phyloseq_component(component, input$component_table_colmax))
}, options = list(
  pageLength = 5 
))
################################################################################
