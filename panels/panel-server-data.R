################################################################################
# Define the available phyloseq datasets for plotting.
################################################################################
trim_qiime_db_names = function(filename){
  filename <- basename(filename)
  return(gsub("^(study_.+)(_split_library_.+)", "\\1", filename))
}
output$qiimeDBopts <- renderUI({
  # Get the current list of available datasets from QIIMEDB
  # Requires RCurl package
  QIIMEDB_ftp = "ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/"
  QIIMEDB_files_list = RCurl::getURL(QIIMEDB_ftp)
  QIIMEDB_files_tab  = read.table(text = QIIMEDB_files_list, stringsAsFactors = FALSE)
  colnames(QIIMEDB_files_tab)[c(5, 8, 9)] <- c("size", "year", "name")
  # Keep only files that follow a certain naming convention that appears to have OTU-cluster results  
  QIIMEDB_keep_entry = grep("study_.+seqs_and_mapping", QIIMEDB_files_tab[, "name"])
  QIIMEDB_files_tab <- QIIMEDB_files_tab[QIIMEDB_keep_entry, ]
  # Remove entries that are too large (above a threshold, in MB)
  QIIMEDB_files_tab <- QIIMEDB_files_tab[((QIIMEDB_files_tab[, "size"]/1E6) < input$qiimeDBsizeMax), ]
  # Create list for widget.
  QIIMEDB_full_links = paste0(QIIMEDB_ftp, QIIMEDB_files_tab$name)
  QIIMEDB_choices = as.list(QIIMEDB_full_links)
  # Add names for searching
  names(QIIMEDB_choices) <- paste0(trim_qiime_db_names(QIIMEDB_files_tab$name),
                                  " (", QIIMEDB_files_tab$year, ")",
                                  " [", round(QIIMEDB_files_tab$size/1e6, digits = 1), " MB]")
  # Add NULL as default
  QIIMEDB_choices <- c(list("NULL"=NULL), QIIMEDB_choices)
  return(
    selectInput("qiime_server_ID", label = "",
                choices = QIIMEDB_choices, selected = "NULL")
  )
})
get_qiime_data = reactive({
  if(input$actionb_data_qiime < 1){
    return(NULL)
  }
  qiime_data = NULL
  if(!is.null(av(input$qiime_server_ID))){
    trash = try({
      qiime_data <- microbio_me_qiime(input$qiime_server_ID, ext = NULL)
    }, silent=TRUE)
  }
  if(inherits(qiime_data, "phyloseq")){
    qiime_data <- list(qiime_data)
    names(qiime_data) <- trim_qiime_db_names(input$qiime_server_ID)
    datalist <<- c(qiime_data, datalist)
  } else {
    message("Attempt made to access qiime server data, but didn't work this pass...")
  }
  return(NULL)
})
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
    # observe({print(paste("Biom File(s) Uploaded:", input$filebiom$name, collapse=", "))})
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
  get_qiime_data()
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
  iDisplayLength = 5 
))
################################################################################
