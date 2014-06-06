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
    observe({print(
      paste("get_loaded_data(). Available objects in datalist:", 
            paste0(names(datalist), collapse=", "), sep=" ")
    )})
  }
  return(NULL)
})
get_biom_data = reactive({
  if(!is.null(input$filebiom$name)){
    observe({print(paste("Biom File(s) Uploaded:", input$filebiom$name, collapse=", "))})
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
    observe({print(
      paste("get_biom_data(). Available objects in datalist:", 
            paste0(names(datalist), collapse=", "), sep=" ")
    )})
  }
  return(NULL)
})  
output$phyloseqDataset <- renderUI({
  # Expect the side-effect of these functions to be to add
  # elements to the datalist, if appropriate
  get_loaded_data()
  get_biom_data()
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