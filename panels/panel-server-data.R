################################################################################
# Define the available phyloseq datasets for plotting.
################################################################################
get_qiime_data = reactive({
  if(input$actionb_data_qiime < 1){
    return(NULL)
  }
  qiime_data = NULL
  if(!is.null(av(input$qiime_server_ID))){
    if( !is.na(as.integer(input$qiime_server_ID)) ){
      zipftp = as(isolate({input$qiime_server_ID}), "integer")
      studyname = input$qiime_server_ID
    } else {
      zipftp = as(isolate({input$qiime_server_ID}), "character")
      studyname = gsub("\\_split\\_.+$", "", basename(zipftp))
    }
    trash = try({qiime_data <- microbio_me_qiime(zipftp, ext=input$qiime_server_ext)}, silent=TRUE)
  }
  if(inherits(qiime_data, "phyloseq")){
    qiime_data <- list(qiime_data)
    names(qiime_data) <- studyname
    datalist <<- c(qiime_data, datalist)
  } else {
    message("Attempt made to access qiime server data, but didn't work this pass...")
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
  if(inherits(ps0, "phyloseq")){
    return(ps0)
  } else {
    return(NULL)
  }
})
output$library_sizes <- renderPlot({
  if(inherits(get_phyloseq_data(), "phyloseq")){
    libtitle = "Histogram of Library Sizes in Selected Data"
    p1 = lib_size_hist() + ggtitle(libtitle)
    otusumtitle = "Histogram of OTU total counts in Selected Data"
    p2 = otu_sum_hist() + ggtitle(otusumtitle)
    gridExtra::grid.arrange(p1, p2, ncol=2)
  } else {
    fail_gen("")
  }
})
output$OTU_count_thresh_hist <- renderPlot({
  if(input$actionb_data < 1){
    return(fail_gen("Click 'Make Histogram' Button"))
  }
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
    return(fail_gen())
  }
})
