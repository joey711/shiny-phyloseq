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
    return(radioButtons("physeq", "Choose Dataset:", datasets))
  })
  physeq = reactive({
    if(!is.null(input$physeq)){
      if(input$physeq %in% includedDatasets){
        data(list=input$physeq)
      } else {
        load(file=input$file1$datapath)
      }
      observe({print(ls())})
      physeq = get(input$physeq)
      #trash = try(physeq <- get(input$physeq), silent=TRUE)
      #!inherits(trash, "try-error")
      return(physeq)
    } else {
      return(NULL)
    }
  })
  output$contents <- renderText({
    observe({print(physeq())})
    return(paste0(capture.output(print(physeq())), collapse="; "))
  })
  ################################################################################
  # Other server stuff.
  ################################################################################  
  
})
