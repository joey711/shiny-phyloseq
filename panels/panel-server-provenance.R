# Provenance: 
# http://en.wikipedia.org/wiki/Provenance
# "
# The primary purpose of tracing the provenance of an object or entity
# is normally to provide contextual and circumstantial evidence 
# for its original production or discovery...
# Comparative techniques, expert opinions, and the results of scientific tests
# may also be used to these ends, 
# but establishing provenance is essentially a matter of documentation.
# "
# And so here we are.
#
# Store available objects in current and global environment.
# Will use this to define an RData image of app-session for 
# code+data reproducibility.
shinyPhyloseqServerObjectsList = union(shinyPhyloseqServerObjectsList,
                                       union(ls(.GlobalEnv), ls()))
# Read header lines. Once.
provHeaderLines = readLines("panels/panel-server-provenance-header.R", warn = FALSE)
# Define function for converting right-hand side
# of simple input lines in event log.
convert_input_lines_value = function(x){
  # Remove the class indicators
  x <- gsub("^\\s*chr\\s*", "", x)
  x <- gsub("^\\s*num\\s*", "", x)
  # Remove the vector length indicator. e.g. [1:3]
  # And add the opening concatenation call, `c(`
  x <- gsub("^\\s*\\[\\d+:\\d\\]\\s+", "c(", x)
  # Remove any lingering leading-space
  x <- gsub("^\\s+", "", x)
  # If there are space-delimited vectors, convert them to space-comma-space
  x <- gsub("[[:blank:]]+", ", ", x)
  # Close the concatenation and ")"
  x <- gsub("(^c\\([^\n]+)", "\\1\\)", x)
}
# Function to convert `input (all)` lines.
input_all_to_Rcode = function(x){
  # Remove initial List statement.
  x <- gsub("List of \\d+\n \\$", "$", x)
  # Convert to input$ list directly
  x <- gsub("$ ", "input$", x, fixed = TRUE)
  x <- gsub(" input$", "input$", x, fixed = TRUE)
  # Convert the colon-delimiter into an assignment
  x <- gsub("(input\\$\\S+)(\\s*:\\s*)", "\\1<-", x)
  # Remove the class indicators
  x <- gsub("<-\\s*chr\\s*", "<-", x)
  x <- gsub("<-\\s*num\\s*", "<-", x)
  # Remove the vector length indicator. e.g. [1:3]
  # And add the opening concatenation call, `c(`
  # Still no spaces.
  x <- gsub("<-\\s*\\[\\d+:\\d\\]\\s+", "<-c(", x)
  # If there are space-delimited vectors, convert them to space-comma-space
  x <- gsub("[[:blank:]]+", ", ", x)
  # Close the concatenation and ")"
  x <- gsub("(<-c[^\n]+)", "\\1\\)", x)
  # Add back space for legibility
  x <- gsub("<-", " <- ", x, fixed = TRUE)
}
# Reactive function that processes the event log and renders
# code to reproduce it as text on screen.
event_code = reactive({
  observe(print(paste("actionb_prov:", input$actionb_prov)))
  if (input$actionb_prov == 0){
    return(NULL)
  }
  ########################################
  # 1. Grab current event log from within Shiny namespace
  ########################################
  eventlog <- shiny:::.graphEnv$log
  # Initialize list of event code, as character strings:
  eventCode = vector("list", length = length(eventlog))
  ########################################
  # 2. Process valueChanges
  ########################################
  vChanges = which(sapply(eventlog, function(x){x$action=="valueChange"}))
  # grab IDs that start with `input$`
  inputLines = grep("^input\\$", sapply(eventlog[vChanges], function(x) x$id))
  inputLines <- vChanges[inputLines]
  # grab IDs that start with `input (all)`
  inputAllLines = grep("input (all)", sapply(eventlog[vChanges], function(x) x$id), fixed=TRUE)
  inputAllLines <- vChanges[inputAllLines]
  # This should be FALSE. Separate event types that are treated separately.
  #length(intersect(inputLines, inputAllLines)) == 0
  # Convert all `input (all)` lines
  eventCode[inputLines] <- sapply(eventlog[inputLines],
                                  function(x){
                                    paste0("# Event: input$* assignment/replacement \n",
                                           paste(x$id, "<-", convert_input_lines_value(x$value)))
                                  }, simplify = FALSE)
  # Convert all `input (all)` lines
  eventCode[inputAllLines] <- sapply(eventlog[inputAllLines],
                                     function(x){
                                       paste0("# Event: input(all) \n", input_all_to_Rcode(x$value))
                                     }, simplify = FALSE)
  ########################################
  # 3. Process all code executions.
  # - ctx observables
  # - ctx observer?
  # - Others?
  ########################################
  ctxlogs = which(sapply(eventlog, function(x){x$action=="ctx"}))
  #   # What `type`s are there?
  #   unique(sapply(eventlog[ctxlogs], function(x) x$type))
  #   table(sapply(eventlog[ctxlogs], function(x) x$type))
  #   # observer
  #   ctxlogs.observer = which(sapply(eventlog[ctxlogs], function(x) x$type=="observer"))
  #   ctxlogs.observer <- ctxlogs[ctxlogs.observer]
  #   eventlog[ctxlogs.observer]
  # isolate
  ctxlogs.isolate = which(sapply(eventlog[ctxlogs], function(x) x$type=="isolate"))
  ctxlogs.isolate <- ctxlogs[ctxlogs.isolate]
  if(length(ctxlogs.isolate)>0){
    # Process isolate.
    # eventlog[ctxlogs.isolate]
  }
  # "observable"s appear to be the reactive function calls.
  ctxlogs.observable = which(sapply(eventlog[ctxlogs],
                                    function(x){x$type=="observable"}))
  ctxlogs.observable <- ctxlogs[ctxlogs.observable]
  # Should be zero
  #intersect(ctxlogs.observable, inputAllLines)
  #intersect(ctxlogs.observable, inputLines)
  eventCode[ctxlogs.observable] <- sapply(eventlog[ctxlogs.observable], function(x){
    return(paste0("isolate({", x$label, "()})"))
  })
  ########################################
  # Return event code as character vector
  ########################################
  # Remove empty lines, unlist, add header, write to file.
  eventCode <- eventCode[sapply(eventCode, length) > 0L]
  eventCode <- unlist(eventCode, recursive = TRUE, use.names = FALSE)
  eventCode <- c(provHeaderLines, eventCode)
  return(eventCode)
})
output$provenance <- renderUI({
  # Convert all newlines to <br/>
  codeLines <- gsub("\n", "<br/>", event_code(), fixed = TRUE)  
  # Write a small subset of the most-recent code to the main panel, as HTML
  return(HTML(paste0(tail(codeLines, 3), collapse = '<br/>')))
})
write_temp_record_files = reactive({
  # Create temp directory for files
  DIR = "www" 
  DIR <- file.path(DIR, paste0("shiny-phyloseq-", simpletime()))
  # Create
  dir.create(DIR)
  Rcodefile = file.path(DIR, "Provenance-Record.R")
  Rdatafile = file.path(DIR, "Provenance-Record.RData")
  # Write the R code log
  writeLines(text = event_code(), con = Rcodefile)
  # Save companion RData image.
  save(list = union(shinyPhyloseqServerObjectsList, ls()), file = Rdatafile)
  observe({print(paste("downloadHandler temp file locations, Rcodefile: ", Rcodefile))})
  observe({print(paste("downloadHandler temp file locations, Rdatafile: ", Rdatafile))})
  # Return the names of the temp files to zip-up.
  return(DIR)
})
compressflag_to_extension = function(x){
  flags = c(".tar", ".tar.gz", ".tar.bz2", ".tar.xz")
  names(flags) <- c("none", "gzip", "bzip2", "xz")
  return(flags[x[1]])
}
output$downloadProvenance <- downloadHandler(
  filename = function(){
    paste0("shiny-phyloseq-record-", simpletime(), compressflag_to_extension(input$compress_prov))
  },
  content = function(file){
    tar(tarfile = file, files=write_temp_record_files(), compression = input$compress_prov)
    unlink(write_temp_record_files(), recursive = TRUE)
  }
)