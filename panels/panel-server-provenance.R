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
  # Trim actionButton input statements to match others
  x <- gsub("^.+shinyActionButtonValue' ", "", x)
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
  x <- gsub("Classes 'numeric', 'shinyActionButtonValue'", "", x)
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
# Define initial reactive value for event_code counter
# that prevents code rendering until download-button click,
# and will intiate a re-render when DL-button is clicked
actionb_prov = reactiveValues(count=0)
# Reactive function that processes the event log and renders
# code to reproduce it as text on screen.
event_code = reactive({
  if(actionb_prov$count < 1 & input$actionb_prov < 1){
    return(NULL)
  }
  ########################################
  # 1. Grab current event log from within Shiny namespace
  ########################################
  # Need to grab token regarding user session.
  # See Joe Cheng branch on feature/graph-domain
  eventlog <- shiny:::.graphStack$as_list()
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
  #   # Convert all `input (all)` lines
  #   eventCode[inputAllLines] <- sapply(eventlog[inputAllLines],
  #                                      function(x){
  #                                        paste0("# Event: input(all) \n", input_all_to_Rcode(x$value))
  #                                      }, simplify = FALSE)
  ########################################
  # 3. Process all code executions.
  # - ctx observables
  # - ctx observer?
  # - Others?
  ########################################
  ctxlogs = which(sapply(eventlog, function(x){x$action=="ctx"}))
  # Omit provenance reactive (if present)
  ProvenanceSelfLinks = which(sapply(eventlog[ctxlogs], function(x){
    length(grep("shiny:::.graphStack$as_list()", x$label, fixed = TRUE)) > 0
  }))
  ctxlogs <- ctxlogs[-ProvenanceSelfLinks]
  # Keep only renderPlot reactives
  renderPlotEvents = which(sapply(eventlog[ctxlogs], function(x){
    length(grep("^output\\$.+renderPlot\\(", x$label)) > 0
  }))
  renderPlotEvents <- ctxlogs[renderPlotEvents]
  eventCode[renderPlotEvents] <- sapply(eventlog[renderPlotEvents], function(x){
    x <- gsub("^output\\$.+renderPlot\\(", "eval(expression(", x$label)
    x <- gsub("\\)$", "))", x)
    # Add comment marker.
    x <- paste0("# Event: renderPlot reactive call \n", x)
    return(x)
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
  if(actionb_prov$count < 1 & input$actionb_prov < 1){
    return("Click button to see code, download session archive...")
  }
  # Write a small subset of the most-recent code to the main panel, as HTML
  # Use the google prettify engine
  # https://code.google.com/p/google-code-prettify/
  x <- knitr::knit2html(fragment.only=TRUE,
                       text = c(
                         '<script src="https://google-code-prettify.googlecode.com/svn/loader/run_prettify.js?lang=r&skin=sunburst"></script>',
                         paste("### Preview Last", input$number_events_prov, "Events"),
                         "```{r last-3-chunks, echo=TRUE, eval=FALSE}",
                         tail(event_code(), input$number_events_prov),
                         "```")
  )
  x <- gsub("<pre>", '<pre class="prettyprint">', x, fixed = TRUE)
  return(HTML(x))
})
write_temp_record_files = reactive({
  # Add 1 to the provenance action counter, should trigger code re-render
  actionb_prov$count <- 1 + actionb_prov$count
  # Create temp directory for files
  DIR = "provenance-staging" 
  DIR <- file.path(DIR, paste0("shiny-phyloseq-provenance-", simpletime()))
  # Create
  dir.create(DIR)
  Rcodefile = file.path(DIR, "Provenance-Record.R")
  Rdatafile = file.path(DIR, "Provenance-Record.RData")
  # Write the R code log
  writeLines(text = event_code(), con = Rcodefile)
  # Save companion RData image.
  save(list = union(shinyPhyloseqServerObjectsList, ls()), file = Rdatafile)
  # Return the names of the temp files to zip-up.
  return(DIR)
})
compressflag_to_extension = function(x){
  flags = c(".tar", ".tar.gz", ".tar.bz2", ".tar.xz")
  # Options exclusively from `utils::tar` function. Could open this up at some point.
  names(flags) <- c("none", "gzip", "bzip2", "xz")
  return(flags[x[1]])
}
output$downloadProvenance <- downloadHandler(
  filename = function(){
    paste0("shiny-phyloseq-record-", simpletime(), compressflag_to_extension(input$compress_prov))
  },
  content = function(file){
    DIR <- write_temp_record_files()
    tar(tarfile = file, files = DIR, compression = input$compress_prov)
    # Cleanup after yourself. Remove temporary records directory.
    unlink(DIR, recursive = TRUE)
  }
)