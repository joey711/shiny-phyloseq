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
shinyPhyloseqServerObjectsList = union(ls(.GlobalEnv), ls())
# Read header lines. Once.
provHeaderLines = readLines("panel-server-provenance-header.R", warn = FALSE)
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
output$provenance <- renderUI({
  # Re-execute this reactive expression after 20 seconds
  invalidateLater(20000)
  # Save current available RData image for generic reproducible access.
  # This should be wrapped within a download UI later...
  save(list = union(shinyPhyloseqServerObjectsList, ls()),
       file = "~/Downloads/provenance.RData")
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
  # - ctx observables and observer
  # - Others?
  ########################################
  
  ########################################
  # Return event code as character vector.
  ########################################
  # Remove empty lines
  eventCode <- eventCode[sapply(eventCode, length) > 0L]
  # Add header and return
  codeLines <- c(provHeaderLines, unlist(eventCode, recursive = TRUE, use.names = FALSE))
  # Convert all newlines to <br/>
  codeLines <- gsub("\n", "<br/>", codeLines, fixed = TRUE)
  # Last few lines  ...
  codeLines <- tail(codeLines)
  # Test listing/saving the special objects visible during a session
  observe({print("Attempt to list available objects:")})
  observe({print(ls(environment(), all.names = TRUE))})
  return(HTML(paste0(codeLines, collapse = '<br/>')))
})