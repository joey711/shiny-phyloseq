# Clear workspace for accurate record
rm(list=ls())
# Load packages and display version
library("shiny"); packageVersion("shiny")
library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")
library("data.table"); packageVersion("data.table")
library("d3Network"); packageVersion("d3Network")
# Load server-side data
load("provenance.RData")
# Remove server key reactive variables, and replace with lists.
rm(input, output)
# Initialize `input` as list
input = vector("list", length = 0)
# Convert all reactives to functions
ObjectClasses = sapply(sapply(ls(), get, simplify = FALSE), class, simplify = FALSE)
reactiveObjects = names(ObjectClasses[which(sapply(ObjectClasses, function(x) x[[1]]=="reactive"))])
# Define a function that takes the name of a reactive object
# and returns the function()-equivalent of that object, as a single string.
reactive_to_function = function(x){
  reactiveLabel = funcall = NULL
  reactiveLabel <- eval(parse(text = paste0("attr(", x, ", 'observable')$.label")))
  # Convert to function call
  funcall <- gsub("^reactive\\(", "function(){", reactiveLabel)
  funcall <- gsub("\\)$", "}", funcall)
  return(funcall)
}
# Now for each reactive object in the workspace, convert it to a vanilla function
for(x in reactiveObjects){
  eval(parse(text = paste(x, "<-", reactive_to_function(x))))
}

#
# Some example test code...
# isolate(p_net())
# eval(parse(text=initializeLines))
# input$layout_net = "reingold.tilford"
# isolate(p_net())
################################################################################
# End Header
################################################################################

