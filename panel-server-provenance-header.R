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
## This would be an awesome hack if it worked...
#attr(input, "readonly") <- FALSE
## This hack is also interesting
# temp = isolate(reactiveValuesToList(input, all.names = TRUE))
# rm(input)
# input <- temp
# The following at least partially works, and gives a concise means
# to reset all reactives in one line. Over and over.
ObjectClasses = sapply(sapply(ls(), get, simplify = FALSE), class, simplify = FALSE)
reactiveObjects = names(ObjectClasses[which(sapply(ObjectClasses, function(x) x[[1]]=="reactive"))])
invalidatedLines = paste0("attr(", reactiveObjects, ", 'observable')$.invalidated <- TRUE")
execCountLines = paste0("attr(", reactiveObjects, ", 'observable')$.execCount <- 0L")
mostRecentCtxIdLines = paste0("attr(", reactiveObjects, ", 'observable')$.mostRecentCtxId <- ''")
valueLines = paste0("attr(", reactiveObjects, ", 'observable')$.value <- NULL")
initializeLines = c(invalidatedLines, execCountLines, mostRecentCtxIdLines, valueLines)
# Now the following line will reset all reactives to invalidated.
eval(parse(text=initializeLines))
#
# Some example test code...
# isolate(p_net())
# eval(parse(text=initializeLines))
# input$layout_net = "reingold.tilford"
# isolate(p_net())
################################################################################
# End Header
################################################################################

