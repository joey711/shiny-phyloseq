################################################################################
# Begin provenance code record HEADER. You can skip reading this part.
# The code specific to your session is AFTER this header (below).
################################################################################
# Clear workspace for accurate record
rm(list=ls())
########################################
# Load packages and display version
########################################
library("shiny"); packageVersion("shiny")
library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")
library("data.table"); packageVersion("data.table")
library("d3Network"); packageVersion("d3Network")
# Create an environment to keep objects from the shiny-phyloseq session
env_runtime = new.env()
# Load server-side data into a special environemnt, `env_runtime`
load("Provenance-Record.RData", envir = env_runtime)
attach(env_runtime)
# Remove server key reactive variables, and replace with lists.
rm(input, output, envir = env_runtime)
# Initialize `input` as list
input = vector("list", length = 0)
########################################
# Convert all reactives to functions
########################################
ObjectClasses = sapply(sapply(ls(envir = env_runtime), get, simplify = FALSE), class, simplify = FALSE)
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
# Now for each reactive object in the env_runtime, convert it to a vanilla function
# and save it to a new environment, but not in the GlobalEnv
env_provenance = new.env()
for(x in reactiveObjects){
  assign(x, value = eval(parse(text = reactive_to_function(x))), envir = env_provenance)
}
# Add to `env_provenance` the non-reactive objects
nonReactive = ls(envir = env_runtime)[!ls(envir = env_runtime) %in% reactiveObjects]
for(x in nonReactive){
  # Take from `env_runtime` and assign to `env_provenance`
  assign(x, value = get(x, envir = env_runtime), envir = env_provenance)
}
# To be safe, remove all reactiveObjects from env_runtime and detach env_runtime
rm(list = reactiveObjects, envir = env_runtime)
rm(env_runtime)
detach(env_runtime)
# Rm the global workspace objects related to this record munging
rm(ObjectClasses, reactive_to_function, reactiveObjects, nonReactive, x)
# Finally, attach `env_provenance`
attach(env_provenance)
################################################################################
# End Header
# Ignore Code Above this Line. It is necessary for proper loading/execution.
################################################################################

