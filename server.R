################################################################################
# Options, default settings, and load packages
################################################################################
# load packages
library("shiny"); packageVersion("shiny")
library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")
library("data.table"); packageVersion("data.table")
library("d3Network"); packageVersion("d3Network")
# Default options for app startup
source("default-parameters.R", local=FALSE)
source("ggsave.R")
# For pasting times into things
simpletime = function(){gsub("[[:punct:][:space:]]", "_", Sys.time())}
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)
# ggplot2 themeing and palettes.
theme_set(theme_bw())

################################################################################
# Included Data
# Define the named list of datasets to choose from
################################################################################
includedDatasets = c("GlobalPatterns", "enterotype", "esophagus", "soilrep")
data(list=includedDatasets)
datalist = list(GlobalPatterns=GlobalPatterns, 
                enterotype=enterotype,
                esophagus=esophagus,
                soilrep=soilrep)

filepath = system.file("extdata", "study_1457_split_library_seqs_and_mapping.zip", package="phyloseq")
suppressWarnings(kostic <- microbio_me_qiime(filepath))
if(inherits(kostic, "phyloseq")){
  datalist <- c(list(study_1457_Kostic=kostic), datalist)
}
# filepath <- system.file("extdata", "study_816_split_library_seqs_and_mapping.tar.gz", package="phyloseq")
# study_816 = microbio_me_qiime(filepath)
# if(inherits(study_816, "phyloseq")){
#   datalist <- c(list(study_816=study_816), datalist)
# }
################################################################################
# Begin Shiny Server definition.
################################################################################
shinyServer(function(input, output){
  # Data panel
  source("panel-server-data.R", local = TRUE)
  # Filtering
  source("panel-server-filter.R", local = TRUE)
  ########################################
  # Reactive UI Definition of Variables
  ########################################
  # Define data-reactive variable lists
  rankNames = reactive({
    rankNames = as.list(rank_names(physeq(), errorIfNULL=FALSE))
    names(rankNames) <- rankNames
    return(rankNames)
  })
  variNames = reactive({
    variNames = as.list(sample_variables(physeq(), errorIfNULL=FALSE))
    names(variNames) <- variNames
    return(variNames)
  })
  vars = function(type="both", withnull=TRUE, singles=FALSE){
    if(!type %in% c("both", "taxa", "samples")){
      stop("incorrect `type` specification when accessing variables for UI.")
    }
    returnvars = NULL
    if(type=="samples"){
      if(singles){
        returnvars <- c(list(Sample="Sample"), variNames())
      } else {
        returnvars <- variNames()
      }
    }
    if(type=="taxa"){
      if(singles){
        returnvars <- c(rankNames(), list(OTU="OTU"))
      } else {
        returnvars <- rankNames()
      }
    } 
    if(type=="both"){
      # Include all variables
      if(singles){
        returnvars <- c(rankNames(), variNames(), list(OTU="OTU", Sample="Sample"))
      } else {
        returnvars <- c(rankNames(), variNames())
      }
    }
    if(withnull){
      # Put NULL first so that it is default when `select` not specified
      returnvars <- c(list("NULL"="NULL"), returnvars)
    }
    return(returnvars)
  }
  # A generic selectInput UI. Plan is to pass a reactive argument to `choices`.
  uivar = function(id, label="Variable:", choices, selected="NULL"){
    selectInput(inputId=id, label=label, choices=choices, selected=selected)
  }
  ########################################
  # Plot Rendering Stuff.
  ########################################
  # Define a dummy "failed plot" to return if render section cannot build valid plot.
  fail_gen = function(main="Graphic Fail.", 
                      subtext="Please change parameters and try again."){
    qplot(x=0, y=0, main=main) + 
      annotate("text", 0, 0, label=":-(",
               size=75, angle=270, hjust=0.5, vjust=0.35) +
      annotate("text", 0, 0, label=subtext, size=10, hjust=0.5, vjust=-7) +
      theme_bw() + 
      theme(panel.border=element_blank(), axis.line=element_blank(),
            axis.text=element_blank(), axis.ticks=element_blank())
  }
  failp = fail_gen()
  # Define a default controlled ggplot printing check for all print rendering
  shiny_phyloseq_print = function(p, f=failp){
    if(inherits(p, "ggplot")){
      # Check that rendering will work
      printout = NULL
      try(printout <- print(p), silent=TRUE)
      if(is.null(printout)){
        # If still NULL, the print-render failed,
        # otherwise print() would have returned a 'list'
        # Nothing was printed. Print the fail graphic in its place.
        print(f)
      }
    } else {
      print(f)
    }
  }    
  # Define generic function to access/clean variables
  # This especially converts "NULL" to NULL
  av = function(x){
    if( isTRUE(all.equal(x, "")) | isTRUE(all.equal(x, "NULL")) ){
      return(NULL)
    }
    return(x)
  }
  # Bar
  source("panel-server-bar.R", local = TRUE)
  # Tree
  source("panel-server-tree.R", local = TRUE)
  # Heatmap
  source("panel-server-heatmap.R", local = TRUE)
  # Richness
  source("panel-server-richness.R", local = TRUE)
  # Ordination
  source("panel-server-ordination.R", local = TRUE)
  # Network
  source("panel-server-net.R", local = TRUE)
  # d3
  source("panel-server-d3.R", local = TRUE)
  # Scatter
  source("panel-server-scatter.R", local = TRUE)
  # Palette
  source("panel-server-palette.R", local = TRUE)
})
