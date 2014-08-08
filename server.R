################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)
# Set Shiny Reaction Log to TRUE
options(shiny.reactlog=TRUE)
# Default ggplot2 theme (Only relevant if panel-specific theme missing or NULL)
theme_set(theme_bw())
################################################################################
# Should use latest GitHub version of shiny
################################################################################
shiny_okay = FALSE
if("shiny" %in% .packages(all.available = TRUE)){
  shiny_min_version = "0.10.1"
  shiny_compare = compareVersion(as.character(packageVersion("shiny")), shiny_min_version)
  if( shiny_compare >= 0 ){
    shiny_okay <- TRUE
  }
}
if(!shiny_okay){
  install.packages("devtools")
  devtools::install_github("shiny", "rstudio")
}
################################################################################
# Install basic required packages if not available/installed.
################################################################################
download_not_installed = function(x){
  availpacks = .packages(all.available = TRUE)
  source("http://bioconductor.org/biocLite.R")
  for(i in x){
    if(!i %in% availpacks){
      biocLite(i)
    }
  }
}
vanilla_install_pkgs = c("data.table", "d3Network", "genefilter", "ggplot2",
                         "grid", "gridExtra", "png", "RColorBrewer", "scales")
download_not_installed(vanilla_install_pkgs)
################################################################################
# phyloseq existence/version test, and installation
################################################################################
phyloseq_okay = FALSE
if("phyloseq" %in% .packages(all.available = TRUE)){
  phyloseq_min_version = "1.9.11"
  phyloseq_compare = compareVersion(as.character(packageVersion("phyloseq")), phyloseq_min_version)
  if( phyloseq_compare >= 0 ){
    phyloseq_okay <- TRUE
  }
}
if(!phyloseq_okay) {
  # Go through recommended phyloseq installation steps
  # (1) Load biocLite
  source("http://bioconductor.org/biocLite.R")
  # (2) Install latest devel version from BioC
  useDevel(devel = TRUE)
  biocLite("phyloseq", suppressUpdates = TRUE)
  # (3) Restore biocLite to release status
  useDevel(devel = FALSE)
}
################################################################################
# Load packages that must be fully-loaded 
################################################################################
shiny_phyloseq_full_load_packages = c("shiny", "phyloseq", "data.table", "scales",
                                      "d3Network", "genefilter", "ggplot2",
                                      "grid", "gridExtra", "png", "RColorBrewer")
for(i in shiny_phyloseq_full_load_packages){
  library(i, character.only = TRUE); packageVersion(i)
}
################################################################################
# Begin Shiny Server definition.
################################################################################
# First store the inventory of objects (for provenance record)
shinyPhyloseqServerObjectsList = ls()

shinyServer(function(input, output){
  # Data panel
  source("panels/panel-server-data.R", local = TRUE)
  # Filtering
  source("panels/panel-server-filter.R", local = TRUE)
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
  # Bar
  source("panels/panel-server-bar.R", local = TRUE)
  # Tree
  source("panels/panel-server-tree.R", local = TRUE)
  # Heatmap
  source("panels/panel-server-heatmap.R", local = TRUE)
  # Richness
  source("panels/panel-server-richness.R", local = TRUE)
  # Ordination
  source("panels/panel-server-ordination.R", local = TRUE)
  # Network
  source("panels/panel-server-net.R", local = TRUE)
  # d3
  source("panels/panel-server-d3.R", local = TRUE)
  # Scatter
  source("panels/panel-server-scatter.R", local = TRUE)
  # Palette
  source("panels/panel-server-palette.R", local = TRUE)
  # Provenance
  source("panels/panel-server-provenance.R", local = TRUE)
})
################################################################################
