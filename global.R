# load packages
# Run the auto-installer/updater code:
source("install.R", local = TRUE)
# Default options for app startup
source("core/default-parameters.R", local = TRUE)
# For pasting times into things
simpletime = function(){gsub("\\D", "_", Sys.time())}
# Graphic-saving utilities
source("core/ggsave.R", local = TRUE)
################################################################################
# Function for standard-out phyloseq print summary in HTML
#
# http://stackoverflow.com/questions/18007440/how-to-change-font-size-in-html5
# http://www.w3schools.com/cssref/pr_font_font-size.asp
# http://stackoverflow.com/questions/19777515/r-shiny-mainpanel-display-style-and-font
################################################################################
output_phyloseq_print_html = function(physeq){
  HTML(
    paste(
      '<p class="phyloseq-print">',
      paste0(capture.output(print(physeq)), collapse=" <br/> "),
      "</p>"
    )
  )
  # Alternative tag way:
  #   do.call("p", args = c(list(class="phyloseq-print", 
  #                              sapply(c("alskfjs", "askfjls"), br, simplify = FALSE, USE.NAMES = FALSE))))
}
################################################################################
# Special variant of numericInput() that has a smaller default width,
# and is much more customizable, including `...`
# and an explicitly exposed `class` argument
#
# This is used by both ui.R and server.R
#
# Some helpful details. 
#
# http://shiny.rstudio.com/tutorial/lesson2/
# http://shiny.rstudio.com/articles/layout-guide.html
# http://stackoverflow.com/questions/20637248/shiny-4-small-textinput-boxes-side-by-side
# http://getbootstrap.com/2.3.2/base-css.html#forms
################################################################################
numericInputRow <- function(inputId, label, value, min = NA, max = NA, step = NA, class="form-control", ...){
  inputTag <- tags$input(id = inputId, type = "number", value = value, class=class, ...)
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      inputTag)
}
textInputRow <- function(inputId, label, value = "", class="form-control", ...){
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class=class, ...))
}
################################################################################
# Supported ggplot2 themes
################################################################################
theme_blank_custom = theme_bw() + theme(
  plot.title = element_text(size = 28),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  axis.text.x      = element_blank(),
  axis.text.y      = element_blank(),
  axis.title.x     = element_blank(),
  axis.title.y     = element_blank(),
  axis.ticks       = element_blank(),
  panel.border     = element_blank()
)
shiny_phyloseq_ggtheme_list <- list(
  bl_wh = theme_bw(),
  blank = theme_blank_custom,
  thin = theme_linedraw(),
  light = theme_light(),
  minimal = theme_minimal(),
  classic = theme_classic(),
  gray = theme_gray()
)
################################################################################
# Included Data
# Define the named list of datasets to choose from
################################################################################
# Create an environment to store original loaded data
env_psdata = new.env()
# Keep server-loaded data into a special environemnt, `env_psdata`
data(list=c("GlobalPatterns", "enterotype", "esophagus"), envir = env_psdata)
load("data/kostic.RData", envir = env_psdata)
load("data/1457_uparse.RData", envir = env_psdata)
attach(env_psdata)
# Define initial list of available datasets
datalist = list(
  closed_1457_uparse = closed_1457_uparse,
  study_1457_Kostic = kostic,
  GlobalPatterns = GlobalPatterns,
  enterotype = enterotype,
  esophagus = esophagus)
########################################
# Plot Rendering Stuff.
########################################
RstudioPNGsp <- png::readPNG("www/RStudio-logo-shiny-phyloseq.png")
RasterRstudio <- grid::rasterGrob(RstudioPNGsp, interpolate=TRUE)
# Define a dummy "failed plot" to return if render section cannot build valid plot.
fail_gen = function(main = "Change settings and/or click buttons.",
                    subtext = "",
                    image = RasterRstudio){
  qplot(x=0, y=0, main=main) +
    annotation_custom(image, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    annotate("text", 0, 0, size=10, color="black", hjust=0.5, vjust=-1,
             label=subtext) +
    theme_bw() + 
    theme(
      plot.title = element_text(size = 28),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x      = element_blank(),
      axis.text.y      = element_blank(),
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      axis.ticks       = element_blank(),
      panel.border     = element_blank()
    )
}
# Define a default controlled ggplot printing check for all print rendering
shiny_phyloseq_print = function(p, f=fail_gen()){
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
# Generic variable-to-facet-formula conversion function
# for facet_grid.
# Could easily make a facet_row equivalent, if needed.
get_facet_grid = function(facetrow=NULL, facetcol=NULL){
  if(is.null(av(facetrow)) & is.null(av(facetcol))){
    return(NULL)
  } else if(is.null(av(facetcol))){
    # If no column value, add a "."
    formstring = paste(paste(facetrow, collapse = "+"), "~", ".")
  } else {
    formstring = paste(
      paste(facetrow, collapse = "+"),
      "~",
      paste(facetcol, collapse = "+")
    )
  }
  return(as.formula(formstring))
}
################################################################################
# Component table rendering functions.
################################################################################
# Defines a function to convert
# a phyloseq data component
# into a data.frame, data.table, or matrix
# For the purpose of DataTables screen rendering
tablify_phyloseq_component = function(component, colmax=25L){
  if(inherits(component, "sample_data")){
    Table = data.frame(component)
  }
  if(inherits(component, "taxonomyTable")){
    Table = component@.Data
  }
  if(inherits(component, "otu_table")){
    if(!taxa_are_rows(component)){component <- t(component)}
    Table = component@.Data
  }
  return(Table[, 1:min(colmax, ncol(Table))])
}
# Determine available table-like components for on-screen rendering
component_options = function(physeq){
  # Initialize the return option list
  component_option_list = list("NULL"="NULL")
  # Get logical vector of components
  nonEmpty = sapply(slotNames(physeq), function(x, ps){!is.null(access(ps, x))}, ps=physeq)
  if(sum(nonEmpty)<1){return(NULL)}
  # Convert to vector of slot-name strings for non-empty components
  nonEmpty <- names(nonEmpty)[nonEmpty]
  # Cull the non-table components
  nonEmpty <- nonEmpty[!nonEmpty %in% c("phy_tree", "refseq")]
  # If no tables available, return default empty option
  if(length(nonEmpty)<1){return(component_option_list)}
  # Otherwise add to the option list and return
  compFuncString = names(phyloseq:::get.component.classes()[nonEmpty])
  if("sam_data" %in% compFuncString){
    compFuncString[compFuncString=="sam_data"] <- "sample_data"
  }
  NiceNames = c(otu_table="OTU Table",
                sample_data="Sample Data",
                tax_table = "Taxonomy Table")
  names(compFuncString) <- NiceNames[compFuncString]
  return(c(component_option_list, as.list(compFuncString)))
}
################################################################################
# Supported distance methods
################################################################################
# List of distances
# Make available for relevant panels.
distlist <- as.list(unlist(phyloseq::distanceMethodList))
names(distlist) <- distlist
distlist <- distlist[which(!distlist %in% c("ANY"))]
################################################################################
# Rescaled extension of `distance` function
################################################################################
scaled_distance = function(physeq, method, type, rescaled=TRUE){
  Dist = phyloseq::distance(physeq, method, type)
  if(rescaled){
    # rescale the distance matrix to be [0, 1]
    Dist <- Dist / max(Dist, na.rm=TRUE)
    Dist <- Dist - min(Dist, na.rm=TRUE)
  }
  return(Dist)
}
################################################################################
# Function to convert a distance matrix and threshold value
# into an edge-table (essentially a sparse graph matrix).
# This is used by multiple panels.
################################################################################
dist_to_edge_table = function(Dist, MaxDistance=NULL, vnames = c("v1", "v2")){
  dmat <- as.matrix(Dist)
  # Set duplicate entries and self-links to Inf
  dmat[upper.tri(dmat, diag = TRUE)] <- Inf
  LinksData = data.table(reshape2::melt(dmat, varnames=vnames, as.is = TRUE))
  setnames(LinksData, old = "value", new = "Distance")
  # Remove self-links and duplicate links
  LinksData <- LinksData[is.finite(Distance), ]
  # Remove entries above the threshold, MaxDistance
  if(!is.null(MaxDistance)){
    LinksData <- LinksData[Distance < MaxDistance, ]
  }
  return(LinksData)
}
################################################################################
