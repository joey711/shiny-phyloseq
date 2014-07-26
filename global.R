# load packages
library("shiny"); packageVersion("shiny")
library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")
library("data.table"); packageVersion("data.table")
library("d3Network"); packageVersion("d3Network")
library("ggplot2")
# Default options for app startup
source("core/default-parameters.R", local = TRUE)
# For pasting times into things
simpletime = function(){gsub("\\D", "_", Sys.time())}
# Graphic-saving utilities
source("core/ggsave.R", local = TRUE)
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
  `black/white` = theme_bw(),
  blank = theme_blank_custom,
  `thin line` = theme_linedraw(),
  light = theme_light(),
  minimal = theme_minimal(),
  classic = theme_classic(),
  gray = theme_gray()
)
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
load("data/kostic.RData")
datalist <- c(list(study_1457_Kostic=kostic), datalist)
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