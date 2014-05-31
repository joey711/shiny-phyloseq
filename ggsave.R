require("shiny")
require("ggplot2")
# Define a vector of supported format labels
vectorGraphicFormats = c("emf", "eps", "pdf", "tex", "svg", "wmf")
rasterGraphicFormats = c("bmp", "jpg", "png", "tiff")
graphicFormats = c(vectorGraphicFormats, rasterGraphicFormats)
graphicTypeUI = function(inputId, label="Select Download Format:", choices=graphicFormats, selected="pdf"){
  selectInput(inputId, label, choices, selected, multiple = FALSE, selectize = TRUE)
}
# Define the ggsave-specific wrappers for graphic device functions
# Code taken directly from `ggplot2::ggsave`
# Vector Graphics (no dpi) 
ggeps <- ggps <- function(..., width, height)
  grDevices::postscript(..., width=width, height=height, onefile=FALSE,
                        horizontal = FALSE, paper = "special")
ggtex <- function(..., width, height)
  grDevices::pictex(..., width=width, height=height)
ggpdf <- function(..., version="1.4")
  grDevices::pdf(..., version=version)
ggsvg <- function(...)
  grDevices::svg(...)
ggwmf <- function(..., width, height)
  grDevices::win.metafile(..., width=width, height=height)
ggemf <- function(..., width, height)
  grDevices::win.metafile(..., width=width, height=height)
# Raster Graphics (dpi needed)
ggpng <- function(..., width, height)
  grDevices::png(...,  width=width, height=height, res = dpi, units = "in")
ggjpg <- jpeg <- function(..., width, height)
  grDevices::jpeg(..., width=width, height=height, res = dpi, units = "in")
ggbmp <- function(..., width, height)
  grDevices::bmp(...,  width=width, height=height, res = dpi, units = "in")
ggtiff <- function(..., width, height)
  grDevices::tiff(..., width=width, height=height, res = dpi, units = "in") 
shiny_ggsave_device = function(..., width=6L, height=5L, dpi=300L, graphictype="pdf"){ 
  # Match type to internal graphic device wrapper
  return(get(paste0("gg", tolower(graphictype))))
} 
ggcontent = function(ggplotobj, graphictype="pdf", width=6, height=5, dpi=300L){
  # Wraps ggsave essential bits to ensure that ggsave and downloadHandler
  # can play nice together.
  return({
    function(file){
      ggsave(filename = file, plot = ggplotobj,
             device = shiny_ggsave_device(width=width, height=height, dpi=dpi, graphictype=graphictype),
             width=width, height=height, units = "in")
      # YOU ARE HERE.
      #ggsave2()
    }
  })
}
ggfilegen = function(prefix, graphictype="pdf"){
  return({
    function(){paste0(prefix, gsub("[[:punct:][:space:]]", "_", Sys.time()), ".", graphictype)}
  })
}



ggsave2 <- function(filename = "ggplot2save2", plot = last_plot(),
                   device = "pdf", path = NULL, scale = 1,
                   width = par("din")[1], height = par("din")[2], units = c("in", "cm", "mm"),
                   dpi = 300, limitsize = TRUE, ...) {
  
  if (!inherits(plot, "ggplot")) stop("plot should be a ggplot2 plot")
  
  eps <- ps <- function(..., width, height)
    grDevices::postscript(..., width=width, height=height, onefile=FALSE,
                          horizontal = FALSE, paper = "special")
  tex <- function(..., width, height)
    grDevices::pictex(..., width=width, height=height)
  pdf <- function(..., version="1.4")
    grDevices::pdf(..., version=version)
  svg <- function(...)
    grDevices::svg(...)
  wmf <- function(..., width, height)
    grDevices::win.metafile(..., width=width, height=height)
  emf <- function(..., width, height)
    grDevices::win.metafile(..., width=width, height=height)
  
  png <- function(..., width, height)
    grDevices::png(...,  width=width, height=height, res = dpi, units = "in")
  jpg <- jpeg <- function(..., width, height)
    grDevices::jpeg(..., width=width, height=height, res = dpi, units = "in")
  bmp <- function(..., width, height)
    grDevices::bmp(...,  width=width, height=height, res = dpi, units = "in")
  tiff <- function(..., width, height)
    grDevices::tiff(..., width=width, height=height, res = dpi, units = "in")
  
  device <- match.fun(device)
  
  units <- match.arg(units)
  convert_to_inches <- function(x, units) {
    x <- switch(units,
                `in` = x,
                cm = x / 2.54,
                mm = x / 2.54 /10
    )
  }
  
  convert_from_inches <- function(x, units) {
    x <- switch(units,
                `in` = x,
                cm = x * 2.54,
                mm = x * 2.54 * 10
    )
  }
  
  # dimensions need to be in inches for all graphic devices
  # convert width and height into inches when they are specified
  if (!missing(width)) {
    width <- convert_to_inches(width, units)
  }
  if (!missing(height)) {
    height <- convert_to_inches(height, units)
  }
  # if either width or height is not specified, display an information message
  # units are those specified by the user
  if (missing(width) || missing(height)) {
    message("Saving ", prettyNum(convert_from_inches(width * scale, units), digits=3), " x ", prettyNum(convert_from_inches(height * scale, units), digits=3), " ", units, " image")
  }
  
  width <- width * scale
  height <- height * scale
  
  if (limitsize && (width >= 50 || height >= 50)) {
    stop("Dimensions exceed 50 inches (height and width are specified in inches/cm/mm, not pixels).",
         " If you are sure you want these dimensions, use 'limitsize=FALSE'.")
  }
  
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  device(file=filename, width=width, height=height, ...)
  on.exit(capture.output(dev.off()))
  print(plot)
  invisible()
}