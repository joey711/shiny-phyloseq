# http://shiny.rstudio.com/tutorial/lesson2/
# http://shiny.rstudio.com/articles/layout-guide.html
# http://stackoverflow.com/questions/20637248/shiny-4-small-textinput-boxes-side-by-side
# http://getbootstrap.com/2.3.2/base-css.html#forms
numericInputRow <- function(inputId, label, value, min = NA, max = NA, step = NA, class="input-small", ...){
  inputTag <- tags$input(id = inputId, type = "number", value = value, class=class, ...)
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  div(style="display:inline-block",
      #class="controls controls-row",
      tags$label(label, `for` = inputId), 
      inputTag)
}
# Define supported download format labels
vectorGraphicFormats = c("emf", "eps", "pdf", "tex", "svg", "wmf")
rasterGraphicFormats = c("bmp", "jpg", "png", "tiff")
graphicFormats = c(vectorGraphicFormats, rasterGraphicFormats)
graphicTypeUI = function(inputId, label="Format", choices=graphicFormats, selected="pdf"){
  selectInput(inputId, label, choices, selected, multiple = FALSE, selectize = TRUE)
}
# Type for distance/network/etc. Samples or Taxa
uitype = function(id="type", selected="taxa"){
  selectInput(inputId=id, label="Calculation: Samples or Taxa?",
              selected=selected,
              choices=list("Taxa"="taxa", "Samples"="samples"))
}
# ui for point size
uiptsz = function(id, ...){
  numericInputRow(inputId=id, label="Size", min=1, max=NA, value=5, step=1, ...)
}
# ui for point opacity slider
uialpha = function(id, ...){
  numericInputRow(inputId=id, label="Opacity", min=0, max=1, value=1, step=0.1, ...)
}
# UI function to define palettes. Reused in many panels.
uipal = function(id, default="Set1"){
  selectInput(id, "Palette",  
              choices = rownames(RColorBrewer::brewer.pal.info), 
              selected = default
  )
}
# UI function to define ggplot2 themes. Reused in many panels.
uitheme = function(id, default="black/white"){
  selectInput(id, "Theme",
              choices = names(shiny_phyloseq_ggtheme_list),
              selected = default
  )
}
################################################################################
# Generic distance UI stuff.
################################################################################
# List of distances
# Make available for relevant panels.
distlist = as.list(unlist(phyloseq::distance("list")))
names(distlist) <- distlist
#   Function to reate ui for distance method selection
#   NOTE: not all distance methods are supported if "taxa" selected for type. 
#   For example, the UniFrac distance and DPCoA cannot be calculated for taxa-wise 
#   distances, because they use a taxa-wise tree as part of their calculation 
#   between samples, and there is no transpose-equivalent for this tree
uidist = function(id, selected="bray"){
  distlist = as.list(unlist(phyloseq::distance("list")))
  names(distlist) <- distlist
  return(selectInput(id, "Distance Method", distlist, selected=selected))
}
# Whether to use proportions or counts
uicttype = function(id="uicttype"){
  radioButtons(inputId=id, label="Count Type",
               choices=c("Counts", "Proportions"),
               selected="Counts")
}
################################################################################
# Generic Ordination UI stuff.
################################################################################
# Define the ordination options list.
# Make available to other panels, if relevant.
ordlist = as.list(phyloseq::ordinate("list"))
names(ordlist) <- ordlist
ordlist = ordlist[-which(ordlist %in% c("MDS", "PCoA"))]
ordlist = c(list("MDS/PCoA"="MDS"), ordlist)
################################################################################
# Define each fluid page
################################################################################
# Define in a single function, a standard definition
make_fluidpage = function(fptitle="", sbp, outplotid){
  fluidPage(
    titlePanel(fptitle),
    sidebarLayout(
      sidebarPanel=sbp,
      mainPanel=mainPanel(plotOutput(outplotid))
    )
  )
}
################################################################################
source("panels/panel-ui-net.R", local = TRUE)
source("panels/panel-ui-bar.R", local = TRUE)
source("panels/panel-ui-ordination.R", local = TRUE)
source("panels/panel-ui-richness.R", local = TRUE)
source("panels/panel-ui-tree.R", local = TRUE)
source("panels/panel-ui-heatmap.R", local = TRUE)
source("panels/panel-ui-scatter.R", local = TRUE)
source("panels/panel-ui-d3.R", local = TRUE)
source("panels/panel-ui-data.R", local = TRUE)
source("panels/panel-ui-filter.R", local = TRUE)
source("panels/panel-ui-palette.R", local = TRUE)
source("panels/panel-ui-provenance.R", local = TRUE)
# Define the full user-interface, `ui`
################################################################################
ui = navbarPage(
  title = a(href="http://joey711.github.io/shiny-phyloseq/", style="color:#F0F0F0",  "Shiny-phyloseq"), 
  tabPanel("Select Dataset", datapage),
  tabPanel("Filter", filterpage),
  tabPanel("Alpha Diversity", richpage),
  tabPanel("Network", netpage),
  tabPanel("d3Network", d3netpage),
  tabPanel("Bar", barpage),
  tabPanel("Ordination", ordpage),
  tabPanel("Tree", treepage),
  tabPanel("Heatmap", heatpage),
  tabPanel("Scatter", scatpage),
  tabPanel("Palette", palpage),
  tabPanel("Provenance", provpage),
  collapsable = TRUE,
  theme = "bootstrap.css",
  windowTitle = "Shiny-phyloseq"
)
shinyUI(ui)
################################################################################
