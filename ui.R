# Default options for app startup
source("default-parameters.R", local=FALSE)
source("ggsave.R", local=FALSE)
# List of distances
distlist = as.list(unlist(phyloseq::distance("list")))
names(distlist) <- distlist
# ui submit button for input changes
uibutton = submitButton("Build/Rebuild Plot", icon("refresh"))
# Type for distance/network/etc. Samples or Taxa
uitype = function(id="type", selected="taxa"){
  selectInput(inputId=id, label="Calculation: Samples or Taxa?",
              selected=selected,
              choices=list("Taxa"="taxa", "Samples"="samples"))
}
# ui for point size slider
uiptsz = function(id="size"){
  sliderInput(inputId=id, label="Point Size:", min=1, max=10, value=5, step=1)
}
# ui for point opacity slider
uialpha = function(id="alpha"){
  sliderInput(inputId=id, label="Opacity:", min=0, max=1, value=1, step=0.1)
}
#   Function to reate ui for distance method selection
#   NOTE: not all distance methods are supported if "taxa" selected for type. 
#   For example, the UniFrac distance and DPCoA cannot be calculated for taxa-wise 
#   distances, because they use a taxa-wise tree as part of their calculation 
#   between samples, and there is no transpose-equivalent for this tree
uidist = function(id, selected="bray"){
  distlist = as.list(unlist(phyloseq::distance("list")))
  names(distlist) <- distlist
  return(selectInput(id, "Distance Method:", distlist, selected=selected))
}
# Whether to use proportions or counts
uicttype = function(id="uicttype"){
  radioButtons(inputId=id, label="Count Type",
               choices=c("Counts", "Proportions"),
               selected="Counts")
} 
# Define the ordination options list.
ordlist = as.list(phyloseq::ordinate("list"))
names(ordlist) <- ordlist
ordlist = ordlist[-which(ordlist %in% c("MDS", "PCoA"))]
ordlist = c(list("MDS/PCoA"="MDS"), ordlist)
################################################################################
# bar_plot sbp definition
################################################################################
sbp_bar = sidebarPanel(uibutton, br(),
                       uiOutput("bar_uix_xvar"),
                       uiOutput("bar_uix_colvar"),
                       textInput("facform_bar", "Facet Formula:", value="NULL"),
                       radioButtons("uicttype_bar", label="Abundance Data Type",
                                    choices=c("Counts", "Proportions")),
                       tags$hr(),
                       h4('Figure Dimensions'),
                       numericInput("width_bar", "Figure Width (inches)", 8, 1, 100, 1),
                       numericInput("height_bar", "Figure Height (inches)", 8, 1, 100, 1),
                       graphicTypeUI("downtype_bar"),
                       downloadButton('downloadBar', 'Download Graphic')
)
################################################################################
# sbp of plot_ordination 
################################################################################
ordtypelist = as.list(phyloseq::plot_ordination("list"))
names(ordtypelist) <- c("Samples", "Species", "Biplot", "Split Plot", "Scree Plot")
sbp_ord = sidebarPanel(uibutton, br(), uitype("type_ord", "samples"),
                       uidist("dist_ord"),
                       uiOutput("ord_uix_color"),
                       uiOutput("ord_uix_shape"),
                       selectInput("ord_method", "Ordination Method:", ordlist, selected="DCA"),
                       selectInput("ord_plot_type", "Ordination Plot Type:", ordtypelist), 
                       textInput("formula", "Ordination Constraint Formula", value="NULL"),
                       uiptsz("size_ord"), uialpha("alpha_ord"),
                       tags$hr(),
                       h4('Figure Dimensions'),
                       numericInput("width_ord", "Figure Width (inches)", 8, 1, 100, 1),
                       numericInput("height_ord", "Figure Height (inches)", 8, 1, 100, 1),
                       graphicTypeUI("downtype_ord"),
                       downloadButton('downloadOrdination', 'Download Graphic')
)
################################################################################
# sbp of plot_richness
################################################################################
richmeasvars = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
uialphameas = selectInput(inputId="measures_alpha",
                          label="Alpha Diversity Measures:",
                          choices=richmeasvars, 
                          selected=c("Chao1", "Shannon", "InvSimpson"),
                          multiple=TRUE)
sbp_rich = sidebarPanel(uibutton, br(), uialphameas,
                        uiOutput("richness_uix_x"), 
                        uiOutput("richness_uix_color"),
                        uiOutput("richness_uix_shape"),
                        uiptsz("size_alpha"),
                        uialpha("alpha_alpha"),
                        tags$hr(),
                        h4('Figure Dimensions'),
                        numericInput("width_rich", "Figure Width (inches)", 8, 1, 100, 1),
                        numericInput("height_rich", "Figure Height (inches)", 8, 1, 100, 1),
                        graphicTypeUI("downtype_rich"),
                        downloadButton('downloadRichness', 'Download Graphic')
)
################################################################################
# sbp of plot_network
################################################################################
# ui for max distance to consider in initializing plot calculations
uinetdistmax = numericInput(inputId="uinetdistmax",
                            label="Max Considered Distance Threshold:",
                            min=0.0,
                            max=1.0,
                            value=0.5,
                            step=0.1)
# ui for distance to display
uinetdispdist = sliderInput("uinetdispdist",
                            "Edge Distance Threshold:",
                            min=0.0,
                            max=1.0,
                            value=0.4,
                            step=0.1)
sbp_net = sidebarPanel(uibutton, br(), uitype("type_net", "samples"),
                       uidist("dist_net"),
                       uiOutput("network_uix_color"),
                       uiOutput("network_uix_shape"),
                       uinetdistmax, 
                       uinetdispdist,
                       uiptsz("size_net"), uialpha("alpha_net"),
                       tags$hr(),
                       h4('Figure Dimensions'),
                       numericInput("width_net", "Figure Width (inches)", 8, 1, 100, 1),
                       numericInput("height_net", "Figure Height (inches)", 8, 1, 100, 1),
                       graphicTypeUI("downtype_net"),
                       downloadButton('downloadNetwork', 'Download Graphic')
)
################################################################################
# sbp for plot_tree()
################################################################################
sbp_tree = sidebarPanel(uibutton,
  selectInput(inputId="method_tree", label="Tree Method", 
              choices=list(`No Points`="treeonly", `Dodged Points`="sampledodge")),
  selectInput(inputId="justify_tree", label="Justify",
              choices=list(Jagged="jagged", Left="left"),
              selected="left"),
  selectInput(inputId="ladderize_tree", label="Ladderize",
              choices=list(Right="right", Left="left", `NULL`="NULL"),
              selected="left"),
  uiOutput("tree_uix_color"),
  uiOutput("tree_uix_shape"),
  uiptsz("size_tree"), 
  uiOutput("tree_uix_tiplabs"),
  radioButtons("plot_tree_radial", label="Coordinate System",
               choices=list(Cartesian="cartesian", Radial="radial")),
  uiOutput("tree_uix_point_thresh"),
  numericInput("margin_tree", "Margin", value=0.2, min=0, step=0.1),
  tags$hr(),
  h4('Figure Dimensions'),
  numericInput("width_tree", "Figure Width (inches)", 8, 1, 100, 1),
  numericInput("height_tree", "Figure Height (inches)", 8, 1, 100, 1),
  graphicTypeUI("downtype_tree"),
  downloadButton('downloadTree', 'Download Graphic')  
)
################################################################################
# sbp for plot_heatmap()
################################################################################
sbp_heat = sidebarPanel(
  uibutton, br(),
  selectInput("ord_method_heat", "Ordination Method (axis ordering):", 
              ordlist, selected="NMDS"),
  uidist("dist_heat"),
  uiOutput("heat_sample_label"),
  uiOutput("heat_taxa_label"),
  uiOutput("heat_sample_order"),
  uiOutput("heat_taxa_order"),
  textInput("locolor_heat", "Low Color", "#000033"),
  textInput("hicolor_heat", "High Color", "#66CCFF"),
  textInput("NAcolor_heat", "Missing Value Color", "black"),
  uicttype("uicttype_heat"),
  tags$hr(),
  h4('Figure Dimensions'),
  numericInput("width_heat", "Figure Width (inches)", 8, 1, 100, 1),
  numericInput("height_heat", "Figure Height (inches)", 8, 1, 100, 1),
  graphicTypeUI("downtype_heat"),
  downloadButton('downloadHeat', 'Download Graphic')
)
################################################################################
# sbp for scatter plot
################################################################################
sbp_scat = sidebarPanel(uibutton, br(), 
  uiOutput("scat_uix_x"),
  uiOutput("scat_uix_y"),
  uiOutput("scat_uix_color"),
  uiOutput("scat_uix_shape"),
  textInput("facform_scat", "Facet Grid Formula:", value="NULL"),
  uiptsz("size_scat"), uialpha("alpha_scat"),
  uicttype("uicttype_scat"),
  tags$hr(),
  h4('Figure Dimensions'),
  numericInput("width_scat", "Figure Width (inches)", 8, 1, 100, 1),
  numericInput("height_scat", "Figure Height (inches)", 8, 1, 100, 1),
  graphicTypeUI("downtype_scat"),
  downloadButton('downloadScatter', 'Download Graphic')
)
################################################################################
# sbp for d3 network
################################################################################
sbp_d3 = sidebarPanel(
  submitButton("Reload d3", icon("refresh")),
  radioButtons(inputId = "type_d3",
               label="Network Node Type:",
               choices=list("Taxa"="taxa", "Samples"="samples"),
               selected="taxa"),  
  selectInput("dist_d3", "Distance Method:", distlist, d3DefaultDistance),
  numericInput(inputId = "dist_d3_threshold",
               label = "Edge Distance Threshold",
               value = LinkDistThreshold,
               min = 0, max = 1, step = 0.025),
  uiOutput("d3_uix_node_label"),
  uiOutput("d3_uix_color"),
  sliderInput(inputId="d3_opacity", label="Opacity:", min=0, max=1, value=1, step=0.1),
  numericInput("d3_link_scale", "Link Size Scaling Factor", 
               value = d3DefaultLinkScaleFactor, min = 1, step = 5),
  textInput("d3_link_color", label = "Link Color:", value = "#666"),
  numericInput(inputId = "width_d3",
               label = "Graphic Width [Pixels]",
               value = 700,
               min = 200, max = 1600, step = 100),
  numericInput(inputId = "height_d3",
               label = "Graphic Height [Pixels]",
               value = 600,
               min = 200, max = 1600, step = 100),
  tags$hr(),
  downloadButton('downloadd3', 'Download Graphic'),
  p('Big thanks to',
    a(href = 'http://christophergandrud.github.io/d3Network/', 'd3Network'),
    'and',
    a(href = 'http://shiny.rstudio.com/', 'Shiny', 'web apps.')
  )
)
#   radioButtons("d3_zoom", label = "Zooming",
#                choices = list('Zoom'=TRUE, 'Not Zoom'=FALSE),
#                selected = FALSE),
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
richpage = make_fluidpage("", sbp_rich, "richness")
# Trial of non-sidebarLayout page. Needs design-devel...
# https://github.com/rstudio/shiny/wiki/Shiny-Application-Layout-Guide#grid-layouts-in-depth
# richpage = fluidPage(titlePanel(""), 
#                       fluidRow(
#                         column(6, sbp_rich),
#                         column(6, plotOutput("richness"))
#                       ),
#                       fluidRow(column(12, 
#                                       p("Testing download plot:"),
#                                       graphicTypeUI("downtype_rich"),
#                                       downloadLink('downloadRichness', 'Download Graphic')
#                       ))
# )
netpage = make_fluidpage("", sbp_net, "network")
barpage = make_fluidpage("", sbp_bar, "bar")
ordpage = make_fluidpage("", sbp_ord, "ordination")
treepage = make_fluidpage("", sbp_tree, "tree")
heatpage = make_fluidpage("", sbp_heat, "heatmap")
scatpage = make_fluidpage("", sbp_scat, "scatter")
# Data I/O page
datapage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h4('Select Dataset'),
      submitButton("Load Selection", icon("refresh")),
      uiOutput("phyloseqDataset"),
      tags$hr(),
      h4('Import Biom-Format File'),
      fileInput('filebiom', "", multiple = TRUE),
      h4('Upload Pre-Imported .RData'),
      fileInput('file1', ""),
      tags$hr(),
      h4('microbio.me/qiime public data'),
      p('(requires phyloseq 1.9.5+)'),
      textInput("qiime_server_ID", "Identifier for QIIME server", value="NULL"),
      radioButtons("qiime_server_ext", "File Extension",
                   choices=list(".zip", ".tgz", ".tar.gz")),
      tags$hr(),
      p('Plot parameters:'),
      numericInput("dataset_count_threshold", "Count Threshold", value=3, min=0, step=1),
      p("See default-parameters.R to change default settings."),
      p('Big thanks to',
        a(href = 'http://shiny.rstudio.com/', 'Shiny', 'web apps.')
      )
    ),
    mainPanel(
      plotOutput("library_sizes"),
      plotOutput("OTU_count_thresh_hist"),
      tags$hr(),
      p("Data Summary:"),
      htmlOutput('contents')
    )
  )
)
# Data Filter page
filterpage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      submitButton("Execute Filter", icon("refresh")),
      p("  "),
      p('Filtering Parameters:'),
      tags$hr(),
      p("Subset Expressions:"),
      textInput("filter_subset_taxa_expr", "`subset_taxa()` Expression: (e.g. Phylum=='Firmicutes')", value="NULL"),
      textInput("filter_subset_samp_expr", "`subset_samples()` Expression: (e.g. SampleType %in% 'Feces')", value="NULL"),
      tags$hr(),
      p('Total Sums Filtering:'),
      numericInput("filter_sample_sums_threshold", "Sample Sums Count Threshold",
                   value=SampleSumDefault, min=0, step=100),
      numericInput("filter_taxa_sums_threshold", "OTU Sums Count Threshold",
                   value=OTUSumDefault, min=0, step=1),
      tags$hr(),
      p('kOverA OTU Filtering:'),
      numericInput("filter_kOverA_count_threshold", "`A` - The Count Value Threshold", value=kovera_A, min=0, step=1),
      uiOutput("filter_ui_kOverA_k")
    ),
    mainPanel(
      plotOutput("filter_summary_plot"),
      tags$hr(),
      p("Original Data:"),
      htmlOutput('filtered_contents0'),
      tags$hr(),
      p("Filtered Data:"),
      htmlOutput('filtered_contents'),
      tags$hr(),
      p("Sample Variables:"),
      textOutput('sample_variables'),
      tags$hr(),
      p("Taxonomic Ranks:"),
      textOutput('rank_names')      
    )
  )
)
# d3network page
d3netpage = fluidPage(
  # Load d3.js
  tags$head(
    tags$script(src = 'http://d3js.org/d3.v3.min.js')
  ),
  # Application title
  titlePanel(''),
  # Sidebar with a slider input for node opacity
  sbp_d3,
  # Show network graph
  mainPanel(htmlOutput("networkPlot"))
)
################################################################################
# Source the ui for palette panel.
source("color-palettes.R")
################################################################################
# Define the full user-interface, `ui`
################################################################################
ui = navbarPage("Shiny + phyloseq",
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
                theme = "bootstrap.css"
)
shinyUI(ui)
