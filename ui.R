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
                                    choices=c("Counts", "Proportions"))
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
                       uiptsz("size_ord"), uialpha("alpha_ord")
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
                         uialpha("alpha_alpha")
)
################################################################################
# sbp of plot_network
################################################################################
# ui for max distance to consider in initializing plot calculations
uinetdistmax = numericInput(inputId="uinetdistmax",
                            label="Network - Build Distance Threshold:",
                            step=0.1, value=0.9)
# ui for distance to display
uinetdispdist = numericInput(inputId="uinetdispdist",
                             label="Network - Edge Distance Display Threshold:",
                             step=0.1, value=0.3)
sbp_net = sidebarPanel(uibutton, br(), uitype("type_net", "samples"),
                       uidist("dist_net"),
                       uiOutput("network_uix_color"),
                       uiOutput("network_uix_shape"),
                       uinetdistmax, uinetdispdist,
                       uiptsz("size_net"), uialpha("alpha_net")
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
  uiOutput("tree_uix_point_thresh"),
  numericInput("margin_tree", "Margin", value=0.2, min=0, step=0.1)
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
  uicttype("uicttype_heat")
)
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
alphapage = make_fluidpage("", sbp_rich, "richness")
netpage = make_fluidpage("", sbp_net, "network")
barpage = make_fluidpage("", sbp_bar, "bar")
ordpage = make_fluidpage("", sbp_ord, "ordination")
treepage = make_fluidpage("", sbp_tree, "tree")
heatpage = make_fluidpage("", sbp_heat, "heatmap")
scatpage = make_fluidpage("", uiOutput("sbp_scat"), "scatter")
# Data I/O page
datapage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      submitButton("Load Selection", icon("refresh")),
      fileInput('file1', 'Choose file to upload'),
      tags$hr(),
      uiOutput("phyloseqDataset"),
      p('Testing data file input,',
        'will have to try some things. Legacy example links follow...',
        a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
        a(href = 'pressure.tsv', 'pressure.tsv'),
        'files, and then try uploading them.'
      )
    ),
    mainPanel(
      textOutput('contents')
    )
  )
)
################################################################################
# Define the full user-interface, `ui`
################################################################################
ui = navbarPage("Shiny + phyloseq",
                tabPanel("Select Dataset", datapage),
                tabPanel("Alpha Diversity", alphapage),
                tabPanel("Network", netpage),
                tabPanel("Abundance Bar Plot", barpage),
                tabPanel("Ordination", ordpage),
                tabPanel("Tree", treepage),
                tabPanel("Heatmap", heatpage),
                tabPanel("Scatter", scatpage)
)
shinyUI(ui)
