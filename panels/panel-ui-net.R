################################################################################
# sbp of plot_network
################################################################################
# ui for max distance to consider in initializing plot calculations
uinetdistmax = numericInput(
  inputId="uinetdistmax",
  label="Edge Distance Ceiling",
  min=0.0,
  max=1.0,
  value=netdist,
  step=0.1
)
sbp_net = sidebarPanel(
  uitype("type_net", "samples"),
  uidist("dist_net"),
  uinetdistmax, 
  uiOutput("network_uix_edgeSlider"),
  uiOutput("network_uix_layout"),
  uiOutput("network_uix_color"),
  uiOutput("network_uix_shape"),
  uiOutput("network_uix_label"),
  tags$hr(),
  h4('Figure Details'),
  uiptsz("size_net"), uialpha("alpha_net"), uipal("pal_net"),
  tags$hr(),
  h4('Figure Dimensions'),
  numericInput("width_net", "Figure Width (inches)", 8, 1, 100, 1),
  numericInput("height_net", "Figure Height (inches)", 8, 1, 100, 1),
  graphicTypeUI("downtype_net"),
  downloadButton('downloadNetwork', 'Download Graphic')
)
################################################################################
netpage = make_fluidpage("", sbp_net, "network")
