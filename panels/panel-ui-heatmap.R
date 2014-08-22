################################################################################
# sbp for plot_heatmap()
################################################################################
sbp_heat = sidebarPanel(
  actionButton("actionb_heat", "Re-Build Graphic", icon("qrcode")),
  br(),
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
  dim_and_down("_heat")
)
################################################################################
heatpage = make_fluidpage("", sbp_heat, "heatmap")