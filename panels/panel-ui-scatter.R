################################################################################
# sbp for scatter plot
################################################################################
sbp_scat = sidebarPanel(
  actionButton("actionb_scat", "Re-Build Plot", icon("refresh")),
  br(),
  uiOutput("scat_uix_x"),
  uiOutput("scat_uix_y"),
  uiOutput("scat_uix_color"),
  uiOutput("scat_uix_shape"),
  textInput("facform_scat", "Facet Grid Formula:", value="NULL"),
  uicttype("uicttype_scat"),
  tags$hr(),
  h4('Figure Details'),
  uiptsz("size_scat"), uialpha("alpha_scat"),
  uipal("pal_scat"),
  uitheme("theme_scat"),
  tags$hr(),
  h4('Figure Dimensions'),
  numericInput("width_scat", "Figure Width (inches)", 8, 1, 100, 1),
  numericInput("height_scat", "Figure Height (inches)", 8, 1, 100, 1),
  graphicTypeUI("downtype_scat"),
  downloadButton('downloadScatter', 'Download Graphic')
)
################################################################################
scatpage = make_fluidpage("", sbp_scat, "scatter")
