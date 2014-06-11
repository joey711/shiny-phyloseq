# R Color Brewer
#RColorBrewer:::namelist
#RColorBrewer::brewer.pal.info
#
# Wes Anderson Palettes - Add these options. Eventually.
#
# https://github.com/karthik/wesanderson#wes-anderson-palettes
#
################################################################################
# Color Palette sbp definition
################################################################################
sbp_pal = sidebarPanel(submitButton("Build/Rebuild Example Plot", icon("refresh")),
                       br(), uipal("pal_main"))
palpage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel=sbp_pal,
    mainPanel=mainPanel(
      h4("Explore Different Palettes"),
      plotOutput("paletteExample"),
      tags$hr(),
      plotOutput("paletteOptions"),
      h4("Palette Details:"),
      dataTableOutput('paletteTable')
    )
  )
)


