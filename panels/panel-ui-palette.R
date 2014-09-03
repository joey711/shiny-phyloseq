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
sbp_pal = sidebarPanel(
  uipal("pal_pal"),
  uitheme("theme_pal")
)
palpage = fluidPage(
  headerPanel("Palette and Theme Documentation Only"),
  sidebarLayout(
    sidebarPanel=sbp_pal,
    mainPanel=mainPanel(
      h4("Explore Different Palettes"),
      plotOutput("paletteExample"),
      fluidRow(column(width = 12, 
                      includeMarkdown("panels/paneldoc/palette.md")
      )),
      tags$hr(),
      plotOutput("paletteOptions"),
      h4("Palette Details:"),
      dataTableOutput('paletteTable')
    )
  )
)


