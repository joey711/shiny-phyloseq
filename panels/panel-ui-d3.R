################################################################################
# sbp for d3 network
################################################################################
sbp_d3 = sidebarPanel(
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
################################################################################
#   radioButtons("d3_zoom", label = "Zooming",
#                choices = list('Zoom'=TRUE, 'Not Zoom'=FALSE),
#                selected = FALSE),
################################################################################
# d3network page
################################################################################
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
