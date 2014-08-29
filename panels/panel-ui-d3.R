################################################################################
# sbp for d3 network
################################################################################
sbp_d3 = sidebarPanel(
  h4('Network Structure'),
  fluidRow(column(width = 12,
    div(class='span6',
        selectInput(inputId = "type_d3",
                    label="Type",
                    choices=list("Taxa"="taxa", "Samples"="samples"),
                    selected="taxa")
    ),
    div(class="span6", selectInput("transform_d3", "Transform",
                                   c("Counts", "Prop", "RLog", "CLR")))
  )),
  fluidRow(column(width = 12,
    div(class='span7',
        selectInput("dist_d3", "Distance", distlist, d3DefaultDistance)
    ),
    div(class='span5',
        numericInputRow(inputId = "dist_d3_threshold",
                        label = "Max D",
                        value = LinkDistThreshold,
                        min = 0, max = 1, step = 0.025, class="span12")
    )
  )),
  h4('Aesthetic Mapping'),
  fluidRow(column(
    width = 12,
    div(class='span6', uiOutput("d3_uix_node_label")),
    div(class='span6', uiOutput("d3_uix_color"))
  )),
  h4('Details'),
  fluidRow(column(
    width = 12,
    div(class='span3', numericInputRow(inputId="d3_opacity", label="Opacity", min=0, max=1, value=1, step=0.1, class="span12")),
    # Edge Scale
    div(class='span3', numericInputRow("d3_link_scale", "Scale", 
                                       value = d3DefaultLinkScaleFactor,
                                       min = 1, step = 5, class="span12")),
    # Single Edge Shade/Color
    div(class='span3', textInput("d3_link_color", label = "Shade", value = "#666"))
  )),
  fluidRow(column(
    width = 12,
    h4("Dimensions and Download"),
    div(class="span3", numericInputRow("width_d3", "Width", 600, 200, 1600, 100, class="span12")),
    div(class="span3", numericInputRow("height_d3", "Height", 600, 200, 1600, 100, class="span12")),
    div(class='span2', div(style="display:inline-block", tags$label("DL"), downloadButton("download_D3", " ")))
  ))
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
  headerPanel('Distance Threshold Network - D3'),
  # Sidebar with a slider input for node opacity
  sbp_d3,
  # Show network graph
  mainPanel(htmlOutput("D3Network")),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/d3Network.md")
  ))
)
