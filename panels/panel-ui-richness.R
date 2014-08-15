################################################################################
# sbp of plot_richness
################################################################################
richmeasvars = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
#sbp_rich = list(
#sbp_rich = sidebarPanel(
#sbp_rich = wellPanel(
# sbp_rich = tags$div(class = "row-fluid",
#            tags$div(class = "span6",
#                     h5("RichTitle:"),
#                     selectInput(width = "40%",
#                                 inputId="measures_rich",
#                                 label="Alpha Diversity Measures:",
#                                 choices=richmeasvars, 
#                                 selected=c("Shannon", "Chao1"),
#                                 multiple=TRUE)
#            )),
#sbp_rich = tags$div(class = "row-fluid",
sbp_rich = sidebarPanel(
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,                  
                  div(class='span6', uiOutput("richness_uix_x")),
                  div(class='span6', uiOutput("richness_uix_color")),
                  div(class='span5', uiOutput("richness_uix_shape")),
                  div(class='span6', 
                      selectInput(inputId="measures_rich",
                              label="Alpha Diversity Measures:",
                              choices=richmeasvars, 
                              selected=c("Shannon", "Chao1"),
                              multiple=TRUE))
  )),
  h4('Details'),
  fluidRow(column(width = 12,
                  div(class='span3', uipal("pal_rich")),
                  div(class='span4', uitheme("theme_rich")),
                  div(class="span2", uiptsz("size_rich", class="span12")),
                  div(class="span2", uialpha("alpha_rich", class="span12"))
                  )),
  fluidRow(column(width = 12,
                  numericInputRow(inputId = "label_max_rich", 
                                  label = "Max. Labels",
                                  value = 30L, min = 0L, step = 1L, class="input-mini"),
                  numericInputRow("x_axis_angle_rich", label = "x-label angle",
                                  value = 90, min = 0, max = 360, step = 45, class="input-mini"),
                  radioButtons(inputId="uicttype_rich",
                               label="Source Data",
                               choices=c("Original", "Filtered"),
                               selected="Original",
                               inline = TRUE)
                  )),
  fluidRow(column(width = 12,
                  h4('Dimensions & Download'),
                  #div(class="controls controls-row",
                  div(class="span3", numericInputRow("width_rich", "Width (in)", 8, 1, 100, 1, class="span12")),
                  div(class="span3", numericInputRow("height_rich", "Height (in)", 8, 1, 100, 1, class="span12")),
                  div(class='span3', graphicTypeUI("downtype_rich")),
                  div(class='span2', div(style="display:inline-block", tags$label("DL"),
                                         downloadButton('downloadRichness', '  ')))
                  ))
)
################################################################################
## https://github.com/rstudio/shiny/wiki/Shiny-Application-Layout-Guide#grid-layouts-in-depth
richpage = fluidPage(
  headerPanel("Alpha Diversity Estimates", "windowTitle"), 
  fluidRow(
    sbp_rich,
    column(width = 8, plotOutput("richness"), offset = 0)
  ),
  fluidRow(
    column(width = 12,
      "Placeholder -  information about this tab here. See ",
      a(href="http://joey711.github.io/shiny-phyloseq/", "the plot_richness tutorial")
    )
  )
)
