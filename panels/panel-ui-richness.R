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
  fluidRow(column(width = 12,                  
                  div(class='span6', uiOutput("richness_uix_x")),
                  div(class='span6', uiOutput("richness_uix_color")),
                  div(class='span5', uiOutput("richness_uix_shape")),
                  div(class='span7', 
                      selectInput(inputId="measures_rich",
                              label="Alpha Diversity Measures:",
                              choices=richmeasvars, 
                              selected=c("Shannon", "Chao1"),
                              multiple=TRUE))
  )),
  fluidRow(column(width = 12,
                  h4('Figure Details'),
  numericInputRow(inputId = "label_max_rich",
                  label = "Max. Labels",
                  value = 30L, min = 0L, step = 1L),
  sliderInput("x_axis_angle_rich", label = "x axis angle",
              value = 90,
              min = 0, max = 360, step = 45, ticks = TRUE),
  uipal("pal_rich"),
  uitheme("theme_rich"),
  uiptsz("size_rich"),
  uialpha("alpha_rich"),
  radioButtons(inputId="uicttype_rich",
               label="Source Data",
               choices=c("Original", "Filtered"),
               selected="Original",
               inline = TRUE),
  tags$hr(),
  h4('Figure Dimensions'),
  numericInput("width_rich", "Figure Width (inches)", 8, 1, 100, 1),
  numericInput("height_rich", "Figure Height (inches)", 8, 1, 100, 1),
  graphicTypeUI("downtype_rich"),
  downloadButton('downloadRichness', 'Download Graphic')
))
)
################################################################################
#richpage = make_fluidpage("Alpha Diversity Estimates", sbp_rich, "richness")

## Trial of non-sidebarLayout page. Needs design-devel...
## https://github.com/rstudio/shiny/wiki/Shiny-Application-Layout-Guide#grid-layouts-in-depth
richpage = fluidPage(
  headerPanel("Alpha Diversity Estimates", "windowTitle"), 
    fluidRow(
      #do.call("column", args = c(list(width=6), sbp_rich)),
      #column(width = 4, sbp_rich),
      sbp_rich,
      column(width = 8, plotOutput("richness"), offset = 0)
    )
)
