################################################################################
# sbp of plot_richness
################################################################################
richmeasvars = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
sbp_rich = sidebarPanel(
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,                  
                  div(class='span6', uiOutput("rich_uix_x")),
                  div(class='span6', uiOutput("rich_uix_color")),
                  div(class='span5', uiOutput("rich_uix_shape")),
                  div(class='span6', 
                      selectInput(inputId="measures_rich",
                              label="alpha Measures",
                              choices=richmeasvars, 
                              selected=c("Shannon", "Chao1"),
                              multiple=TRUE)),
                  div(class='span7', uiOutput("rich_uix_label")),
                  div(class='span2', 
                      numericInputRow("label_size_rich", "Lab Sz", 3, 0.5, step=0.5, class = "span12")),
                  div(class='span2',
                      numericInputRow("label_vjust_rich", "V-Just", 2, 0, class = "span12"))
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
                  numericInputRow("x_axis_angle_rich", label = "Angle",
                                  value = 90, min = 0, max = 360, step = 45, class="input-mini"),
                  radioButtons(inputId="uicttype_rich",
                               label="Source Data",
                               choices=c("Original", "Filtered"),
                               selected="Original",
                               inline = TRUE)
                  )),
  dim_and_down("_rich")
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
    column(width = 12, includeMarkdown("panels/paneldoc/richness.md"))
  )
)
