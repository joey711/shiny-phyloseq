################################################################################
# sbp of plot_richness
################################################################################
richmeasvars = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
uialphameas = selectInput(inputId="measures_alpha",
                          label="Alpha Diversity Measures:",
                          choices=richmeasvars, 
                          selected=c("Chao1", "Shannon", "InvSimpson"),
                          multiple=TRUE)
sbp_rich = sidebarPanel(actionButton("actionb_rich", "(Re)Build Graphic", icon("bar-chart-o")),
                        br(), uialphameas,
                        uiOutput("richness_uix_x"), 
                        uiOutput("richness_uix_color"),
                        uiOutput("richness_uix_shape"),
                        h4('Figure Details'),
                        uipal("pal_rich"),
                        uitheme("theme_rich"),
                        uiptsz("size_rich"),
                        uialpha("alpha_rich"),
                        radioButtons(inputId="uicttype_rich", label="Source Data",
                                     choices=c("Original", "Filtered"),
                                     selected="Original"),
                        tags$hr(),
                        h4('Figure Dimensions'),
                        numericInput("width_rich", "Figure Width (inches)", 8, 1, 100, 1),
                        numericInput("height_rich", "Figure Height (inches)", 8, 1, 100, 1),
                        graphicTypeUI("downtype_rich"),
                        downloadButton('downloadRichness', 'Download Graphic')
)
################################################################################
richpage = make_fluidpage("", sbp_rich, "richness")
