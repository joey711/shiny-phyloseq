################################################################################
# sbp of plot_richness
################################################################################
richmeasvars = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
uialphameas = selectInput(inputId="measures_rich",
                          label="Alpha Diversity Measures:",
                          choices=richmeasvars, 
                          selected=c("Shannon", "Chao1"),
                          multiple=TRUE)
sbp_rich = sidebarPanel(uialphameas,
                        uiOutput("richness_uix_x"), 
                        uiOutput("richness_uix_color"),
                        uiOutput("richness_uix_shape"),
                        numericInput(inputId = "label_max_rich", label = "Max. Labels",
                                     value = 30L, min = 0L, step = 1L),
                        h4('Figure Details'),
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
)
################################################################################
richpage = make_fluidpage("", sbp_rich, "richness")
