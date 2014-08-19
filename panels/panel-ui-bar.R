################################################################################
# bar_plot sbp definition
################################################################################
sbp_bar = sidebarPanel(actionButton("actionb_bar", "(Re)Build Graphic", icon("signal")),
                       br(),
                       uiOutput("bar_uix_xvar"),
                       uiOutput("bar_uix_colvar"),
                       uiOutput("bar_uix_facetrow"),
                       uiOutput("bar_uix_facetcol"),
                       numericInputRow("x_axis_angle_bar", label = "x-label angle",
                                       value = 90, min = 0, max = 360, step = 45, class="input-mini"),
                       radioButtons("uicttype_bar", label="Abundance Data Type",
                                    choices=c("Counts", "Proportions")),
                       tags$hr(),
                       uipal("pal_bar"),
                       uitheme("theme_bar"),
                       h4('Figure Dimensions'),
                       numericInput("width_bar", "Figure Width (inches)", 8, 1, 100, 1),
                       numericInput("height_bar", "Figure Height (inches)", 8, 1, 100, 1),
                       graphicTypeUI("downtype_bar"),
                       downloadButton('downloadBar', 'Download Graphic')
)
################################################################################
barpage = make_fluidpage("", sbp_bar, "bar")
