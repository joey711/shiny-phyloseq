################################################################################
# bar_plot sbp definition
################################################################################
sbp_bar = sidebarPanel(actionButton("actionb_bar", "Re-Build Graphic", icon("refresh")),
                       br(),
                       uiOutput("bar_uix_xvar"),
                       uiOutput("bar_uix_colvar"),
                       textInput("facform_bar", "Facet Formula:", value="NULL"),
                       radioButtons("uicttype_bar", label="Abundance Data Type",
                                    choices=c("Counts", "Proportions")),
                       tags$hr(),
                       uipal("pal_bar"),
                       h4('Figure Dimensions'),
                       numericInput("width_bar", "Figure Width (inches)", 8, 1, 100, 1),
                       numericInput("height_bar", "Figure Height (inches)", 8, 1, 100, 1),
                       graphicTypeUI("downtype_bar"),
                       downloadButton('downloadBar', 'Download Graphic')
)
################################################################################
barpage = make_fluidpage("", sbp_bar, "bar")
