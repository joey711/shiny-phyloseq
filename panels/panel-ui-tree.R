################################################################################
# sbp for plot_tree()
################################################################################
sbp_tree = sidebarPanel(#actionButton("actionb_tree", "Re-Build Graphic", icon("tree")),
                        selectInput(inputId="method_tree", label="Tree Method", 
                                    choices=list(`No Points`="treeonly", `Dodged Points`="sampledodge")),
                        selectInput(inputId="justify_tree", label="Justify",
                                    choices=list(Jagged="jagged", Left="left"),
                                    selected="left"),
                        selectInput(inputId="ladderize_tree", label="Ladderize",
                                    choices=list(Right="right", Left="left", `NULL`="NULL"),
                                    selected="left"),
                        uiOutput("tree_uix_color"),
                        uiOutput("tree_uix_shape"),
                        uiOutput("tree_uix_tiplabs"),
                        radioButtons("plot_tree_radial", label="Coordinate System",
                                     choices=list(Cartesian="cartesian", Radial="radial")),
                        uiOutput("tree_uix_point_thresh"),
                        numericInput("margin_tree", "Margin", value=0.2, min=0, step=0.1),
                        tags$hr(),
                        h4('Figure Details'),
                        uiptsz("size_tree"), 
                        uipal("pal_tree"),
                        uitheme("theme_tree", default = "blank"),
                        h4('Figure Dimensions'),
                        numericInput("width_tree", "Figure Width (inches)", 8, 1, 100, 1),
                        numericInput("height_tree", "Figure Height (inches)", 8, 1, 100, 1),
                        graphicTypeUI("downtype_tree"),
                        downloadButton('downloadTree', 'Download Graphic')  
)
################################################################################
treepage = make_fluidpage("", sbp_tree, "tree")
