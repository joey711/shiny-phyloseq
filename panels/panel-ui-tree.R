################################################################################
# sbp for plot_tree()
################################################################################
sbp_tree = sidebarPanel(#actionButton("actionb_tree", "Re-Build Graphic", icon("tree")),
  h4("Structure"),
  fluidRow(column(width=12,
                  div(class="span4",
                      selectInput(inputId="method_tree", label="Points", 
                                  choices=list(None="treeonly", Dodged="sampledodge"))),
                  div(class="span4",
                      selectInput(inputId="justify_tree", label="Justify",
                                  choices=list(Left="left", Jagged="jagged"))),
                  div(class="span4",
                      selectInput(inputId="ladderize_tree", label="Ladderize",
                                  choices=list(Left="left", Right="right", `NULL`="NULL")))
  )),
  fluidRow(column(width=12,
                  div(class="span5",
                      selectInput("plot_tree_radial", label="Coordinates",
                                  choices=list(Cartesian="cartesian", Radial="radial"))),
                  div(class="span4", uiOutput("tree_uix_point_thresh")),
                  div(class="span3", 
                      numericInputRow("margin_tree", "Margin", value=0.2, min=0, step=0.1, class="span12"))
  )),
  h4("Aesthetic Mapping"),
  fluidRow(column(width=12,
                  div(class="span4", uiOutput("tree_uix_color")),
                  div(class="span4", uiOutput("tree_uix_shape")),
                  div(class="span4", uiOutput("tree_uix_tiplabs"))
  )),
  theme_ui_details("_tree", them=FALSE, ptsz=TRUE,
                   addList=list(
                     div(class="span4", uitheme("theme_tree", default = "blank")))),
  dim_and_down("_tree")
)
################################################################################
treepage = make_fluidpage("", sbp_tree, "tree", "tree.md")
