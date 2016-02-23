################################################################################
# dfabund_plot sbp definition
################################################################################
sbp_dfabund = sidebarPanel(
  actionButton("actionb_dfabund", "(Re)Build Graphic", icon("signal")),
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,
                  div(class='col-md-6', uiOutput("dfabund_uix_xvar", inline = TRUE)),
                  div(class='col-md-6', uiOutput("dfabund_uix_colvar", inline = TRUE)),
                  div(class='col-md-6', uiOutput("dfabund_uix_facetrow", inline = TRUE)),
                  div(class='col-md-5', uiOutput("dfabund_uix_facetcol", inline = TRUE)),
                  div(class='col-md-6', selectInput("uicttype_dfabund", label="Data",
                                                 choices=c("Counts", "Proportions"))),
                  div(class="col-md-7", uiOutput("dfabund_uix_constraint"))
  )),
  theme_ui_details("_dfabund", addList = list(div(class="col-md-3", 
    numericInputRow("x_axis_angle_dfabund", label = "Angle",
                    value = 90, min = 0, max = 360, step = 45, class="col-md-12")
  ))),
  dim_and_down("_dfabund")
)
################################################################################
dfabundpage = make_fluidpage("Differential Abundance Plot", sbp_dfabund, "dfabund", "dfabund.md")
