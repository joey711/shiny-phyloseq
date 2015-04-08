################################################################################
# bar_plot sbp definition
################################################################################
sbp_bar = sidebarPanel(
  actionButton("actionb_bar", "(Re)Build Graphic", icon("signal")),
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,
                  div(class='col-md-6', uiOutput("bar_uix_xvar", inline = TRUE)),
                  div(class='col-md-6', uiOutput("bar_uix_colvar", inline = TRUE)),
                  div(class='col-md-6', uiOutput("bar_uix_facetrow", inline = TRUE)),
                  div(class='col-md-5', uiOutput("bar_uix_facetcol", inline = TRUE)),
                  div(class='col-md-6', selectInput("uicttype_bar", label="Data",
                                                 choices=c("Counts", "Proportions")))
  )),
  theme_ui_details("_bar", addList = list(div(class="col-md-3", 
    numericInputRow("x_axis_angle_bar", label = "Angle",
                    value = 90, min = 0, max = 360, step = 45, class="col-md-12")
  ))),
  dim_and_down("_bar")
)
################################################################################
barpage = make_fluidpage("Flexible Bar Plot", sbp_bar, "bar", "bar.md")
