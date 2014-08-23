################################################################################
# bar_plot sbp definition
################################################################################
sbp_bar = sidebarPanel(
  actionButton("actionb_bar", "(Re)Build Graphic", icon("signal")),
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,
                  div(class='span6', uiOutput("bar_uix_xvar", inline = TRUE)),
                  div(class='span6', uiOutput("bar_uix_colvar", inline = TRUE)),
                  div(class='span6', uiOutput("bar_uix_facetrow", inline = TRUE)),
                  div(class='span5', uiOutput("bar_uix_facetcol", inline = TRUE))
  )),
  radioButtons("uicttype_bar", label="Abundance Data Type", inline = TRUE,
               choices=c("Counts", "Proportions")),
  theme_ui_details("_bar", addList = list(div(class="span3", 
    numericInputRow("x_axis_angle_bar", label = "x-label angle",
                    value = 90, min = 0, max = 360, step = 45, class="span12")
  ))),
  dim_and_down("_bar")
)
################################################################################
barpage = make_fluidpage("Flexible Bar Plot", sbp_bar, "bar")
