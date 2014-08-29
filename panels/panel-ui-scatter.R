################################################################################
# sbp for scatter plot
################################################################################
sbp_scat = sidebarPanel(#actionButton("actionb_scat", "Re-Build Plot", icon("refresh")),
  h4("Aesthetic Mapping"),
  fluidRow(column(width = 12,
    div(class="span6", uiOutput("scat_uix_x")),
    div(class="span6", uiOutput("scat_uix_y"))
  )),
  fluidRow(column(width = 12,
    div(class="span6", uiOutput("scat_uix_color")),
    div(class="span6", uiOutput("scat_uix_shape"))
  )),
  fluidRow(column(width = 12,
    div(class='span6', uiOutput("scat_uix_facetrow")),
    div(class='span5', uiOutput("scat_uix_facetcol")),
    div(class="span4", selectInput("transform_scat", "Transform",
                                   c("Counts", "Prop", "RLog", "CLR")))
  )),
  fluidRow(column(width = 12,
    div(class='span7', uiOutput("scat_uix_label")),
    div(class='span2', 
        numericInputRow("label_size_scat", "Lab Sz", 3, 0.5, step=0.5, class = "span12")),
    div(class='span2',
        numericInputRow("label_vjust_scat", "V-Just", 2, 0, class = "span12"))
  )),
  theme_ui_details("_scat", ptsz=TRUE, alpha=TRUE),
  dim_and_down("_scat")
)
################################################################################
scatpage = make_fluidpage("Flexible Scatter Plot", sbp_scat, "scatter", "scatter.md")
