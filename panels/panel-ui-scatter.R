################################################################################
# sbp for scatter plot
################################################################################
sbp_scat = sidebarPanel(#actionButton("actionb_scat", "Re-Build Plot", icon("refresh")),
  h4("Aesthetic Mapping"),
  fluidRow(column(
    width = 12,
    div(class="span6", uiOutput("scat_uix_x")),
    div(class="span5", uiOutput("scat_uix_y")),  
    div(class="span6", uiOutput("scat_uix_color")),
    div(class="span5", uiOutput("scat_uix_shape")),
    div(class='span6', uiOutput("scat_uix_facetrow")),
    div(class='span5', uiOutput("scat_uix_facetcol")),
    div(class="span5", uicttype("uicttype_scat"))
  )),
  theme_ui_details("_scat", ptsz=TRUE, alpha=TRUE),
  dim_and_down("_scat")
)
################################################################################
scatpage = make_fluidpage("Flexible Scatter Plot", sbp_scat, "scatter", "scatter.md")
