################################################################################
# sbp of plot_ordination 
################################################################################
ordtypelist = as.list(phyloseq::plot_ordination("list"))
names(ordtypelist) <- c("Samples", "Species", "Biplot", "Split Plot", "Scree Plot")
sbp_ord = sidebarPanel(
  h4("Structure"),
  fluidRow(column(
    width = 12,
    div(class="col-md-4", selectInput("ord_plot_type", "Display", ordtypelist)), 
    div(class="col-md-4", selectInput("ord_method", "Method", ordlist, selected="DCA")),
    div(class="col-md-4", uiOutput("ord_uix_dist"))
  )),
  fluidRow(column(
    width = 12,
    div(class="col-md-4", selectInput("transform_ord", "Transform",
                                   c("Counts", "Prop", "RLog", "CLR"))),
    div(class="col-md-7", uiOutput("ord_uix_constraint"))
  )),
  fluidRow(column(width = 12,
                  div(class="col-md-8", h4("Aesthetic Mapping")),
                  div(class="col-md-2", numericInputRow("axis_x_ord", "X", 1L, 1L, step = 1L, class = "col-md-12")),
                  div(class="col-md-2", numericInputRow("axis_y_ord", "Y", 2L, 1L, step = 1L, class = "col-md-12"))
  )),
  fluidRow(column(
    width = 12,
    div(class="col-md-6", uiOutput("ord_uix_color")),
    div(class="col-md-5", uiOutput("ord_uix_shape")),
    div(class='col-md-6', uiOutput("ord_uix_facetrow")),
    div(class='col-md-5', uiOutput("ord_uix_facetcol")),
    div(class='col-md-7', uiOutput("ord_uix_label")),
    div(class='col-md-2', 
        numericInputRow("label_size_ord", "Lab Sz", 3, 0.5, step=0.5, class = "col-md-12")),
    div(class='col-md-2',
        numericInputRow("label_vjust_ord", "V-Just", 2, 0, class = "col-md-12"))
  )),
  theme_ui_details("_ord", ptsz=TRUE, alpha=TRUE),
  dim_and_down("_ord")
)
################################################################################
ordpage = fluidPage(theme = shinytheme("cosmo"),
  headerPanel("Ordination Plot"),
  fluidRow(sbp_ord,
    column(width = 8,
           div(class="col-md-12", plotOutput("ordination")),
           div(class="col-md-12", br())
  )),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/ordination.md")
  ))
)
################################################################################
