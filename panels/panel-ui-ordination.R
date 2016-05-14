################################################################################
# sbp of plot_ordination 
################################################################################
ordtypelist = as.list(phyloseq::plot_ordination("list"))[1:4]
names(ordtypelist) <- c("Samples", "Species", "Biplot", "Split Plot")
sbp_ord = sidebarPanel(
  h4("Define Ordination"),
  fluidRow(column(width = 12,
                  div(class="col-md-4",
                      selectInput(inputId = "ord_plot_type",
                                  label = "Display",
                                  choices = ordtypelist)), 
                  div(class="col-md-4",
                      selectInput(inputId = "ord_method",
                                  label = "Method", 
                                  choices = ordlist, 
                                  selected="MDS")),
                  div(class="col-md-4", uiOutput("ord_uix_dist"))
  )),
  fluidRow(column(width = 12,
                  div(class="col-md-4", 
                      selectInput(inputId = "transform_ord", 
                                  label = "Transform", 
                                  choices = c("Counts", "Prop", "RLog", "CLR"), 
                                  selected = "Prop")),
                  div(class="col-md-7", uiOutput("ord_uix_constraint"))
  )),
  fluidRow(column(width = 12,
                  div(class="col-md-4",
                      h4("Aesthetic Mapping")),
                  div(class="col-md-4", 
                      numericInputRow(inputId = "axis_x_ord",
                                      label = "X", 
                                      value = 1L, 
                                      min = 1L, 
                                      step = 1L, 
                                      class = "col-md-12")),
                  div(class="col-md-4",
                      numericInputRow(inputId = "axis_y_ord",
                                      label = "Y",
                                      value = 2L, 
                                      min = 1L, 
                                      step = 1L, 
                                      class = "col-md-12"))
  )),
  fluidRow(column(width = 12,
    div(class="col-md-6", uiOutput("ord_uix_color")),
    div(class="col-md-5", uiOutput("ord_uix_shape")),
    div(class='col-md-6', uiOutput("ord_uix_facetrow")),
    div(class='col-md-5', uiOutput("ord_uix_facetcol")),
    div(class='col-md-12', uiOutput("ord_uix_tooltip")),
    div(class='col-md-6', uiOutput("ord_uix_label")),
    div(class='col-md-3', 
        numericInputRow(inputId = "label_size_ord",
                        label = "Lab Sz", 
                        value = 3, 
                        min = 0.5, 
                        step=0.5, 
                        class = "col-md-12")),
    div(class='col-md-3',
        numericInputRow(inputId = "label_vjust_ord",
                        label = "V-Just", 
                        value = 2,
                        min = 0, 
                        class = "col-md-12"))
  )),
  theme_ui_details("_ord", ptsz=TRUE, alpha=TRUE),
  dim_and_down("_ord"),
width = 4)
################################################################################
ordpage = fluidPage(theme = shinytheme("cosmo"),
  headerPanel("Ordination Plot"),
  fluidRow(sbp_ord,
           column(width = 8, 
                  column(width = 12, offset = 0, 
                         plotlyOutput("ordination_plotly")
                  ),
                  column(width = 6, offset = 6, 
                         plotlyOutput("scree_plotly")
                  ))
  ),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/ordination.md")
  ))
)
################################################################################
