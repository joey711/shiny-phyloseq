################################################################################
# sbp of plot_ordination 
################################################################################
ordtypelist = as.list(phyloseq::plot_ordination("list"))
names(ordtypelist) <- c("Samples", "Species", "Biplot", "Split Plot", "Scree Plot")
sbp_ord = sidebarPanel(
  h4("Structure"),
  fluidRow(column(
    width = 12,
    div(class="span4", selectInput("ord_plot_type", "Display", ordtypelist)), 
    div(class="span4", selectInput("ord_method", "Method", ordlist, selected="DCA")),
    div(class="span4", uiOutput("ord_uix_dist"))
  )),
  fluidRow(column(
    width = 12,
    div(class="span4", selectInput("transform_ord", "Transform",
                                   c("Counts", "Prop", "RLog", "CLR"))),
    div(class="span7", uiOutput("ord_uix_constraint"))
  )),
  fluidRow(column(width = 12,
                  div(class="span8", h4("Aesthetic Mapping")),
                  div(class="span2", numericInputRow("axis_x_ord", "X", 1L, 1L, step = 1L, class = "span12")),
                  div(class="span2", numericInputRow("axis_y_ord", "Y", 2L, 1L, step = 1L, class = "span12"))
  )),
  fluidRow(column(
    width = 12,
    div(class="span6", uiOutput("ord_uix_color")),
    div(class="span5", uiOutput("ord_uix_shape")),
    div(class='span6', uiOutput("ord_uix_facetrow")),
    div(class='span5', uiOutput("ord_uix_facetcol")),
    div(class='span7', uiOutput("ord_uix_label")),
    div(class='span2', 
        numericInputRow("label_size_ord", "Lab Sz", 3, 0.5, step=0.5, class = "span12")),
    div(class='span2',
        numericInputRow("label_vjust_ord", "V-Just", 2, 0, class = "span12"))
  )),
  theme_ui_details("_ord", ptsz=TRUE, alpha=TRUE),
  dim_and_down("_ord")
)
################################################################################
ordpage = fluidPage(
  headerPanel("Ordination Plot"),
  fluidRow(sbp_ord,
    column(width = 8,
           div(class="span8", plotOutput("ordination")),
           div(class="span8", br()),
           div(class="span8", br()),
           div(class="span8", h5("Scree Plot", icon("bar"))),
           div(class="span8", plotOutput("scree_ord"))
  )),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/ordination.md")
  ))
)
################################################################################
