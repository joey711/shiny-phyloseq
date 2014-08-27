################################################################################
# sbp of plot_ordination 
################################################################################
ordtypelist = as.list(phyloseq::plot_ordination("list"))
names(ordtypelist) <- c("Samples", "Species", "Biplot", "Split Plot", "Scree Plot")
sbp_ord = sidebarPanel(
  h4("Structure"),
  fluidRow(column(
    width = 12,
    div(class="span4", uitype("type_ord", "samples")),
    div(class="span4", selectInput("ord_method", "Method", ordlist, selected="DCA")),
    div(class="span4", uidist("dist_ord"))
  )),
  fluidRow(column(
    width = 12,
    div(class="span4", selectInput("ord_plot_type", "Display", ordtypelist)), 
    div(class="span7", uiOutput("ord_uix_constraint"))
  )),
  h4("Aesthetic Mapping"),
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
  fluidRow(
    sbp_ord,
    column(width=6, 
           plotOutput("ordination")
    ),
    column(width = 2, plotOutput("scree_ord"))
  ),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/ordination.md")
  ))
)
################################################################################
