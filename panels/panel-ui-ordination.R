################################################################################
# sbp of plot_ordination 
################################################################################
ordtypelist = as.list(phyloseq::plot_ordination("list"))
names(ordtypelist) <- c("Samples", "Species", "Biplot", "Split Plot", "Scree Plot")
sbp_ord = sidebarPanel(
  h4("Structure"),
  fluidRow(column(
    width = 12,
    div(class="span3", uitype("type_ord", "samples")),
    div(class="span4", selectInput("ord_method", "Method", ordlist, selected="DCA")),
    div(class="span5", uidist("dist_ord")),
    div(class="span4", selectInput("ord_plot_type", "Display", ordtypelist)), 
    div(class="span7", textInput("formula", "Constraint", value="NULL"))
  )),
  h4("Aesthetic Mapping"),
  fluidRow(column(
    width = 12,
    div(class="span6", uiOutput("ord_uix_color")),
    div(class="span5", uiOutput("ord_uix_shape"))
  )),
  theme_ui_details("_ord", ptsz=TRUE, alpha=TRUE),
  dim_and_down("_ord")
)
################################################################################
# ordpage = make_fluidpage("", sbp_ord, "ordination")
dummyText = paste0(rep("  \n  ", 7), collapse = "")
ordpage = fluidPage(
  headerPanel("Ordination Plot"),
  fluidRow(
    sbp_ord,
    column(width=8, 
           plotOutput("ordination"),
           h4("Scree"),
           #       tags$hr(dummyText),
           #       tags$br(dummyText),
           #       tags$hr(dummyText),
           #       tags$br(dummyText),
           #       tags$hr(dummyText),
           plotOutput("scree_ord")
  )),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/ordination.md")
  ))
)

