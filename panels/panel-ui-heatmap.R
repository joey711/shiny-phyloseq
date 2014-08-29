################################################################################
# sbp for plot_heatmap()
################################################################################
sbp_heat = sidebarPanel(
  actionButton("actionb_heat", "(Re)Build Graphic", icon("qrcode")),
  h4("Structure"),
  fluidRow(column(width = 12,
                  div(class="span4",
                      selectInput("ord_method_heat", "Method", 
                                  ordlist, selected="NMDS")),
                  div(class="span4", uidist("dist_heat")),
                  div(class="span4", selectInput("transform_heat", "Transform",
                                                 c("Counts", "Prop", "RLog", "CLR")))
  )),
  h4("Labels"),
  fluidRow(column(width = 12,
                  div(class="span6", uiOutput("heat_sample_label")),
                  div(class="span6", uiOutput("heat_taxa_label"))
  )),
  h4("Manual Ordering"),
  fluidRow(column(width = 12,
                  div(class="span6", uiOutput("heat_sample_order")),
                  div(class="span6", uiOutput("heat_taxa_order"))
  )),
  h4("Color Scale"),
  fluidRow(column(width = 12,
                  div(class="span4", 
                      textInputRow("locolor_heat", "Low", "#000033", class="span12")),
                  div(class="span4", 
                      textInputRow("hicolor_heat", "High", "#66CCFF", class="span12")),
                  div(class="span4", 
                      textInputRow("NAcolor_heat", "Missing", "black", class="span12"))
  )),
  dim_and_down("_heat")
)
################################################################################
heatpage = make_fluidpage("Microbiome Heatmap", sbp_heat, "heatmap", "heatmap.md")