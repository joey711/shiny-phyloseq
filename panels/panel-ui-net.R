################################################################################
# sbp of plot_network
################################################################################
# ui for max distance to consider in initializing plot calculations
uinetdistmax = numericInputRow(
  inputId="uinetdistmax",
  label="Max D",
  min=0.0,
  max=1.0,
  value=netdist,
  step=0.1,
  class="span12"
)
sbp_net = sidebarPanel(
  h4('Network Structure'),
  fluidRow(column(width = 12,
                  div(class='span4', selectInput(inputId="type_net", label="Type", selected="samples",
                                                 choices=list("Taxa"="taxa", "Samples"="samples"))),
                  div(class='span8', uiOutput("net_uix_layout"))
  )),
  fluidRow(column(width = 12,
                  div(class="span4", selectInput("transform_net", "Transform",
                                                 c("Counts", "Prop", "RLog", "CLR"))),
                  div(class='span5', uidist("dist_net")),
                  div(class='span3', uinetdistmax)
  )),
  fluidRow(column(width = 12,
                  div(class='span8', uiOutput("net_uix_edgeSlider")))),
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,
                  div(class='span6', uiOutput("net_uix_color")),
                  div(class='span6', uiOutput("net_uix_shape")),
                  div(class='span7', uiOutput("net_uix_label")),
                  div(class='span2', 
                      numericInputRow("label_size_net", "Lab Sz", 3, 0.5, step=0.5, class = "span12")),
                  div(class='span2',
                      numericInputRow("label_vjust_net", "V-Just", 2, 0, class = "span12"))
  )),
  theme_ui_details("_net", them = FALSE, ptsz = TRUE, alpha = TRUE,
    addList = list(
      div(class="span3",
          numericInputRow("RNGseed_net", "R-Seed", value = 711L,
                          min = 1L, step = 1L, class="span12"))
  )),
  dim_and_down("_net")
)
################################################################################
netpage = make_fluidpage("Distance Threshold Network", sbp_net, "network",
                         markdownDoc = "network.md")
