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
  class="col-md-12"
)
sbp_net = sidebarPanel(
  h4('Network Structure'),
  fluidRow(column(width = 12,
                  div(class='col-md-4',
                      selectInput(inputId="type_net",
                                  label="Type", 
                                  selected="samples",
                                  choices=list("Taxa"="taxa", "Samples"="samples"))),
                  div(class='col-md-8', uiOutput("net_uix_layout"))
  )),
  fluidRow(column(width = 12,
                  div(class="col-md-4", selectInput("transform_net", "Transform",
                                                 c("Counts", "Prop", "RLog", "CLR"))),
                  div(class='col-md-5', uidist("dist_net")),
                  div(class='col-md-3', uinetdistmax)
  )),
  fluidRow(column(width = 12,
                  div(class='col-md-8', uiOutput("net_uix_edgeSlider")))),
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,
                  div(class='col-md-6', uiOutput("net_uix_color")),
                  div(class='col-md-6', uiOutput("net_uix_shape")),
                  div(class='col-md-7', uiOutput("net_uix_label")),
                  div(class='col-md-2', 
                      numericInputRow("label_size_net", "Lab Sz", 3, 0.5, step=0.5, class = "col-md-12")),
                  div(class='col-md-2',
                      numericInputRow("label_vjust_net", "V-Just", 2, 0, class = "col-md-12"))
  )),
  theme_ui_details("_net", them = FALSE, ptsz = TRUE, alpha = TRUE,
    addList = list(
      div(class="col-md-3",
          numericInputRow("RNGseed_net", "R-Seed", value = 711L,
                          min = 1L, step = 1L, class="col-md-12"))
  )),
  dim_and_down("_net")
)
################################################################################
netpage = make_fluidpage("Distance Threshold Network", sbp_net, "network",
                         markdownDoc = "network.md")
