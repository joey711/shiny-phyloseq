################################################################################
# sbp of plot_network
################################################################################
# ui for max distance to consider in initializing plot calculations
uinetdistmax = numericInputRow(
  inputId="uinetdistmax",
  label="Max.D",
  min=0.0,
  max=1.0,
  value=netdist,
  step=0.1,
  class="span12"
)
sbp_net = sidebarPanel(
  h4('Network Structure'),
  fluidRow(column(width = 12,
                  div(class='span3', selectInput(inputId="type_net", label="", selected="samples",
                                                 choices=list("Taxa"="taxa", "Samples"="samples"))),
                  div(class='span2', uinetdistmax),
                  div(class='span6', uiOutput("network_uix_layout"))
  )),
  uidist("dist_net"),
  uiOutput("network_uix_edgeSlider"),
  h4('Aesthetic Mapping'),
  fluidRow(column(width = 12,
                  div(class='span6', uiOutput("network_uix_color")),
                  div(class='span6', uiOutput("network_uix_shape")),
                  div(class='span6', uiOutput("network_uix_label"))
  )),
  theme_ui_details("_net", them = FALSE, ptsz = TRUE, alpha = TRUE,
    addList = list(div(class="span3", 
                       numericInputRow("text_size_net", label="Label Size",
                                       min=1, max=NA, value=8, step=2, class="span12")),
                   div(class="span3",
                       numericInputRow("text_hjust_net", label="Label h-just",
                                       min=1, max=NA, value=1, step=0.1, class="span12")),
                   div(class="span3",
                       numericInputRow("RNGseed_net", "RNG Seed", value = 711L,
                                       min = 1L, step = 1L, class="span12"))
                   )
  ),
  dim_and_down("_net")
)
################################################################################
netpage = make_fluidpage("Distance Threshold Network", sbp_net, "network")
