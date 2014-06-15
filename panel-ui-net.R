################################################################################
# sbp of plot_network
################################################################################
# ui for max distance to consider in initializing plot calculations
uinetdistmax = numericInput(inputId="uinetdistmax",
                            label="Max Considered Distance Threshold:",
                            min=0.0,
                            max=1.0,
                            value=netdist,
                            step=0.1)
# ui for distance to display
uinetdispdist = sliderInput("uinetdispdist", "Edge Distance Threshold Animation:",
                            animate=animationOptions(interval=interval, loop=loop),
                            min=0.0,
                            max=netdist,
                            value=0.5*netdist,
                            step=step)
sbp_net = sidebarPanel(actionButton("actionb_net", "Re-build Network", icon("refresh")),
                       br(),
                       uinetdispdist,
                       uinetdistmax, 
                       uitype("type_net", "samples"),                       
                       uidist("dist_net"),
                       uiOutput("network_uix_layout"),
                       uiOutput("network_uix_color"),
                       uiOutput("network_uix_shape"),
                       uiOutput("network_uix_label"),
                       tags$hr(),
                       h4('Figure Details'),
                       uiptsz("size_net"), uialpha("alpha_net"), uipal("pal_net"),
                       tags$hr(),
                       h4('Figure Dimensions'),
                       numericInput("width_net", "Figure Width (inches)", 8, 1, 100, 1),
                       numericInput("height_net", "Figure Height (inches)", 8, 1, 100, 1),
                       graphicTypeUI("downtype_net"),
                       downloadButton('downloadNetwork', 'Download Graphic')
)
################################################################################
netpage = make_fluidpage("", sbp_net, "network")
