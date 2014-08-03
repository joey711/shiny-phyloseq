
choicesDiffAbund = list("DESeq2", "edgeR", "edgeR-robust", "metagenomeSeq")
method_checks = checkboxGroupInput("diffabund_types", "Differential Abundance Methods",
                   choices = choicesDiffAbund,
                   #c(list("NULL"=NULL), choicesDiffAbund),
                   selected = NULL,
                   inline = FALSE)
sbp_diffabund = sidebarPanel(
  method_checks,
  numericInput("diffabund_sig_threshold", "P-Value Maximum",
               value = 0.05, min = 0.0, max=1.0, step = 0.05),
  uiOutput("diffabund_uix_testvars"),
  uiOutput("diffabund_uix_x"), 
  uiOutput("diffabund_uix_color"),
  uiOutput("diffabund_uix_shape"),
  numericInput("size_diffabund", "Point Size:", value = 5, min = 1, step = 1),
  numericInput("alpha_diffabund", "Point Opacity:", value = 1, min = 0, max = 1, step = 0.1),
  uitheme("theme_diffabund"),
  #numericInput("diffabund_ncol", "Number of Columns", value = 1L, min = 1L, step = 1L),
  tags$hr(),
  h4('Figure Dimensions'),
  numericInput("width_diffabund", "Figure Width (inches)", 8, 1, 100, 1),
  numericInput("height_diffabund", "Figure Height (inches)", 8, 1, 100, 1),
  graphicTypeUI("downtype_diffabund"),
  downloadButton('downloaddiffabund', 'Download Graphic')  
)

diffabundpage = make_fluidpage("", sbp_diffabund, "diffabund")
