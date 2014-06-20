# Provenance: 
# http://en.wikipedia.org/wiki/Provenance
# "
# The primary purpose of tracing the provenance of an object or entity
# is normally to provide contextual and circumstantial evidence 
# for its original production or discovery...
# Comparative techniques, expert opinions, and the results of scientific tests
# may also be used to these ends, 
# but establishing provenance is essentially a matter of documentation.
# "
# And so here we are.
# 
################################################################################
# Provenance Tracking Code Record, sbp definition
################################################################################
sbp_prov = sidebarPanel(actionButton("actionb_prov", "Render Code", icon("code")),
                       br(),
                       h3("Records code/session up to button-click above."),
                       selectInput("compress_prov", "Compression Type",
                                   choices = c("none", "gzip", "bzip2", "xz"),
                                   selected = "gzip", multiple = FALSE),
                       downloadButton('downloadProvenance', 'Download Files')
)
provpage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel=sbp_prov,
    mainPanel=mainPanel(
      h5("Snippet of App-Executed Code. Click the 'Render Code' Button to Update."),
      tags$hr(),
      htmlOutput('provenance')
    )
  )
)
