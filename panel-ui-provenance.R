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
sbp_prov = sidebarPanel(submitButton("Render Code", icon("refresh")),
                       br(), h3("Not sure what else to put here yet. A download button perhaps."))
provpage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel=sbp_prov,
    mainPanel=mainPanel(
      h4("Record of App-Executed Code. Make sure to click the 'Render Code' button."),
      tags$hr(),
      htmlOutput('provenance')
    )
  )
)
