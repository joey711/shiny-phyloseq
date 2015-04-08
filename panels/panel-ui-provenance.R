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
sbp_prov = sidebarPanel(
  h4("Archive"),
  fluidRow(column(width = 12,
    div(class="col-md-5", 
        selectInput("compress_prov", "Compression",
                    choices = c("none", "gzip", "bzip2", "xz"),
                    selected = "gzip", multiple = FALSE)),
    div(class='col-md-6', div(style="display:inline-block", tags$label(div(icon("code"), icon("archive"))),
                           downloadButton("downloadProvenance",
                                          tags$label("Download Archive")
                                          )
                           )
    )
  )),
  h4("Code Preview"),
  fluidRow(column(width = 12,
                  div(class='col-md-3', 
                      numericInputRow("number_events_prov", "# Events", 5, 1, step=1, class = "col-md-12")),
                  div(class='col-md-9', div(style="display:inline-block", tags$label(icon("code")),
                                         actionButton("actionb_prov", "Preview Code", icon("repeat")))
                      )
  ))
)
provpage = fluidPage(
  fluidRow(
    h1("Provenance Tracking "),
    h2(" Archive Your Session")
  ),
  fluidRow(
    sbp_prov,
    column(width=8,
           htmlOutput('provenance')
    )
  ),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/provenance.md")
  ))
)
