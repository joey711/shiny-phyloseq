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
  fluidRow(column(width = 12,
    div(class='span6', actionButton("actionb_prov", "Render Code", icon("code")))
  )),
  fluidRow(column(width = 12,
    div(class='span3', 
        numericInputRow("number_events_prov", "# Events", 5, 1, step=1, class = "span12")),              
    div(class="span5", 
        selectInput("compress_prov", "Compression",
                    choices = c("none", "gzip", "bzip2", "xz"),
                    selected = "gzip", multiple = FALSE)),
    div(class='span4', div(style="display:inline-block", tags$label(""),
                           downloadButton("downloadProvenance", tags$label(div("D.L.", icon("code"), icon("archive"))))
                           )
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
