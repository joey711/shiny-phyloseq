################################################################################
# Define each fluid page
################################################################################
# Define in a single function, a standard definition
make_fluidpage = function(fptitle="", sbp, outplotid){
  fluidPage(
    titlePanel(fptitle),
    sidebarLayout(
      sidebarPanel=sbp,
      mainPanel=mainPanel(plotOutput(outplotid))
    )
  )
}
alphapage = make_fluidpage("", uiOutput("sbp_alpha"), "richness")
netpage = make_fluidpage("", uiOutput("sbp_net"), "network")
barpage = make_fluidpage("", uiOutput("sbp_bar"), "bar")
ordpage = make_fluidpage("", uiOutput("sbp_ord"), "ordination")
treepage = make_fluidpage("", uiOutput("sbp_tree"), "tree")
heatpage = make_fluidpage("", uiOutput("sbp_heat"), "heatmap")
scatpage = make_fluidpage("", uiOutput("sbp_scat"), "scatter")
# Data I/O page
datapage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      submitButton("Load Selection", icon("refresh")),
      fileInput('file1', 'Choose file to upload'),
      tags$hr(),
      uiOutput("phyloseqDataset"),
      p('Testing data file input,',
        'will have to try some things. Legacy example links follow...',
        a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
        a(href = 'pressure.tsv', 'pressure.tsv'),
        'files, and then try uploading them.'
      )
    ),
    mainPanel(
      textOutput('contents')
    )
  )
)
################################################################################
# Define the full user-interface, `ui`
################################################################################
ui = navbarPage("Shiny + phyloseq",
                tabPanel("Select Dataset", datapage),
                tabPanel("Alpha Diversity", alphapage),
                tabPanel("Network", netpage),
                tabPanel("Abundance Bar Plot", barpage),
                tabPanel("Ordination", ordpage),
                tabPanel("Tree", treepage),
                tabPanel("Heatmap", heatpage),
                tabPanel("Scatter", scatpage)
)
shinyUI(ui)
