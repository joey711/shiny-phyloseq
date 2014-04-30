# fileInput('file1', 'Choose phyloseq RData file to upload')

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
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
))
