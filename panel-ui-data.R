# Data I/O page
datapage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h4('Select Dataset'),
      submitButton("Load Selection", icon("refresh")),
      uiOutput("phyloseqDataset"),
      tags$hr(),
      h4('Import Biom-Format File'),
      fileInput('filebiom', "", multiple = TRUE),
      h4('Upload Pre-Imported .RData'),
      fileInput('file1', ""),
      tags$hr(),
      h4('microbio.me/qiime public data'),
      p('(requires phyloseq 1.9.5+)'),
      textInput("qiime_server_ID", "Identifier for QIIME server", value="NULL"),
      radioButtons("qiime_server_ext", "File Extension",
                   choices=list(".zip", ".tgz", ".tar.gz")),
      tags$hr(),
      p('Plot parameters:'),
      numericInput("dataset_count_threshold", "Count Threshold", value=3, min=0, step=1),
      p("See default-parameters.R to change default settings."),
      p('Big thanks to',
        a(href = 'http://shiny.rstudio.com/', 'Shiny', 'web apps.')
      )
    ),
    mainPanel(
      plotOutput("library_sizes"),
      plotOutput("OTU_count_thresh_hist"),
      tags$hr(),
      p("Data Summary:"),
      htmlOutput('contents')
    )
  )
)
