# Data I/O page
datapage = fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h3('Select Dataset'),
      uiOutput("phyloseqDataset"),
      tags$hr(),
      h4(a("Upload Biom-Format File",
           href="http://joey711.github.io/phyloseq/import-data.html#import_biom")),
      fileInput('filebiom', "", multiple = TRUE),
      tags$hr(),
      h4(a("Upload Custom .RData File",
           href="http://joey711.github.io/phyloseq/import-data.html")),
      fileInput('file1', ""),
      tags$hr(),
      actionButton("actionb_data_qiime", "Load QIIME Data", icon("cloud-download")),
      h4(a("QIIME Public Microbiome Data",
                 href="http://www.microbio.me/qiime/")),
      p('(requires phyloseq 1.9.5+)'),
      textInput("qiime_server_ID", "Identifier for QIIME server", value="NULL"),
      radioButtons("qiime_server_ext", "File Extension",
                   choices=list(".zip", ".tgz", ".tar.gz")),
      tags$hr(),
      p('Summary Graphic:'),
      actionButton("actionb_data", "Make Histogram", icon("bar-chart-o")),
      numericInput("dataset_count_threshold", "Count Threshold", value=3, min=0, step=1),
      p("See core/default-parameters.R to change default settings."),
      tags$hr(),
      p('Big thanks to',
        a(href = 'http://shiny.rstudio.com/', 'Shiny', 'web apps.')
      )
    ),
    mainPanel(
      p("Data Summary:"),
      htmlOutput('contents'),
      tags$hr(),
      plotOutput("library_sizes"),
      plotOutput("OTU_count_thresh_hist")
    )
  )
)
