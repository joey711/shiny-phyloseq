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
      h4(a("Upload .RData File",
           href="http://joey711.github.io/phyloseq/import-data.html")),
      fileInput('file1', ""),
      tags$hr(),
      h4(a("QIIME-DB Data",
                 href="http://www.microbio.me/qiime/")),
      actionButton("actionb_data_qiime", "Load QIIME Data", icon("cloud-download")),
      uiOutput("qiimeDBopts"),
      tags$hr(),
      p('Summary Graphic:'),
      actionButton("actionb_data", "Make Histogram", icon("bar-chart-o")),
      numericInput("dataset_count_threshold", "Count Threshold", value=3, min=0, step=1),
      p("See core/default-parameters.R to change default settings."),
      tags$hr(),
      p('Big thanks to',
        a(href = 'http://shiny.rstudio.com/', 'Shiny', 'web apps.')
      ),
      p('For documentation, check out:',
        a(href = "http://joey711.github.io/shiny-phyloseq/", "'About Shiny-phyloseq'")
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
