# Data I/O page
sbp_data = sidebarPanel(
  uiOutput("phyloseqDataset"),
  h5("Upload",
     a("Biom-Format", href="http://joey711.github.io/phyloseq/import-data.html#import_biom"),
     "File"),
  fileInput('filebiom', "", multiple = TRUE),
  h5("Upload", 
     a(".RData", href="http://joey711.github.io/phyloseq/import-data.html"),
     "File"),
  fileInput('file1', ""),
  h5("Load", a("QIIME-DB", href="http://www.microbio.me/qiime/"), "Data"),
  fluidRow(column(width = 12,
                  div(class="span3", 
                      numericInputRow("qiimeDBsizeMax", "", 
                                      value = 50L, min = 0, step = 10L, class="span12")
                  ),
                  actionButton("actionb_data_qiime", "Load QIIME-DB",
                               icon("cloud-download"))
  )),
  uiOutput("qiimeDBopts")
)
datapage = fluidPage(
  headerPanel("Dataset Upload and Selection"),
  sbp_data,
  column(width = 8,
    plotOutput("library_sizes"),
    p("Data Summary:"),
    htmlOutput('contents')
  ),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/data.md")
  ))
)
