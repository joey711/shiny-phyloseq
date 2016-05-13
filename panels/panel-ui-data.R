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
  h5("Upload", a("Tree", href="http://rgm.ogalab.net/RGM/R_rdfile?f=phyloseq/man/read_tree.Rd&d=R_BC"), "File"),
  fileInput('treefile', "", multiple = FALSE)
)

datapage = fluidPage(
  headerPanel("Dataset Upload and Selection"),
  sbp_data,
  column(width = 8,
    h4("Histograms for Selected Data"),
    plotOutput("library_sizes"),
    h4("Data Summary"),
    htmlOutput('contents'),
    h4("Display Component Table"),
    fluidRow(column(width = 12,
                    div(class="col-md-8", uiOutput("uix_available_components_orig")),
                    div(class="col-md-3", numericInputRow("component_table_colmax", "Max. Columns",
                                    value = 25L, min = 1L, step = 5L, class="col-md-12"))
    )),
    dataTableOutput('ps0ComponentTable')
  ),
  fluidRow(column(width = 12,
                  includeMarkdown("panels/paneldoc/data.md")
  ))
)
