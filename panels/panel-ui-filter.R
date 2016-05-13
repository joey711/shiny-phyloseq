################################################################################
filterpage = fluidPage(
  headerPanel("Basic Data Filtering"), 
  fluidRow(
    # Sidebar Panel default is 4-column.
    sidebarPanel(
      actionButton("actionb_filter", "Execute Filter", icon("filter")),
      h4("Subset Taxa"),
      fluidRow(column(width=12,
                      div(class="col-md-6", uiOutput("filter_uix_subset_taxa_ranks")),
                      div(class="col-md-6", uiOutput("filter_uix_subset_taxa_select"))
      )),
      h4("Subset Samples"),
      fluidRow(column(width=12,
                      div(class="col-md-6", uiOutput("filter_uix_subset_sample_vars")),
                      div(class="col-md-6", uiOutput("filter_uix_subset_sample_select"))
      )),
      h4('Total Sums Filtering'),
      fluidRow(column(width=12,
                      div(class="col-md-6",
                          numericInputRow("filter_sample_sums_threshold", "Sample Min.",
                                          value=SampleSumDefault, min=0, step=100, class="col-md-12")),
                      div(class="col-md-6",
                          numericInputRow("filter_taxa_sums_threshold", "Taxa Min.",
                                          value=OTUSumDefault, min=0, step=1, class="col-md-12"))
      )),
      h4('kOverA OTU Filtering'),
      fluidRow(column(width=12,
                      div(class="col-md-6",
                          numericInputRow("filter_kOverA_count_threshold", "A",
                                          value=kovera_A, min=0, step=1, class="col-md-12")), 
                      div(class="col-md-6", uiOutput("filter_ui_kOverA_k"))
      ))
    ),
    # Now the Main Panel.
    column(
      width = 8, offset = 0, 
      h4("Histograms Before and After Filtering"),
      plotOutput("filter_summary_plot"),
      h4("Data Summaries"),
      fluidRow(
        column(width = 6,
               p("Original"),
               htmlOutput('filtered_contents0')
        ),
        column(width = 6,
               p("Filtered Data:"),
               htmlOutput('filtered_contents')
      )),
      h4("Component Table, Filtered Data"),
      fluidRow(column(width=12,
          div(class="col-md-8", uiOutput("uix_available_components_filt")),
          div(class="col-md-3", numericInputRow("component_table_colmax_filt", "Max. Columns",
                                             value = 25L, min = 1L, step = 5L, class="col-md-12"))
      )),
      dataTableOutput('physeqComponentTable')
    )
  ),
  fluidRow(
    column(width = 12, includeMarkdown("panels/paneldoc/filter.md"))
  )
)
################################################################################
