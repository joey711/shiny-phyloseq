################################################################################
filterpage = fluidPage(
  headerPanel("Basic Data Filtering"), 
  fluidRow(
    # Sidebar Panel default is 4-column.
    sidebarPanel(
      actionButton("actionb_filter", "Execute Filter", icon("filter")),
      p("  "),
      p('Filtering Parameters:'),
      tags$hr(),
      h4("Subset Taxa"),
      uiOutput("filter_uix_subset_taxa_ranks"),
      uiOutput("filter_uix_subset_taxa_select"),
      h4("Subset Samples"),
      uiOutput("filter_uix_subset_sample_vars"),
      uiOutput("filter_uix_subset_sample_select"),
      tags$hr(),
      p('Total Sums Filtering:'),
      numericInput("filter_sample_sums_threshold", "Sample Sums Count Threshold",
                   value=SampleSumDefault, min=0, step=100),
      numericInput("filter_taxa_sums_threshold", "OTU Sums Count Threshold",
                   value=OTUSumDefault, min=0, step=1),
      tags$hr(),
      p('kOverA OTU Filtering:'),
      numericInput("filter_kOverA_count_threshold", "`A` - The Count Value Threshold", value=kovera_A, min=0, step=1),
      uiOutput("filter_ui_kOverA_k")
    ),
    column(width = 8, offset = 0, 
      plotOutput("filter_summary_plot"),
      tags$hr(),
      p("Original Data:"),
      htmlOutput('filtered_contents0'),
      tags$hr(),
      p("Filtered Data:"),
      htmlOutput('filtered_contents'),
      tags$hr(),
      p("Sample Variables:"),
      textOutput('sample_variables'),
      tags$hr(),
      p("Taxonomic Ranks:"),
      textOutput('rank_names')      
    )
  ),
  fluidRow(
    column(width = 12, includeMarkdown("panels/paneldoc/filter.md"))
  )
)
################################################################################
