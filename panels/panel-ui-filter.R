################################################################################
filterpage = fluidPage(
  titlePanel("Basic Data Filtering"),
  sidebarLayout(
    sidebarPanel(
      actionButton("actionb_filter", "Execute Filter", icon("filter")),
      p("  "),
      p('Filtering Parameters:'),
      tags$hr(),
      p("Subset Expressions:"),
      textInput("filter_subset_taxa_expr", "`subset_taxa()` Expression: (e.g. Phylum=='Firmicutes')", value="NULL"),
      textInput("filter_subset_samp_expr", "`subset_samples()` Expression: (e.g. SampleType %in% 'Feces')", value="NULL"),
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
    mainPanel(
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
  )
)
################################################################################

# richpage = fluidPage(
#   headerPanel("Alpha Diversity Estimates", "windowTitle"), 
#   fluidRow(
#     sbp_rich,
#     column(width = 8, plotOutput("richness"), offset = 0)
#   ),
#   fluidRow(
#     column(width = 12,
#            "Placeholder -  information about this tab here. See ",
#            a(href="http://joey711.github.io/shiny-phyloseq/", "the plot_richness tutorial")
#     )
#   )
# )
