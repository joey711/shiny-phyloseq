# d3 code section
################################################################################
# networkD3 uix
################################################################################
output$d3_uix_color <- renderUI({
  selectInput("color_d3", "Color",
              choices = vars(input$type_d3, TRUE, TRUE),
              selected = d3NetworkColorVar)
})
output$d3_uix_node_label <- renderUI({
  selectInput("d3_node_label", "Label",
              choices = vars(input$type_d3, TRUE, TRUE),
              selected = d3NodeLabelVar,
              multiple = TRUE)
})
################################################################################
# d3 interactive network graphic 
################################################################################
# Get data.
physeq_d3 = reactive({
  return(
    switch({input$transform_d3},
           Counts = physeq(),
           Prop = physeqProp(),
           RLog = physeqRLog(),
           CLR = physeqCLR(),
           physeq()
    )
  )
})
# Define global reactive distance matrix. 
# Re-calc only if method or plot-type change.
d3distReact <- reactive({
  idist = NULL
  try({idist <- scaled_distance(physeq_d3(), method=input$dist_d3, type=input$type_d3, rescaled = TRUE)}, silent=TRUE)
  if(is.null(idist)){warning("d3dist: Could not calculate distance matrix with these settings.")}
  return(idist)
})  
calculate_links_data = reactive({
  LinksData = dist_to_edge_table(d3distReact(),
                                 input$dist_d3_threshold,
                                 c("Source", "target"))
  # Don't sort yet, instead create mapping variable from Source ID to link node ID
  # d3link nodes are numbered from 0.
  nodeUnion = union(LinksData$Source, LinksData$target)
  d3lookup = (0:(length(nodeUnion)-1))
  names(d3lookup) <- nodeUnion
  # In-place replacement.
  LinksData[, Source:=d3lookup[Source]]
  LinksData[, target:=d3lookup[target]]
  # Order by the `d3lookup` node ID, in this case, the Source label
  setkey(LinksData, Source)
  # Create covariates table (taxa in this case)
  if(input$type_d3 == "taxa"){
    NodeData = data.frame(OTU=nodeUnion, tax_table(physeq_d3())[nodeUnion, ], stringsAsFactors = FALSE)
  } else {
    NodeData = data.frame(Sample=nodeUnion, sample_data(physeq_d3())[nodeUnion, ], stringsAsFactors = FALSE)      
  }
  NodeData$ShowLabels <- apply(NodeData[, input$d3_node_label, drop=FALSE], 1, paste0, collapse="; ")
  return(list(link=data.frame(LinksData), node=NodeData))
})  
default_Source = function(x){
  if(is.null(av(x))){
    if(input$type_d3=="taxa"){
      return("OTU")
    } else {
      return("Sample")
    }
  } else {
    return(x)
  }
}
# # Wrapper function for Shiny-phyloseq D3 Network definition.
# sps_D3_network = function(standAlone=FALSE, parentElement="#D3Network", file=NULL){
#   networkD3::forceNetwork(
#     Links = calculate_links_data()$link, 
#     Nodes = calculate_links_data()$node,
#     Source = "Source",
#     Target = "target",
#     Value = "Distance",
#     NodeID = "ShowLabels",
#     height = input$height_d3,
#     width = input$width_d3,
#     Group = default_Source(input$color_d3),
#     linkColour = input$d3_link_color,
#     opacity = input$d3_opacity,
#     parentElement = parentElement,
#     standAlone = standAlone,
#     d3Script = "d3.v3.min.js",
#     file=file
#   )
# }
# # Send to in-panel element.
# output$D3Network <- renderPrint({
#   sps_D3_network()
# })
output$D3Network <- renderForceNetwork({
  forceNetwork(
    Links = calculate_links_data()$link, 
    Nodes = calculate_links_data()$node,
    Source = "Source",
    Target = "target",
    Value = "Distance",
    NodeID = "ShowLabels",
    #height = input$height_d3,
    #width = input$width_d3,
    Group = default_Source(input$color_d3),
    linkColour = input$d3_link_color,
    opacity = input$d3_opacity)
})

# # Downloadable standalone HTML file.
# content_d3 = function(file){
#   return(
#     sps_D3_network(standAlone=TRUE, parentElement="body", file=file)
#   )
# }
# output$download_D3 <- downloadHandler(filename = function(){paste0("d3_", simpletime(), ".html")},
#                                      content = content_d3)
#zoom = as.logical(input$d3_zoom),