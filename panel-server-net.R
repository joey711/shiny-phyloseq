################################################################################
# plot_network() ui
################################################################################
# Choose layout method
available_layouts_net = list(
  auto = igraph::layout.auto,
  random = igraph::layout.random,
  circle = igraph::layout.circle,
  sphere = igraph::layout.sphere,
  fruchterman.reingold = igraph::layout.fruchterman.reingold,
  kamada.kawai = igraph::layout.kamada.kawai,
  spring = igraph::layout.spring,
  reingold.tilford = igraph::layout.reingold.tilford,
  fruchterman.reingold.grid = igraph::layout.fruchterman.reingold.grid,
  lgl = igraph::layout.lgl,
  graphopt = igraph::layout.graphopt,
  svd = igraph::layout.svd
)
output$network_uix_layout <- renderUI({
  selectInput("layout_net", "Network Layout Method",
              choices = names(available_layouts_net),
              selected = "fruchterman.reingold")
})
output$network_uix_color <- renderUI({
  uivar("color_net", "Color Variable:", vars(input$type_net))
})
output$network_uix_shape <- renderUI({
  uivar("shape_net", "Shape Variable:", vars(input$type_net))
})
output$network_uix_label <- renderUI({
  selectInput("label_net", "Node Label:",
              choices = vars(input$type_net, TRUE, TRUE),
              selected = default_netLabel,
              multiple = TRUE)
})
################################################################################
# Static Network Plot using ggplot2 
################################################################################
# 1. 
# Calculate Distance
scaled_distance = function(physeq, method, type, rescaled=TRUE){
  Dist = phyloseq::distance(physeq, method, type)
  if(rescaled){
    # rescale the distance matrix to be [0, 1]
    Dist <- Dist / max(Dist, na.rm=TRUE)
    Dist <- Dist - min(Dist, na.rm=TRUE)
  }
  return(Dist)
}
# Only returns distance matrix, regardless of distance-method argument
Distance_net <- reactive({
  idist = NULL
  try({idist <- scaled_distance(physeq(), 
                                method=input$dist_net,
                                type=input$type_net,
                                rescaled = TRUE)},
      silent=TRUE)
  if(is.null(idist)){warning("Distance_net: Could not calculate distance matrix with these settings.")}
  return(idist)
})
# 2.
# Create edge table
dist_to_edge_table = function(Dist, MaxDistance=NULL, vnames = c("v1", "v2")){
  dmat <- as.matrix(Dist)
  # Set duplicate entries and self-links to Inf
  dmat[upper.tri(dmat, diag = TRUE)] <- Inf
  LinksData = data.table(reshape2::melt(dmat, varnames=vnames, as.is = TRUE))
  setnames(LinksData, old = "value", new = "Distance")
  # Remove self-links and duplicate links
  LinksData <- LinksData[is.finite(Distance), ]
  # Remove entries above the threshold, MaxDistance
  if(!is.null(MaxDistance)){
    LinksData <- LinksData[Distance < MaxDistance, ]
  }
  return(LinksData)
}
LinksData0 = reactive({
  dist_to_edge_table(Distance_net(), MaxDistance=input$uinetdistmax)
})
# 3. Create vertex layout
# Make the vertices-coordinates data.table
vertex_layout = function(LinksData, physeq=NULL, type="samples",
                         laymeth=igraph::layout.fruchterman.reingold, ...){
  # `physeq` can be anything, only has effect when non-NULL returned by sample_data or tax_table
  g = igraph::graph.data.frame(LinksData, directed=FALSE)
  vertexDT = data.table(laymeth(g, ...),
                        vertex=igraph::get.vertex.attribute(g, "name"))
  setkey(vertexDT, vertex)
  setnames(vertexDT, old = c(1, 2), new = c("x", "y"))
  extraData = NULL
  if( type == "samples" & !is.null(sample_data(physeq, FALSE)) ){
    extraData <- data.table(data.frame(sample_data(physeq)), key = "rn", keep.rownames = TRUE)
  } else if( type == "taxa" & !is.null(tax_table(physeq, FALSE)) ){
    extraData <- data.table(as(tax_table(physeq), "matrix"), key = "rn", keep.rownames = TRUE)
  }
  # Only mod vertexDT if extraData exists
  if(!is.null(extraData)){
    # Join vertexDT, extraData using data.table syntax. Presumes `vertex` is key in both.
    setnames(extraData, old = "rn", new = "vertex")
    vertexDT <- vertexDT[extraData]
    vertexDT <- vertexDT[!is.na(x), ]
  }
  return(vertexDT)
}
vertexDT = reactive({
  if(is.null(av(input$layout_net))){
    # If layout method unavailable, use default
    return(vertex_layout(LinksData0(), physeq(), type = input$type_net))
  }
  return(
    vertex_layout(LinksData0(), physeq(),
                  type = input$type_net,
                  laymeth = available_layouts_net[[input$layout_net]])
  )
})
# 4.
# Update the links layout for ggplot, x, y, xend, yend
link_layout = function(LinksData, vertexDT){
  linkstart = vertexDT[LinksData$v1, x, y]
  linkend = vertexDT[LinksData$v2, x, y]
  setnames(linkend, old = c("y", "x"), new = c("yend", "xend"))
  LinksData <- cbind(LinksData, linkstart, linkend)
  return(LinksData)  
}
LinksData = reactive({
  link_layout(LinksData0(), vertexDT())
})
# 5.
# Define ggplot2 network plot
links_to_ggplot = function(LinksData, vertexDT, vertmap=aes(x, y), vert_size=NULL, vert_alpha=NULL){
  p0 = ggplot(data=LinksData) + 
    geom_segment(aes(x, y, xend=xend, yend=yend, size=Distance, alpha=Distance)) +
    geom_point(mapping = vertmap,
               data = vertexDT, 
               size = vert_size,
               alpha = vert_alpha) +
    scale_alpha(range = c(1, 0.1)) + 
    scale_size(range = c(2, 0.25))
  net_theme = theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    axis.ticks       = element_blank(),
    panel.border     = element_blank()
  )
  p0 <- p0 + net_theme
  return(p0)
}
p_net = reactive({
  vertmap = aes_string(x="x", y="y", colour=av(input$color_net), shape=av(input$shape_net))
  links_to_ggplot(LinksData(), vertexDT(), vertmap,
                  vert_size = av(input$size_net),
                  vert_alpha = av(input$alpha_net))
})
p_net_label = reactive({
  NodeData = vertexDT()
  if(is.null(av(input$label_net)) | !all(input$label_net %in% colnames(NodeData))){
    return(p_net())
  }
  NodeData$ShowLabels <- apply(NodeData[, input$label_net, with=FALSE], 1, paste0, collapse="; ")
  NodeData
  return(p_net() + geom_text(aes(x, y, label=ShowLabels),
                             data = NodeData,
                             size = 2, hjust = 1.35, na.rm = TRUE))
})
# 6.
# Reactive update to ggplot
p_net_update = reactive({
  p <- p_net_label()
  p$data <- LinksData()[Distance < input$uinetdispdist, ]
  return(p)
})
# label=isolate(av(input$label_net)),
finalize_network_plot = reactive({
  return(p_net_update() + scale_colour_brewer(palette = input$pal_net))
})
# Render plot in panel and in downloadable file with format specified by user selection
output$network <- renderPlot({
  shiny_phyloseq_print(finalize_network_plot())
}, width=function(){72*input$width_net}, height=function(){72*input$height_net})
output$downloadNetwork <- downloadHandler(
  filename = function(){paste0("Network_", simpletime(), ".", input$downtype_net)},
  content = function(file){
    ggsave2(filename=file,
            plot=finalize_network_plot(),
            device=input$downtype_net,
            width=input$width_net, height=input$height_net, dpi=300L, units="in")
  }
)
################################################################################