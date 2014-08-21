# d3 Network Panel Details

This panel provides a special d3 interactive interface to 
[phyloseq](http://joey711.github.io/phyloseq)'s
[plot_net function](http://joey711.github.io/phyloseq/plot_network-examples.html).
Calculation time depends a lot on the size of your data (number of OTUs/samples)
and the distance method chosen. 

## Widget Sections

### Network Structure

This top section defines key aspects of the network structure. 

- **Type** - Toggle between a network with Samples or Taxa as nodes. 
- **Max D** - Maximum Distance. Include an edge if two nodes have a smaller distance than this value.
- **Distance** - The distance method. This is a direct interface to phyloseq's
[distance function](http://joey711.github.io/phyloseq/distance)

### Aesthetic Mapping

These widgets correspond to aesthetic mappings on nodes.
`Color` and `Shape` are the same as in other panels. 
`Label` allows labeling of nodes with text,
according to one or more selected variables.
If multiple non-`NULL` variables selected,
their values are concatenated together
in order as a single label for each node.
The size and horisontal justification of labels
can be adjusted in the `Details` section below.

### Details

- `Opacity` - refer to the way nodes (points) are drawn.
- `Link Size Scaling Factor` - stuff...

### Dimensions and Downloads

Figure dimensions and download button (`DL`).
There is no file-format button because the format
is always a stand-alone HTML that should be sharable as an interactive graphic.

### Acknowledgements

Big thanks to Christopher Gandrud for
[d3Network](http://christophergandrud.github.io/d3Network/)
and also to the team at 
[Shiny and RStudio](http://shiny.rstudio.com).


