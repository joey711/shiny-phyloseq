# Network Panel Details

This panel provides an interface to 
[phyloseq](http://joey711.github.io/phyloseq)'s
[plot_net function](http://joey711.github.io/phyloseq/plot_network-examples.html).
Calculation time depends a lot on the size of your data (number of OTUs/samples)
and the distance method chosen. 

## Widget Sections

### Network Structure

This top section defines key aspects of the network structure. 

- **Type** - Toggle between a network with Samples or Taxa as nodes. 
- **Max D** - Maximum Distance. Include an edge if two nodes have a smaller distance than this value.
- **Layout** - The network layout method. Many are supported.
See [igraph layout doc](http://www.inside-r.org/packages/cran/igraph/docs/layout) for details.
Many of these include a randomness component, and the RNG Seed is a separate
- **Distance** - The distance method. This is a direct interface to phyloseq's
[distance function](http://joey711.github.io/phyloseq/distance)
- **Draw Max.** - Maximum Render Distance. Distances larger than this (but less than `Max D`)
affect the network layout structure, but are not drawn in the graphic.
This widget is a slider in which the right-hand side should always match `Max D`.
The `Play` button result in a network animation that scans across values for `Draw Max.`,
allowing you to quickly inspect the global connectivity as a function of threshold choice.
In some cases you may want to modify `Max D` after reviewing this animation
(or manually changing the slider yourself)
- **Transform** - This option is very important.
It determines whether to use filtered-counts, 
or some transformation of the filtered count values.
See the `Transform` panel for the definition of each available option.

### Aesthetic Mapping

These widgets correspond to aesthetic mappings on nodes (points).

- **Color** - The variable selected here is mapped to point/node color.
- **Shape** - The variable selected here is mapped to point/node shape.
- **Label** - allows labeling of nodes with text,
according to one or more selected variables.
If multiple non-`NULL` variables selected,
their values are concatenated together
in order as a single label for each node.
The size and horisontal justification of labels
can be adjusted in the `Details` section below.

### Details

- `Palette`, `Size`, and `Opacity` - refer to the way nodes (points) are drawn.
- `RNG Seed` is the random number generator seed
used in some network layout methods (see above).
- `Label Sz` and `H-just` - are size and horizontal-justification values
that modify the node text labels, if any.

### Dimensions and Downloads

Figure dimensions (inches), file format, and download button (`DL`).


