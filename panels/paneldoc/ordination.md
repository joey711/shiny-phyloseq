# Ordination Panel Details

This panel provides an interface to 
[phyloseq](http://joey711.github.io/phyloseq)'s
[plot_ordination function](http://joey711.github.io/phyloseq/plot_ordination-examples.html).

## Widget Sections

### Structure

This top section defines key aspects of 
the ordination calculation.
This includes the distance on which it is based,
the method used to decompose that distance
into axes for graphical exploration,
and which features of the ordination result to display.

- **Type** - Toggle between a network with Samples or Taxa as nodes. 
- **Method** - The Ordination Method to use. This is an interface to
phyloseq's [ordinate function](http://joey711.github.io/phyloseq/ordinate).
- **Distance** - The distance method. This is a direct interface to phyloseq's
[distance function](http://joey711.github.io/phyloseq/distance).
- **Display** - The ordination display type.
This is equivalent to the `type` argument in 
the [plot_ordination function](http://joey711.github.io/phyloseq/plot_ordination-examples.html).
- **Constraint** - The variables to include for constrained ordination.
Multiple non-`NULL` variables are combined in a formula.
e.g. `~ Var1 + Var2 + Var3`.
Not that for ordination constraints only the right-hand side is relevant.
Generally speaking, this is less flexible than a full [formula interface]().
For conditioning variables, explicit interaction terms,
and other features of R formulae, please use phyloseq/R directly.

### Aesthetic Mapping

These widgets correspond to aesthetic mappings on nodes.
`Color` and `Shape` are the same as in other panels. 
For instance, `Color` maps point-color to a particular variable.

### Details

`Palette`, `Theme`, `Size`, and `Opacity` - refer to the way points are drawn.

- **Palette** - Refers to color palette.
Only applicable if you have selected a `Color` variable
in the Aesthetic Mapping section.
Explore the `Palette` panel within Shiny-phyloseq,
or more details about color palettes at [ColorBrewer](http://colorbrewer2.org/).
- **Theme** - For the most part these refer to
[ggplot2 themes](http://docs.ggplot2.org/0.9.2.1/theme.html),
which generally encompass stylistic graphical features,
such as axis line types, axis label fonts and spacing, background color, etc.
- **Size** - This refers to point size.
- **Opacity** - The transparency of points in the graphic,
on a `[0, 1]` scale;
with zero meaning so transparent points are invisible,
and one meaning points are perfectly opaque (the default).

### Dimensions and Downloads

Figure dimensions (in inches), file format, and download button (`DL`).


