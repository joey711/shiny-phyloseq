# Scatter Panel Details

This panel provides an interface to 
[phyloseq](http://joey711.github.io/phyloseq)'s
`psmelt` function.
In this case, the result of melting the abundance data and its covariates/annotations
is rendered as a scatter plot.

### Aesthetic Mapping

Like the `Bar` panel, this panel is intended
as a tool to explore the abundance values directly.
What you see in the plot is a result of your description
of how to map aspects of the data to the plot.

- **X-Axis** - The variable selected here is mapped to the horizontal ("X") axis.
The default selection is `"Sample"`,
which means every sample gets a separate discrete position on the horizontal axis.
- **Y-Axis** - The variable selected here is mapped to the vertical ("Y") axis.
By default, the "Y" axis is mapped to the abundance value indicated by the `Data` widget
at the bottom of this subsection.
- **Color** - The variable selected here is mapped to point color.
- **Shape** - The variable selected here is mapped to point shape.
For both Color and Shape, take care that selected discrete variables
do not surpass the number of available aesthetic categories
(e.g. more categories in data than you have color shades or shape types).
- **Facet Row** - This is an interface to
[ggplot2's facet_grid function](http://docs.ggplot2.org/0.9.3.1/facet_grid.html).
Faceting is a means of splitting the data into separate panels
according to one or more variables in the data.
This can have many advantages, especially to alleviate overplotting,
and to clarify key comparisons.
In this case, the `Facet Row` widget provides available variables
that will be mapped to panel-rows in the resulting grid of facets panels.
You can select more than one variable.
You should delete/unselect NULL when selecting facets.
- **Facet Col** - This is the same as `Facet Row` above,
but this widget controls the variables that will arrange the data
into panel columns.
- **Label** - A variable to map to point label.
The text at each point will reflect the value of the chosen variable.
The small numeric widgets to the right adjust
label size (`Lab Sz`) and vertical justification (`V-Just`).
- **Transform** - This option is very important.
It determines whether to use filtered-counts, 
or some transformation of the filtered count values.
See the `Transform` panel for the definition of each available option.


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
- **Size** - The size of each data point.
- **Opacity** - Adjusts the transparency of points when less than 1. `[0, 1]` scale.

### Dimensions and Downloads

Figure dimensions (in inches), file format, and download button (`DL`).

