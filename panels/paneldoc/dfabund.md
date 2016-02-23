# Bar Panel Details

This panel provides an interface to 
[phyloseq](http://joey711.github.io/phyloseq)'s
[plot_bar function](http://joey711.github.io/phyloseq/plot_bar-examples.html).

## Widget Sections

- **(Re)Build Graphic** button - no plot will be built, or rebuilt, until you click
the `(Re)Build Graphic` button at the top of the sidebar.

### Aesthetic Mapping

Like the `Scatter` panel, this panel is intended
as a tool to explore the abundance values directly.
What you see in the plot is a result of your description
of how to map aspects of the data to the plot.

- **X-Axis** - The variable selected here is mapped to the horizontal ("X") axis.
The default selection is `"Sample"`,
which means every sample gets a separate discrete position on the horizontal axis.
The "Y" axis is always mapped to the abundance value indicated by the `Data` widget
at the bottom of this subsection.
- **Color** - The variable selected here is mapped to the fill color of bars.
Take care that selected discrete variables
do not surpass the number of available aesthetic categories
(e.g. more categories in data than you have color shades).
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
- **Data** - Whether to use filtered `Counts` or a transformation.
`Proportion` is a simple, provided transformation.

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
- **Angle** - This refers to the label angle for the horizontal ("X") axis.

### Dimensions and Downloads

Figure dimensions (in inches), file format, and download button (`DL`).


