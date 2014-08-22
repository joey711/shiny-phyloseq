# Filter Panel Details

This panel is a limited but hopefully useful interface
to phyloseq's [Preprocessing infrastructure](http://joey711.github.io/phyloseq/preprocess.html).
It is intended for some of the simpler filtering tasks supported by phyloseq.
For more advanced filtering consider manipulating your data
via R/phyloseq directly, prior to uploading it to the Shiny-phyloseq app.
See the data tab for further info about importing/saving/uploading data. 

**No filter calculations are performed until the** `Execute Filter` **button is clicked**

Most downstream analyses will use the data 
that results from the most-recent click of this panel's button.

### Subset section

This section of the sidebar panel 
is for subsetting the microbiome data
through dynamic pairs of variable selection widgets.
The first/left widget allows you to select a variable in the dataset
(a sample variable or taxonomic rank),
and the second widget allows you to select 
one or more classes of the just-selected variable.
Upon execution, only the samples (or OTUs) 
with the selected classes (or taxa) 
will remain in the dataset.

- **subset taxa**
- **subset samples**

Filtering and subsetting functions are evaluated
in the order in which they appear in the sidebar panel.

### Order of filtering operations

- (1) First the data is subsetted 
by specific taxonomic ranks or sample covariates,
if any such subsetting arguments are provided.
- (2) Second, minimum thresholds for 
library size or cross-dataset OTU-counts
are evaluated and the data filtered accordingly.
- (3) Third, an interface for [genefilter's kOverA filtering](http://www.bioconductor.org/packages/release/bioc/manuals/genefilter/man/genefilter.pdf)
is evaluated on the result of any other filtering up to this. It has the form

```r
flist = filterfun(kOverA(k, A))
ps0 <- filter_taxa(ps0, flist, prune=TRUE)
```
where `filterfun` and `kOverA` are from [the genefilter package](http://www.bioconductor.org/packages/release/bioc/html/genefilter.html).

After clicking the button to execute the filtering parameters,
a set of histograms of OTU and library count totals are displayed
for comparing raw (original) and filtered results.

