
# Shiny-phyloseq

[Shiny-phyloseq](http://joey711.github.io/shiny-phyloseq/)
is an interactive web application that provides 
a graphical user interface to the microbiome analysis package for R,
called [phyloseq](http://joey711.github.io/phyloseq/).
For details about using the phyloseq package directly,
see [The phyloseq Homepage](http://joey711.github.io/phyloseq/).

## Citation

Shiny-phyloseq is provided under a free-of-charge, open-source license (A-GPL3).
All we require is that you cite/attribute the following
in any work that benefits from this code or application.

### The App

McMurdie and Holmes (2014) [Shiny-phyloseq: Web Application for Interactive Microbiome Analysis with Provenance Tracking](http://bioinformatics.oxfordjournals.org/content/early/2014/10/02/bioinformatics.btu616).

Bioinformatics (Oxford, England), 31(2), 282–283.
DOI 10.1093/bioinformatics/btu616



### "Under the Hood"

McMurdie and Holmes (2013)
[phyloseq: An R package for reproducible interactive analysis and graphics of microbiome census data](http://dx.plos.org/10.1371/journal.pone.0061217). 

PLoS ONE 8(4):e61217.

## Launching Shiny-phyloseq Local Session

While it is possible to host the server "back end" somewhere
so that users only need to point their web browser to a link,
it is also possible to launch both the back and front "ends" on your local machine.
The server back end will be an R session on your own machine,
while the front end is your web browser,
pointed to the appropriate local URL.

### [Quick install/launch instructions](http://joey711.github.io/shiny-phyloseq/Install.html)

Simply [launching Shiny-phyloseq](http://joey711.github.io/shiny-phyloseq/Install.html)
should also install missing/old packages.
Make sure that you first have installed
[the latest version of R](http://cran.r-project.org/).

The following R code will launch Shiny-phyloseq on most systems.


```r
install.packages("shiny") 
shiny::runGitHub("shiny-phyloseq","joey711")
```

See the 
[Shiny-phyloseq installation instructions](http://joey711.github.io/shiny-phyloseq/Install.html),
for further details.
