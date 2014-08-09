# Install Shiny-phyloseq

If you are reading this, it is because you are interested
in launching/running an instance of Shiny-phyloseq on your own computer.
This requires that you have both a *front end*
([browser](http://en.wikipedia.org/wiki/Web_browser))
and a *back end* ([R](http://cran.r-project.org/)).

---

## Front End

The Shiny-phyloseq *front end*, the GUI web application,
will run on any modern web browser.

#### Supported:

- [Firefox](https://www.mozilla.org/en-US/firefox/new/)
- [Chrome](https://www.google.com/intl/en-US/chrome/browser/)
- [Safari](https://www.apple.com/safari/) 
- other modern [browsers](http://en.wikipedia.org/wiki/Web_browser)
likely fine for connecting to a Shiny-phyloseq session.

#### Not Supported:

- Older versions of Microsoft's Internet Explorer 

---

## Back End

For running the Shiny-phyloseq *back end*, 
you must have 
[the latest version of R installed on your system](http://cran.r-project.org/),
as well as several additional R packages.

#### Auto-Install Shiny-phyloseq (Recommended)

Once you have installed or updated to 
[the latest version of R](http://cran.r-project.org/),
simply launching Shiny-phyloseq
will also install requisite R packages that are missing or old.
Note that this also **requires** an **internet connection** and
**installation permission** on your system.

The following R code will launch Shiny-phyloseq on most systems.


```r
install.packages("shiny")
shiny::runGitHub("shiny-phyloseq","joey711")
```

If this worked without errors, then you are done! No further installation necessary.


#### Manual Install Dependencies (Special Case Only)

If the previous code worked, you can skip this section.

The following R code will install required packages on your local system.
Note that this assumes that you have an active internet connection and updated R installation.


```r
install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/joey711/shiny-phyloseq/master/install.R")
```

This code executes the same package install/update code that is used by the auto-installer in Shiny-phyloseq itself (and will be run if the previous "Auto-Install" instructions work for you). However, in special cases you may want to run this as a one-time batch event, for instance of a test of system compatibility. This is only recommended/necessary if you have never launched Shiny-phyloseq before *and* you want/need to perform/test installation ahead of launch. In addition, the web link provides the exact recommended dependency and installation code, in case your system has special requirements or you need to debug installation manually -- for instance if you have special development versions of packages, or a non-standard installation of R. For most systems, this "manual install" is not necessary.

