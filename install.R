################################################################################
# Check that the currently-installed version of R
# is at least the minimum required version.
################################################################################
R_min_version = "3.1.0"
R_version = paste0(R.Version()$major, ".", R.Version()$minor)
if(compareVersion(R_version, R_min_version) < 0){
  stop("You do not have the latest required version of R installed.\n", 
       "Launch should fail.\n",
       "Go to http://cran.r-project.org/ and update your version of R.")
}
################################################################################
# Install basic required packages if not available/installed.
################################################################################
download_not_installed = function(x){
  availpacks = .packages(all.available = TRUE)
  if(any(!x %in% availpacks)){
    source("http://bioconductor.org/biocLite.R")
    for(i in x[!x %in% availpacks]){
      cat("Installing", i, "package using biocLite... \n")
      biocLite(i)
    }
  }
}
vanilla_install_pkgs = c("data.table", "d3Network", "genefilter", "ggplot2",
                         "grid", "gridExtra", "png", "RColorBrewer", "scales")
download_not_installed(vanilla_install_pkgs)
################################################################################
# Should use latest GitHub version of shiny
################################################################################
shiny_okay = FALSE
if("shiny" %in% .packages(all.available = TRUE)){
  shiny_min_version = "0.10.1"
  shiny_compare = compareVersion(as.character(packageVersion("shiny")), shiny_min_version)
  if( shiny_compare >= 0 ){
    shiny_okay <- TRUE
  }
}
if(!shiny_okay){
  install.packages("devtools")
  devtools::install_github("shiny", "rstudio")
}
################################################################################
# phyloseq existence/version test, and installation
################################################################################
phyloseq_okay = FALSE
if("phyloseq" %in% .packages(all.available = TRUE)){
  phyloseq_min_version = "1.9.11"
  phyloseq_compare = compareVersion(as.character(packageVersion("phyloseq")), phyloseq_min_version)
  if( phyloseq_compare >= 0 ){
    phyloseq_okay <- TRUE
  }
}
if(!phyloseq_okay) {
  # Go through recommended phyloseq installation steps
  # (1) Load biocLite
  source("http://bioconductor.org/biocLite.R")
  # (2) Install latest devel version from BioC
  useDevel(devel = TRUE)
  biocLite("phyloseq", suppressUpdates = TRUE)
  # (3) Restore biocLite to release status
  useDevel(devel = FALSE)
}
################################################################################
# Load packages that must be fully-loaded 
################################################################################
shiny_phyloseq_full_load_packages = c("shiny", "phyloseq", vanilla_install_pkgs)
for(i in shiny_phyloseq_full_load_packages){
  library(i, character.only = TRUE); packageVersion(i)
}
################################################################################
