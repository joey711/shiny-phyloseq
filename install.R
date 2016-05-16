################################################################################
# Check that the currently-installed version of R
# is at least the minimum required version.
################################################################################
R_min_version = "3.3.0"
R_version = paste0(R.Version()$major, ".", R.Version()$minor)
if(compareVersion(R_version, R_min_version) < 0){
  stop("You do not have the latest required version of R installed.\n", 
       "Launch should fail.\n",
       "Go to http://cran.r-project.org/ and update your version of R.")
}
################################################################################
# Install basic required packages if not available/installed.
################################################################################
install_missing_packages = function(pkg, version = NULL, verbose = TRUE){
  availpacks = .packages(all.available = TRUE)
  source("http://bioconductor.org/biocLite.R")
  missingPackage = FALSE
  if(!any(pkg %in% availpacks)){
    if(verbose){
      message("The following package is missing.\n",
              pkg, "\n",
              "Installation will be attempted...")
    }
    missingPackage <- TRUE
  }
  if(!is.null(version) & !missingPackage){
    # version provided and package not missing, so compare.
    if( compareVersion(a = as.character(packageVersion(pkg)),
                       b = version) < 0 ){
      if(verbose){
        message("Current version of package\n", 
                pkg, "\t", 
                packageVersion(pkg), "\n",
                "is less than required.
                Update will be attempted.")
      }
      missingPackage <- TRUE
    }
  }
  if(missingPackage){
    biocLite(i, suppressUpdates = TRUE)
  }
}
################################################################################
# Define list of package names and required versions.
################################################################################
deppkgs = c(phyloseq = "1.16.0",
            biomformat = "1.0.0",
            shiny = "0.13.2",
            shinythemes = "1.0.1", 
            ggplot2 = "2.1.0", 
            data.table = "1.9.6",
            networkD3 = "0.2.10",
            genefilter = "1.54.0", 
            grid = "3.3.0",
            gridExtra = "2.2.1", 
            markdown = "0.7.7", 
            rmarkdown = "0.9.6",
            png = "0.1.7", 
            RColorBrewer = "1.1.2",
            scales = "0.4.0")
# Loop on package check, install, update
pkg1 = mapply(install_missing_packages,
              pkg = names(deppkgs), 
              version = deppkgs,
              MoreArgs = list(verbose = TRUE), 
              SIMPLIFY = FALSE,
              USE.NAMES = TRUE)
################################################################################
# Load packages that must be fully-loaded 
################################################################################
for(i in names(deppkgs)){
  library(i, character.only = TRUE)
  message(i, " package version:\n", packageVersion(i))
}
################################################################################
