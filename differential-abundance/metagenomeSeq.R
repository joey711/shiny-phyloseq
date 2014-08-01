# Function to convert form phyloseq object to metagenomeSeq object
phyloseq_to_metagenomeSeq = function(physeq){
  require("metagenomeSeq")
  require("phyloseq")
  # Enforce orientation
  if(!taxa_are_rows(physeq)){physeq <- t(physeq)}
  OTU = as(otu_table(physeq), "matrix")
  # Convert sample_data to AnnotatedDataFrame
  ADF = AnnotatedDataFrame(data.frame(sample_data(physeq)))
  # define dummy "feature" data for OTUs, using their name
  # Helps with extraction and relating to taxonomy later on.
  TDF = AnnotatedDataFrame(data.frame(OTUname = taxa_names(physeq), row.names=taxa_names(physeq)))
  # Create the metagenomeSeq object
  MGS = newMRexperiment(counts=OTU, phenoData=ADF, featureData=TDF)
  # Trigger metagenomeSeq to calculate its Cumulative Sum scaling factor.
  MGS = cumNorm(MGS)
  return(MGS)
}
# Function to perform relevant test (`fitZig`)
# - `MGS` - A metagenomeSeq object, most-likely produced using the conversion tool make_metagenomeSeq()
# - `variable` - the variable among the sample_data (aka "phenoData" in metagenomeSeq) that you want to test
# - `physeq` - optional phyloseq data object that has the relevant `tax_table`.
# `phyloseq` or `taxonomyTable` is fine.
test_metagenomeSeq = function(MGS, variable, physeq=NULL){
  require("metagenomeSeq")
  require("phyloseq")
  # Create the `mod` variable used in the fitZig test.
  if( inherits(variable, "factor") ){
    # If variable is already a factor, use directly in model.matrix
    mod = model.matrix( ~ variable )
  } else if( inherits(variable, "matrix") ){
    # If it is a matrix, assume that model.matrix() has been used already
  } else if( inherits(variable, "character") ){ 
    # If it is a character that specifies a variable in phenoData, 
    # use the corresponding variable from MGS
    if( variable %in% colnames(phenoData(MGS)@data) ){
      mod = model.matrix(~phenoData(MGS)@data[, variable])
    } else {
      stop("The phenoData variable name you specified is not present in `phenoData(MGS)`")
    }
  } else if( inherits(variable, "formula") ){
    mod = model.matrix(variable, data = phenoData(MGS)@data)
  } else {
    stop("Improper specification of the experimental design variable for testing. See `variable` argument")
  }
  # Wrapper to run the Expectation-maximization algorithm 
  # and estimate $f_count$ fits with the zero-inflated Guassian (z.i.g.)
  fit = fitZig(MGS, mod)
  # You need to specify all OTUs to get the full table from MRfulltable. 
  x = MRfulltable(fit, number=nrow(assayData(MGS)$counts))
  # if any OTUs left out, rm those from x. Detected by NA rownames.
  x = x[!is.na(rownames(x)), ]
  # Modify this data.frame by adding the OTUnames. Clip the ":1" added to the OTU names
  rownames(x) <- gsub(":1", "", x=rownames(x), fixed=TRUE)
  x$OTUnames <- as.character(rownames(x))
  if( !is.null(tax_table(physeq, errorIfNULL=FALSE)) ){
    # Attach the bacterial taxonomy to the table, if available
    TAX = data.frame(tax_table(physeq))
    TAX$OTUnames <- as.character(rownames(TAX))    
    y = merge(x, TAX, by="OTUnames") 
  } else {
    y = x
  }
  # Sort and return
  y = y[order(y$adjPvalue), ]
  return(y)
}