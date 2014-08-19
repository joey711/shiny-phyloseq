################################################################################
# Animation Parameters
################################################################################
# The frame delay interval in milliseconds
interval=1000
# Whether or not to loop the value range, or stop after one pass
loop=TRUE
################################################################################
# Filter Default Parameters
################################################################################
# sample_sums_threshold
# input$filter_sample_sums_threshold
SampleSumDefault = 1000
# taxa_sums threshold 
# input$filter_taxa_sums_threshold
OTUSumDefault = 10
# KoverA Default Filtering Settings
kovera_A = 0
kovera_k = 0
################################################################################
# Network Default Parameters
################################################################################
# The default maximum distance with which the network structure is based. 
netdist = 0.6
# The number of steps to in include in an animation.
animation_steps = 20
default_netLabel = "NULL"
netThreshColorVariableDefault = "DIAGNOSIS"
netThreshShapeVariableDefault = "SEX"
netThreshDistanceMethod = "bray"
################################################################################
# d3 Network Default Parameters
################################################################################
LinkDistThreshold = 0.4
d3DefaultLinkScaleFactor = 40
d3DefaultDistance = "bray"
d3NetworkColorVar = "Family"
d3NodeLabelVar = c("Phylum", "Order", "Class", "Family", "OTU")

################################################################################