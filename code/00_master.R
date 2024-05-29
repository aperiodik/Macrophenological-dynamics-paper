# Code for reproducing the analysis in the publication:
# "Macrophenological dynamics from citizen science plant occurrence data".
# Karin Mora et al.

rm(list=ls())

# list of packages we need for the analytics
packages2use = c(
  # for data wrangling
  "tidyverse",
  # for some plots
  "reshape2",
  "gridExtra",
  # for ecological distance computations
  "vegan",
  # for dimension reduction
  "dimRed",
  "igraph",
  # for cca
  "yacca",
  # for clustering
  "apcluster",
  # for spatial machine learning
  "CAST",
  "caret",
  #"randomForest",
  # for PCA (not that this has also a CCA, so we need to take care to yacca::cca)
  "FactoMineR",
  "factoextra",
  # For parallel computing
  "parallel",
  # for geo processing
  "rgdal",
  "sp",
  "rgeos",
  # for plotting
  "corrplot",
  "pals",
  "wordcloud",
   # for producing and saving maps
  "mapview",
  "htmlwidgets",
  "webshot",
  "leaflet")
  
for(p in packages2use){
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# nodes for parallel computations
#n_nodes = min(40, detectCores()-1)

# wrapper function for generating the maps
cat("====== load extra code =======\n")
source("code/f_map2mtb.R")
# function for computing k-NN graph and derive geodesic distance matrix
source("code/f_d2dgeo.R")
# function to truncate a vector by quantiles
source("code/f_trunc_quant.R")

################################################
## (I) Load data
# This is needed for the main computations (II) and plotting of results (III)
################################################

##-------------
# read data and set time series properties
cat("====== run 01_read_data.R =======\n")
source("code/01_read_data.R")

################################################
## (II) Main computations: 
# * time series aggregation
# * Isomap computation
# * run once (approx computation time 7 hours)
################################################

# aggregate time series, i.e. average annual cycle
cat("====== run 02_aggregate_time_series.R =======\n")
source("code/02_aggregate_time_series.R")

##-------------
# compute dimension reduction with Isomap
# and residual variance
cat("====== run 03_dimredIso.R =======\n")
source("code/03_dimredIso.R")

##-------------

################################################
## (III) Plot results
# * canonical correlation analyses are performed as part of figure generation
# * required data are loaded for each figure, respectively.
#   In other words, once the main computations (II) have been performed, 
#   plots can be generated independently
################################################

##-------------
# plot: Observation counts across average annual cycle across Germany
cat("====== run Fig_2_scaledCounts.R =======\n")
source("code/Fig_2_scaledCounts.R")

##-------------
# plot: Observation counts: map human population density
cat("====== run Fig_2_map_PopDensity.R =======\n")
source("code/Fig_2_map_PopDensity.R")

##-------------
# plot: Observation counts per grid cell across average annual cycle
cat("====== run Fig_2_maps_NoObsSc.R =======\n")
source("code/Fig_2_maps_NoObsSc.R")

##-------------
# plot map: isomap components across annual cycle
# these plots were used to create Fig 10-13 in the appendix, too.
cat("====== run Fig_3_mapsIso.R =======\n")
source("code/Fig_3_mapsIso.R")

##-------------
# plot scaled residual variance across average annual cycle
cat("====== run Fig_4_scaledResidualVariance.R =======\n")
source("code/Fig_4_scaledResidualVariance.R")

##-------------
# plot scaled residual variance vs scaled observation counts across average annual cycle
# these plots were used to create Fig 14 in the appendix, too.
cat("====== run Fig_5_sRVvsSCounts.R =======\n")
source("code/Fig_5_sRVvsSCounts.R")

##-------------
# compute canonical correlation analysis of isomap components of consecutive time steps 
# across average annual cycle  CCorA(Y(t),Y(t+1)) and 
# plot canonical correlations against time
cat("====== run Fig_6_cancorAnnual.R =======\n")
source("code/Fig_6_cancorAnnual.R")

##-------------
# compute canonical correlation analysis of isomap components of consecutive time steps 
# across average annual cycle  CCorA(Y(t),Y(t+1)) and 
# canonical correlations against scaled observation counts
# these plots were used to create Fig 17 in the appendix, too.
cat("====== run Fig_7_cancorVSsCounts.R =======\n")
source("code/Fig_7_cancorVSsCounts.R")

##-------------
# compute canonical correlation analysis of isomap components 
# and human population density
# across average annual cycle CCorA(Y(t),HPop) and
# plot canonical correlations and structural correlations squared
cat("====== run Fig_8_CCorA_Iso_Pop.R =======\n")
source("code/Fig_8_CCorA_Iso_Pop.R")

##-------------
# figures in appendix:
# plot: Observation counts across each other 
# across average annual cycle across Germany
cat("====== run Fig_9app_scObsVSmedObs.R =======\n")
source("code/Fig_9app_scObsVSmedObs.R")

##-------------
# figures in appendix:
# compute canonical correlation analysis of isomap components of consecutive time steps 
# across average annual cycle  CCorA(Y(t),Y(t+1)) and 
# plot the maps of canonical variates
cat("====== run Fig_15app_CCorA_mapsCV.R =======\n")
source("code/Fig_15app_CCorA_mapsCV.R")

##-------------
# figures in appendix:
# compute canonical correlation analysis of isomap components of consecutive time steps 
# across average annual cycle  CCorA(Y(t),Y(t+1)) and 
# plot structural correlations squared
cat("====== run Fig_16app_CCorA_SCSq.R =======\n")
source("code/Fig_16app_CCorA_SCSq.R")
