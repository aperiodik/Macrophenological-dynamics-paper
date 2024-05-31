# Appendix plots: fig S9
# canonical correlation analysis 
# between isomap components Y of consecutive windows CCorA(Y(i),Y(i+1)):
#   * compute canonical variates
#   * plot canonical variates: 

# libraries
library("lattice")
library("yacca")

# load data
load("resu/DimRed_FI_wsize90_skp7_kNN16.Rdata")
load("resu/winlist_wsize90_skp7.Rdata")

# no of Isomap components for CCorA
noComp <- 5

###################
# Canonical correlation analysis CCorA(Y(i),Y(i+1))
###################

nowi2 <- nowi-1

# Create list of canonical variates from CCorA(Y(i),Y(i+1))
CanCor.list <- lapply(1:nowi2, function(i){
  # find common locations
  idx_mtbO = sort(intersect(MTB.list[[i]], MTB.list[[i+1]]))
  isoWW = as.matrix(filter(as.data.frame(dimR.list[[i]][-1,]), MTB.list[[i]] %in% idx_mtbO))
  isoWW2 = as.matrix(filter(as.data.frame(dimR.list[[i+1]][-1,]), MTB.list[[i+1]] %in% idx_mtbO))
  # CCorA(Y(i),Y(i+1))
  cca.fit <- cca(isoWW[, 1:noComp], isoWW2[ , 1:noComp])
  # extract canonical variates of Y(i)
  cvSc <- cca.fit$canvarx
})

###################
# Plots canonical variates
###################

reftw <- 19 # for each time window (tw) determine: reference tw for directions of components
for (i in 1:nowi2) { 
  whichwin <- sprintf("%02.0f",i) # add 0-padding for correct file name
  # choose time window 19, as it has most locations for comparison
  # locations: isomap components of TW i, j and 19, with common locations
  idx_mtbO2 = sort(intersect(MTB.list[[i]], MTB.list[[i+1]]))
  idx_mtb19 = sort(intersect(MTB.list[[reftw]], MTB.list[[reftw+1]]))
  idx_mtb = sort(intersect(idx_mtbO2,idx_mtb19))
  # iso components with common location
  isoW19 = as.matrix(filter(as.data.frame(CanCor.list[[reftw]]), idx_mtb19 %in% idx_mtb))
  # all isomap components, with common locations
  isoW = as.matrix(filter(as.data.frame(CanCor.list[[i]]), idx_mtbO2 %in% idx_mtb))
  
  # check direction of each vector for same orientation
  sgnD<- unlist(lapply(1:dim(isoW)[[2]], function(i){
    # determine angle to obtain the same orientation for all components
    x <- isoW19[,i]
    y <- isoW[,i]
    # angle between two vectors
    theta <- round( (x%*%y / ( sqrt(x%*%x) * sqrt(y%*%y) )) ,2)
    if (theta<0){sgD <- 1}
    else sgD <- (-1)
  }))
  
  # orient isomap components the same way (change sign)
  isoW <- isoW %*% diag(sgnD)
  
  # for unified visualisation set max/min value +/-flm
  flm <- 2.5
  isoW[isoW > flm] = flm
  isoW[isoW <  (-flm)] = -flm
  
  plotmap <- isoW
  # plot maps-------------------------------------------------------------------
  for (k in c(16)) {    
    # choose colormpa from here https://kwstat.github.io/pals/
    palette = brewer.rdbu(100) # blue (low, negative) to red (high, positive)
    #palette = cubicl(100) # ocean.matter(100)
    #ocean.matter(100) #ocean.haline(100) #cubicl(100)ocean.matter(100) #brewer.rdbu(100) # 
    
    # plot each time window
    for (ploter in c("Win")) { 
      
      # for the leading 5 dimensions
      for (idim in 1:noComp) {
        whichcv <- idim
        P = plotmap[ ,idim]
        
        # set the color range
        scale_range = c(-flm,flm) #c(-plP, plP) # zero at centre for cold-hot plots
        
        # merge by name
        dim2plot  = as.data.frame(cbind(idx_mtb, P))
        colnames(dim2plot) = c("NAME", "score")
        
        # https://stackoverflow.com/questions/40276569/reverse-order-in-r-leaflet-continuous-legend
        # check plot(1, 1, pch = 19, cex = 3, col = pal(1))
        pal = colorNumeric(
          palette,
          domain = scale_range,
          na.color = "transparent",
          alpha = T,
          reverse = TRUE)
        
        pal_rev = colorNumeric(
          palette,
          domain = scale_range,
          na.color = "transparent",
          alpha = T,
          reverse = FALSE)
        
        # generate the map
        the_Map = map_mtb(dim2plot, idx_mtb, MTB, pal, pal_rev, scale_range)
        # the_Map
        # save the map as html and png
        filename = paste(getwd(), "/figs/Fig15_MapWWcv-wsize", wsize, "-skp-",skp,"k-", k,"-noc-",noComp, "-CV-", whichcv,ploter,whichwin, sep = "")
        saveWidget(the_Map, paste(filename, ".html", sep = ""))
        webshot(url = paste("file://", filename, ".html", sep = ""),
                file = paste(filename, ".png", sep = ""),
                #vwidth = 1200,
                #vheight = 1300,
                vwidth = 500,
                vheight = 630,
                debug = TRUE)
      }
    }
  }
}  
