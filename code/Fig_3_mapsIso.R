# maps of isomap components per time step
# This generates all plots for figures 3, and appendix figures 10-13

# libraries
library("lattice")
library("yacca")

load("resu/DimRed_FI_wsize90_skp7_kNN16.Rdata")
load("resu/winlist_wsize90_skp7.Rdata")

# no. of components
noComp <- 5
###################
# Plot
###################

# for each time window (tw) i determine: reference tw for directions of components
reftw <- 19 
for (i in 1:nowi) {
  whichwin <- sprintf("%02.0f",i) # add 0-padding for correct file name
  # choose time window 19, as it has most locations for comparison
  # locations: isomap components of TW 19, with common locations
  idx_mtb = sort(intersect(MTB.list[[i]], MTB.list[[reftw]]))
  # iso components with common location
  isoW19 = as.matrix(filter(as.data.frame(dimR.list[[reftw]][-1,]), MTB.list[[reftw]] %in% idx_mtb))
  # all isomap components, with common locations
  isoW = as.matrix(filter(as.data.frame(dimR.list[[i]][-1,]), MTB.list[[i]] %in% idx_mtb))
  
  # unify orientation isomap components
  sgnD<- unlist(lapply(1:dim(isoW)[[2]], function(i){
    # determine angle to obtain the same orientation for all components
    x <- isoW19[,i]
    y <- isoW[,i]
    # angle between two vectors
    theta <- round( (x%*%y / ( sqrt(x%*%x) * sqrt(y%*%y) )) ,2)
    if (theta<0){sgD <- 1}
    else sgD <- (-1)
  }))
 
  # change the sign of the isomap components
  isoW <- isoW %*% diag(sgnD)

  plotmap <- isoW
  # plot maps-------------------------------------------------------------------
  for (k in c(16)) {   
    # choose colormpa from here https://kwstat.github.io/pals/
    palette = brewer.brbg(100) # blue (low, negative) to red (high, positive)
    #palette = cubicl(100) # ocean.matter(100)
    #ocean.matter(100) #ocean.haline(100) #cubicl(100)ocean.matter(100) #brewer.rdbu(100) # 

    for (ploter in c("Win")) { 
      # for the leading 5 dimensions
      for (idim in 1:noComp) {
        P = plotmap[ ,idim]
        whichc = idim
        # set the color range for unified visualisation
        if (idim == 1) {flm = 2.5}
        else if (idim >1) {flm = 1.0}
        
        # make max/min value +/-flm
        P[P > flm] = flm
        P[P <  (-flm)] = -flm
        
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
        #the_Map
        # save the map as html and png
        filename = paste(getwd(), "/figs/Fig3_MapIso-wsize", wsize, "-skp-",skp,"k-", k,"-noc-",noComp,"-C-", whichc,ploter,whichwin, sep = "")
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
