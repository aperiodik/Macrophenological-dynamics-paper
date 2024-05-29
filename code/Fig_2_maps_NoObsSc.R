# map of the scaled number of species per grid cell 
# Plot: Fig.2a

# libraries
library("lattice")
library("yacca")

load("resu/DimRed_FI_wsize90_skp7_kNN16.Rdata")
load("resu/winlist_wsize90_skp7.Rdata")

###################
# map per time window
###################

for (i in 1:nowi) {
  whichwin <- sprintf("%02.0f",i) # add 0-padding for correct file name
  idx_mtb = MTB.list[[i]]
  # observation counts per time i and grid
  isoW = as.vector(rowSums(as.data.frame(datawin.list[[i]])))
  print(min(isoW))
  plotmap <- isoW/max(isoW) # scaling the number of observations
  flmM <- 1 #max(plotmap)
  flm <- 0 #min(plotmap)
  # plot maps-------------------------------------------------------------------
  for (k in c(16)) {   
    # choose colormpa from here https://kwstat.github.io/pals/
    palette = cubehelix(5)#rev(brewer.set2(5)) # blue (low, negative) to red (high, positive)
    #palette = cubicl(100) # ocean.matter(100)
    #ocean.matter(100) #ocean.haline(100) #cubicl(100)ocean.matter(100) #brewer.rdbu(100) # 
    
    # for Florkart and Flora Incognita
    for (ploter in c("Win")) { 
        P = plotmap
        # set the min/max colors to quantiles 0.01 and 0.99
        #P = truncate_P(P)
        
        # set the color range
        scale_range = c(flm,flmM) #c(-plP, plP) # zero at centre for cold-hot plots
        
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
        filename = paste(getwd(), "/figs/Fig2_MapObsNoScaled-wsize", wsize, "-skp-",skp,"k-", k,ploter,whichwin, sep = "")
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
