# maps of human population density: number of people per grid
# Plot: Fig.2b

# libraries
library("lattice")
library("yacca")

load("resu/DimRed_FI_wsize90_skp7_kNN16.Rdata")
load("resu/winlist_wsize90_skp7.Rdata")

###################
# set up for map
###################

# set time window
i <- 19 # choose time window 19, as it has most locations for comparison
  # locations: isomap components of TW 19, with common locations
  idx_mtb = MTB.list[[i]]
  # iso components with common location
  zensusI <- as.matrix(filter(zensus, zensus$mtbschnitt_id %in% idx_mtb))
  # scale population to have max value 1, note: min value = 0
  zensusI2 <- zensusI[,2]/max(zensusI[,2])
  
  # color limits: make max/min value +/-flm
  zensusI3 = zensusI[,2]
  flmx = 90000 #max(zensusI3)*0.2
  flm = 10000
  zensusI3[zensusI3 > flmx] = flmx
  zensusI3[zensusI3 < flm] = flm  
  plotmap <- zensusI3
  # plot maps-------------------------------------------------------------------
  for (k in c(16)) {
    # choose colourmap from here https://kwstat.github.io/pals/
    palette = rev(brewer.ylgnbu(5)) #linearl(5) #rev(brewer.set2(4)) # blue (low, negative) to red (high, positive)
    #palette = cubicl(100) # ocean.matter(100)
    #ocean.matter(100) #ocean.haline(100) #cubicl(100)ocean.matter(100) #brewer.rdbu(100) # 
    
    # for data
    for (ploter in c("Win")) { 
        P = plotmap
        # set the min/max colors to quantiles 0.01 and 0.99
        #P = truncate_P(P)
        
        # set the color range
        scale_range = c(flm,flmx) #c(-plP, plP) # zero at centre for cold-hot plots
        
        # megr by name
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
        filename = paste(getwd(), "/figs/Fig2_MapHumPopulation", sep = "")
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

