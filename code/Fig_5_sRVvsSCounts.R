# Plots: Figure 5 and Figure 14 in the appendix
#   scaled residual variance vs. observation counts across Germany 
#   (total observations, species, locations) per time step

# colours
library("yacca")
library("viridis") 
library("RColorBrewer")

# load data
load("resu/DimRed_FI_wsize90_skp7_kNN16.Rdata")
load("resu/winlist_wsize90_skp7.Rdata")

# computing observation counts
datainfo <- lapply(1:nowi, function(i){
  xx <- datawin.list[[i]]
  output <- as.vector(c(i,          # 1. time window
                        dim(xx)[1], # 2. no of MTBs
                        dim(xx)[2], # 3. no of species
                        c(sum(xx)), # 4. no of observations
                        median(apply(xx,1,sum)), # 5. median of no. of species per MTB
                        # this measure is very similar to no. of observations per time window
                        max(apply(xx,1,sum)) # 6. max no. of species observed per MTB at time t
  ))
})

datainfom <- matrix(unlist(datainfo),nrow=nowi,byrow=T)
# scaling
datainfom1 <- apply(datainfom,2,function(x){x/max(x)}) # max variable (obs, spec, loc)
#datainfom2 <- apply(datainfom,2,function(x){(x-min(x))/(max(x)-min(x))})
#datainfomZ <- apply(datainfom,2,function(x){(x-mean(x))/(sd(x))})

# residual variance
resid_var <- lapply(1:nowi, function(i){
  # extract residual variance
  x <- dimR.list[[i]][1,]
  x <- x/max(x) # infinity-norm scaled
  x
})

# residual variance
res_varM <- matrix(unlist(resid_var),nrow=nowi,byrow=T)

################################################
## PLOT: residual variance vs scaled no of locations across Germany
################################################
for (rvch in c(4,5,15)) { 
  fina <- paste("figs/Fig5_Resvar",rvch,"VSscLocs.pdf", sep = "")
  pdf(fina,
      width = 8, height = 6, # Width and height in inches
      family="Helvetica",        # font tpye
      pointsize=22,          # text size
      bg = "white",          # Background color
      colormodel = "cmyk",   # Color model (cmyk is required for most publications)
      #paper = "A4"          # Paper size
  )
  
  layout(t(1:2),widths=c(6,1.2))
  # c(bottom, left, top, right) margins
  # default c(5,4,4,2)+0.1
  par(mar=c(5, 4, 0.7, 0.5)+0.3, 
      mgp=c(3, 0.5, 0)) # move tick labels out
  
  season <- c(
    rep(4, 9), # winter
    rep(1, 13), # spring
    rep(2, 13), # summer
    rep(3, 13), # autumn
    rep(4, 5)
  )
  # plot each matrix column
  plotxx <- datainfom1[c(1:53,1),2] # no of MTBs
  plotyy <- res_varM[c(1:53,1),rvch] 
  vck = length(plotyy)
  myco <- brewer.divdiv(4) #
  myco <- kovesi.cyclic_mygbm_30_95_c78(vck) #isol(vck) #ocean.matter(vck)
  
  plot(plotxx, plotyy,
          type = "o", # plot type
          cex = 1.2,
          pch = 20,
          lty = 1, # line type
          col= "gray", # lines colours
          lwd = 2, # line width
          #cex = 1, # font expansion
          xlab = "Scaled no. of locations",
          ylab = "Scaled residual variance", 
          xlim = c(min(plotxx),1),
          ylim = c(min(plotyy),1),
          #xaxt = ,#"n", # remove tick labels
          #yaxt = ,#"n", # remove tick labels
          las = 2 # x-axis label:vertical, y-axis label:horizontal
  )
  
  lines(plotxx, plotyy,
        type = "p", # plot type
        cex = 2,
        pch = 20,
        lty = 1, # line type
        col= myco, # lines colours
        lwd = 2, # line width
  )
  
  seasi <- c(1,10,23,36,49)
  lines(plotxx[seasi],plotyy[seasi],
        type = "p", # plot type
        cex = 2,
        pch = 10, # 1
        )

  # 2nd plot for colourbar
  # c(bottom, left, top, right) margins
  # default c(5,4,4,2)+0.1
  par(mar=c(5,0.5,3.5,2)+0.3)
  image(y=1:vck,z=t(rev(1:vck)), 
        col=myco, 
        axes=FALSE, 
        #main="t-win",
        font.main = 1, # plain
        #cex.main=1
  )
  axis(4,
       at = c(length(plotyy)-49, length(plotyy)-36,
              length(plotyy)-23, length(plotyy)-10),
       las = 2, # 
       #labels = rev(c(1,10,20,30,40,50)), 
       labels = rev(c("Spr", "Sum", "Aut", "Win")),
       #cex.axis=1,
       mgp=c(0,0.5,0)) # margins
  
  dev.off()
}

################################################
## PLOT: residual variance vs scaled no of species across Germany
################################################

for (rvch in c(4,5,15)) {
  fina <- paste("figs/Fig5_Resvar",rvch,"VSscSpecNo.pdf", sep = "")
  pdf(fina,
    width = 8, height = 6, # Width and height in inches
    family="Helvetica",        # font tpye
    pointsize=22,          # text size
    bg = "white",          # Background color
    colormodel = "cmyk",   # Color model (cmyk is required for most publications)
    #paper = "A4"          # Paper size
  )
  
  layout(t(1:2),widths=c(6,1.2))
  # c(bottom, left, top, right) margins
  # default c(5,4,4,2)+0.1
  par(mar=c(5, 4, 0.7, 0.5)+0.3, 
      mgp=c(3, 0.5, 0)) # move tick labels out
  
  season <- c(
    rep(4, 9), # winter
    rep(1, 13), # spring
    rep(2, 13), # summer
    rep(3, 13), # autumn
    rep(4, 5)
  )
  # plot each matrix column
  plotyy <- res_varM[c(1:53,1),rvch] 
  plotxx <- datainfom1[c(1:53,1),3] #no. of species
  vck = length(plotyy)
  myco <- brewer.divdiv(4) #
  myco <- kovesi.cyclic_mygbm_30_95_c78(vck) #isol(vck) #ocean.matter(vck)
  
  plot(plotxx, plotyy,
       type = "o", # plot type
       cex = 1.2,
       pch = 20,
       lty = 1, # line type
       col= "gray", # lines colours
       lwd = 2, # line width
       #cex = 1, # font expansion
       ylab = "Scaled residual variance", 
       xlab = "Scaled no. of species",
       xlim = c(min(plotxx),1),
       ylim = c(min(plotyy),1),
       #xaxt = ,#"n", # remove tick labels
       #yaxt = ,#"n", # remove tick labels
       las = 2 # x-axis label:vertical, y-axis label:horizontal
  )
  
  lines(plotxx, plotyy,
        type = "p", # plot type
        cex = 2,
        pch = 20,
        lty = 1, # line type
        col= myco, # lines colours
        lwd = 2, # line width
  )
  
  seasi <- c(1,10,23,36,49)
  lines(plotxx[seasi],plotyy[seasi],
        type = "p", # plot type
        cex = 2,
        pch = 10,
  )

  # 2nd plot for colourbar
  # c(bottom, left, top, right) margins
  # default c(5,4,4,2)+0.1
  par(mar=c(5,0.5,3.5,2)+0.3)
  image(y=1:vck,z=t(rev(1:vck)), 
        col=myco, 
        axes=FALSE, 
        #main="Time", #Win",
        font.main = 1,
        #cex.main=1
  )
  axis(4,
       at = c(length(plotyy)-49, length(plotyy)-36,
              length(plotyy)-23, length(plotyy)-10),
       las = 2, # 
       #labels = rev(c(1,10,20,30,40,50)), 
       labels = rev(c("Spr", "Sum", "Aut", "Win")),
       #cex.axis=1,
       mgp=c(0,0.5,0)) # margins
  
  dev.off()
}

################################################
## PLOT: residual variance vs scaled no of total observations across Germany
################################################

for (rvch in c(1:15)) { # dimension
  fina <- paste("figs/Fig5_Resvar",rvch,"VSscObsno.pdf", sep = "")
  pdf(fina,
      width = 8, height = 6, # Width and height in inches
      family="Helvetica",        # font tpye
      pointsize=22,          # text size
      bg = "white",          # Background color
      colormodel = "cmyk",   # Color model (cmyk is required for most publications)
      #paper = "A4"          # Paper size
  )
  
  layout(t(1:2),widths=c(6,1.2))
  # c(bottom, left, top, right) margins
  # default c(5,4,4,2)+0.1
  par(mar=c(5, 4, 0.7, 0.5)+0.3, 
      mgp=c(3, 0.5, 0)) # move tick labels out
  
  season <- c(
    rep(4, 9), # winter
    rep(1, 13), # spring
    rep(2, 13), # summer
    rep(3, 13), # autumn
    rep(4, 5)
  )
  # plot each matrix column
  plotyy <- res_varM[1:53,rvch] 
  plotxx <- datainfom1[1:53,4] #no. of observations
  vck = length(plotyy)
  myco <- brewer.divdiv(4) #
  myco <- kovesi.cyclic_mygbm_30_95_c78(vck) #isol(vck) #ocean.matter(vck)
  
  plot(plotxx, plotyy,
       type = "o", # plot type
       cex = 1.2,
       pch = 20,
       lty = 1, # line type
       col= "gray", # lines colours
       lwd = 2, # line width
       #cex = 1, # font expansion
       ylab = "Scaled residual variance", 
       xlab = "Scaled no. of observation",
       xlim = c(min(plotxx),1),
       ylim = c(min(plotyy),1),
       #xaxt = ,#"n", # remove tick labels
       #yaxt = ,#"n", # remove tick labels
       las = 2 # x-axis label:vertical, y-axis label:horizontal
  )
  
  lines(plotxx, plotyy,
        type = "p", # plot type
        cex = 2,
        pch = 20,
        lty = 1, # line type
        col= myco, # lines colours
        lwd = 2, # line width
  )
  
  seasi <- c(1,10,23,36,49)
  lines(plotxx[seasi],plotyy[seasi],
        type = "p", # plot type
        cex = 2,
        pch = 10,
  )

  # 2nd plot for colourbar
  # c(bottom, left, top, right) margins
  # default c(5,4,4,2)+0.1
  par(mar=c(5,0.5,3.5,2)+0.3)
  image(y=1:vck,z=t(rev(1:vck)), 
        col=myco, 
        axes=FALSE, 
        main="Time", #Win",
        font.main = 1,
        #cex.main=1
  )
  axis(4,
       at = c(length(plotyy)-49, length(plotyy)-36,
              length(plotyy)-23, length(plotyy)-10),
       las = 2, # 
       #labels = rev(c(1,10,20,30,40,50)), 
       labels = rev(c("Spr", "Sum", "Aut", "Win")),
       #cex.axis=1,
       mgp=c(0,0.5,0)) # margins
  
  dev.off()
}

