# Plots for Figure 8: CCorA(Y(i),Human Population)
# * compute and plot canonical correlations 
# * compute and plot structural correlations squared of Y(i)


# colours
library("yacca")
library("viridis") 
library("RColorBrewer")

# labels for plot
xvtic = c(1,seq(10,nowi, by=13))
xvlab = c("1/1-30/3", #1
          "4/3-1/6", #10= met. spring
          "3/6-31/8", #23=met. summer
          "2/9-30/11", #36=met. autumn
          "2/12-1/3" #49 met. winter
)

# load data
load("resu/DimRed_FI_wsize90_skp7_kNN16.Rdata")
load("resu/winlist_wsize90_skp7.Rdata")


#############################
# canonical correlations
########################

nocom <- 5
X_ccorA <- lapply(1:nowi, function(i){
  idx_mtb = MTB.list[[i]]
  # remove first row, which is residual variance
  isoW = as.matrix(filter(as.data.frame(dimR.list[[i]][-1,]), MTB.list[[i]] %in% idx_mtb))
  
  # human population, find common locations
  zensusI <- as.matrix(filter(zensus, zensus$mtbschnitt_id %in% idx_mtb))
  # scale population to have max value 1, note: min value = 0
  zensusI2 <- zensusI[,2]/max(zensusI[,2])
  isoW2 <- isoW[, 1:nocom] 
  isoY2 <- zensusI2
  
  # CCorA(Y(i),Human Population)
  cca.fit <- cca(isoW2, isoY2)#,use.eigs=TRUE)
  
  # canonical correlations
  cancorlist <- as.vector(cca.fit$corr)
})

X_ccorAm <- matrix(unlist(X_ccorA),nrow=nowi,byrow=T)


############################
# structural correlations squared for isomap components
#############################
# Squared structural correlations for x variables on each canonical variate 
# (i.e., fraction of x variance associated with each variate).

Y_ccorA <- lapply(1:nowi, function(i){
  idx_mtb = MTB.list[[i]]
  # remove first row, which is residual variance
  isoW = as.matrix(filter(as.data.frame(dimR.list[[i]][-1,]), MTB.list[[i]] %in% idx_mtb))
  
  # human population, find common locations
  zensusI <- as.matrix(filter(zensus, zensus$mtbschnitt_id %in% idx_mtb))
  # scale population to have max value 1
  zensusI2 <- zensusI[,2]/max(zensusI[,2])
  
  isoW2 <- isoW[, 1:nocom]
  isoY2 <- zensusI2
  
  # CCorA(Y(i),Human Population)
  cca.fit <- cca(isoW2, isoY2)#,use.eigs=TRUE)
  
  # compute structural correlations squared
  cancorlist <- as.vector(cca.fit$xstructcorrsq)
})

Y_ccorAm <- matrix(unlist(Y_ccorA),nrow=nowi,byrow=T)

############--------------------------------
# Plot: canonical correlations
############--------------------------------

for (i in 1){
  a0<- i # which component to plot
  a1 <-(nocom)*(a0-1)+1
  a2 <-(nocom)*a0
  cvi <- i#c(a1:a2)
  print(cvi)

  fina <- paste("figs/Fig8_CCorA_Iso_Pop_corr.pdf", sep = "")
  pdf(fina,       # File name
    width = 8, height = 3, # Width and height in inches
    family="Helvetica",        # font tpye
    pointsize=22,          # text size
    bg = "white",          # Background color
    colormodel = "cmyk",   # Color model (cmyk is required for most publications)
    #paper = "A4"          # Paper size
  )

  par(mar=c(5, 4, 0.5, 0.5)+0.1)#, mgp=c(3, 1, 0))

  matplot(X_ccorAm[,cvi], 
        type = "o", # plot type
        lty = 1, # line type
        col=c("black"),
        lwd = 2,
        pch = c(20),
        cex = 0.5,
        xlab = "", 
        ylab = "Can. corr.",
        xlim = c(0,nowi+10),
        ylim = c(0.5,0.72),
        xaxt = "n", # remove tick labels
        yaxt = "n", # remove tick labels
        las = 2 # x-axis label:vertical, y-axis label:horizontal
  )

  axis(1, 
     at = xvtic, # vector with label location
     las = 2, # vertical labels
     labels = FALSE #xvlab # vector with labels
  )
  # https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
  # rotate x-axis labels
  text(x = xvtic,
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.09,
     ## Use names from the data list.
     labels = xvlab,
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 1)

  # x-axis title and distance to labels
  title(xlab = "Time window", line = 4) 

  fylab <- c(0.5,0.6,0.7)#c(0.1,0.3,0.5,0.7)
  axis(2, at = fylab , # vector with label location
     las = 2, # vertical labels
     labels = fylab # vector with labels
  )

  dev.off()
}


############--------------------------------
# Plot: structural correlations squared
############--------------------------------

# colours
mycol <-colorRampPalette(brewer.pal(12,"Paired"))
mycolN <- c(2,4,6,8,9)

for (i in 1){
  a0<- i # which component to plot
  a1 <-(nocom)*(a0-1)+1
  a2 <-(nocom)*a0
  cvi <- c(a1:a2)
  print(cvi)
  
  fina <- paste("figs/Fig8_CCorA_Iso_Pop_SCSq.pdf", sep = "")
  pdf(fina,       # File name
      width = 8, height = 6, # Width and height in inches
      family="Helvetica",        # font tpye
      pointsize=22,          # text size
      bg = "white",          # Background color
      colormodel = "cmyk",   # Color model (cmyk is required for most publications)
      #paper = "A4"          # Paper size
  )
  
  par(mar=c(5, 4, 0.5, 0.5)+0.1)#, mgp=c(3, 1, 0))
  
  matplot(Y_ccorAm[,cvi], 
          type = "o", # plot type
          lty = 1, # line type
          col=c(mycol(12)[mycolN]),
          lwd = 2,
          pch = c(20),
          cex = 0.5,
          xlab = "", 
          ylab = "SCSq",
          xlim = c(0,nowi+10), ylim = c(0,1),
          xaxt = "n", # remove tick labels
          yaxt = "n", # remove tick labels
          las = 2 # x-axis label:vertical, y-axis label:horizontal
  )
  
  axis(1, 
       at = xvtic, # vector with label location
       las = 2, # vertical labels
       labels = FALSE #xvlab # vector with labels
  )
  # https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
  # rotate x-axis labels
  text(x = xvtic,
       ## Move labels to just below bottom of chart.
       y = par("usr")[3] - 0.1,
       ## Use names from the data list.
       labels = xvlab,
       ## Change the clipping region.
       xpd = NA,
       ## Rotate the labels by 35 degrees.
       srt = 35,
       ## Adjust the labels to almost 100% right-justified.
       adj = 0.965,
       ## Increase label size.
       cex = 1)
  
  # x-axis title and distance to labels
  title(xlab = "Time window", line = 4) 
  
  fylab <- c(0,0.5,1)#c(0.1,0.3,0.5,0.7)
  axis(2, at = fylab , # vector with label location
       las = 2, # vertical labels
       labels = fylab # vector with labels
  )
  
  legend("topright",
         inset = c(0.01, 0), 
         legend = c(1:nocom),
         col = c(mycol(12)[mycolN]),
         x.intersp = 0.05, # space btw symbol and text
         #lty = c(matrix(1,1,length(rvvec)+1),2),
         #xjust = 1, # justify legend: 0 = left, 0.5 = centre, 1 = right
         yjust = 1, # justify legend: 0 = top, 0.5 = centre, 1 = bottom
         bty = "n", #no box
         lwd = 1,
         lty = c(NA,NA,NA,NA,NA),
         pch = c(16,16,16,16,16),
         #cex = 1, # font expansion
         title = "MPS",
         horiz = F, # shape: row = T(rue), col = F(alse)
         #xpd = TRUE # legend outside plot
  )
  
  dev.off()
}