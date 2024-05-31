# canonical correlation analysis 
# between isomap components of consecutive windows CCorA(Y(i),Y(i+1)):
#   * compute canonical correlations
#   * plot for figure 6

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

##############################################################
# Canonical Correlation 
# CCorA(Y(i), Y(i+1))
##############################################################

nowi2 <- nowi-1
nocomw <- 5 # 5 or 15  # number of components for calculation

canCorWW <- lapply(1:nowi2, function(i){
  # find common location
  idx_mtb = sort(intersect(MTB.list[[i]], MTB.list[[i+1]]))
  isoW  = as.matrix(filter(as.data.frame(dimR.list[[i]][-1,]), MTB.list[[i]] %in% idx_mtb))
  isoW2 = as.matrix(filter(as.data.frame(dimR.list[[i+1]][-1,]), MTB.list[[i+1]] %in% idx_mtb))
  # CCorA(Y(i), Y(i+1))
  cca.fit <- cca(isoW[, 1:nocomw], isoW2[ , 1:nocomw])
  # get canonical correlations
  cancorlistW <- round(as.vector(cca.fit$corr),5)
})

canCorWWm <- matrix(unlist(canCorWW),nrow=nowi2,byrow=T)

# plot
nameme = paste0("figs/Fig6_cancorAnnual",nocomw,".pdf")
pdf(nameme,       # File name
    width = 8, height = 6, # Width and height in inches
    # 8,6 or 8,5
    family="Helvetica",        # font tpye
    pointsize=22,          # text size
    bg = "white",          # Background color
    colormodel = "cmyk",   # Color model (cmyk is required for most publications)
    #paper = "A4"          # Paper size
)

par(mar=c(5, 4, 0.5, 0.5)+0.1)
#mycol = colorRampPalette(rev(brewer.pal(5,"Dark2")))
mycol = colorRampPalette(rev(brewer.pal(4,"Set2")))
nocomw = 4 # number of components to plot
matplot(c(1:nowi2),canCorWWm[,1:nocomw], 
        type = "o", # plot type
        lty = 1, # line type
        col=mycol(nocomw),
        lwd = 2,
        pch = 20,
        cex = 0.5, # font expansion
        xlab = "",#Time window (TW)", 
        ylab = "Canonical correlations",
        #main = "TW(i) and TW(i+1)", 
        xlim = c(0,nowi+13), ylim = c(0.45,1),
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

#yvtic = c(0.5,0.6,0.7,0.8,0.9,1)
#yvlab = c(0.5,0.6,0.7,0.8,0.9,1)
yvtic = c(0,0.2,0.4,0.6,0.8,1)
yvlab = c(0,0.2,0.4,0.6,0.8,1)

axis(2, at = yvtic,
     las = 2,
     labels = yvlab)

legend("topright",
       inset = c(0.01, 0), 
       legend = c(expression(rho[1]),expression(rho[2]),expression(rho[3]),expression(rho[4])),#c(1:nocomw),
       col = mycol(nocomw),
       x.intersp = 0.05, # space btw symbol and text
       #lty = c(matrix(1,1,length(rvvec)+1),2),
       #xjust = 1, # justify legend: 0 = left, 0.5 = centre, 1 = right
       yjust = 1, # justify legend: 0 = top, 0.5 = centre, 1 = bottom
       bty = "n", #no box
       lwd = 1,
       #lty = c(1,1,1,1),#
       lty = c(NA,NA,NA,NA),
       pch = c(16,16,16,16),
       cex = 1, # font expansion
       #title = expression(rho),
       horiz = F, # shape: row = T(rue), col = F(alse)
       #xpd = TRUE # legend outside plot
)

dev.off()

