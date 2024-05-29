# Plots: Figure 4
#   * scaled (min-max) residual variance of all time windows (TW)
#   * scaled residual variance of a selection of TW

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

load("resu/DimRed_FI_wsize90_skp7_kNN16.Rdata")
# residual variance of static data (previous work) for comparison
load("data/ResidualVariances1821Y.Rdata")

# get previously computed residual variance and scale it
resid_var <- lapply(1:nowi, function(i){
  # extract residual variance
  x <- dimR.list[[i]][1,]
  x <- x/max(x) # scaled by the infinity-norm
  x
})

res_varM <- matrix(unlist(resid_var),nrow=nowi,byrow=T)
vck <- dim(dimR.list[[1]])[2] # no. of components and hence colours
res_varY <-rbind(res_var_fi/max(res_var_fi),res_var_fk/max(res_var_fk)) # change names depending on input file

##############################################################
#------------------------------
# plot: scaled residual variance across annual cycle
#------------------------------
##############################################################

# Customizing the figure output
pdf("figs/Fig4_sRVannual.pdf",       # File name
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
par(mar=c(5, 4, 0.5, 0.5)+0.1)#, mgp=c(3, 1, 0))

plotme <- res_varM[,1:vck]

# plot each matrix column
matplot(c(1:nowi),plotme, 
        type = "o", # plot type
        cex = 0.5,
        pch = 20,
        lty = 1, # line type
        col=viridis(vck), # lines colours
        lwd = 2, # line width
        #cex = 1, # font expansion
        xlab = "", 
        ylab = "Scaled residual variance",
        xlim = c(0,nowi+1),
        ylim = c(0.2,1),
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
     y = par("usr")[3] - 0.07,
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

fylab <- c(0,0.2,0.4,0.6,0.8,1)#c(0.1,0.3,0.5,0.7)
axis(2, at = fylab , # vector with label location
     las = 2, # vertical labels
     labels = fylab # vector with labels
)

# grid()

# 2nd plot for colourbar:
# c(bottom, left, top, right) margins, default c(5,4,4,2)+0.1
par(mar=c(5,0.5,3.5,2)+0.1)
image(y=1:vck,z=t(rev(1:vck)), 
      col=c(viridis(vck)), 
      axes=FALSE, 
      main="Dim",
      font.main = 1,
      #cex.main=1
)
axis(4,at = c(1,6,11,15), 
     las = 2, 
     labels = rev(c(1,5,10,15)), 
     #cex.axis=1,
     mgp=c(0,0.5,0))

dev.off()

##############################################################
#------------------------------
# plot:  residual variance for a selection of time windows:
# first window, spring, summer, autumn, winter, static data
#------------------------------
##############################################################

rvvec = c(1,10,23,36,49)
# 10 = spring, 23 = summer, 36 = autumn, 49 = winter
FewWin = c(xvlab, "all")
vck=dim(dimR.list[[1]])[2]
nocu = length(rvvec)

pdf("figs/Fig4_sRVselection.pdf",       # File name
    width = 8, height = 6, # Width and height in inches
    family="Helvetica",        # font tpye
    pointsize=22,          # text size
    bg = "white",          # Background color
    colormodel = "cmyk",   # Color model (cmyk is required for most publications)
    #paper = "A4"          # Paper size
)

# c(bottom, left, top, right) margins
par(mar=c(3.5, 4, 0.5, 0.5)+0.1)#, mgp=c(3, 1, 0))
mycol = colorRampPalette(rev(brewer.pal(5,"Dark2")))
plotme <- res_varM[rvvec,1:vck]
#plot matrix rows
matplot(t(plotme), 
        type = "o", # plot type
        cex = 0.5, 
        pch = 20,
        lty = 1, # line type
        col=mycol(nocu),
        lwd = 2, # line width
        #cex = 2, # font 
        xlab = "", ylab = "Scaled residual variance",
        xlim = c(0.5,vck+5),
        ylim = c(0,1),
        xaxt = "n", # remove tick labels
        #yaxt = "n", # remove tick labels
        las = 2 # x-axis label:vertical, y-axis label:horizontal
)

matlines(res_varY[1,],
         type = "o",
         lty = c(1,3),
         cex = 0.5,
         pch = 20,
         col = "azure4",
         lwd = 2)
xvtic2 = c(1,seq(5,16, by=5))
axis(1, at = xvtic2,
     #las = 2,
     labels = xvtic2)
title(xlab = "Dimension", line = 2.5) 

axis(2, at = c(0,0.2,0.4,0.6), # vector with label location
     las = 2, # vertical labels
     labels = c(0,0.2,0.4,0.6) # vector with labels
)
plotme2 <- as.vector(c(plotme[,15],res_varY[1,15]))+c(0.01,0.03,-0.01, 0.01,0.01,-0.04)#,-0.03)

text(c(1,1,1, 1,1,1)*15.2,
     plotme2,
     labels = c(FewWin),
     cex = 1, pos = 4, 
     col = c(mycol(nocu),"azure4"))

dev.off()
