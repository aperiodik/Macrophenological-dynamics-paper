# Plot: observation counts in Fig.2c

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

load("resu/winlist_wsize90_skp7.Rdata")

# computing observation counts
datainfo <- lapply(1:nowi, function(i){
  xx <- datawin.list[[i]]
  output <- as.vector(c(i, # time window
                      dim(xx)[1], # no of MTBs
                      dim(xx)[2], # no of species
                      c(sum(xx)), # no of observations
                      median(apply(xx,1,sum)), # median no. of species per MTB
                      # this measure is very similar to no. of observsations per time window
                      mean(apply(xx,1,sum)), # mean no. of species per MTB
                      min(apply(xx,1,sum)), # min no. of species per MTB
                      max(apply(xx,1,sum)) # max no. of species per MTB
  ))
})

datainfom <- matrix(unlist(datainfo),nrow=nowi,byrow=T)

# scaling
datainfom1 <- apply(datainfom,2,function(x){x/max(x)})
#datainfom2 <- apply(datainfom,2,function(x){(x-min(x))/(max(x)-min(x))})
#datainfomZ <- apply(datainfom,2,function(x){(x-mean(x))/(sd(x))})


###########################################
# PLOT 
#-----------------------------

pdf("figs/Fig2_scaledCounts.pdf",       # File name
    width = 8, height = 5, # Width and height in inches
    family="Helvetica",        # font type
    pointsize=22,          # text size
    bg = "white",          # Background color
    colormodel = "cmyk",   # Color model (cmyk is required for most publications)
    #paper = "A4"          # Paper size
)

par(mar=c(5, 4, 0.5, 0.5)+0.1)#, mgp=c(3, 1, 0))
mycol = colorRampPalette(brewer.pal(12,"Paired"))

matplot(datainfom1[,c(2:4)],
        type = "o", # plot type
        lty = 1, # line type
        col=mycol(12)[c(2,4,8)], # colour
        lwd = 2,
        pch = 20,
        cex = 0.5,
        xlab = "", #Time window (TW)", 
        ylab = paste("Scaled count"),
        xlim = c(0,nowi+1), 
        ylim = c(-0.2,1),
        xaxt = "n", # remove tick labels
        yaxt = "n", # remove tick labels
        las = 2 # x-axis label:vertical, y-axis label:horizontal
)

axis(1, 
     at = xvtic, # vector with label location
     las = 2, # vertical labels
     labels = FALSE #xvlab # vector with labels
)

# rotate x-axis labels
# https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
text(x = xvtic,
     ## Move labels to just below bottom of chart.
     y = par("usr")[3] - 0.15,
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

yvtic = c(0,0.2,0.4,0.6,0.8,1)
yvlab = c(0,0.2,0.4,0.6,0.8,1)
axis(2, at = yvtic,
     las = 2,
     labels = yvlab)

legend("bottomleft",#"topright",
       #inset = c(0.001, 0), 
       legend = c("Location","Species","Observations"),
       col = mycol(12)[c(2,4,8)], #c(viridis(10)[3],viridis(10)[6],viridis(10)[9]),
       x.intersp = 0.05, # space btw symbol and text
       xjust = 1, # justify legend: 0 = left, 0.5 = centre, 1 = right
       yjust = 1, # justify legend: 0 = top, 0.5 = centre, 1 = bottom
       bty = "n", #no box
       text.width = 10,
       lwd = 1,
       lty = c(NA,NA,NA,NA), # marker
       pch = c(16,16,16,16), # marker size
       cex = 1, # font expansion
       #title = "",
       horiz = T, # shape: row = T(rue), col = F(alse)
       #xpd = TRUE # legend outside plot
)
#grid()
dev.off()

