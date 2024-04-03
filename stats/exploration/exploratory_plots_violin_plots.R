
library(vioplot)
my.bin <- function(x, breaks){
  if(length(breaks) == 1)labelsx <- c(1:breaks)
  if(length(breaks) > 1) labelsx <- c(1:(length(breaks)-1))
  # cutting x into bins (creates a vector of bin labels)
  newbins <- cut(x, breaks, labels = labelsx, include.lowest = TRUE)
  # calculating the mean for each bin
  meanx <- tapply(x, newbins, mean)
  xid <- meanx[newbins]
  xid
}

my.binomconfi<-function(y,plot=TRUE){
  require(binom)
  suc<-sum(y);
  tri<-length(y);
  binom.confint(suc,tri,method="wilson")[1,4:6]
}

lines.cis<-function(confis,unique.x){
  if(dim(confis)[1]!=length(unique.x))stop(print("rowlength of CIs not equal length of unique.x"))
  for (i in 1:length(unique.x)){
    lines(x=rep(unique.x[i],2),y=confis[i,2:3])
  }
}

axval<-1#2.7
medval <- 100
require(plotrix)

# load data, filter data as seen in the other code we have, but this is just here now more as a placeholder.
mmdata = read.csv("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/effort/all/binned_effort_UTC.csv")


# cut timeseries to include complete years: 2009 to 2014
#zcdata = zcdata[zcdata$year>= 2009 & zcdata$year<= 2019,]

graphics.off()
{#plot.new()            # Create empty plot in RStudios' default window
dev.new(width = 7,height = 8,noRStudioGD = TRUE)   # Create new plot window
par(mfrow=c(3,3))
breakN = 25
  
  # 1 year
  xid <- my.bin(mmdata$year, breaks = breakN)
  colx <- tapply(mmdata$MmPres,xid,length)
  cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  cols <- ifelse(colx <= 100,"yellow",3)
  cols[which(colx <= 20)] <- "orange"
  cols[which(colx <= 10)] <- "red"
  tab <- tapply(mmdata$MmPres,xid,mean)
  lim = which(colx > 100)
  plot(tab ~ as.numeric(names(tab)),ylab="Mean Presence",xlab="Year",cex.axis=axval,cex.lab=axval, pch=19, col = cols,ylim = range(0,max(cis[lim,])))
  lines.cis(cis,sort(unique(xid)))
  
  # 2 Day of Month
  xid <- my.bin(mmdata$jd, breaks = breakN)
  colx <- tapply(mmdata$MmPres,xid,length)
  cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  cols <- ifelse(colx <= 100,"yellow",3)
  cols[which(colx <= 20)] <- "orange"
  cols[which(colx <= 10)] <- "red"
  tab <- tapply(mmdata$MmPres,xid,mean)
  lim = which(colx > 100)
  plot(tab ~ as.numeric(names(tab)),ylab="",xlab="Day of Year",cex.axis=axval,cex.lab=axval, pch=19, col = cols,ylim = range(0,max(cis[lim,])))
  lines.cis(cis,sort(unique(xid)))
  
  # 3 timeofd
  # xid <- my.bin(mmdata$tod, breaks = breakN)
  # colx <- tapply(mmdata$MmPres,xid,length)
  # cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  # cols <- ifelse(colx <= 100,"yellow",3)
  # cols[which(colx <= 20)] <- "orange"
  # cols[which(colx <= 10)] <- "red"
  # tab <- tapply(mmdata$MmPres,xid,mean)
  # lim = which(colx > 100)
  # plot(tab ~ as.numeric(names(tab)),ylab="",xlab="Normalized Time of Day",cex.axis=axval,cex.lab=axval, pch=19, col = cols,ylim = range(0,max(cis[lim,])))
  # lines.cis(cis,sort(unique(xid)))
  
  # 4 Vessel presence
  cis<-matrix(unlist(tapply(mmdata$MmPres,mmdata$sPres,my.binomconfi)),ncol=3,byrow=TRUE)
  plot(tapply(mmdata$MmPres,mmdata$sPres,mean),xaxt="n",ylab="Mean Presence",xlab="Presence of Ships",cex.axis=axval,cex.lab=axval, pch= 19, col=3, ylim = range(0,max(cis)))
  axis(side=1,at = c(1,2),labels=c("0 = Absence","1 = Presence"),cex.axis=axval,cex.lab=axval)
  lines.cis(cis,unique.x = c(1,2))# sonar counts
  
  # 5 Range to Nearest Vessel
  #div <- ifelse(max(zcdata$sProp) > 1, 100, 1) # make sure to use the original (untransformed) version of sProp
  xid<-my.bin(mmdata$Ice_pc, breaks = breakN)
  #xid<-my.bin(zcdata$sProp, breaks = max(zcdata$sProp*100)*2)
  colx <- tapply(mmdata$Ice_pc,xid,length)
  cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  cols <- ifelse(colx <= 100,"yellow",3)
  cols[which(colx <= 20)] <- "orange"
  cols[which(colx <= 10)] <- "red"
  tab <- tapply(mmdata$MmPres,xid,mean)
  lim = which(colx > 100)
  plot(tab ~ as.numeric(names(tab)),pch=19,ylab="",xlab="Percent Ice Cover",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  lines.cis(cis,sort(unique(xid)))
  
  # 6 Max Sound Pressure Level
   xid <- my.bin(mmdata$maxSPL, breaks = breakN)
  #xid <- my.bin(zcdata$contsPres, breaks = max(zcdata$contsPres))
  colx <- tapply(mmdata$maxSPL,xid,length)
  cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  cols <- ifelse(colx <= 100,"yellow",3)
  cols[which(colx <= 20)] <- "orange"
  cols[which(colx <= 10)] <- "red"
  tab <- tapply(mmdata$MmPres,xid,mean)
  lim = which(colx > 100)
  plot(tab ~ as.numeric(names(tab)),pch=19,ylab="",xlab="Maximum Sound Pressure Level (dBrms [20-10,000 Hz])",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  lines.cis(cis,sort(unique(xid)))
  
  # 6 Max Sound Pressure Level - sPres == 1
  xid <- my.bin(mmdata$maxSPL[mmdata$sPres == 1], breaks = breakN)
  #xid <- my.bin(zcdata$contsPres, breaks = max(zcdata$contsPres))
  colx <- tapply(mmdata$maxSPL[mmdata$sPres == 1],xid,length)
  cis<-matrix(unlist(tapply(mmdata$MmPres[mmdata$sPres == 1],xid,my.binomconfi)),ncol=3,byrow=TRUE)
  cols <- ifelse(colx <= 100,"yellow",3)
  cols[which(colx <= 20)] <- "orange"
  cols[which(colx <= 10)] <- "red"
  tab <- tapply(mmdata$MmPres[mmdata$sPres == 1],xid,mean)
  lim = which(colx > 100)
  plot(tab ~ as.numeric(names(tab)),pch=19,ylab="",xlab="Maximum Sound Pressure Level (dBrms [20-10,000 Hz])",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  lines.cis(cis,sort(unique(xid)))
 
  # 6 Max Sound Pressure Level - sPres == 1
  xid <- my.bin(mmdata$maxSPL[mmdata$sPres == 0], breaks = breakN)
  #xid <- my.bin(zcdata$contsPres, breaks = max(zcdata$contsPres))
  colx <- tapply(mmdata$maxSPL[mmdata$sPres == 0],xid,length)
  cis<-matrix(unlist(tapply(mmdata$MmPres[mmdata$sPres == 0],xid,my.binomconfi)),ncol=3,byrow=TRUE)
  cols <- ifelse(colx <= 100,"yellow",3)
  cols[which(colx <= 20)] <- "orange"
  cols[which(colx <= 10)] <- "red"
  tab <- tapply(mmdata$MmPres[mmdata$sPres == 0],xid,mean)
  lim = which(colx > 100)
  plot(tab ~ as.numeric(names(tab)),pch=19,ylab="",xlab="Maximum Sound Pressure Level (dBrms [20-10,000 Hz])",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  lines.cis(cis,sort(unique(xid)))
  
   
  # # 7 sLag
  # xid<-my.bin(mmdata$sit, breaks = breakN)
  # #xid <- my.bin(zcdata$sLag, breaks = max(zcdata$sLag))
  # colx <- tapply(mmdata$sit,xid,length)
  # cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  # cols <- ifelse(colx <= 100,"yellow",3)
  # cols[which(colx <= 20)] <- "orange"
  # cols[which(colx <= 10)] <- "red"
  # tab <- tapply(mmdata$MmPres,xid,mean)
  # lim = which(colx > 100)
  # plot(tab ~ as.numeric(names(tab)),pch=19,ylab="Mean Presence",xlab="Thin Sea Ice Thickness (cm)",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  # lines.cis(cis,sort(unique(xid)))

  # 8 RNV
  mmdata$minRange[mmdata$sPres == 0] = 50
  xid <- my.bin(mmdata$minRange, breaks = c(seq(min(mmdata$minRange[mmdata$sPres == 1]), round(max(mmdata$minRange[mmdata$sPres == 1]))+1, length.out = breakN),50000))
  colx <- tapply(mmdata$minRange,xid,length)
  cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  cols <- ifelse(colx <= 100,"yellow",3)
  cols[which(colx <= 20)] <- "orange"
  cols[which(colx <= 10)] <- "red"
  tab <- tapply(mmdata$MmPres,xid,mean)
  lim = which(colx > 100)
  #plot(tab ~ as.numeric(names(tab)),pch=19,ylab="Mean Presence",xlab="Binned Sonar maxRLpp (dB)",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(cis[lim,]))
  valLim = range(cis[lim,])
  gapfrom = 43#500
  gapto = 47#97
  numtab = as.numeric(names(tab))
  gap.plot(round(as.numeric(names(tab))), tab,
           gap = c(gapfrom, gapto),
           gap.axis = "x",xtics=round(seq(round(numtab[2]),round(numtab[length(numtab)]),length.out = 4)),ytics = round(seq(valLim[1],valLim[2],length.out = 4),3),
           pch=19,ylab="",xlab="Range to Nearest Vessel (m)",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  
  rng = sort(round(unique(xid)))#-(gapto-gapfrom)
  rng[25] = 50000 #rng[25]+(gapto-gapfrom)-1
  lines.cis(cis,rng)
  
  # 8 RNV
  # xid <- my.bin(mmdata$minRange, breaks = c(0, seq(min(mmdata$minRange[mmdata$sPres == 1]), round(max(mmdata$minRange))+1, length.out = breakN)))
  # colx <- tapply(mmdata$minRange,xid,length)
  # cis<-matrix(unlist(tapply(mmdata$MmPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  # cols <- ifelse(colx <= 100,"yellow",3)
  # cols[which(colx <= 20)] <- "orange"
  # cols[which(colx <= 10)] <- "red"
  # tab <- tapply(mmdata$MmPres,xid,mean)
  # lim = which(colx > 100)
  # #plot(tab ~ as.numeric(names(tab)),pch=19,ylab="Mean Presence",xlab="Binned Sonar maxRLpp (dB)",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(cis[lim,]))
  # valLim = range(cis[lim,])
  # gapfrom = 3#500
  # gapto = 50#97
  # numtab = as.numeric(names(tab))
  # gap.plot(round(as.numeric(names(tab))), tab,
  #          gap = c(gapfrom, gapto),
  #          gap.axis = "x",xtics=round(seq(round(numtab[2]),round(numtab[length(numtab)]),length.out = 4)),ytics = round(seq(valLim[1],valLim[2],length.out = 4),3),
  #          pch=19,ylab="",xlab="Range to Nearest Vessel (km)",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  # 
  # rng = sort(round(unique(xid)))-(gapto-gapfrom)
  # rng[1] = 0
  # lines.cis(cis,rng)
   
  # # 9 cumSEL
  # xid <- my.bin(zcdata$cumSEL, breaks = c(0, seq(120, round(max(zcdata$cumSEL))+1, length.out = breakN)))
  # colx <- tapply(zcdata$cumSEL,xid,length)
  # cis<-matrix(unlist(tapply(zcdata$zcPres,xid,my.binomconfi)),ncol=3,byrow=TRUE)
  # cols <- ifelse(colx <= 100,"yellow",3)
  # cols[which(colx <= 20)] <- "orange"
  # cols[which(colx <= 10)] <- "red"
  # tab <- tapply(zcdata$zcPres,xid,mean)
  # lim = which(colx > 100)
  # #plot(tab ~ as.numeric(names(tab)),pch=19,ylab="Mean Presence",xlab="Binned Sonar maxRLpp (dB)",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(cis[lim,]))
  # valLim = range(cis[lim,])
  # gapfrom = 3
  # gapto = 117
  # numtab = as.numeric(names(tab))
  # gap.plot(round(as.numeric(names(tab))), tab,
  #          gap = c(gapfrom, gapto),
  #          gap.axis = "x",xtics= round(seq(round(numtab[2]),round(numtab[length(numtab)]),length.out = 4)),ytics = round(seq(valLim[1],valLim[2],length.out = 4),3),
  #          pch=19,ylab="",xlab="Sonar cumulative SEL\n(dBpp re 1uPa2s) per minute",cex.axis=axval,cex.lab=axval, col= cols,ylim = range(0,max(cis[lim,])))
  # 
  # rng = sort(round(unique(xid)))-(gapto-gapfrom)
  # rng[1] = 0
  # lines.cis(cis,rng)

}
