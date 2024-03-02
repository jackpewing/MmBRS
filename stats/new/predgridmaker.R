predgridmaker <- function (model1, thingstoshow,namethingstoshow ,otherstuff=otherstuff, otherstuffvalue=otherstuffvalue,axisLabel=axisLabel, maxy=maxyVal, maxviolin=maxViolinVal){
  #########Produces an effect plot from a variable, typically plotted on the response scale with all variables at given levels. 
  ##Inputs
  #model1 Model object
  #thingstoshow: the relevant input data (to get a min and max)
  #namethingstoshow: the name of the variable (for the xaxis label)
  #otherstuff: the other variables in the model (I could probably automate this)
  ##otherstuffvalue  (the values of the other variables that will be set for the plot (typically mean values))
  ##maxy. Can be set to NA in which case the maxium of the upper 97.5% bound is used. Sets the limit of the y axis. 
  
  bootN = 1000
  interaction <- F    ####only works if there is not an interaction
  for (i in 1:length (otherstuff)){
    eval (parse (text= paste (otherstuff[i], "=", otherstuffvalue[i]))   )
  }
  if (is.factor (thingstoshow)==F){
    if (interaction==T){
      ###
    }else{
      thingstoshow2 <- seq(min (thingstoshow), max(thingstoshow), length=bootN)}}else{
        thingstoshow2 <-sort (unique (thingstoshow))
      }
  phrase1 <- c("expand.grid(thingstoshow2")
  for (j in 1: length (otherstuff)){
    phrase1 <- paste (phrase1,otherstuff[j], sep=",")
    if (j==length (otherstuff)){
      phrase1 <- paste (phrase1,")")
    }
  }
  names2 <- c("thingstoshow2", otherstuff)
  gridpred <- eval(parse (text=phrase1)) ######creates a grid to predict over
  names (gridpred)[1] <- namethingstoshow
  names (gridpred)[2:dim (gridpred)[2]]<- otherstuff
  gridpred$Pred <- predict (model1, gridpred, type="response")
  bspreds <- matrix (NA, bootN, dim (gridpred)[1])
  bootstrapmodel <- model1
  set.seed (101)
  #############Bootstrapping to obtain confidence interval 
  rcoefs <- try(MASS::mvrnorm(bootN, coef(model1), summary(model1)$cov.scaled), silent = T)
  if (is.null(rcoefs) || length(which(is.na(rcoefs) ==  T)) > 0) {
    rcoefs <- MASS::mvrnorm(bootN, coef(model1), as.matrix(nearPD(summary(model1)$cov.scaled)$mat))
  }   #####bootstrap replicate of model coefficient. 
  for (i in 1:bootN){ 
    bootstrapmodel$coefficients <- rcoefs[i,]    
    bspreds[i,] <- predict (bootstrapmodel, gridpred, type="response")
  }
  lower <- apply (bspreds, 2, quantile, 0.025) 
  upper <- apply (bspreds, 2, quantile, 0.975) 
  lineS = 1.5
  linesT = 2
  cexS = 1.6
  if (is.na(maxy)==T){maxy=max(upper, na.rm=T)}
  if (is.factor (thingstoshow)==F){
    #    if (thingstoshow=="newjd"){tempjd <- ifelse (gridpred[,1]<0, gridpred[,1]+365,gridpred[,1] )
    #       plot (tempjd[1:489], gridpred$Pred[1:489], type="l" ,ylab="Probability" , ylim=c(min (lower), max(upper)), xlim=c(0,366), xlab="Dayofyear")
    #       #plot (tempjd[1:489], gridpred$Pred[1:489], type="l" ,ylab="Probability" , ylim=c(min (lower), maxy), xlim=c(0,366), xlab="Dayofyear")
    #      lines (tempjd[490:1000], gridpred$Pred[490:1000])
    #       lines(tempjd[1:489] , lower[1:489], col = "red", lty = 2)
    #       lines(tempjd[490:1000] , lower[490:1000], col = "red", lty = 2)
    #      lines(tempjd[490:1000] , upper[490:1000], col = "red", lty = 2)
    #       lines(tempjd[1:489] , upper[1:489], col = "red", lty = 2)
    # }else {
    #plot (gridpred[,1], gridpred$Pred ,type="l",lwd = lineS,ylab="Probability" ,xlab = axisLabel, ylim=c(min (lower), max(upper)), cex.axis = cexS, cex.lab = cexS )
    plot (gridpred[,1], gridpred$Pred ,type="l",lwd = lineS,ylab="Probability" ,xlab = axisLabel, ylim=c(0, max(upper)), cex.axis = cexS, cex.lab = cexS )
    lines(gridpred[,1] , lower, col = "orange", lty = linesT,lwd = lineS)
    lines(gridpred[,1] , upper, col = "orange", lty = linesT,lwd = lineS) }
  # }
  
  
  else{
    #plot (gridpred[,1], gridpred$Pred, pch=20 ,ylab="Probability" ,xlab = axisLabel, ylim=c(min (lower),max(upper)), cex.axis =cexS, cex.lab = cexS )
    plot (gridpred[,1], gridpred$Pred, pch=20 ,ylab="Probability" ,xlab = axisLabel, ylim=c(0,max(upper)), cex.axis =cexS, cex.lab = cexS )
    arrows(seq(1, length (gridpred[,1])), gridpred$Pred  , seq(1, length (gridpred[,1])),   lower, angle=90, col = "orange", lty = linesT)
    arrows(seq(1, length (gridpred[,1])) ,gridpred$Pred,  seq(1, length (gridpred[,1])),  upper, angle=90, col = "orange", lty = linesT) 
  }
  
  ##adds violin plot
  if (is.factor(thingstoshow)==F){
    # if (thingstoshow=="newjd"){thingstoshow<- "jd"}###plots on original scale. 
    # #violinrug<-myviolin(xvals = thingstoshow,miny = min(lower,na.rm=T),maxy = max(upper,na.rm=T),rel=5)
    violinrug<-myviolin(xvals = thingstoshow,miny = 0,maxy = max(upper,na.rm=T),rel=5)
    polygon(violinrug,col=rgb(0,1,1,alpha=.3)) }
    else{
     violinrug<-myviolin(xvals = as.numeric(thingstoshow),miny = 0,maxy = max(upper,na.rm=T),rel=5)
    polygon(violinrug,col=rgb(0,1,1,alpha=.3)) 
    }
  return (gridpred )             
}
