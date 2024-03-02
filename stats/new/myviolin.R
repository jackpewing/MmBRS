myviolin<-function(xvals,miny,maxy,rel=10){
  ###produces a violin plot of the the observed frequency of a particular variable
  ##Inputs: relevant variable and the min and max of the graph. 
  #densx<-density(xvals)
  densx<-density(xvals, from = min(xvals), to = max(xvals))
  x <- c(densx$x[1], densx$x,densx$x[length(densx$x)])
  y <- c(0, densx$y,0)
  y1<- y/max(y)*((maxy-miny)/rel)
  
  newy<-miny+y1
  data.frame(x=x,y=newy)
}