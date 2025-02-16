rm(list=ls())
## Set working Directory
setwd("C:/Users/JHENDERSON/OneDrive - Ramboll/Projects/XOM_PH/R Code")

require(openxlsx)
require(pwr)
Action.Level<-read.xlsx("SILinputCOCs.xlsx",sheet = 1, rows = 1, colNames = F,cols=2:7)
for(i in 1:6){
  X<-read.xlsx("SILinputCOCs.xlsx",sheet = 1, startRow = 5, colNames = TRUE,cols=i+1)
  X.name<-colnames(X)
  
  print(X.name)
  X<-X[,1]
  print(t.test(X,mu=Action.Level[1,i],alternative ="less",conf.level = 0.95))
  
  
  effect.size=seq(-2,0,length=200)
  par(new=F)
  x<-effect.size*sd(X)+Action.Level[1,i]
  xrange <- range(x) 
  yrange <- c(0,1)
  
  # set up the plot 
  n.size<-c(10,20,30,40,50,60,70)
  nn<-length(n.size)
  plot(xrange, yrange, type="n", xlab=X.name,ylab=expression("Type II Error ("*beta*")"))
  ylabel <- seq(0, 1, by = 0.1)
  axis(2, at = ylabel)
  colors <- rainbow(nn) 
  # add lines
  for (j in 1:nn){
    beta<-1-pwr.t.test(n=n.size[j], d = effect.size, sig.level = 0.05,type = "one.sample", alternative = "less")$power
    lines(x, beta, type="l", lwd=1.5, col=colors[j]) 
  }
  # add a title and subtitle 
  title(paste("Power Curves for",X.name))
  
  # add a legend 
  legend(xrange[1], yrange[2], n.size, cex=0.8, lty=1, col=colors, title="n")
  grid()
}


