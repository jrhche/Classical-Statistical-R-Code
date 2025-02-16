plotClassifier <- function(model, train. = train, test. = test, pred. = pred) {
  # This function is specifically written for the Spiral data and illustrates the classifier. Does not 
  # generalize beyond classification of 2D data
  train. <- as.data.frame(train.) 
  pred. <- as.data.frame(pred.)
  n=0.5*(sqrt(nrow(test.))-1) 
  d <- 1.5
  h <- d*(-n:n)/n
  plot(train.[,-3], pch = 19,col = train.[,3],cex = 0.5, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), 
       main = "classifier")
  contour(h, h, z = array(ifelse(pred.[,1] == "Red",0,1), dim=c(2*n+1,2*n+1)), col="blue",
          lwd = 2, add = TRUE)
}