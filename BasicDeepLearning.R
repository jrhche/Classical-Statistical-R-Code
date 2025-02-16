# clearing the memory and closing active figures
rm(list = ls())
graphics.off()

require(h2o)
source("plotClassifier.R")


# The path to the train and test data
trainDataPath <- "Spiral Data/spiral.csv"
testDataPath <- "Spiral Data/grid.csv"

# The columns corresponding to the features and response in the train and test data files
xcols = c(1,2)
ycols = 3

# epochs is the number of epochs in the deep learning model. See H2O manual for more details
Nepochs <- 10000

# hidden is a vector that indicates the number of layers and number of nodes within each layer.
# For instance c(200,200) indicates two hidden layers each with 200 neurons
Nhidden <- c(75,75)

net.activation = "Rectifier"
  
h2o.init(nthreads=-1, max_mem_size="8G")
# clean slate - just in case the cluster was already running
h2o.removeAll() 

train <- h2o.importFile(path = normalizePath(trainDataPath))
test  <- h2o.importFile(path = normalizePath(testDataPath))


# ___________________________________________________________________________________________________
# Model training
# See https://www.rdocumentation.org/packages/h2o/versions/3.16.0.2/topics/h2o.deeplearning for a 
# complete list of parameters and options you can pass to the function
# You may also find the following reference useful:
# http://docs.h2o.ai/h2o-tutorials/latest-stable/tutorials/deeplearning/index.html
model <- h2o.deeplearning(xcols, ycols, train, hidden = Nhidden,
                          epochs = Nepochs, activation = "Rectifier",
                          export_weights_and_biases=TRUE)
# ___________________________________________________________________________________________________


# Making the predictions
pred <- h2o.predict(model, test)

# ___________________________________________________________________________________________________
# This part is a custom function I wrote for the Spiral data and illustrates the classifier. Does not 
# generalize beyond classification of 2D data 
plotClassifier(model = model, train. = train, test. = test, pred. = pred)
# ___________________________________________________________________________________________________