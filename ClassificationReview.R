rm(list = ls()) 
graphics.off()
library(ISLR)
library(class)


# This data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, 
#from the beginning of 2001 until the end of 2005. For each date, we have recorded the
# percentage returns for each of the five previous trading days, Lag1 through Lag5. 
# We have also recorded Volume (the number of shares traded on the previous day, in billions),
# Today (the percentage return on the date in question) and Direction (whether the market was 
# Up or Down on this date).

# Loading the Smarket data available in the package ISLR                                                                                                                                                                                                                                                                                                 
MyData = Smarket

# We use data collected before 2005 as training and data during 2005 as the test
train.ind = which(MyData[,1]<2005)
test.ind = which(MyData[,1]==2005)

MyData.train = MyData[train.ind,]
MyData.test = MyData[test.ind,]

# modeling with respect to all features except Today and Year
mod.LR=glm(Direction~. - Today-Year,data=MyData.train,family=binomial)
print(summary(mod.LR))
coef(mod.LR)
print(summary(mod.LR))
# See the classification session sample codes for the definition of Deviance, Fisher Scoring and  Information Criteria

# evaluating the predicted probabilities 
pred.probs=predict(mod.LR,MyData.test, type="response")

yhat=rep("Down",252)
yhat[pred.probs>.5]="Up"
table(yhat,MyData.test[,'Direction'])
# the prediction accuracy 
print(mean(yhat==MyData.test[,'Direction']))
# confidence interval for the coefficients
print(confint(mod.LR))


# comparison with the KNN
set.seed (1)
feature.inds = c('Lag1','Lag2','Lag3','Lag4','Lag5','Volume')
knn.pred=knn(MyData.train[,feature.inds],MyData.test[,feature.inds],MyData.train[,'Direction'] ,k=1)
print(mean(knn.pred==MyData.test[,'Direction']))

# SEE THE CLASSIFICATION LECTURE SAMPLE PROGRAMS FOR INSTANCES OF LDA AND QDA

