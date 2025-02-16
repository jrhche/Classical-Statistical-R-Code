rm(list = ls()) 
graphics.off()


options(warn=-1)
library(ISLR)


# This data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, 
#from the beginning of 2001 until the end of 2005. For each date, we have recorded the
# percentage returns for each of the five previous trading days, Lag1 through Lag5. 
# We have also recorded Volume (the number of shares traded on the previous day, in billions),
# Today (the percentage return on the date in question) and Direction (whether the market was 
# Up or Down on this date).

#attach(Smarket)


library(MASS)
# training based on the available data before 2005
train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# Interestingly, the QDA predictions are accurate almost 60% of the time, even though the 2005
# data was not used to fit the model. This level of accu- racy is quite impressive for stock market
# data, which is known to be quite hard to model accurately. This suggests that the quadratic form 
# assumed by QDA may capture the true relationship more accurately than the linear forms assumed by 
# LDA and logistic regression. However, we recommend evaluating this method’s performance on a larger 
# test set before betting that this approach will consistently beat the market!

