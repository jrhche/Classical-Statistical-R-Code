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

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit

lda.pred=predict(lda.fit, Smarket.2005)
# The predict() function returns a list with three elements. The first ele- ment, class, contains LDA’s 
# predictions about the movement of the market. The second element, posterior, is a matrix whose kth column 
# contains the posterior probability that the corresponding observation belongs to the kth class
# linear discriminants, obtained by computing −0.642 × Lag1 − 0.514 × Lag2 for each of the training observations
# In other words, these are the multipliers of the elements of X = x in (4.19)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
# Applying a 50 % threshold to the posterior probabilities allows us to recre- ate the predictions contained 
# in lda.pred$class.

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
# Notice that the posterior probability output by the model corresponds to the probability that the 
# market will decrease

lda.pred$posterior[1:20,1]
lda.class[1:20]
# No days in 2005 meet that threshold! In fact, the greatest posterior prob- ability of 
# decrease in all of 2005 was 52.02 %.
sum(lda.pred$posterior[,1]>.9)