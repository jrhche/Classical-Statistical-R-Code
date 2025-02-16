rm(list = ls()) 
graphics.off()


library(ISLR)


# This data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, 
#from the beginning of 2001 until the end of 2005. For each date, we have recorded the
# percentage returns for each of the five previous trading days, Lag1 through Lag5. 
# We have also recorded Volume (the number of shares traded on the previous day, in billions),
# Today (the percentage return on the date in question) and Direction (whether the market was 
# Up or Down on this date).
                                                                                                                                                                                                                                                                                                 
attach(Smarket)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
print(summary(glm.fits))
coef(glm.fits)
summary(glm.fits)$coef
# note that all p-values are quite large showing mild connection between the features and response
# If you want a summary of the outputs here is some note from:

# Deviance
# We see the word Deviance twice over in the model output. Deviance is a measure of goodness of fit 
#of a generalized linear model. Or rather, it’s a measure of badness of fit–higher numbers indicate worse fit.
# R reports two forms of deviance – the null deviance and the residual deviance. The null deviance shows how well
#the response variable is predicted by a model that includes only the intercept (grand mean).
# For our example, we have a value of 43.9 on 31 degrees of freedom. Including the independent variables (weight 
#and displacement) decreased the deviance to 21.4 points on 29 degrees of freedom, a significant reduction in deviance.
# The Residual Deviance has reduced by 22.46 with a loss of two degrees of freedom.
# 
# Fisher Scoring
# What about the Fisher scoring algorithm? Fisher’s scoring algorithm is a derivative of Newton’s method for 
# solving maximum likelihood problems numerically.
# For model1 we see that Fisher’s Scoring Algorithm needed six iterations to perform the fit.
# This doesn’t really tell you a lot that you need to know, other than the fact that the model 
# did indeed converge, and had no trouble doing it.
# 
# Information Criteria
# The Akaike Information Criterion (AIC) provides a method for assessing the quality of your model through 
# comparison of related models.  It’s based on the Deviance, but penalizes you for making the model more 
# complicated.  Much like adjusted R-squared, it’s intent is to prevent you from including irrelevant predictors.
# However, unlike adjusted R-squared, the number itself is not meaningful. If you have more than one similar 
# candidate models (where all of the variables of the simpler model occur in the more complex models), then you 
# should select the model that has the smallest AIC.
# So it’s useful for comparing models, but isn’t interpretable on its own.

glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
# on the training data we get (507+145)/1250 = 52.2% accuracy (on an independent test, this is less)
print(mean(glm.pred==Direction))

