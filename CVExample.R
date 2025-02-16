rm(list = ls()) 
graphics.off()

library(ISLR)
library(boot)
# LOOCV for mpg auto problem
glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)

library(boot)
# the default is LOOCV
cv.err=cv.glm(Auto,glm.fit)
print(cv.err$delta)

# evaluating the LOOCV for 5 different polynomial models
cv.error=rep(0,5)
for (i in 1:5){
 glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
 cv.error[i]=cv.glm(Auto,glm.fit)$delta[1] 
 }
print(cv.error) # This is the value of CV_n
# we see that the second order polynomial presents the desired knee point

# Now doing a 10 fold CV:
set.seed(17)
cv.error.10=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
print(cv.error.10)# This is the value of CV_k 









