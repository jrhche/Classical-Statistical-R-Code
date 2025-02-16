

# The following code is a simple example on how to use the bootstrap. To use the bootstrap we do not need
# to know what is going on in estimating a parameter and all we need to do is boosting. So technically you
# can write a function that generates your parameter of interest from the input data and use the boot to
# find the standard error

# Let the parameter of interest be the coefficient of horsepower^2 in our Auto glm problem 
rm(list = ls()) 
graphics.off()

library(ISLR)
library(boot)

#####################################################################
boot.fn=function(data,index)
return(coef(lm(mpg~poly(horsepower ,3) ,data=data,subset=index))[3])
#####################################################################

# Lets see what the standard error for that coefficient is using the model data
lm.fit = lm(mpg~poly(horsepower ,3) ,data=Auto)
print(summary(lm.fit))
# the coef value is 44.090 and the standard error is 4.375


# Now lets see if we can obtain similar results using the bootsrap 
set.seed(1)
boot.stats = boot(Auto ,boot.fn ,1000)
print(boot.stats)
# estimated coef = 44.08953
# Standard error = 4.281842

# NOTE THAT A POWERFUL FEATURE OF BOOTSTRAP IS INSIDE THE FUNCTION BOOT.FN ANY COMPLICATED CALCULATION CAN BE PERFORMED
# AND YET THE BOOTSTRAP IS ABLE TO GENERATE THE STANDARD ERROR WITH REASONABLE ACURACY FOR US!