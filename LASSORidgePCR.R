# A brief overview of Ridge, LASSO and Principal Component Regression

rm(list = ls()) 
graphics.off()

library(ISLR)
library(glmnet)
library(pls)

# eliminating the NA entries 
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# a grid for the possible values of lambda
grid=10^seq(10,-2,length=100)

# alpha = 0 --> Ridge, alpha = 1 --> LASSO
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

# comparing the magnitude of the coefficients for two different lambdas
print(ridge.mod$lambda[50])
print(coef(ridge.mod)[,50])
print(sqrt(sum(coef(ridge.mod)[-1,50]^2)))


print(ridge.mod$lambda[60])
print(coef(ridge.mod)[,60])
print(sqrt(sum(coef(ridge.mod)[-1,60]^2)))


# Model selection:
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)

bestlam=cv.out$lambda.min
print(bestlam)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
print(mean((ridge.pred-y.test)^2)) # mse = 96015.51

# what if we did not do the Ridge? (lambda=0)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
print(mean((ridge.pred-y.test)^2)) #mse = 114723.6

##################################LASSO REGRESSION########################
##################################LASSO REGRESSION########################

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
print(mean((lasso.pred-y.test)^2))
# what would have happened if no regularization?
lasso.pred.0=predict(lasso.mod,s=0,newx=x[test,])
print(mean((lasso.pred.0-y.test)^2))

# we want to see the sparsity of the solutions 
lasso.mod.full=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(lasso.mod.full,type="coefficients",s=bestlam)[1:20,]
print(lasso.coef)

##################################PCR########################
##################################PCR########################

set.seed(2)
# option scale does the standardizing
# when we set validation="CV" then pcr performs 10-fold CV for each number of possible components
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
print(summary(pcr.fit))
# plotting the CV curves
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
# focusing only on the training data above
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
# since the lowest CV happens at M=7:
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
print(mean((pcr.pred-y.test)^2))# pcrMSE:96556.22





