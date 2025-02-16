rm(list = ls()) 
graphics.off()


library(ISLR)

# Step 1: Reading the data

MyData <- read.csv(file="Auto.csv", header=TRUE,  sep=",")

# Step 2: Let's remove some features that are not useful, here the car name
MyData <- MyData[,-9]

# Step 3: Split the data into train and test
# We can do the splitting randomly or deterministically (say indices 1:350 are training data and 351:397 are test data) 
splitting = 'random'

if (splitting == 'random'){
  rand.ind = sample(seq(1:392),392)
  MyData.train = MyData[rand.ind[1:350],]
  MyData.test = MyData[rand.ind[351:392],]
}else{
  MyData.train = MyData[1:350,]
  MyData.test = MyData[351:392,]
}

# Step 4: Fit a model, regressing mpg against all the features


mod1 <- lm(mpg~.  ,data=MyData.train)
mod.summary = summary(mod1)

print(mod.summary)
print(cor(MyData))
# if you explicity want to access the p-values
pvals = mod.summary$coefficients[,'Pr(>|t|)']
# if you explicity want to access the t-statistics
tstats = mod.summary$coefficients[,'t value']

# we observe large p-values for cylinders, horsepower and acceleration
# Step 5: lets fit a model that excludes acceleration
mod2 <- lm(mpg~. - acceleration ,data=MyData.train)
mod.summary2 = summary(mod2)

print(mod.summary2)
# for this new fit we see that all p-values are small

# Model 2 treats all variables as nurmerics, what if we want to consider a variable as categorical? For instance in this case "origin"
MyData.train.cat <- MyData.train
MyData.test.cat <- MyData.test
MyData.train.cat$origin <- factor(MyData.train.cat$origin)
MyData.test.cat$origin <- factor(MyData.test.cat$origin)

# Lets fit a third model with origin treated as categorical data
mod3 <- lm(mpg~. - acceleration ,data=MyData.train.cat)
mod.summary3 = summary(mod3)

print(mod.summary3)

# Step 6: Lets see which model presents a lower error with respect to the test data
y1 = predict(mod1,MyData.test)
y2 = predict(mod2,MyData.test)
y3 = predict(mod3,MyData.test.cat)

# comparing the Euclidean distances
E1 = norm(as.matrix(y1 - MyData.test[,'mpg']))
E2 = norm(as.matrix(y2 - MyData.test[,'mpg']))
E3 = norm(as.matrix(y3 - MyData.test.cat[,'mpg']))
print(c(E1,E2,E3))

# Step 7: looking into the confidence level for some variables, say for year
print(confint(mod1, parm = "year"))
print(confint(mod2, parm = "year", level = 0.99))
print(confint(mod3, parm = "year"))

# if you want to manually check the confidence interval for, say year in mod1:
df = 350 - 8 # this is n-p, the degrees of freedom for the t-statistic
estVal = mod.summary$coefficients['year',1]
st.Err = mod.summary$coefficients['year',2]
MyCt = c(estVal+qt(.025,df)*st.Err, estVal-qt(0.025,df)*st.Err)
print('Our own CI calculation:')
print(MyCt)