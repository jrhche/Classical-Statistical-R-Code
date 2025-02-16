#  MSA 8350 Legal Analytics
#  Claimant Outcome Forecasting Model - Team Project
#  Jamie Henderson (jhenderson49)

rm(list = ls()) 
graphics.off()

library(ISLR)
library(xts)
library(MASS)
library(caret)
library(class)
library(e1071)

# Set working Directory
setwd("D:/Projects/Folders/Projects-Backup/GSU/Legal Analytics/Project/")

# Read in training and test data files
Train = read.csv('LogRegrTraining.csv', header=TRUE)
Test = read.csv('LogRegrTest.csv', header=TRUE)

# This data set consists of FINRA forum and Award type documents a variety of features including
# Claim_Outcome (dependent variable), Pro Se Status, Claimant Type, Document Word Count, Claimant Gender/Type, Claimant Representative
# gender/type, month of filing, year of filing, number of arbitrators, and geographic region of filing (as independent variables).
# The intent is to forecase the Claimant Outcome (Win/Loss - 1/0) based on the above described features, all but word count are binary
# values (given as dummy type variables).
                                                                                                                                                                                                                                                                                                 
# Logistic Regression
glm.fits=glm(Claim_Outcome~factor(Pro_Se_Status)+wrd_cnt+factor(claimant_type)+factor(CL_Fem)+factor(CL_Mal)+factor(CL_1)+factor(CL_Biz)+ factor(CL_Trst)+ factor(CLR_Fem)+ factor(CLR_Mal)+ factor(CLR_mix)+ factor(M1)+ factor(M2)+ factor(M3)+ factor(M4)+ factor(M5)+ factor(M6)+ factor(M7)+ factor(M8)+ factor(M9)+ factor(M10)+ factor(M11)+ factor(M12)+ factor(Y2000)+ factor(Y2004)+ factor(Y2005)+ factor(Y2006)+ factor(Y2007)+ factor(Y2008)+ factor(Y2009)+ factor(Y2010)+ factor(Y2011)+ factor(Y2012)+ factor(Y2013)+ factor(Y2014)+ factor(Y2015)+ factor(Y2016)+ factor(Y2017)+ factor(Y2018)+ factor(ARB1)+ factor(ARB2)+ factor(ARB3)+ factor(ARB4)+ factor(ARB6)+ factor(Carribean)+ factor(Midwest)+ factor(Mountainwest)+ factor(Northeast)+ factor(Pacific)+ factor(Pacificwest)+ factor(Southeast), data=Train, family=binomial(link='logit'))
print(summary(glm.fits))
coef(glm.fits)
summary(glm.fits)$coef

# Note that all p-values are quite large from most features thus a weak connection between the features and response.
# However, significant (low p-values) are evident for Pro Se Status, Word Count (wrd_cnt), Claimant Type, Claimant Reprsentative
# gender, and geographic region (i.e. midwest, southeast and carribean).


glm.probs=predict(glm.fits,Test,type="response")
Cl_Out.test <- (Test[,'Claim_Outcome'])

glm.probs[1:10]
glm.pred=rep(0,1472)
glm.pred[glm.probs>.5]=1
table(glm.pred,Cl_Out.test)
print(mean(glm.pred==Cl_Out.test))
#(1321+1)/1472 = 89.8% accuracy

# Linear Discriminant Analysis (LDA)
lda.fit=lda(Claim_Outcome~factor(Pro_Se_Status)+wrd_cnt+factor(claimant_type)+factor(CL_Fem)+factor(CL_Mal)+factor(CL_1)+factor(CL_Biz)+ factor(CL_Trst)+ factor(CLR_Fem)+ factor(CLR_Mal)+ factor(CLR_mix)+ factor(M1)+ factor(M2)+ factor(M3)+ factor(M4)+ factor(M5)+ factor(M6)+ factor(M7)+ factor(M8)+ factor(M9)+ factor(M10)+ factor(M11)+ factor(M12)+ factor(Y2000)+ factor(Y2004)+ factor(Y2005)+ factor(Y2006)+ factor(Y2007)+ factor(Y2008)+ factor(Y2009)+ factor(Y2010)+ factor(Y2011)+ factor(Y2012)+ factor(Y2013)+ factor(Y2014)+ factor(Y2015)+ factor(Y2016)+ factor(Y2017)+ factor(Y2018)+ factor(ARB1)+ factor(ARB2)+ factor(ARB3)+ factor(ARB4)+ factor(ARB6)+ factor(Carribean)+ factor(Midwest)+ factor(Mountainwest)+ factor(Northeast)+ factor(Pacific)+ factor(Pacificwest)+ factor(Southeast), data=Train)
lda.pred=predict(lda.fit, Test)
table(lda.pred$class,Cl_Out.test)
print(mean(lda.pred$class==Cl_Out.test))
#(1310+5)/1472 = 89.3% accuracy

#k-Nearest Neighbour Classification (KNN)
#k=1
set.seed (1)
feature.inds = c('Pro_Se_Status','wrd_cnt','claimant_type','CL_Fem','CL_Mal','CL_1','CL_Biz','CL_Trst','CLR_Fem','CLR_Mal','CLR_mix','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12','Y2000','Y2004','Y2005','Y2006','Y2007','Y2008','Y2009','Y2010','Y2011','Y2012','Y2013','Y2014','Y2015','Y2016','Y2017','Y2018','ARB1','ARB2','ARB3','ARB4','ARB6','Carribean','Midwest','Mountainwest','Northeast','Pacific','Pacificwest','Southeast')
knn.pred=knn(Train[,feature.inds],Test[,feature.inds],Train[,'Claim_Outcome'] ,k=1)
KNN1_Acc <-print(mean(knn.pred==Test[,'Claim_Outcome']))
# 84.3% accuracy

#k=5
set.seed (1)
feature.inds = c('Pro_Se_Status','wrd_cnt','claimant_type','CL_Fem','CL_Mal','CL_1','CL_Biz','CL_Trst','CLR_Fem','CLR_Mal','CLR_mix','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12','Y2000','Y2004','Y2005','Y2006','Y2007','Y2008','Y2009','Y2010','Y2011','Y2012','Y2013','Y2014','Y2015','Y2016','Y2017','Y2018','ARB1','ARB2','ARB3','ARB4','ARB6','Carribean','Midwest','Mountainwest','Northeast','Pacific','Pacificwest','Southeast')
knn.pred=knn(Train[,feature.inds],Test[,feature.inds],Train[,'Claim_Outcome'] ,k=5)
KNN1_Acc <-print(mean(knn.pred==Test[,'Claim_Outcome']))
# 89.1% accuracy

#k=10
set.seed (1)
feature.inds = c('Pro_Se_Status','wrd_cnt','claimant_type','CL_Fem','CL_Mal','CL_1','CL_Biz','CL_Trst','CLR_Fem','CLR_Mal','CLR_mix','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12','Y2000','Y2004','Y2005','Y2006','Y2007','Y2008','Y2009','Y2010','Y2011','Y2012','Y2013','Y2014','Y2015','Y2016','Y2017','Y2018','ARB1','ARB2','ARB3','ARB4','ARB6','Carribean','Midwest','Mountainwest','Northeast','Pacific','Pacificwest','Southeast')
knn.pred=knn(Train[,feature.inds],Test[,feature.inds],Train[,'Claim_Outcome'] ,k=10)
KNN1_Acc <-print(mean(knn.pred==Test[,'Claim_Outcome']))
# 89.7% accuracy


