# Shewhart-CUSUM Control Chart Method to Detect Future Changes in Water Quality
# 

# R-Code Author: James R. Henderson, PE (Formation Environmental, LLC)
# Based on algorithms from USEPA. 2006c. "Data Quality Assessment: Statistical Methods for Practitioners."
# EPA/240/B-06/003 EPA QA/G-9S. Washington D.C.: United States Environmental Protection Agency.
# http://www2.epa.gov/quality/guidance-data-quality-assessment.

#  

# R is an open source statistics and graphics package available at https://www.r-project.org/.
# https://cran.r-project.org/web/packages/corrr/corrr.pdf

####################################################################################################
rm(list=ls())
# Set working Directory
setwd("C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Simplot-Smoky/R-Code")

suppressPackageStartupMessages(require(openxlsx))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library("ggpubr"))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(metan))
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(qcc))

Data <-read.xlsx("MnTotalMW7.xlsx")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)


##### Detrend Data #######
plot.ts(Data$Mn)
trend=lm(Data$Mn~Data$Date)
detrend=residuals(trend)
plot.ts(detrend)
detrend
Data$Mn-detrend
plot(trend)

summary(trend)
summary(detrend)