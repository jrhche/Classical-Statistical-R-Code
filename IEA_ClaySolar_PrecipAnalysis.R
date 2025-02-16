# R-Code Author: James R. Henderson, PE (Formation Environmental)
# Based on algorithms from USEPA. 2006c. "Data Quality Assessment: Statistical Methods for Practitioners."
# EPA/240/B-06/003 EPA QA/G-9S. Washington D.C.: United States Environmental Protection Agency.
# http://www2.epa.gov/quality/guidance-data-quality-assessment.

# R is an open source statistics and graphics package available at https://www.r-project.org/.

####################################################################################################

rm(list=ls())
## Set working Directory
setwd("C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/IEA/R-Code")
options(warn=-1)

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rstatix))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(coin))
suppressPackageStartupMessages(library(exactRankTests))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(require(openxlsx))
suppressPackageStartupMessages(require(pwr))
suppressPackageStartupMessages(require(rcompanion))
suppressPackageStartupMessages(require(effsize))
suppressPackageStartupMessages(require(dplyr))


############################# READ RAW INPUT DATA ##########################################################################
Data<-read.xlsx("PrecipTempData3679912.xlsx",detectDates = TRUE)
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)


################## All Data Jan 1, 2015 thru April 30, 2024 ################################################################

ggplot(data = Data) +
  geom_col(mapping = aes(x = DATE, y = PRCP)) +
  labs(x = "Date", 
       y = "Precipitation (in)") +
    theme_bw()

################## All Data Nov. 7, 2023 thru Dec. 7, 2023 ################################################################
Data<-read.xlsx("Dec23PrecipTempData3679912.xlsx",detectDates = TRUE)
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

SumPRCP <- sum(Data$PRCP)

datebreaks <- seq(as.Date("2023-11-07"), as.Date("2023-12-07"), by = "2 days")

time_series <- ggplot(data = Data) +
               geom_col(mapping = aes(x = DATE, y = PRCP)) +
                labs(x = "Date", 
                 y = "Precipitation (in)") +
                 theme_bw()

time_series +
  scale_x_date(breaks = datebreaks) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(aes(as.Date("2023-11-13"), 2.1, label=paste("30-day rainfall total: ", SumPRCP, "(in)"))) 

ggsave("time_seriesDec2023.png", width = 15, height = 8, units =c("cm"), dpi = 300)

################## All Data Dec. 6, 2021 thru Jan. 6, 2022################################################################
Data<-read.xlsx("Jan22PrecipTempData3679912.xlsx",detectDates = TRUE)
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

SumPRCP <- sum(Data$PRCP)

datebreaks <- seq(as.Date("2021-12-06"), as.Date("2022-01-06"), by = "2 days")

time_series <- ggplot(data = Data) +
  geom_col(mapping = aes(x = DATE, y = PRCP)) +
  labs(x = "Date", 
       y = "Precipitation (in)") +
  theme_bw()

time_series +
  scale_x_date(breaks = datebreaks) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(aes(as.Date("2021-12-12"), 3, label=paste("30-day rainfall total: ", SumPRCP, "(in)"))) 

ggsave("time_seriesJan2022.png", width = 15, height = 8, units =c("cm"), dpi = 300)


################## All Data March 4, 2021 thru April 4, 2021 ################################################################
Data<-read.xlsx("April21PrecipTempData3679912.xlsx",detectDates = TRUE)
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

SumPRCP <- sum(Data$PRCP)

datebreaks <- seq(as.Date("2021-03-04"), as.Date("2021-04-04"), by = "2 days")

time_series <- ggplot(data = Data) +
  geom_col(mapping = aes(x = DATE, y = PRCP)) +
  labs(x = "Date", 
       y = "Precipitation (in)") +
  theme_bw()

time_series +
  scale_x_date(breaks = datebreaks) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(aes(as.Date("2021-03-12"), .9, label=paste("30-day rainfall total: ", SumPRCP, "(in)"))) 

ggsave("time_seriesApr2021.png", width = 15, height = 8, units =c("cm"), dpi = 300)

################## All Data Dec. 29, 2017 thru Jan. 29, 2018 ################################################################
Data<-read.xlsx("Jan18PrecipTempData3679912.xlsx",detectDates = TRUE)
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

SumPRCP <- sum(Data$PRCP)

datebreaks <- seq(as.Date("2017-12-29"), as.Date("2018-01-29"), by = "2 days")

time_series <- ggplot(data = Data) +
  geom_col(mapping = aes(x = DATE, y = PRCP)) +
  labs(x = "Date", 
       y = "Precipitation (in)") +
  theme_bw()

time_series +
  scale_x_date(breaks = datebreaks) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(aes(as.Date("2018-01-04"), 2.4, label=paste("30-day rainfall total: ", SumPRCP, "(in)"))) 

ggsave("time_seriesJan2018.png", width = 15, height = 8, units =c("cm"), dpi = 300)

################# All Data March 16, 2016 thru April 16, 2016 ################################################################
Data<-read.xlsx("April16PrecipTempData3679912.xlsx",detectDates = TRUE)
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

SumPRCP <- sum(Data$PRCP)

datebreaks <- seq(as.Date("2016-03-16"), as.Date("2016-04-16"), by = "2 days")

time_series <- ggplot(data = Data) +
  geom_col(mapping = aes(x = DATE, y = PRCP)) +
  labs(x = "Date", 
       y = "Precipitation (in)") +
  theme_bw()

time_series +
  scale_x_date(breaks = datebreaks) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(aes(as.Date("2016-03-21"), 2.4, label=paste("30-day rainfall total: ", SumPRCP, "(in)"))) 

ggsave("time_seriesApril2016.png", width = 15, height = 8, units =c("cm"), dpi = 300)

################# All Data July 24. 2015 thru August 24, 2015  ################################################################
Data<-read.xlsx("Aug15PrecipTempData3679912.xlsx",detectDates = TRUE)
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

SumPRCP <- sum(Data$PRCP)

datebreaks <- seq(as.Date("2015-07-24"), as.Date("2015-08-24"), by = "2 days")

time_series <- ggplot(data = Data) +
  geom_col(mapping = aes(x = DATE, y = PRCP)) +
  labs(x = "Date", 
       y = "Precipitation (in)") +
  theme_bw()

time_series +
  scale_x_date(breaks = datebreaks) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(aes(as.Date("2015-07-29"), 0.9, label=paste("30-day rainfall total: ", SumPRCP, "(in)"))) 

ggsave("time_seriesAug2015.png", width = 15, height = 8, units =c("cm"), dpi = 300)



