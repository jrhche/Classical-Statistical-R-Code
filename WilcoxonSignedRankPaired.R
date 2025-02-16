rm(list=ls())
## Set working Directory
setwd("C:/Users/JHENDERSON/OneDrive - Ramboll/Projects/XOM_PH/R Code")

library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(coin)
require(openxlsx)
require(pwr)
library(scales) # For the trans_format function

# Given a vector x, return a vector of powers of 10 that encompasses all values
# in x.
breaks_log10 <- function(x) {
  low <- floor(log10(min(x)))
  high <- ceiling(log10(max(x)))
  
  10^(seq.int(low, high))
}

Paired_PAHs<-read.xlsx("PAHS_REPLACEMENT_ANALYSIS_1.xlsx")
Paired_PAHs

my_data <- data.frame( 
  group = rep(c("RI", "PDI/NF"), each = 63),
  weight = c(Paired_PAHs$RI_RESULT_ppb, Paired_PAHs$PDI_RESULT_ppb)
)

library("dplyr")
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Plot weight by group and color by group
library("ggpubr")
p <-ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("RI", "PDI/NF"),
          ylab = "Total PAHs ppb", xlab = "Groups")
p + scale_y_log10()


p2 <- ggpaired(my_data, x = "group", y = "weight",
         color = "group", line.color = "gray", line.size = 0.4,ylab = "Total PAHs ppb", xlab = "Groups",order = c("RI", "PDI/NF"),
         palette = "jco")
  #stat_compare_means(paired = TRUE)
p2 + annotation_logticks() + scale_y_log10(breaks = breaks_log10,
                    labels = trans_format(log10, math_format(10^.x)))

# Subset weight data before treatment (RI)
RI <- subset(my_data,  group == "RI", weight,
             drop = TRUE)

# subset weight data after treatment (PID/NF)
PDI_NF <- subset(my_data,  group == "PDI/NF", weight,
                 drop = TRUE)

# compute the paired differences
d <- with(my_data, 
          weight[group == "RI"] - weight[group == "PDI/NF"])

# Shapiro-Wilk normality test for the differences
shapiro.test(d) 

# Compute Paired t Test
t.test(Paired_PAHs$PDI_RESULT_ppb, Paired_PAHs$RI_RESULT_ppb, paired = TRUE, alternative = "two.sided")

# Compute Paired Wilcoxon Test
wilcox.test(Paired_PAHs$PDI_RESULT_ppb,Paired_PAHs$RI_RESULT_ppb, paired=TRUE, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)




