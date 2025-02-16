# Correlation Analysis - TCE Crawl Space Concentrations vs. Distance from Source
# SRG Global/Settlers Grove Neighborhood - Covington, Georgia

# R-Code Author: James R. Henderson, PE (Ramboll)
# Based on algorithms from USEPA. 2006c. "Data Quality Assessment: Statistical Methods for Practitioners."
# EPA/240/B-06/003 EPA QA/G-9S. Washington D.C.: United States Environmental Protection Agency.
# http://www2.epa.gov/quality/guidance-data-quality-assessment.

# Correlation Benchmarks on Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.).
# New Jersey: Lawrence Erlbaum.(pp. 79-80) and
# Rosenthal, J.A. (1996), "Qualitative descriptors of strength of association and effect size," Journal of Social
# Service Research, 21(4): 37-59

# Conversion of Kendall (t) correlation coefficient to Pearson (r) for comparison to Cohen's Benchmarks is based on:

### Walker, David A. (2003) "JMASM9: Converting Kendall's Tau For Correlational Or Meta-Analytic Analyses,"
### Journal of Modern Applied Statistical Methods: Vol. 2 : Iss. 2 , Article 26.

### Gilpin, A. R. (1993). Table for conversion of Kendall's tau to Spearman's rho within the context of measures
### of magnitude of effect for meta-analysis.Educational and Psychological Measurement, 53, 87-92. 

# R is an open source statistics and graphics package available at https://www.r-project.org/.
# https://cran.r-project.org/web/packages/corrr/corrr.pdf

####################################################################################################
rm(list=ls())
# Set working Directory
setwd("C:/Users/JHENDERSON/OneDrive - Formation Environmental LLC/Projects/SRG/R Code")

suppressPackageStartupMessages(require(openxlsx))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library("ggpubr"))
suppressPackageStartupMessages(library(fitdistrplus))
suppressPackageStartupMessages(library(dplyr))


# CORRELATION ANALYSIS OF ALL CRAWL SPACE TCE DATA VS DISTANCE FROM COMBINED PLUMES CENTERLINE
#Based on data collected from 2013-21.

Data <-read.xlsx("TCEoutincrawl_allres.xlsx")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

# Data <- filter(Data,TCEval0>0)

# Raw Scatterplot of TCE Crawl Space Concentrations vs Distance from Plume Centerline
ggscatter(Data, x = "Distance", y = "TCEval0", 
          xlab = "Distance from Plume Centerline - FT", ylab = "TCE ug/m3 - Crawlspace")

# Check for the normality of TCE
ggqqplot(Data$TCEval0, ylab = "TCE - ug/m3")
shapiro.test(Data$TCEval0)
# p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution, thus can assume non-normality.


# Will perform both Pearson Correlation Analysis and Kendall Correlation Analysis on non-normal TCE data, but Kendall recommended due to non-normality

# Pearson Correlation Analysis, TCE Crawl Space Concentration vs Distance from Plume Center Line
pea_r <- cor.test(Data$Distance, Data$TCEval0, method=c("pearson"))
pea_r

# Compare to Cohen's Pearson (r) Benchmarks (Cohen, 1988, pp. 79-80) & (Rosenthal, 1996)
## < 0.1 = insubstantial
## 0.1 - 0.3 = small
## 0.3 - 0.5 = medium
## > 0.5 - 0.7 = large
## > 0.7 = very large

### using Pearson, statistically significant (p<0.05) 'Small' magnitude correlation (r=-0.26) observed between TCE Crawl Space and Distance from Plume Source ###

# Kendall (tau) Correlation Analysis, TCE Crawl Space Concentration vs Distance from Plume Center Line
ken_t <- cor.test(Data$Distance, Data$TCEval0, method=c("kendall"), conf.level=0.95)
ken_t

# Convert Kendall (tau) coefficient to Pearson (r) for comparison to Cohen's Benchmarks (Walker, 2003/Gilpin, 1993)
tau_r_equivalent <-sin(pi*(ken_t$estimate)*0.5)
tau_r_equivalent

# Compare to Cohen's Pearson (r) Benchmarks (Cohen, 1988, pp. 79-80) & (Rosenthal, 1996)
## < 0.1 = insubstantial
## 0.1 - 0.3 = small
## 0.3 - 0.5 = medium
## > 0.5 - 0.7 = large
## > 0.7 = very large

### using Kendall, statistically significant (p<0.05) 'Medium' magnitude correlation (tau_r equivalent=-0.45) observed between TCE Crawl Space and Distance from Plume Source ###

# Raw Scatterplot of TCE Crawl Space Concentrations vs Distance from Plume Centerline w/ Kendall-Tau and Pearson Coefficients Posted
ggscatter(Data, x = "Distance", y = "TCEval0", 
          add = "reg.line", conf.int = TRUE,  
          add.params = list(color = "blue", fill = "lightgray"),
          xlab = "Distance from Plume Centerline - FT", ylab = "TCE ug/m3 - Crawlspace") + stat_cor(method = "kendall", label.x = 1000, label.y = .8, cor.coef.name = c("tau")) + stat_cor(method = "pearson", label.x = 1000, label.y = .83, cor.coef.name = c("r"))

### statistically significant (p<0.05) 'Medium' magnitude correlation (t= 0.3, 0.45 r equivalent) observed between TCE Crawl Space and Distance from Plume Source ###

ggplot(
  data = Data,
  mapping = aes(x = Distance, y = TCEval0, color = Type)
) +
  geom_point(mapping = aes(color = Type, shape = Type)) +
  labs(
    title = "TCE Air Concentration versus Distance from Plume",
    subtitle = "Crawlspace, Indoor, and Outdoor Air",
    x = "Distance - ft)", y = "TCE ug/m3")


