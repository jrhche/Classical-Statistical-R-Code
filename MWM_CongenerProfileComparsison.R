# Product/Supplier Pattern-Trend Analyses
# Mount Vernon Mills - Trion, Georgia

# R-Code Author: James R. Henderson, MS, PE - Formation Environmental, LLC

# R is an open source statistics and graphics package available at https://www.r-project.org/.
# https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdfstat

# Montgomery, Douglas C., and George C. Runger. Applied Statistics and Probability
# for Engineers." 6th Edition, John Wiley & Sons, 2014.

# Cohen, Yosef, and Jeremiah Y. Cohen. Statistics and Data with R: An applied
# approach through examples." John Wiley & Sons, 2008.

# DeGroot, Morris H., and Mark J. Schervish. Probability and statistics." 4th Edition, Pearson, 2011.

# Bain, Lee J., and Max Engelhardt. Introduction to probability and mathematical statistics."
# 2nd Edition, Brooks/Cole, 1992.

# Casella, George, and Roger L. Berger. Statistical inference." 2nd Edition, Pacific Grove, CA: Duxbury, 2002.

####################################################################################################
rm(list=ls())
# Set working Directory
setwd("C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Parris_MtVernon_Mills/R Code")

suppressPackageStartupMessages(require(openxlsx))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library("ggpubr"))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(forcats))

# Read analytics base table (ABT)

Data <-read.xlsx("PFAS_BarChartsl_Input.xlsx")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)



# Data Dictionary
# Supplier - Chemical Company Supplier of PFAS Containing Product
# Allocation- % allocation share determined by mass balance model at Raccoon Creek Intake to Summerville Drinking Water Plant
# Total_Short - Total Short Chain PFAS containing product (lbs) estimated to have been discharged in MVM wastewater
# Total_Long - Total Long Chain PFAS containing product (lbs) estimated to have been discharged in MVM wastewater
# Total_LS - Total Short +Long Chain PFAS containing product (lbs) estimated to have been discharged in MVM wastewater
# Years - total number of years Supplier provided product to MVM


# Total Short+Long Chain Fluorochemical Usage Analysis by Supplier

Data1 <-read.xlsx("PFAS_BarChartsl_Input.xlsx")
kable(Data1) %>%
  kable_styling(bootstrap_options = "striped", font_size = 8)

# Plot
Data1 %>%
  mutate(Sample = fct_reorder(Sample, Order)) %>%
  ggplot(aes(x=Analyte, y=PcntAbund)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~Sample, ncol=1, strip.position = "top") +
  geom_bar(fill = Group)

+
  scale_x_continuous(breaks = scales::breaks_extended(n = 21))+
  scale_y_continuous(breaks = scales::breaks_extended(n = 10))+
  ggtitle("Supplier Fluorochemical Usage (Short + Long) Time Series - MVM") +  ylab("Total Short + Long Chain - lbs")


# Variable Width Bar Chart - Comparison of Total PFAS Long+Short Chain Product Use to Allocation Share

bc <-ggplot(Data, aes(x=Allocation, y=reorder(Supplier, +Allocation), width=Total_LS*.0000005)) + 
  geom_bar(aes(fill=format(Total_LS, nsmall=1, big.mark=",")), stat="identity", position="identity") +
  labs( x="", fill = "Total Long+Short (lbs)") +
  scale_fill_brewer(palette="BrBG")

bc <- bc + ggtitle("Relative Comparison of Total PFAS L+S Chain Product Use to Raccoon Creek Allocation Share") + xlab("% Allocation Share") + ylab("")
bc <- bc + coord_cartesian(xlim = c(0,100))
bc <- bc + geom_text(aes(label = paste((Allocation),"% (",Years,"years)")), hjust = -0.2, check_overlap = T)
bc

# Total Short Chain Fluorochemical Usage Analysis by Supplier

Data2 <-read.xlsx("SupplierTotal_Short.xlsx")
kable(Data2) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

# Plot
Data2 %>%
  mutate(Supplier = fct_reorder(Supplier, Order)) %>%
  ggplot(aes(x=Year, y=Total_Short)) + 
  geom_bar(stat='identity', fill="DarkGreen")+
  facet_wrap(~Supplier, ncol=1, strip.position = "top")+
  scale_x_continuous(breaks = scales::breaks_extended(n = 15))+
  scale_y_continuous(breaks = scales::breaks_extended(n = 4))+
  ggtitle("Supplier Fluorochemical Usage (Short Chain) Time Series - MVM") +  ylab("Total Short Chain - lbs")


# Variable Width Bar Chart - Comparison of Total PFAS Short Chain Product Use to Allocation Share

bc <-ggplot(Data, aes(x=Alloc_Short, y=reorder(Supplier, +Alloc_Short), width=Total_Short/1800000)) + 
  geom_bar(aes(fill=format(Total_Short, nsmall=1, big.mark=",")), stat="identity", position="identity") +
  labs( x="", fill = "Total Short (lbs)") +
  scale_fill_brewer(palette="BrBG")

bc <- bc + ggtitle("Relative Comparison of Total PFAS Short Chain Product Use to Raccoon Creek Allocation Share") + xlab("% Allocation Share") + ylab("")
bc <- bc + coord_cartesian(xlim = c(0,100))
bc <- bc + geom_text(aes(label = paste((Alloc_Short),"% (",Years_short,"years)")), hjust = -0.2, check_overlap = T)
bc

# Total Long Chain Fluorochemical Usage Analysis by Supplier

Data3 <-read.xlsx("SupplierTotal_Long.xlsx")
kable(Data3) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

# Plot
Data3 %>%
  mutate(Supplier = fct_reorder(Supplier, Order)) %>%
  ggplot(aes(x=Year, y=Total_Long)) + 
  geom_bar(stat='identity', fill="DarkGreen")+
  facet_wrap(~Supplier, ncol=1, strip.position = "top")+
  scale_x_continuous(breaks = scales::breaks_extended(n = 15))+
  scale_y_continuous(breaks = scales::breaks_extended(n = 4))+
  ggtitle("Supplier Fluorochemical Usage (Long Chain) Time Series - MVM") +  ylab("Total Long Chain - lbs")


# Variable Width Bar Chart - Comparison of Total PFAS Long Chain Product Use to Allocation Share

bc <-ggplot(Data, aes(x=Alloc_Long, y=reorder(Supplier, +Alloc_Long), width=Total_Long/800000)) + 
  geom_bar(aes(fill=format(Total_Long, nsmall=1, big.mark=",")), stat="identity", position="identity") +
  labs( x="", fill = "Total Long (lbs)") +
  scale_fill_brewer(palette="BrBG")

bc <- bc + ggtitle("Relative Comparison of Total PFAS Long Chain Product Use to Racoon Creek Allocation Share") + xlab("% Allocation Share") + ylab("")
bc <- bc + coord_cartesian(xlim = c(0,100))
bc <- bc + geom_text(aes(label = paste((Alloc_Long),"% (",Years_long,"years)")), hjust = -0.2, check_overlap = T)
bc



# Raccoon Creek Sludge Sites/Farms Analysis

Data4 <-read.xlsx("RaccoonSludgeSites.xlsx")
kable(Data4) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

# Plot
Data4 %>%
  mutate(Supplier = fct_reorder(Farm, Order)) %>%
  ggplot(aes(x=Year, y=Tons_Sludge)) + 
  geom_bar(stat='identity', fill="Black")+
  facet_wrap(~Supplier, ncol=1, strip.position = "top")+
  scale_x_continuous(breaks = scales::breaks_extended(n = 15))+
  scale_y_continuous(breaks = scales::breaks_extended(n = 4))+
  ggtitle("Trion WWTP Sludge Application Time Series - Raccoon Creek Watershed") +  ylab("Sludge/Biosolids - tons")






