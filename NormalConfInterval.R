rm(list=ls())
# Set working Directory
setwd("C:/Users/JHENDERSON/OneDrive - Ramboll/Projects/GCRC/PBI_Analyses/")

library(EnvStats)


# Step 1: Reading the data
dataset <-read.csv("SC_Turb_CuNi.csv")
dataset <- data.frame(dataset)

kable(dataset) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

object <- dataset[[8]]
object <- as.vector(object)
is.vector(object)

enorm(object, ci = TRUE)


