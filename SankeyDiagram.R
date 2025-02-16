# Load necessary libraries
library(networkD3)
library(dplyr)
library(readxl)
library(tidyverse)


rm(list=ls())
 
file_path <-"C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Parris_MtVernon_Mills/R Code/sankeyinput.xlsx"
df <- read_excel(file_path, sheet = "Sheet1")

# Select relevant columns for the Sankey diagram (Supplier flow)
df_sankey <- df %>%
  select(Supplier, `MWM Plant`, `Trion WWTP`, `Jarrett BS Application`, `Jarrett SW/Runoff`, `Raccoon Creek`, `Summerville Intake`, Value)

# Reshape data for Sankey: convert wide to long format
edges <- df_sankey %>%
  pivot_longer(cols = -c(Supplier, Value), names_to = "Target", values_to = "Source") %>%
  select(Source, Target, Value)

# Assign unique IDs to nodes
nodes <- data.frame(name = unique(c(edges$Source, edges$Target)))

# Map node names to indices
edges$Source <- match(edges$Source, nodes$name) - 1
edges$Target <- match(edges$Target, nodes$name) - 1

# Create Sankey diagram
sankey <- sankeyNetwork(Links = edges, Nodes = nodes,
                        Source = "Source", Target = "Target",
                        Value = "Value", NodeID = "name",
                        fontSize = 12, nodeWidth = 30)

# Plot the Sankey diagram
sankey

