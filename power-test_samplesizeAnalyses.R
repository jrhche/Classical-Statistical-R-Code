# Power Analysis to Identify Sample Size
# Author: James R. Henderson, MS, PE




# Clear the environment to ensure a fresh start
rm(list = ls())

# Load required libraries
require(openxlsx)  # For reading Excel files
require(pwr)       # For power analysis

# Set the working directory (update this path as needed)
setwd("C:/Users/JHENDERSON/OneDrive - Formation Environmental LLC/Projects/XOM_PH/R Code")

# Read action level values from the first row of columns 2 to 7 in the Excel file
Action.Level <- read.xlsx("SILinputCOCs.xlsx", sheet = 1, rows = 1, colNames = FALSE, cols = 2:7)

# Define the sample sizes to be evaluated in power analysis
sample_sizes <- c(10, 20, 30, 40, 50, 60, 70)
num_sizes <- length(sample_sizes)

# Loop through six columns of data from the Excel file
for (i in 1:6) {
  
  # Read column i+1 starting from row 5, including column names
  data <- read.xlsx("SILinputCOCs.xlsx", sheet = 1, startRow = 5, colNames = TRUE, cols = i + 1)
  data_name <- colnames(data)  # Extract column name
  data_values <- data[,1]      # Extract values from the first column
  
  # Print the name of the dataset
  print(data_name)
  
  # Perform a one-sample t-test comparing the data against the action level
  test_result <- t.test(data_values, mu = Action.Level[1, i], alternative = "less", conf.level = 0.95)
  print(test_result)
  
  # Define the effect size range for power analysis
  effect_size <- seq(-2, 0, length = 200)
  adjusted_values <- effect_size * sd(data_values) + Action.Level[1, i]  # Adjust effect size based on standard deviation
  
  # Define x and y ranges for the plot
  x_range <- range(adjusted_values)
  y_range <- c(0, 1)  # Type II error (beta) range
  
  # Initialize an empty plot for power curves
  plot(x_range, y_range, type = "n", xlab = data_name, ylab = expression("Type II Error (" * beta * ")"))
  
  # Customize y-axis labels
  axis(2, at = seq(0, 1, by = 0.1))
  
  # Define colors for different sample sizes
  colors <- rainbow(num_sizes)
  
  # Loop through different sample sizes and compute power analysis
  for (j in 1:num_sizes) {
    beta_values <- 1 - pwr.t.test(n = sample_sizes[j], d = effect_size, sig.level = 0.05, 
                                  type = "one.sample", alternative = "less")$power
    lines(adjusted_values, beta_values, type = "l", lwd = 1.5, col = colors[j])  # Plot power curve
  }
  
  # Add a title to the plot
  title(paste("Power Curves for", data_name))
  
  # Add a legend for different sample sizes
  legend(x_range[1], y_range[2], legend = sample_sizes, cex = 0.8, lty = 1, col = colors, title = "Sample Size (n)")
  
  # Add grid lines for better visualization
  grid()
}



