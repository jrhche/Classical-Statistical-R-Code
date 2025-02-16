####################################################################################################
# Goodness of Fit Analyses with Graphical Visualization
# DRAFT WORKING CODE - INTERNAL USE ONLY
####################################################################################################
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

# R-Code Debugging/Optimization, OpenAI. (2025). ChatGPT (January 13 version) [Large language model]. Retrieved from https://chat.openai.com/

# Clear workspace
rm(list=ls())

# Set working directory (edit accordingly)
setwd("C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Henderson ML & Analytics Code/R Code/")

# Load libraries
library(dplyr)
library(fitdistrplus)
library(tidyr)
library(purrr)
library(ggplot2)
library(stats)
library(flexsurv)

# Import Source Data (edit accordingly)
data <- readxl::read_excel("C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Henderson ML & Analytics Code/R Code/UCL_input.xlsx", sheet = "PreREMInput")

# Remove non-positive values
data <- data %>%
  filter(Pb_ppm_s > 0)

# Distributions to test
distributions <- c("norm", "lnorm", "gamma", "exp", "weibull", "llogis")


# Perform GOF tests and plot fits
perform_gof_and_plot <- function(domain, values) {
  results <- list()
  plots <- list()
  
  for (dist in distributions) {
    fit <- tryCatch({
      fitdist(values, dist)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(fit)) {
      # Perform KS test
      ks_p_value <- tryCatch({
        ks_test <- switch(dist,
                          norm = ks.test(values, "pnorm", mean = fit$estimate["mean"], sd = fit$estimate["sd"]),
                          lnorm = ks.test(values, "plnorm", meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]),
                          gamma = ks.test(values, "pgamma", shape = fit$estimate["shape"], rate = fit$estimate["rate"]),
                          exp = ks.test(values, "pexp", rate = fit$estimate["rate"]),
                          weibull = ks.test(values, "pweibull", shape = fit$estimate["shape"], scale = fit$estimate["scale"]),
                          llogis = ks.test(values, "pllogis", shape = fit$estimate["shape"], scale = fit$estimate["scale"]),
                          NA)
        ks_test$p.value
      }, error = function(e) {
        NA
      })
      
      # Save GOF results
      results[[dist]] <- data.frame(
        Domain = domain,
        Distribution = dist,
        AIC = fit$aic,
        BIC = fit$bic,
        KS_P_Value = ks_p_value
      )
      
      # Create plots for the fitted distributions
      x_vals <- seq(min(values), max(values), length.out = 100)
      fitted_density <- switch(dist,
                               norm = dnorm(x_vals, mean = fit$estimate["mean"], sd = fit$estimate["sd"]),
                               lnorm = dlnorm(x_vals, meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]),
                               gamma = dgamma(x_vals, shape = fit$estimate["shape"], rate = fit$estimate["rate"]),
                               exp = dexp(x_vals, rate = fit$estimate["rate"]),
                               weibull = dweibull(x_vals, shape = fit$estimate["shape"], scale = fit$estimate["scale"]),
                               llogis = dllogis(x_vals, shape = fit$estimate["shape"], scale = fit$estimate["scale"]))
      
      plot_data <- data.frame(x = x_vals, Density = fitted_density, Distribution = dist)
      
      plots[[dist]] <- ggplot() +
        geom_histogram(aes(x = values, y = after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
        geom_line(data = plot_data, aes(x = x, y = Density, color = Distribution), size = 1.2) +
        labs(title = paste("Domain:", domain, "-", dist, "Fit"),
             x = "Values", y = "Density") +
        theme_minimal()
      
    }
  }
  
  # Combine all results into a single DF
  results_df <- do.call(rbind, results)
  list(Results = results_df, Plots = plots)
}


# Perform GOF tests and generate plots for each domain
gof_results <- data %>%
  group_by(Domain) %>%
  summarise(
    Analysis = list(perform_gof_and_plot(Domain[1], Pb_ppm_s)),
    .groups = "drop"
  )

# Export results and plots
gof_summary <- bind_rows(lapply(gof_results$Analysis, `[[`, "Results"))
all_plots <- lapply(gof_results$Analysis, `[[`, "Plots")


# Sort results by AIC
gof_summary <- gof_summary %>%
  arrange(Domain, AIC)

# Print Summary Data
print(gof_summary)

# Print Plots to Viewer
all_plots

# Save plots
output_dir <- "C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Henderson ML & Analytics Code/R Code/Plots/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

for (i in seq_along(gof_results$Domain)) {
  domain_name <- gof_results$Domain[i]
  domain_plots <- all_plots[[i]]
  
  for (dist_name in names(domain_plots)) {
    ggsave(
      filename = paste0(output_dir, domain_name, "_", dist_name, ".png"),
      plot = domain_plots[[dist_name]],
      width = 8, height = 6
    )
  }
}

# Interpretation Legend
# - Domain: Area Name
# - Distribution: Tested distribution
# - AIC: Akaike Information Criterion (lower is better)
# - BIC: Bayesian Information Criterion (lower is better)
# - KS_P_Value: P-value from the Kolmogorov-Smirnov test (higher is better)
#
# Each domain's histogram with fitted distribution curves is saved in the output directory.
