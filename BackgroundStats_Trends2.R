# Simplot-EMF Strip Chart Analyses

# R-Code Author: James R. Henderson, MS, PE - Formation Environmental, LLC

# R is an open source statistics and graphics package available at https://www.r-project.org/.
# https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdfstat


####################################################################################################
rm(list=ls())
# Set working Directory
setwd("C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Simplot-EMF Background Analysis/R-Code")

suppressPackageStartupMessages(require(openxlsx))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library("ggpubr"))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(geomtextpath))
suppressPackageStartupMessages(library(scales))




# PhosP_T with LAP Fields

rm(list=ls())

Data <-read.xlsx("ABT.xlsx", sheet = "PhosP_T_A2LAP")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

MedianCI(Data$PhosPv, conf.level=0.95, na.rm=TRUE)

# All Phos Log Scale

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanPhosPv = mean(PhosPv, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = PhosPv)) + 
  geom_jitter(position = position_jitter(0.2)) +
  scale_y_log10() +  # Apply log scale to the y-axis
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Phosphorus Strip Chart (Log Scale)", 
       x = "Date", 
       y = "PhosP (mg/L, log scale)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanPhosPv, 
                 label = paste0(Decade, "\nMean: ", round(MeanPhosPv, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Display the plot
print(p)


# All Phos Log Scale with minor tics labeled

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanPhosPv = mean(PhosPv, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

log10_minor_break <- function() {
  function(x) {
    minx <- floor(min(log10(x), na.rm = TRUE)) - 1
    maxx <- ceiling(max(log10(x), na.rm = TRUE)) + 1
    n <- maxx - minx
    10^seq(minx, maxx, by = 1/10)
  }
}

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = PhosPv)) + 
  geom_jitter(position = position_jitter(0.2)) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),  # Major breaks
    minor_breaks = log10_minor_break(),  # Minor breaks
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Phosphorus Strip Chart (Log Scale)", 
       x = "Date", 
       y = "PhosP (mg/L, log scale)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanPhosPv, 
                 label = paste0(Decade, "\nMean: ", round(MeanPhosPv, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Display the plot
print(p)



# All Phos Raw Unclipped

# Compute median confidence interval
MedianCI(Data$PhosPv, conf.level=0.95, na.rm=TRUE)

# Create a Decade Variable
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanPhosPv = mean(PhosPv, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Fit regression model
model <- lm(PhosPv ~ DateDec, data = Data)
r_squared <- summary(model)$r.squared
p_value <- summary(model)$coefficients[2,4]

# Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = PhosPv)) + 
  geom_jitter(position = position_jitter(0.2)) +
  scale_x_continuous(limits = c(1990, 2030)) +
  labs(title = "Phosphorus Strip Chart w/ LAP Fields", 
       x = "Date", 
       y = "PhosP (mg/L)") +
  theme_minimal()

# Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)

# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

# Add Mean Labels
p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanPhosPv, 
                 label = paste0(Decade, "\nMean: ", round(MeanPhosPv, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")

# Add a Regression Line with R-squared and p-value
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(aes(x = max(DateDec) - 5, y = predict(model, newdata = data.frame(DateDec = max(DateDec) - 5)), 
                label = paste0("R² = ", round(r_squared, 3), ", p = ", format.pval(p_value, digits=3, eps=0.001))),
            color = "red", size = 5, hjust = 0)

# Display the plot
print(p)




# All Phos Clipped at 0.5

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanPhosPv = mean(PhosPv, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = PhosPv)) + 
  geom_jitter(position = position_jitter(0.2)) +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Phosphorus Strip Chart w/ LAP Fields - Clipped at 0.5", 
       x = "Date", 
       y = "PhosP (mg/L)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanPhosPv, 
                 label = paste0(Decade, "\nMean: ", round(MeanPhosPv, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Step 7: Clip the y-axis at 0.5 without removing data
p <- p + 
  coord_cartesian(ylim = c(NA, 0.5))

# Display the plot
print(p)



# PhosP_T w/o LAP Fields

rm(list=ls())

Data <-read.xlsx("ABT.xlsx", sheet = "PhosP_T_A2")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

MedianCI(Data$PhosPv, conf.level=0.95, na.rm=TRUE)

# All Phos No LAP  Raw Unclipped

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanPhosPv = mean(PhosPv, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = PhosPv)) + 
  geom_jitter(position = position_jitter(0.2)) +
  # Remove or comment out the log scale transformation
  # scale_y_log10() +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Phosphorus Strip Chart w/o LAP Fields", 
       x = "Date", 
       y = "PhosP (mg/L)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanPhosPv, 
                 label = paste0(Decade, "\nMean: ", round(MeanPhosPv, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Display the plot
print(p)





# PhosP_T LAP Fields Only

rm(list=ls())

Data <-read.xlsx("ABT.xlsx", sheet = "PhosP_T_LAPo")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

MedianCI(Data$PhosPv, conf.level=0.95, na.rm=TRUE)

# All Phos LAP Only  Raw Unclipped

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanPhosPv = mean(PhosPv, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = PhosPv)) + 
  geom_jitter(position = position_jitter(0.2)) +
  # Remove or comment out the log scale transformation
  # scale_y_log10() +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Phosphorus Strip Chart LAP Fields Only", 
       x = "Date", 
       y = "PhosP (mg/L)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanPhosPv, 
                 label = paste0(Decade, "\nMean: ", round(MeanPhosPv, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Display the plot
print(p)


# All Phos Clipped at 0.5

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanPhosPv = mean(PhosPv, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = PhosPv)) + 
  geom_jitter(position = position_jitter(0.2)) +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Phosphorus Strip Chart LAP Fields Only - Clipped at 0.5", 
       x = "Date", 
       y = "PhosP (mg/L)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanPhosPv, 
                 label = paste0(Decade, "\nMean: ", round(MeanPhosPv, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Step 7: Clip the y-axis at 0.5 without removing data
p <- p + 
  coord_cartesian(ylim = c(NA, 0.5))

# Display the plot
print(p)




# NitrateN w/ LAP Fields

rm(list=ls())

Data <-read.xlsx("ABT.xlsx", sheet = "Nitrate ALT2LAP")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

MedianCI(Data$NitrateN, conf.level=0.95, na.rm=TRUE)


# All Nitrate w/ LAP  Raw Unclipped

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanNitrate = mean(NitrateN, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = NitrateN)) + 
  geom_jitter(position = position_jitter(0.2)) +
  # Remove or comment out the log scale transformation
  # scale_y_log10() +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Nitrate Strip Chart w/ LAP Fields", 
       x = "Date", 
       y = "Nitrate (mg/L)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanNitrate, 
                 label = paste0(Decade, "\nMean: ", round(MeanNitrate, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Display the plot
print(p)





# NitrateN w/o LAP Fields

rm(list=ls())

Data <-read.xlsx("ABT.xlsx", sheet = "Nitrate ALT2")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

MedianCI(Data$NitrateN, conf.level=0.95, na.rm=TRUE)


# All Nitrate w/o LAP  Raw Unclipped

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanNitrate = mean(NitrateN, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = NitrateN)) + 
  geom_jitter(position = position_jitter(0.2)) +
  # Remove or comment out the log scale transformation
  # scale_y_log10() +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Nitrate Strip Chart w/o LAP Fields", 
       x = "Date", 
       y = "Nitrate (mg/L)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanNitrate, 
                 label = paste0(Decade, "\nMean: ", round(MeanNitrate, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Display the plot
print(p)



# NitrateN LAP Fields Only

rm(list=ls())

Data <-read.xlsx("ABT.xlsx", sheet = "Nitrate ALT2LAPo")
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

MedianCI(Data$NitrateN, conf.level=0.95, na.rm=TRUE)


# All Nitrate w/o LAP  Raw Unclipped

# Step 1: Create a Decade Variable with the last group as 2020-2025
Data <- Data %>%
  mutate(Decade = case_when(
    DateDec >= 1990 & DateDec < 2000 ~ "1990-1999",
    DateDec >= 2000 & DateDec < 2010 ~ "2000-2009",
    DateDec >= 2010 & DateDec < 2020 ~ "2010-2019",
    DateDec >= 2020 & DateDec <= 2025 ~ "2020-2025",
    TRUE ~ NA_character_
  ))

# Step 2: Calculate Decade-Wise Means
decade_means <- Data %>%
  group_by(Decade) %>%
  summarise(MeanNitrate = mean(NitrateN, na.rm = TRUE)) %>%
  filter(!is.na(Decade))

# Step 3: Plot the Strip Chart with Jitter
p <- ggplot(Data, aes(x = DateDec, y = NitrateN)) + 
  geom_jitter(position = position_jitter(0.2)) +
  # Remove or comment out the log scale transformation
  # scale_y_log10() +
  scale_x_continuous(limits = c(1990, 2030)) +  # Extend x-axis to 2030
  labs(title = "Nitrate Strip Chart LAP Fields Only", 
       x = "Date", 
       y = "Nitrate (mg/L)") +
  theme_minimal()

# Step 4: Add Vertical Lines to Demarcate Decades
p <- p + 
  geom_vline(xintercept = c(1990, 2000, 2010, 2020, 2025), 
             linetype = "dashed", color = "grey", size = 1)  # Thicker lines

# Step 5: Label the Mean PhosP for Each Decade
# Define label positions at the midpoint of each period
decade_labels <- data.frame(
  Decade = c("1990-1999", "2000-2009", "2010-2019", "2020-2025"),
  x = c(1995, 2005, 2015, 2022.5)  # Midpoint of each period for labeling
)

# Merge decade means with their corresponding x positions for labeling
decade_means <- merge(decade_means, decade_labels, by = "Decade")

p <- p + 
  geom_label(data = decade_means, 
             aes(x = x, y = MeanNitrate, 
                 label = paste0(Decade, "\nMean: ", round(MeanNitrate, 3))),
             vjust = -0.5, size = 5, fontface = "bold", 
             fill = alpha("gray", 0.5), color = "black")  # Translucent gray background

# Step 6: Add a Regression Line
p <- p + 
  geom_smooth(method = "lm", se = FALSE, color = "blue")  # Regression line

# Display the plot
print(p)





