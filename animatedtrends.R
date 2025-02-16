# Clear workspace
rm(list = ls())

# Set working directory
setwd(
  "C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Southwire/GCRC/R-Code"
)

# Load necessary libraries
suppressPackageStartupMessages({
  library(openxlsx)
  library(kableExtra)
  library(ggpubr)
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(forcats)
  library(gganimate)
  library(gifski)
  library(fitdistrplus)
})

# Read Data
Data <- read.xlsx("WellsCOCtimeseries.xlsx", colNames = TRUE)

# Display Data Table
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

# Define max Copper value for annotations
max_copper <- max(Data$Copper, na.rm = TRUE)

# Generate Animated Time Series Plot for Copper
myPlot <- ggplot(Data,
                 aes(
                   x = dec_date,
                   y = Copper,
                   group = sys_loc_code,
                   color = factor(loc_desc)
                 )) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +  # Ensure points are included in animation
  scale_x_continuous(breaks = seq(2004, 2026, 2)) +
  guides(color = guide_legend(title = "Well Types")) +
  labs(
    title = 'Tank House Wells Time Series - Copper',
    subtitle = 'Year: {frame_along}',
    # Dynamic subtitle for animation
    x = "Date/Year",
    y = "Copper - mg/L"
  ) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 16),
    plot.title = element_text(
      size = 20,
      color = "black",
      face = "bold.italic"
    ),
    legend.title = element_text(size = 18, color = "black"),
    legend.text = element_text(size = 16, color = "black"),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  
  # Add vertical lines for RW events
  geom_vline(
    xintercept = 2020.5,
    colour = "gray",
    size = 20,
    alpha = 0.5,
    linetype = 'solid'
  ) +
  geom_vline(
    xintercept = 2006.5,
    colour = "gray",
    size = 50,
    alpha = 0.5,
    linetype = 'solid'
  ) +
  
  # RW Shutdown Callout (adjacent to the vertical line)
  annotate(
    "label",
    x = 2020,
    y = max_copper * 0.5,
    label = "RW Shutdown",
    fill = "white",
    color = "black",
    size = 6,
    label.size = 0.4,
    hjust = 0
  ) +
  
  # RW Startup Callout (adjacent to the vertical line)
  annotate(
    "label",
    x = 2005,
    y = max_copper * 0.8,
    label = "RW Startup",
    fill = "white",
    color = "black",
    size = 6,
    label.size = 0.4,
    hjust = 0
  ) +
  
  # Animation
  transition_reveal(dec_date)

# Render and Save GIF with slower animation (30% slower than original)
animate(
  myPlot,
  duration = 28,
  fps = 25,
  width = 900,
  height = 900,
  renderer = gifski_renderer()
)
anim_save("Copper.avi")


# Nickel

# Clear workspace
rm(list = ls())

# Set working directory
setwd(
  "C:/Users/jhenderson/OneDrive - Formation Environmental LLC/Projects/Southwire/GCRC/R-Code"
)

# Load necessary libraries
suppressPackageStartupMessages({
  library(openxlsx)
  library(kableExtra)
  library(ggpubr)
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(forcats)
  library(gganimate)
  library(gifski)
  library(fitdistrplus)
})

# Read Data
Data <- read.xlsx("WellsCOCtimeseries.xlsx", colNames = TRUE)

# Display Data Table
kable(Data) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

# Define max Nickel value for annotations
max_nickel <- max(Data$Nickel, na.rm = TRUE)

# Generate Animated Time Series Plot for Nickel
myPlot <- ggplot(Data,
                 aes(
                   x = dec_date,
                   y = Nickel,
                   group = sys_loc_code,
                   color = factor(loc_desc)
                 )) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +  # Ensure points are included in animation
  scale_x_continuous(breaks = seq(2004, 2024, 2)) +
  guides(color = guide_legend(title = "Well Types")) +
  labs(
    title = 'Tank House Wells Time Series - Nickel',
    subtitle = 'Year: {frame_along}',
    # Dynamic subtitle for animation
    x = "Date/Year",
    y = "Nickel - mg/L"
  ) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 16),
    plot.title = element_text(
      size = 20,
      color = "black",
      face = "bold.italic"
    ),
    legend.title = element_text(size = 18, color = "black"),
    legend.text = element_text(size = 16, color = "black"),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  
  # Add vertical lines for RW events
  geom_vline(
    xintercept = 2020.5,
    colour = "gray",
    size = 20,
    alpha = 0.5,
    linetype = 'solid'
  ) +
  geom_vline(
    xintercept = 2006.5,
    colour = "gray",
    size = 50,
    alpha = 0.5,
    linetype = 'solid'
  ) +
  
  # RW Shutdown Callout (adjacent to the vertical line)
  annotate(
    "label",
    x = 2020,
    y = max_nickel * 0.5,
    label = "RW Shutdown",
    fill = "white",
    color = "black",
    size = 6,
    label.size = 0.4,
    hjust = 0
  ) +
  
  # RW Startup Callout (adjacent to the vertical line)
  annotate(
    "label",
    x = 2005,
    y = max_nickel * 0.8,
    label = "RW Startup",
    fill = "white",
    color = "black",
    size = 6,
    label.size = 0.4,
    hjust = 0
  ) +
  
  # Animation
  transition_reveal(dec_date)

# Render and Save GIF with slower animation (30% slower than original)
animate(
  myPlot,
  duration = 28,
  fps = 25,
  width = 900,
  height = 900,
  renderer = gifski_renderer()
)
anim_save("Nickel.gif")
anim_save("Nickel.avi")
