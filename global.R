# Kane Williams 2024 
# Data423-24S2 Assignment 1
# Contact: (pkw21@uclive.ac.nz)

# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
#library(plotly) # Interactivity
library(shinycssloaders) # spinner
# library(grid) # titles
library(gridExtra) # Autoplot

### QUICK VIEW ###
library(DT) # Data Tables
library(summarytools) # dfsummary

### MISSING VALUES ###
library(visdat) # Vis-miss plot
library(UpSetR) # Upset chart

### SIMILARITIES
library(corrgram) # Corrgram Plot

### DISTRIBUTIONS
library(car) # Boxplot
library(RColorBrewer) # for Boxplot for some reason

### RELATIONSHIPS
library(vcd) # Mosaic
library(GGally) # Pairs Plot

library(thematic) # for plot colours
library(ragg) # for fonts
library(bslib) # for themes.

# Allows themes to change the plot colours and fonts.
options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")

# NOTE: Run bs_theme_preview() to view different themes to choose :-)
# Run in R console.
