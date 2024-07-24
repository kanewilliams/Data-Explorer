# Kane Williams 2024 
# Data423-24S2 Assignment 1
# Contact: (pkw21@uclive.ac.nz)

# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)

library(DT) # Data Tables

library(thematic) # for plot colours
library(ragg) # for fonts
library(bslib) # for themes.

# Allows themes to change the plot colours and fonts.
options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")

# NOTE: Run bs_theme_preview() to view different themes to choose :-)
# Run in R console.
