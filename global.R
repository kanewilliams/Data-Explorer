# Kane Williams 2024 
# Data423-24S2 Assignment 1
# Contact: (pkw21@uclive.ac.nz)

# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)

library(thematic) # for plot colours
library(ragg) # for fonts
library(bslib) # for themes.

# Allows themes to change the plot colours and fonts.
options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")

# NOTE: Run bs_theme_preview() to view different themes to choose :-)

# --- Set global variables
#MAX_UPLOAD_SIZE <- 5 * 1024^2

# --- Configure global settings
#options(shiny.maxRequestSize = MAX_UPLOAD_SIZE)

# --- Define a function to be used in both UI and server. E.g.
#get_column_names <- function(data) {
#  if (is.null(data)) return(NULL)
#  return(names(data))
#}
