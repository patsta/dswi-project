################################################################################
# Authors: Martin Clément, Patrice Stampfli, Sander van den Bogaert
# Date: 11.12.2021
# Description: Project paper
################################################################################
# 1. - Packages
# 2. - Dataset
# 3. - Tidy up!
# 4. - Visualization


# 1. - Packages-----------------------------------------------------------------
# Packages used in this research (You may need to install the packages 
# before loading them)
library(readr)
library(scatterplot3d) 
library(ggplot2)

# 2. - Dataset------------------------------------------------------------------
# import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
initial_data <- read.csv('BodyFat.csv')
