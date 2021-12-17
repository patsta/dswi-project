################################################################################
# Authors: Martin Clément, Patrice Stampfli, Sander van den Bogaert
# Date: 17.12.2021
# Description: Project paper
################################################################################
# 1. - Packages
# 2. - Dataset
# 3. - Tidy Data
# 4. - Linear model 
# 5. - Model


# 1. - Packages-----------------------------------------------------------------
# Packages used in this research (You may need to install the packages 
# before loading them)
library(readr)
library(scatterplot3d) 
library(ggplot2)
library(rgl)
library(car) 

# 2. - Dataset------------------------------------------------------------------
# import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
initial_data <- read.csv('BodyFat.csv')

# attach the model
attach(initial_data)

# 3. - Tidy data----------------------------------------------------------------
# convert lbs to kg
initial_data$WEIGHT <- initial_data$WEIGHT *  0.45359237

# convert inches to cm
initial_data$HEIGHT <- initial_data$HEIGHT *  2.54

View(initial_data)

# 4. - Linear model-------------------------------------------------------------
#plot the bodyfat according to the age
plot(BODYFAT ~ AGE)
# make a linear model to predict bodyfat from age
m1 <- lm(BODYFAT ~ AGE)
# plot the prediction on the graph
abline(m1)
# Age is responsible for this amount of the variation
summary(m1)$r.squared

# 5. Model according to age and weight------------------------------------------
# plot the bodyfat according to the age and weight
scatter3d(x=AGE, z=BODYFAT, y=WEIGHT, surface=TRUE)
# make a linear model to predict bodyfat from age and weight
m2 <- lm(BODYFAT ~ AGE + WEIGHT)
# Age and weight together account for this amount of the variation
summary(m2)$adj.r.squared

# 5. full model
# make a linear model to predict bodyfat from all the variables
m3 <- lm(BODYFAT ~ ., data = initial_data)
# how much variation is there between the prediction and the actual value
var(m3$residuals)
# All data together accounts for this amount of the variation
summary(m3)$adj.r.squared
# Summary of all the variables and their contribution to the model
summary(m3)


# Model for density
m4 <- lm(BODYFAT ~ DENSITY)
# Density accounts for this amount of the variation
summary(m4)$adj.r.squared

# 6. Full model without Density
# linear model to predict body from all the variables except Density
m5 <- lm(BODYFAT ~ . -DENSITY, data = initial_data)
# how much variation is there between the prediction and the actual value
var(m5$residuals)
# The model accounts for this amount of the variation
summary(m5)$adj.r.squared
# Summary of all the variables and their contribution to the model
summary(m5)

