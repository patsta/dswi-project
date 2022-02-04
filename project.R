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
library(tidyverse)

# 2. - Dataset------------------------------------------------------------------
# import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
initial_data <- read.csv('BodyFat.csv')

# 3. - Tidy data----------------------------------------------------------------
# convert lbs to kg
initial_data$WEIGHT <- initial_data$WEIGHT *  0.45359237

# convert inches to cm
initial_data$HEIGHT <- initial_data$HEIGHT *  2.54

# add column BMI to the table
initial_data <- mutate(initial_data, BMI = WEIGHT / HEIGHT^2 *10000)

# check for special cases
initial_data <- filter(initial_data, HEIGHT > 140)
initial_data <- filter(initial_data, IDNO != 48)
initial_data <- filter(initial_data, IDNO != 96)


# attach the dataset
attach(initial_data)
View(initial_data)

# Variation in the original data
var(BODYFAT)


# 4. - Linear models------------------------------------------------------------
# 4.1 - Age
#plot the bodyfat according to the age
plot(BODYFAT ~ AGE)
# make a linear model to predict bodyfat from age
m1 <- lm(BODYFAT ~ AGE)
# plot the prediction on the graph
abline(m1)
# Variation after adjusting for age
var(mean(BODYFAT)+m1$residuals)
# Age is responsible for this amount of the variation
summary(m1)$r.squared
# Summary of AGE and the contribution to the model
summary(m1)


# 4.2 - Age and weight
# plot the bodyfat according to the age and weight
scatter3d(x=AGE, z=WEIGHT, y=BODYFAT, surface=TRUE)
# make a linear model to predict bodyfat from age and weight
m2 <- lm(BODYFAT ~ AGE + WEIGHT)
# Variation after adjusting for age and weight
var(mean(BODYFAT)+m2$residuals)
# Age and weight together account for this amount of the variation
summary(m2)$r.squared
# Summary of both variables and their contribution to the model
summary(m2)


# 4.3 - BMI
# plot the bodyfat according to the BMI
plot(BODYFAT ~ BMI)
# make a linear model to predict bodyfat from age
m3 <- lm(BODYFAT ~ BMI)
# plot the prediction on the graph
abline(m3)
# Variation after adjusting for BMI
var(mean(BODYFAT)+m3$residuals)
# BMI is responsible for this amount of the variation
summary(m3)$r.squared
# Summary of BMI and the contribution to the model
summary(m3)



# 4.4 - Full model
# make a linear model to predict bodyfat from all the variables
m4 <- lm(BODYFAT ~ ., data = initial_data)
# how much variation is there between the prediction and the actual value
var(mean(BODYFAT)+m4$residuals)
# All data together accounts for this amount of the variation
summary(m4)$r.squared
# Summary of all the variables and their contribution to the model
summary(m4)


# 4.5 - Density
# plot the bodyfat according to the Density
plot(BODYFAT ~ DENSITY)
# make a linear model to predict bodyfat from DENSITY
m5 <- lm(BODYFAT ~ DENSITY)
# plot the prediction on the graph
abline(m5)
# Variation after adjusting for DENSITY
var(mean(BODYFAT)+m5$residuals)
# DENSITY is responsible for this amount of the variation
summary(m5)$r.squared
# Summary of DENSITY and the contribution to the model
summary(m5)


# 4.6 - Full model without Density
# linear model to predict body from all the variables except Density
m6 <- lm(BODYFAT ~ . -DENSITY, data = initial_data)
# how much variation is there between the prediction and the actual value
var(mean(BODYFAT) + m6$residuals)
# The model accounts for this amount of the variation
summary(m6)$r.squared
# Summary of all the variables and their contribution to the model
summary(m6)
