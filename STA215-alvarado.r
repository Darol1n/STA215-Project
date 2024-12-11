## Project:  STA 215, Spring 2024, Final Project
# Located:   Posit Cloud
# File Name: lorde songs
# Date:      2024_2_29
# Who:       Kevin Janas



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
setwd("H:/sta215")
raw_data <- read_delim("raw_data.csv")
data <- na.omit(raw_data)



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################


# Length of the song
mean(data$pop_2020)
sd(data$pop_2020)
hist(data$pop_2020)
summary(data$pop_2020)
min(data$pop_2020)
max(data$pop_2020)


# Streams per song
mean(data$img_rate)
sd(data$img_rate)
hist(data$img_rate)
summary(data$img_rate)
min(data$img_rate)
max(data$img_rate)


# Statistic on release name
table(data$pop_estimate_2020)

# Statistic on emotion felt during the song
table(data$emotion_me)




##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
boxplot(data$violent_crime_rate_2019 ~ data$pop_over_500k_2021)
anova <- aov(data$violent_crime_rate_2019 ~ data$pop_over_500k_2021)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
plot(data$pop_2020, data$img_rate)

# add x line and y line for means
meanx <- mean(data$pop_2020)
meany <- mean(data$img_rate)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(data$img_rate ~ data$pop_2020, data = data)
summary(linear_relationship)


# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(data$pop_2020, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$pop_over_500k_2021, data$vcrime10)
chisq.test(data$pop_over_500k_2021, data$vcrime10)