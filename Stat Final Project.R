## Project:  STA 215, Fall 2024, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Set Working Directory 
setwd("H:/stat215")

# Load data 
library(readr)
raw_data <- read_csv("dataset.csv")
dataset <- na.omit(raw_data)
View(dataset)
#including our data set is what allows the script to analyze and complete all of these tests#
##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
#This command generates a frequency table for the funding variable.#
table(dataset$funding)

#This command generates a frequency table for the salary variable#
table(dataset$salary)

#This provides a summary of the homeless variable, including minimum, first quartile (Q1), median, mean, third quartile (Q3), and maximum values.#
summary(dataset$homeless)
sd(dataset$homeless)

#This provides summary statistics for the reduced_lunch variable. Similar to the homeless summary, it shows the min, max, median, and mean of reduced_lunch, which may represent a binary indicator. Since it's numeric, this also includes quartiles, which can provide insights into the distribution.# 
summary(dataset$reduced_lunch)

#This calculates the standard deviation of the reduced_lunch variable.#
sd(dataset$reduced_lunch)




##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
#This boxplot compares the distribution of the homeless variable across different levels of the funding variable.#
boxplot(homeless ~ funding, data = dataset)

#This command performs an Analysis of Variance (ANOVA) to assess whether there are significant differences in the means of the homeless variable across different funding categories#
anova <- aov(homeless ~ funding, data = dataset)

#This command outputs the summary of the ANOVA mode#
summary(anova)





##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
#This command creates a scatter plot where the x-axis represents reduced_lunch and the y-axis represents homeless.#
plot(dataset$reduced_lunch, dataset$homeless)

#meany <- mean(dataset$homeless) calculates the mean of the homeless variable (average homelessness rate across all observations).#
#meanx <- mean(dataset$reduced_lunch) calculates the mean of the reduced_lunch variable (average reduced lunch rate across all observations).#
meany <- mean(dataset$homeless)
meanx <- mean(dataset$reduced_lunch)

#abline(h = meanx, col = "black") adds a horizontal line at the mean of reduced_lunch on the scatter plot. This helps to visually show the average value of reduced_lunch across the dataset.#
#abline(v = meany, col = "black") adds a vertical line at the mean of homeless on the scatter plot. This helps to visually show the average value of homeless across the dataset.#
abline(h = meanx, col = "black")
abline(v = meany, col = "black")

#This performs a linear regression of homeless on reduced_lunch, where homeless is the dependent variable and reduced_lunch is the independent variable.#
linear_relationship <- lm(homeless ~ reduced_lunch, data = dataset)

#This command provides a summary of the linear regression model.#
summary(linear_relationship)

#This adds the fitted regression line to the scatter plot (abline(linear_relationship, col = "red")), making it easier to visualize the linear relationship between reduced_lunch and homeless.#
abline(linear_relationship, col = "red")





##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$reduced_lunch, residuals(linear_relationship))


# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
#This command creates a contingency table (cross-tabulation) of two categorical variables: funding and salary.#
table(dataset$funding, dataset$salary)

#This performs a Chi-squared test of independence on the contingency table created earlier#
chisq.test(table(dataset$funding, dataset$salary))
