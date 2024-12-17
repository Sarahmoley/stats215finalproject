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
table(dataset$funding)

table(dataset$salary)

summary(dataset$homeless)
sd(dataset$homeless)


summary(dataset$reduced_lunch)
sd(dataset$reduced_lunch)
#standard deveation for variable 1-reduced lunch#
#homeless and reduced lunch 



##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
#creates the boxplot representing the relatiosnhip between homelessness and funding#
boxplot(homeless ~ funding, data = dataset)
anova <- aov(homeless ~ funding, data = dataset)
summary(anova)





##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
plot(dataset$reduced_lunch, dataset$homeless)

#what will help us find the mean of homelessness and reduced lunch#
meany <- mean(dataset$homeless)
meanx <- mean(dataset$reduced_lunch)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(homeless ~ reduced_lunch, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")

#this will make the scatterplot to find the relationship between homelessness and reduced lunch 



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
table(dataset$funding, dataset$salary)

chisq.test(table(dataset$funding, dataset$salary))
