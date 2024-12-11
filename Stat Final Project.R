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
raw_data <- read_csv("raw_data.csv")
dataset <- na.omit(raw_data)
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table(dataset$funding)

table(dataset$salary)


summary(dataset$homeless)
sd(dataset$homeless)


summary(dataset$reduced_lunch)
sd(dataset$reduced_lunch)





##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
boxplot(homeless ~ funding, data = dataset)
anova <- aov(homeless ~ funding, data = dataset)
summary(anova)





##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
plot(dataset$quantIV, dataset$quantDV)

meany <- mean(dataset$homeless)
meanx <- mean(dataset$reduced_lunch)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(homeless ~ reduced_lunch, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")


meany <- mean(dataset_withououtlier$song_length)
meanx <- mean(dataset_withououtlier$personal_enjoyment)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(personal_enjoyment ~ song_length, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")

linear_plot <- plot(dataset_withoutlistensoutlier$number_of_listens, dataset_withoutlistensoutlier$personal_enjoyment)
print(linear_plot)
meany <- mean(dataset_withoutlistensoutlier$number_of_listens)
meanx <- mean(dataset_withoutlistensoutlier$personal_enjoyment)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(personal_enjoyment ~ number_of_listens, data = dataset_withoutlistensoutlier)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$song_length, residuals(linear_relationship))
plot(dataset_withoutlistensoutlier$number_of_listens, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$funding, dataset$salary)

chisq.test(table(dataset$funding, dataset$salary))
