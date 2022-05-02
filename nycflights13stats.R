#Loading all the packages 
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library("nycflights13"))


# Q1: Address the outliers for departure delay as described in the outliers lectures, using 0.997 and 0.003 as the cutoffs.  
# What percentage of data remains following the removal of these outliers?  
# Creating upper and lower bounds to get rid of all the outliers
upper_bound <- quantile(flights$dep_delay, 0.997, na.rm = TRUE)
lower_bound <- quantile(flights$dep_delay, .003, na.rm = TRUE)
out_obs <- which(flights$dep_delay > upper_bound | flights$dep_delay < lower_bound)

# This creates a new data set called 'noout' which is the data without outliers
# This data set will be used for questions 2-5
noout <- flights[-out_obs,]

# This takes the total rows of all the data, subtracts the outliers, divides by total rows, and mutiplies by 100
# Q1 = 99.45% meaning that amount of data remains after the outliers are taken out
Q1 <- (nrow(flights)- length(out_obs))/nrow(flights)*100


# Q2: Run cor.test for the relationship between departure delay and distance. 
# Because p-value < 2.2e-16 we can conclude that there is a correlation between departure delay and distance
# The correlation is -0.02527463 
Q2 <- cor.test(noout$dep_delay, noout$distance)


# Q3: Create a regression predicting departure delay from distance.
#This gave a summary of a regression ran between dep_delay based on distance (of the flight)
Q3 <- summary(lm(dep_delay ~ distance, noout))


#Q4: Calculate standardized regression coefficients with lm.beta for the summary from Q3.
#This takes the answer from Q3 and standardizes the coefficients by using the package lm.beta
Q4 <- lm.beta(lm(dep_delay ~ distance, noout))


#Q5: Create another regression, this time adding carrier to the regression from Q3.  
#This adds another viariable into the regression. The new variable is "carrier"
Q5 <- summary(lm(noout$dep_delay ~ noout$distance + noout$carrier))