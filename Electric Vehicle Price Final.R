# Load necessary libraries
library(tidyverse)
install.packages("caret")
library(caret)
library(randomForest)
library(ranger)
library(coin)
library(gbm)
library(ggplot2)
library(ggtext)
library(lsr)  # For etaSquared function
library(fastDummies)
library(car)
library(glmnet)
library(dplyr)
library(corrplot)

# Load the data
cars_data<-read.csv("D:/IIMK/DSBD course material/Capstone Project/Electric Vehicles.csv",header=T)
attach(cars_data)
names(cars_data)
summary(cars_data)
dim(cars_data)
str(cars_data)
head(cars_data)

# Checking the correlation
a<-cor(cars_data[-c(2,3,15,16)]) 
# correlation matrix neglecting 2nd,3rd,14th,15th column as it is irrelevant
a

head(round(a,2))

corrplot(a, method="circle")
corrplot(a, method="number")
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(a)
head(p.mat[, 1:14])
corrplot(a, type="full", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

corrplot(a, type="full", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")

# Set seed for reproducibility
set.seed(123)

# Split the data into training (60%) and test (40%) sets
trainIndex <- createDataPartition(cars_data, p = 0.6, list = FALSE)
train_data <- cars_data[trainIndex, ]
test_data <- cars_data[-trainIndex, ]

# train_indices <- createDataPartition(1:nrow(cars_data), p = 0.6, list = FALSE)
# 
# # Split the data into train and test sets
# train_data <- cars_data[train_indices, ]
# test_data <- cars_data[-train_indices, ]



# Verify the split
dim(cars_data)
dim(train_data)
dim(test_data)


#Predicting Germany Price
# Multiple Linear Regression Model with "Germany_price_before_incentives " as dependent variable
mod_1=lm(Germany_price_before_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Estimated_US_Value+Netherlands_price_before_incentives+UK_price_after_incentives+Number_of_seats,data=train_data)
summary(mod_1)


# Multiple Linear Regression Model with "Germany_price_before_incentives " as dependent variable
mod_2=lm(Germany_price_before_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+UK_price_after_incentives+Number_of_seats,data=train_data)
summary(mod_2)

# Multiple Linear Regression Model with "Germany_price_before_incentives " as dependent variable
mod_3=lm(Germany_price_before_incentives~Battery.KWHR+Price_per_km_of_range+Efficiency+UK_price_after_incentives+Number_of_seats,data=train_data)
summary(mod_3)

# Multiple Linear Regression Model with "Germany_price_before_incentives " as dependent variable
mod_4=lm(Germany_price_before_incentives~Battery.KWHR+Price_per_km_of_range+Efficiency+Number_of_seats,data=train_data)
summary(mod_4)

# Multiple Linear Regression Model with "Germany_price_before_incentives " as dependent variable
mod_5=lm(Germany_price_before_incentives~Battery.KWHR+Price_per_km_of_range+Number_of_seats,data=train_data)
summary(mod_5)

# Multiple Linear Regression Model with "Germany_price_before_incentives " as dependent variable
mod_6=lm(Germany_price_before_incentives~Battery.KWHR+(Price_per_km_of_range*Range)+Number_of_seats,data=train_data)
summary(mod_6)

# Multiple Linear Regression Model with "Germany_price_before_incentives " as dependent variable
mod_7=lm(Germany_price_before_incentives~(Battery.KWHR*Battery.KWHR)+Price_per_km_of_range+Range+Number_of_seats,data=train_data)
summary(mod_7)
mod_8=lm(Germany_price_before_incentives~(Battery.KWHR*Battery.KWHR)+Price_per_km_of_range+Range,data=train_data)
summary(mod_8)
















#Predicting Netherlands Price
# Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_11=lm(Netherlands_price_before_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Estimated_US_Value+Germany_price_before_incentives+UK_price_after_incentives+Number_of_seats,data=train_data)
summary(mod_11)


#  Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_21=lm(Netherlands_price_before_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+UK_price_after_incentives+Number_of_seats,data=train_data)
summary(mod_21)

# Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_31=lm(Netherlands_price_before_incentives~Battery.KWHR+Price_per_km_of_range+Range+Efficiency,data=train_data)
summary(mod_31)

# Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_41=lm(Netherlands_price_before_incentives~(Battery.KWHR*Battery.KWHR)+Price_per_km_of_range+Range+Efficiency,data=train_data)
summary(mod_41)

# Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_51=lm(Netherlands_price_before_incentives~Battery.KWHR+(Price_per_km_of_range*Range)+Efficiency,data=train_data)
summary(mod_51)

# Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_61=lm(Netherlands_price_before_incentives~Battery.KWHR+(Price_per_km_of_range*Range)+(Efficiency*Range),data=train_data)
summary(mod_61)

# Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_71=lm(Netherlands_price_before_incentives~Battery.KWHR+Price_per_km_of_range+Range+Efficiency+Germany_price_before_incentives+UK_price_after_incentives+Estimated_US_Value,data=train_data)
summary(mod_71)
# Multiple Linear Regression Model with "Netherlands_price_before_incentives " as dependent variable
mod_81=lm(Netherlands_price_before_incentives~Battery.KWHR+Price_per_km_of_range+Range+Efficiency+Germany_price_before_incentives,data=train_data)
summary(mod_81)



# Multiple Linear Regression Model UK Price
mod_31=lm(UK_price_after_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Estimated_US_Value+Germany_price_before_incentives+Netherlands_price_before_incentives+Number_of_seats,data=train_data)
summary(mod_31)

mod_32=lm(UK_price_after_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Number_of_seats,data=train_data)
summary(mod_32)

mod_33=lm(UK_price_after_incentives~Battery.KWHR+Price_per_km_of_range+Range+Efficiency,data=train_data)
summary(mod_33)

mod_34=lm(UK_price_after_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Number_of_seats+Germany_price_before_incentives,data=train_data)
summary(mod_34)


mod_35=lm(UK_price_after_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Number_of_seats+Netherlands_price_before_incentives,data=train_data)
summary(mod_35)

mod_36=lm(UK_price_after_incentives~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Number_of_seats+Estimated_US_Value,data=train_data)
summary(mod_36)

mod_37=lm(UK_price_after_incentives~Price_per_km_of_range+X0.100+Range+Fastcharge+Estimated_US_Value,data=train_data)
summary(mod_37)


#Predicting US Price:

# Multiple Linear Regression Model US Price
mod_41=lm(Estimated_US_Value~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Germany_price_before_incentives+Netherlands_price_before_incentives+UK_price_after_incentives+Number_of_seats,data=train_data)
summary(mod_41)

# Multiple Linear Regression Model US Price
mod_42=lm(Estimated_US_Value~Battery.KWHR+Price_per_km_of_range+X0.100+Top_Speed+Range+Efficiency+Fastcharge+Number_of_seats,data=train_data)
summary(mod_42)


# Multiple Linear Regression Model US Price
mod_43=lm(Estimated_US_Value~Battery.KWHR+Price_per_km_of_range+Efficiency+Germany_price_before_incentives,data=train_data)
summary(mod_43)

# Multiple Linear Regression Model US Price
mod_44=lm(Estimated_US_Value~Battery.KWHR+Price_per_km_of_range+Efficiency+UK_price_after_incentives,data=train_data)
summary(mod_44)

# Multiple Linear Regression Model US Price
mod_45=lm(Estimated_US_Value~Battery.KWHR+Price_per_km_of_range+Efficiency+Netherlands_price_before_incentives,data=train_data)
summary(mod_45)