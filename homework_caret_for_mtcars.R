# --------------------------------------------
# Script Name: Basic R (object-oriented programming)
# Purpose: This scribes how to use R packages and 
#          how to write effective R code.

# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-03
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# Install necessary packages

install.packages("caret")
install.packages("randomForest")
install.packages("fastDummies")

# Load libraries

library(fastDummies)
library(caret)  
library(randomForest)  


# Load the mtcars dataset

data(mtcars)

# Inspect data structure

str(mtcars)

#Check for missing values in the entire data frame

any_missing_in_data <- any(is.na(mtcars))
cat("Does the data frame have missing data?", any_missing_in_data)

# Convert 'am' and 'vs' columns to factors since they represent categorical variables
# 'am' indicates the type of transmission (0=automatic, 1=manual)
# 'vs' represents the engine type of the vehicle

mtcars$am <- as.factor(mtcars$am)
mtcars$vs <- as.factor(mtcars$vs)

# Convert categorical variables to dummy (binary) variables using 'fastDummies'

mtcars_with_dummies <- dummy_cols(mtcars, select_columns = c("am", "vs"))
str(mtcars_with_dummies)

# Get important features to train the model 
# A.Selecting variable importance by visualization
# Method 1: Visualize variable importance using box plots

typeof(mtcars_with_dummies)
mtcars_dummies <- as.data.frame(mtcars_with_dummies)
x <- as.matrix(mtcars_dummies[, -1])
y <- mtcars_dummies$mpg
featurePlot(x, y, plot="box",
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))
# Method 2: Calculate correlation between features and the target variable 'mpg'
# Convert the feature matrix to a data frame for easier use with ggplot2
features_df <- as.data.frame(x)
# Add the target column to the feature data frame
features_df$mpg <- y
# Calculate the correlation between each feature and the target variable 'mpg'
correlations <- cor(mtcars_dummies[, -ncol(mtcars_dummies)], mtcars_dummies$mpg)
# Find the index of the feature with the highest absolute correlation
most_important_feature_index <- which.max(abs(correlations))
# Extract the most important feature and the target variable
most_important_feature <- features_df[, most_important_feature_index]
most_important_target <- features_df$mpg
# Plot a boxplot of the most important feature versus the target using ggplot2
p <- ggplot(data = data.frame(most_important_feature, most_important_target), 
            aes(x = most_important_feature, y = most_important_target)) +
  geom_boxplot() +
  labs(x = "Most Important Feature", y = "MPG", title = "Boxplot of Most Important Feature vs. MPG")

print(p)

# B.Feature selection with RFE 
# Set the seed for reproducibility
set.seed(100)
ctrl <- rfeControl(functions=rfFuncs, # 使用随机森林作为模型
                   method="cv",        # 使用交叉验证
                   number=10,          # 10折交叉验证
                   repeats=5,          # 重复5次
                   verbose=FALSE)      # 不显示详细信息

# Perform feature selection using RFE

rf_model <- rfe(x, y, sizes=1:ncol(x), rfeControl=ctrl)
print(rf_model)

#splitting 'mtcars_with_dummies' into training and test datasets

set.seed(123)
row_indices <- 1:nrow(mtcars)
trainingIndex <- createDataPartition(row_indices, p = 0.8, list = FALSE)
mtcars_trainingSet <- mtcars[trainingIndex, ]
mtcars_testSet <- mtcars[-trainingIndex, ]

# Training and turning the model
# Set the training control parameters
rControl <- trainControl(method = "cv", 
                         number = 10, 
                         savePredictions = "final",
                         classProbs = FALSE, 
                         summaryFunction = defaultSummary)
# Train a random forest model
rf_fit <- train(mpg ~ ., 
                data = mtcars_trainingSet, 
                method = "rf", 
                metric = "RMSE",
                tuneLength = 5, 
                preProcess = c("center", "scale"),
                trControl = rControl,
                verbose = FALSE)

# Predict on the test data
predictions_rpart <- predict(rf_fit, newdata = mtcars_testSet)
# Evaluate regression performance
Metrics::rmse(mtcars_testSet$mpg, predictions_rpart)


# Plot the actual values against the predicted values

plot(mtcars_testSet$mpg, predictions_rpart, 
     xlab = "Actual MPG", ylab = "Predicted MPG", main = "Actual vs. Predicted MPG")
abline(a = 0, b = 1, col = "red", lty = 2) # Add a reference line for perfect prediction


