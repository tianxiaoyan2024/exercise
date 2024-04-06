# --------------------------------------------
# Script Name: Basic R (object-oriented programming)
# Purpose: This scribes how to use R packages and 
#          how to write effective R code.

# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-05
#
# --------------------------------------------
install.packages("caret")
install.packages("randomForest")
install.packages("fastDummies")

library(fastDummies)
library(caret)  # 提供特征选择和模型训练的工具
library(randomForest)  # 提供随机森林算法
library(GGally)  # 提供ggpairs函数用于特征可视化


data(mtcars)

# Inspect data structure

str(mtcars)

#Check for missing values in the entire data frame

any_missing_in_data <- any(is.na(mtcars))
cat("Does the data frame have missing data?", any_missing_in_data)

#Note:Although it is numerical in the original dataset of 'mtcars', 
#     the 'am' list shows the type of transmission (0=automatic, 1=manual).
#     Similarly, the 'Vs' represents the engine type of the vehicle,
#     so I chose to consider them as categorical variables.

mtcars$am <- as.factor(mtcars$am)
mtcars$vs <- as.factor(mtcars$vs)

# conversion to dummy (binary) variables with 'fastDummies'

mtcars_with_dummies <- dummy_cols(mtcars, select_columns = c("am", "vs"))
str(mtcars_with_dummies)

#splitting 'mtcars_with_dummies' into training and test datasets
set.seed(123)
row_indices <- 1:nrow(mtcars)
trainingIndex <- createDataPartition(row_indices, p = 0.8, list = FALSE)
trainingSet <- mtcars[trainingIndex, ]
testSet <- mtcars[-trainingIndex, ]

#
typeof(trainingSet)
trainingSet <- as.data.frame(trainingSet)
x <- as.matrix(trainingSet[, -1])
y <- trainingSet$mpg
featurePlot(x, y, plot="box",
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))

library(ggplot2)

# 假设您已经有了一个数据框 trainingSet
# 将 trainingSet 转换为数据框（如果它还不是的话）
trainingSet <- as.data.frame(trainingSet)

# 提取特征和目标
x <- as.matrix(trainingSet[, -1])  # 特征
y <- trainingSet$mpg  # 目标

# 将特征矩阵转换为数据框，以便于使用 ggplot2
features_df <- as.data.frame(x)

# 添加目标列到特征数据框中
features_df$mpg <- y

# 使用 ggplot2 绘制箱线图
p <- ggplot(features_df, aes(x = features_df[, 1], y = mpg)) +  # 这里我们只绘制第一个特征与目标之间的箱线图
  geom_boxplot() +
  labs(title = "Boxplot of MPG vs. First Feature", x = "First Feature", y = "MPG") +
  theme_minimal()

# 显示图形
print(p)


library(ggplot2)
library(dplyr)

# 假设 trainingSet 已经是数据框，并且最后一列是目标变量 mpg
# 计算每个特征与目标变量 mpg 的相关性
correlations <- cor(trainingSet[, -ncol(trainingSet)], trainingSet$mpg)

# 找到相关性绝对值最大的特征的索引
most_important_feature_index <- which.max(abs(correlations))

# 提取最重要的特征和目标变量
most_important_feature <- trainingSet[, most_important_feature_index + 1]
most_important_target <- trainingSet$mpg

# 使用 ggplot2 绘制最重要的特征与目标之间的箱线图
p <- ggplot(data = data.frame(most_important_feature, most_important_target), 
            aes(x = most_important_feature, y = most_important_target)) +
  geom_boxplot() +
  labs(title = "Boxplot of MPG vs. Most Important Feature", 
       x = "Most Important Feature", 
       y = "MPG") +
  theme_minimal()

print(p)


featurePlot(data.frame(x, y), columns = 1:ncol(x), title = "Initial Feature Visualization")
