library(ggplot2)
library(dplyr)
library(nnet)
library(randomForest)
library(class)

wholesale_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv", header = TRUE)

selected_data <- wholesale_data %>%
  select(Region, Fresh, Milk, Grocery, Frozen, Detergents_Paper, Delicassen)

set.seed(123)
sample_size <- floor(0.7 * nrow(selected_data))
train_indices <- sample(seq_len(nrow(selected_data)), size = sample_size)

train_data <- selected_data[train_indices, ]
test_data <- selected_data[-train_indices, ]
train_data$Region <- as.factor(train_data$Region)
test_data$Region <- as.factor(test_data$Region)

logistic_model <- multinom(Region ~ ., data = train_data)
predictions <- predict(logistic_model, newdata = test_data)
accuracy <- mean(predictions == test_data$Region)
print(paste("Accuracy of Logistic Regression Model:", accuracy))
conf_matrix <- table(predictions, test_data$Region)
print("Confusion Matrix:")
print(conf_matrix)


rf_model <- randomForest(Region ~ ., data = train_data)
rf_predictions <- predict(rf_model, newdata = test_data)
rf_accuracy <- mean(rf_predictions == test_data$Region)
print(paste("Accuracy of Random Forest Model:", rf_accuracy))
rf_conf_matrix <- table(rf_predictions, test_data$Region)
print("Confusion Matrix for Random Forest:")
print(rf_conf_matrix)


k <- 5  # Number of neighbors (you can adjust this)
knn_model <- knn(train = train_data[, -1], test = test_data[, -1], cl = train_data$Region, k = k)

# Evaluate the KNN model
knn_accuracy <- mean(knn_model == test_data$Region)
print(paste("Accuracy of KNN Model (k =", k, "):", knn_accuracy))

# Create a confusion matrix for KNN
knn_conf_matrix <- table(knn_model, test_data$Region)
print("Confusion Matrix for KNN:")
print(knn_conf_matrix)







