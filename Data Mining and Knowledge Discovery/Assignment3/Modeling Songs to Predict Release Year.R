### LOAD LIBRARIES - install with:
library(kohonen)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(MASS)
library(Hmisc)
library(caret)
library(rpart)
library(xgboost)
library(tidyverse)
library(tree)
library(randomForest)
library(e1071)
library(caTools)
library(caret)
library(glmnet)
library(parallel)
library(e1071)

# Load the dataset
data <- read.csv("./YearPredictionMSD.txt", sep=",", header = FALSE)

# Check for missing values
sum(is.na(data))

# Check the class type
sapply(data, class)

# Plot histograms of each variable to see their distributions
hist(data)

# Create a data frame that counts the number of songs for each year
nsongs <- as.data.frame(table(data$V1))
# Rename the columns in the data frame
names(nsongs) <- c("Year", "nsongs")
# Plot a bar chart with the number of songs for each year
barplot(nsongs$nsongs, names.arg=nsongs$Year, xlab="Year", ylab="Number of songs")
# Compute the correlation matrix:
cor_matrix <- cor(data)
# Extract the absolute values of the correlations for each attribute:
abs_cor <- abs(cor_matrix["V1",])
# Sort the absolute correlations in descending order:
sorted_abs_cor <- sort(abs_cor, decreasing = TRUE)
head(sorted_abs_cor,11)

summary(data)

boxplot(data$V1,
        ylab = "data$V1"
)

Q <- quantile(data$V1, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$V1)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿
eliminated <- subset(data, data$V1 > (Q[1] - 1.5*iqr) & data$V1 < (Q[2]+1.5*iqr))

boxplot(eliminated$V1,
        ylab = "data$V1"
)

ggplot(eliminated) +
  aes(x = eliminated$V1) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()


# Create a data frame that counts the number of songs for each year
nsongs <- as.data.frame(table(eliminated$V1))
# Rename the columns in the data frame
names(nsongs) <- c("Year", "nsongs")
# Plot a bar chart with the number of songs for each year
barplot(nsongs$nsongs, names.arg=nsongs$Year, xlab="Year", ylab="Number of songs")


#-----------Split the data into train and test sets -------------
# Split the data into train and test sets
train_data <- eliminated[1:463715, ]
test_data <- eliminated[463716:nrow(eliminated), ]

# Scale the features using the mean and standard deviation of the train set
train_mean <- apply(train_data[, -1], 2, mean)
train_sd <- apply(train_data[, -1], 2, sd)
train_data[, -1] <- scale(train_data[, -1], center = train_mean, scale = train_sd)
test_data[, -1] <- scale(test_data[, -1], center = train_mean, scale = train_sd)

# Convert the target variable to numeric
train_data$V1 <- as.numeric(train_data$V1)
test_data$V1 <- as.numeric(test_data$V1)


# -------------------------Linear model using correlation------------------------------------

# Using the correlation to train and test
selected_data <- eliminated[, c(1,2, 7, 4, 64, 41, 8, 68, 47, 37, 70)]

# # Split the data into train and test sets
train_data_cor <- selected_data[1:463715, ]
test_data_cor <- selected_data[463716:nrow(selected_data), ]

# Scale the features using the mean and standard deviation of the train set
train_mean <- apply(train_data_cor[, -1], 2, mean)
train_sd <- apply(train_data_cor[, -1], 2, sd)
train_data_cor[, -1] <- scale(train_data_cor[, -1], center = train_mean, scale = train_sd)
test_data_cor[, -1] <- scale(test_data_cor[, -1], center = train_mean, scale = train_sd)

# Convert the target variable to numeric
train_data_cor$V1 <- as.numeric(train_data_cor$V1)
test_data_cor$V1 <- as.numeric(test_data_cor$V1)


# Fit a linear regression model
model_5 <- lm(V1 ~ ., data = train_data_cor)
# Make predictions on the test set
predictions_5 <- predict(model_5, newdata = test_data_cor)
# Evaluate the performance of the model
rmse <- sqrt(mean((test_data_cor$V1 - predictions_5)^2))
print(rmse)
# Calculate the mean absolute error (MAE) between the predicted and actual values
mae_5 <- mean(abs(predictions_5 - test_data_cor$V1))
mae_5

# Calculate the accuracy of the predictions
accuracy_5 <- mean(test_data_cor$V1 == round(predictions_5))
print(accuracy_5)


# ---------------------linear regression using all data -----------------------
# Fit a linear regression model
model_5 <- lm(V1 ~ ., data = train_data)
# Make predictions on the test set
predictions_5 <- predict(model_5, newdata = test_data)
# Evaluate the performance of the model
rmse <- sqrt(mean((test_data$V1 - predictions_5)^2))
print(rmse)
# Calculate the mean absolute error (MAE) between the predicted and actual values
mae_5 <- mean(abs(predictions_5 - test_data$V1))
mae_5

ggplot(data.frame(actual = test_data$V1, predicted = predictions_5),
       aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Actual Release Year", y = "Predicted Release Year") +
  ggtitle("Actual vs. Predicted Release Year (Linear Regression)")

# Calculate the accuracy of the predictions
accuracy_5 <- mean(test_data$V1 == round(predictions_5))
print(accuracy_5)


# -------------------------Decision tree model-------------------------------
# Fit a decision tree model
model_dt <- rpart(V1 ~ ., data = train_data)
# Make predictions on the test set
predictions_dt <- predict(model_dt, newdata = test_data)
# Evaluate the performance of the model
rmse <- sqrt(mean((test_data$V1 - predictions_dt)^2))
print(rmse)

# Calculate the mean absolute error (MAE) between the predicted and actual values
mae_5 <- mean(abs(predictions_dt - test_data$V1))
mae_5

# Create a scatterplot of the predicted vs. actual values
ggplot(test_data, aes(x = V1, y = predictions_dt)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Decision Tree Model") + 
  xlab("Actual Value") + 
  ylab("Predicted Value")

accuracy_dt <- mean(test_data$V1 == round(predictions_dt))
print(accuracy_dt)


# -------------------------SVM model-------------------------------
# Split the training data into multiple parts
parts <- split(train_data, f=rep(1:4, each=nrow(train_data) %/% 4))
# Train the SVM model in parallel
cl <- makeCluster(4)
clusterExport(cl, c("train_data", "svm"))
model_svm_list <- parLapply(cl, parts, function(x) svm(V1 ~ ., data = x, type = "C-classification", kernel = "linear"))
stopCluster(cl)
# Make predictions on the testing set using each of the models in `model_svm_list`
predictions_list <- lapply(model_svm_list, function(model) predict(model, newdata = test_data))
# Combine the predictions into one
predictions <- unlist(predictions_list)
# Convert the predictions vector to a numeric variable
predictions <- as.numeric(as.character(predictions))
# Calculate the RMSE and MAE
# Calculate the MSE
mse <- mean((predictions - test_data$V1)^2)
# Calculate the RMSE
rmse <- sqrt(mse)
# Calculate the MAE
mae <- mean(abs(predictions - test_data$V1))# Display the RMSE and MAE
# Calculate the accuracy
accuracy <- mean(predictions == test_data$V1)
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("Accuracy:", accuracy, "\n")

#display
ggplot(data.frame(actual = round(test_data$V1), 
                  predicted = round(predictions)),
       aes(x = actual, y = predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Actual Release Year", y = "Predicted Release Year") +
  ggtitle("Actual vs. Predicted Release Year (SVM)")



# ----------------------------------------------------------------
# Confusion maxtrix models
factor_data$V1 <- as.factor(data$V1)
train_data_fac <- factor_data[1:train_size, ]
test_data_fac <- factor_data[(train_size+1):(train_size+test_size), ]


# -------------------------random forest--------------------------------
# Train a random forest model
model_rf <- randomForest(V1 ~ ., data = train_data_fac)
# Make predictions on the testing set
predictions_rf <- predict(model_rf, newdata = test_data_fac)
# Evaluate the performance of the model
confusionMatrix(predictions_rf, test_data_fac$V1)

ggplot(data.frame(actual = test_data_fac$V1, predicted = predictions_rf),
       aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(x = "Actual Release Year", y = "Predicted Release Year") +
  ggtitle("Actual vs. Predicted Release Year (Random Forest)")



#--------------------lasso regression -------------------------------------------------------------
#provides better prediction accuracy than linear regression as data values 
#are shrunk towards a central point as the mean

x <- data.matrix(train_data[,c(2:91)])
y <- train_data$V1
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)
#finding optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
new = data.matrix(test_data[,c(2:91)])
pred_lasso <- predict(best_model, s = best_lambda, newx = new)

# Evaluate the performance of the model
rmse <- sqrt(mean((test_data$V1 - pred_lasso)^2))
print(rmse)

# Calculate the mean absolute error (MAE) between the predicted and actual values
mae_lasso <- mean(abs(pred_lasso - test_data$V1))
mae_lasso

# Calculate the accuracy of the predictions
accuracy_lasso <- mean(test_data$V1 == round(pred_lasso))
print(accuracy_lasso)


#--------------------------Naive Bayes------------------------------------
# Create training (80%) and test (20%) sets for the attrition data.
# Use set.seed for reproducibility
# split as shown for individual testing purposes
set.seed(123)
split <- initial_split(data, prop = .8, strata = "V1")
train <- training(split)
test  <- testing(split)

#plot the graph 
train %>% 
  select(V1) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

# create response and feature data
features <- setdiff(names(train), "Year")
x <- train[, features]
y <- train$Year

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)


#---------------------------regression trees-------------------------------------------------------------
(r.tree <- rpart(V1~.,data=train_data, method = "anova"))

plot(r.tree)
text(r.tree, digits=4)

# Make predictions on the test set
predictions_rt <- predict(r.tree, newdata = test_data)
# Evaluate the performance of the model
rmse <- sqrt(mean((test_data$V1 - predictions_rt)^2))
print(rmse)
# Calculate the mean absolute error (MAE) between the predicted and actual values
mae_rt <- mean(abs(predictions_rt - test_data$V1))
mae_rt
# Create a scatterplot of the predicted vs. actual values
ggplot(test_data, aes(x = V1, y = predictions_rt)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Actual vs Predicted Release Year (Regression Tree)") + 
  xlab("Actual Value") + 
  ylab("Predicted Value")

accuracy_rt <- mean(test_data$V1 == round(predictions_rt))
print(accuracy_rt)


#--------------------------random forest 1 Binning method------------------------------------
df <- read.table("YearPredictionMSD.txt", sep = ",", header = FALSE)
X <- df[,-1] # Features
y <- df[,1] # Target

y <- as.factor(ifelse(y < 1975, 0, 1))

set.seed(42)
train_idx <- sample(1:nrow(df), nrow(df)*0.7, replace=FALSE)
X_train <- X[train_idx,]
X_test <- X[-train_idx,]
y_train <- y[train_idx]
y_test <- y[-train_idx]

X_train_std <- scale(X_train)
X_test_std <- scale(X_test)

rf <- randomForest(X_train_std, y_train, ntree=100)
rf_pred <- predict(rf, data.frame(X_test_std), type="class")
rf_accuracy <- mean(rf_pred == y_test)

print(paste("Accuracy: ", rf_accuracy))



#--------------------------random forest 5 Binning method------------------------------------
X <- data[,-1] # Features
y <- data[,1] # Target

# Divide the years into 5 bins
#cut function to create 5 equally-sized bins based on the range of values in the y variable.
y_bins <- as.factor(cut(y, breaks = 5))

set.seed(42)
train_idx <- createDataPartition(y_bins, p = 0.7, list = FALSE)
X_train <- X[train_idx,]
X_test <- X[-train_idx,]
y_train <- y_bins[train_idx]
y_test <- y_bins[-train_idx]

X_train_std <- scale(X_train)
X_test_std <- scale(X_test)

rf <- randomForest(X_train_std, y_train, ntree=100)
rf_pred <- predict(rf, data.frame(X_test_std), type="class")
rf_accuracy <- mean(rf_pred == y_test)

print(paste("Accuracy: ", rf_accuracy))


#--------------------------SVM Binning method------------------------------------
df <- read.table("YearPredictionMSD.txt", sep = ",", header = FALSE)
X <- df[,-1] # Features
y <- df[,1] # Target

# Divide the years into 5 bins
#cut function to create 5 equally-sized bins based on the range of values in the y variable.
y_bins <- as.factor(cut(y, breaks = 5))

# Split the data into training and testing sets
set.seed(42)
train_idx <- createDataPartition(y_bins, p = 0.7, list = FALSE)
X_train <- X[train_idx,]
X_test <- X[-train_idx,]
y_train <- y_bins[train_idx]
y_test <- y_bins[-train_idx]

# Standardize the data
X_train_std <- scale(X_train)
X_test_std <- scale(X_test)

#-------------------classfication-------------------------
# -----------RF model-------
rf <- randomForest(X_train_std, y_train, ntree=100)
rf_pred <- predict(rf, data.frame(X_test_std), type="class")
rf_accuracy <- mean(rf_pred == y_test)

# ---------Train the SVM model----
svm_model <- svm(x = X_train_std, y = y_train, kernel = "radial", cost = 1, gamma = 0.01)

# Make predictions on the testing set
svm_pred <- predict(svm_model, X_test_std)
svm_accuracy <- mean(svm_pred == y_test)

# ---------KNN-------------
knn_model <- knn(X_train_std, X_test_std, y_train, k = 5)
knn_accuracy <- mean(knn_model == y_test)

# -----Train the decision tree model----
dt_model <- rpart(y_train ~ ., data = data.frame(X_train_std, y_train), method = "class")

# Make predictions on the testing set
dt_pred <- predict(dt_model, as.data.frame(X_test_std), type = "class")
dt_accuracy <- mean(dt_pred == y_test)



# Print the accuracy of the model
print(paste("RF Accuracy: ", rf_accuracy))

print(paste("SVM Accuracy: ", svm_accuracy))

print(paste("KNN Accuracy: ", knn_accuracy))

print(paste("DT Accuracy: ", dt_accuracy))


