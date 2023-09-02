# Import necessary libraries
library(kohonen)
library(ggplot2)
library(MASS)
library(Hmisc)
library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(ROCR)

# DataPreprocessing (Question 1)
# Load the dataset
cw.all <- read.csv('creditworthiness.csv')

# check for null values
sum(is.na(cw.all))

# Classified the data with and without credit.rating
cw.known <- subset(cw.all, credit.rating > 0)
cw.unknow <- subset(cw.all, credit.rating == 0)

set.seed(123)
# splite the data into testing and training
cw.train <- cw.known[1:(nrow(cw.known)/2),]
cw.test <- cw.known[-(1:(nrow(cw.known)/2)),]

# Create a data frame for the hypothetical customer
medianCust = data.frame()
newData =c(0,1,1,0,3,0,3,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
medianCust = rbind(medianCust , newData)
colnames(medianCust) = names(cw.known)[-46]

# Question 2: Decision tree & random forest
train.rpart=rpart(factor(credit.rating)~., data=cw.train)
print(train.rpart)

# To predict the credit rating of a hypothetical “median” customer.
cust.pred <- predict(train.rpart, medianCust, type = 'class')
cust.pred

# Make predictions on the test set using the decision tree model
test.pred <- predict(train.rpart, cw.test, type = 'class')

# Create the confusion matrix
confusionDT <- table(test.pred, cw.test$credit.rating)
print(confusionDT)
# Calculate the overall accuracy rate
accuracyDT <- sum(diag(confusionDT)) / sum(confusionDT)
print(accuracyDT)

# get the count of all classes in credit.rating using the table() function
beforeCountFreq = table(cw.train$credit.rating)

#find the probability of each class
beforeClassProb = beforeCountFreq /sum(beforeCountFreq)

#calculate entropy (before split)
beforeEntropy = sum( beforeClassProb * log2(beforeClassProb))

# Functionary == 0
countFreq0 = table(cw.train$credit.rating[cw.train$functionary == 0])
classProb0 = countFreq0/sum(countFreq0)
(functionaryEnt0 = -sum(classProb0 * log2(classProb0)))

# Functionary == 1
countFreq1 = table(cw.train$credit.rating[cw.train$functionary == 1])
classProb1 = countFreq1/sum(countFreq1)
(functionaryEnt1 = -sum(classProb1 * log2(classProb1)))

ent = (beforeEntropy - (
  functionaryEnt0 * sum(countFreq0) +
    functionaryEnt1 * sum(countFreq1)
) /
  sum(sum(countFreq0) + sum(countFreq1)))
print(ent)

# Random Forest model
rf.cw.train = randomForest(factor(credit.rating)~., data=cw.train)
rf.pred = predict(rf.cw.train, cw.test[,-46])

# Confusion matrix
confusionRF = with(cw.test, table(rf.pred,credit.rating))
# Overall accuracy rate
accuarcyRF = sum(diag(confusionRF))/sum(confusionRF)
accuarcyRF

# Fit to a model using randomForest after the tuning
RFTuned.cw.train = randomForest(factor(credit.rating)~.,data=cw.train, mtry=12,ntree=500,stepFactor=2,improve=0.2)

RFTuned.pred = predict(RFTuned.cw.train, cw.test[,-46])

# Confusion matrix
confusionRFTuned = with(cw.test, table(RFTuned.pred, credit.rating))
confusionRFTuned

# Overall accuracy rate
accuarcyTunedRF = sum(diag(confusionRFTuned))/sum(confusionRFTuned)
accuarcyTunedRF

# Question 3: SVM
svmfit = svm(factor(credit.rating)~.,data=cw.train, kernel='radial')
print(svmfit)

# Predict the credit rating of a hypothetical "median“ customer
predict(svmfit, medianCust, decision.values = TRUE)


# Predict the crefusion matrix for predicting the credit rating from the SVM on the test set
svm.pred = predict(svmfit, cw.test[,-46])

# Generate the confusion matrix
confusionSVM = with(cw.test, table(svm.pred, credit.rating))
confusionSVM
# Overall accuracy rate
accuarcySVM = sum(diag(confusionSVM))/sum(confusionSVM)
accuarcySVM

# Improve the SVM model
summary(tune.svm(credit.rating~., data=cw.train, kernel='radial',
                  cost= 10^c(0:2), gamma = 10^c(-4:-1)))

# Fit a model using SVM
svmTuned = svm(factor(credit.rating)~.,data = cw.train, kernel='radial',
               cost=100, gamma=0.0001)

# Predict the values on test set
svmTuned.pred = predict(svmTuned, cw.test[, -46])

# Produce confusion matrix
confusionTunedSVM = with(cw.test, table(svmTuned.pred, credit.rating))

# Overall accuracy rate
accuarcyTunedSVM = sum(diag(confusionTunedSVM))/sum(confusionTunedSVM)
accuarcyTunedSVM

# Question 4: Naive Bayes
nb = naiveBayes(credit.rating~., data=cw.train)
predict(nb, medianCust, type='class')
predict(nb, medianCust, type='raw')

nb.pred = predict(nb, cw.test[,-46])

confusionNB = with(cw.test, table(nb.pred, credit.rating))
confusionNB

accuarcyNB = sum(diag(confusionNB))/sum(confusionNB)
accuarcyNB

nb
# The output shows that a Naive Bayes Classifier for Discrete Predictors was fitted to the data with the formula credit.rating~. and the training data cw.train. 
# The output then shows the a-priori probabilities of the response variable credit.rating. In this case, there are three possible values for credit.rating (1, 2, and 3), and the probabilities are 0.2303772, 0.5127421, and 0.2568807, respectively. 
# The type='class' argument was used to return the predicted class labels for the median customer, while type='raw' was not specified, so it returns the raw probabilities. A confusion matrix was created by comparing the resulting predictions to the true values of the credit.rating variable in the test data, and the overall accuracy rate was calculated.


# Question 5
# Create a named vector with the variable names and their values
accuarcy <- c(accuracyDT, accuarcyRF, accuarcySVM, accuarcyNB,accuarcyTunedRF, accuarcyTunedSVM)
names(accuarcy) <- c("accuracyDT", "accuracyRF", "accuracySVM", "accuracyNB","accuarcyTunedRF", "accuarcyTunedSVM")

# Find the name of the variable with the largest value
largest_var <- names(accuarcy)[which.max(accuarcy)]

# Print the name of the variable with the largest value
cat("The variable with the largest value is", largest_var, "with a value of", accuarcy[largest_var], "\n")

#Among the classifiers, only decision tree has the accuracy of 0.612, and the rest classifiers random forest, SVM and Naive Bayes have much lower accuracy which mean they have a poor performance. 
ranked_accuarcy <- accuarcy[order(accuarcy, decreasing = TRUE)]
ranked_accuarcy

# Question 6: logistic regression
glm.fit <- glm((credit.rating==1)~., data=cw.train,family=binomial)

options(width=130)
summary(glm.fit)

# Make predictions for test data
glm.pred <- predict(glm.fit, newdata=cw.test, type="response")

# Convert predicted probabilities to class labels
glm.pred.class <- ifelse(glm.pred > 0.5, 1, 2)

# Calculate accuracy
accuracy <- mean(glm.pred.class == cw.test$credit.rating)
accuracy

# The output displays the coefficients for each independent variable, along with the standard error, z-value, and p-value. The coefficients represent the estimated change in the log odds of having good credit for a one unit increase in the corresponding independent variable, holding all other variables constant. The z-value and p-value indicate the statistical significance of each coefficient, with smaller p-values indicating stronger evidence against the null hypothesis that the coefficient is equal to zero.
# Predictors with low p-values are considered statistically significant. In the given output, we can see that functionary, re.balanced..paid.back..a.recently.overdrawn.current.account, gender, age and recently.defaulted all have very low p-values, indicating that they are significant predictors of credit rating.
# However, some of the variables have very large standard errors, such as Intercept and FI3O.credit.score, which could indicate that they are not statistically significant.


summary(tune.svm((credit.rating==1)~., data=cw.train,kernel='radial',cost=10^c(-2:2),gamma=10^c(-4:1),type='C'))

(svm2 = svm(I(credit.rating==1)~., data=cw.train, type='C'))

# Predict the values on test set SVM
svm.fit.pred = predict(svm2, cw.test[,-46], decision.values= TRUE)
# Confusion matrix
confusionSVM = prediction(-attr(svm.fit.pred,'decision.values'),cw.test$credit.rating==1)

# Create rocs curve based on prediction
rocsSVM <- performance(confusionSVM, 'tpr','fpr')

# Predict the values on test set[GLM]
glm.fit.pred = predict(glm.fit, cw.test[,-46])
# Confusion matrix
confusionGLM = prediction(glm.fit.pred, cw.test$credit.rating==1)
# Create rocs curve based on prediction
rocsGLM <- performance(confusionGLM, 'tpr', 'fpr')

# Plot the graph
plot(rocsGLM, col='red')
plot(rocsSVM, col='blue', add=TRUE)
abline(0,1,lty=3)

# Add the legend to the graph
legend(0.6, 0.6, c('glm', 'svm'), col=c("red", "blue"), lty=1:1)



