# Assignment 1 Discussion
# INFO411, 2023S1
# SJ
#
# ------------------- Classification ---------------------------
# Load the required library
library(RSNNS)
# To train the MLP model to classified based on the following
# interested columns.
interestedColumns = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# Seperate value from targets
trainValues = classifiedData[, interestedColumns]
unknownValues = unclassifiedData[, interestedColumns]
# Use decodeClassLabels() to decode class labels from a
# numerical or levels vector to a binary matrix.
trainTargets = decodeClassLabels(classifiedData[, 46])
# Split the data into training and testing data set
trainSet = splitForTrainingAndTest(trainValues, trainTargets, ratio =
                                     0.2)
# Normalized the training data set
trainSet = normTrainingAndTestSet(trainSet)
# Train the MLP model
model = mlp(
  trainSet$inputsTrain,
  trainSet$targetsTrain,
  size = c(20),
  learnFuncParams = c(0.001),
  maxit = 500,
  inputsTest = trainSet$inputsTest,
  targetsTest = trainSet$targetsTest
)

predictTestSet = predict(model, trainSet$inputsTest)
# Predict the unknown set
predictUnknownSet = predict(model, unknownValues)
# Compute the confusion matrix
confusionMatrix(trainSet$targetsTrain, fitted.values(model))
confusionMatrix(trainSet$targetsTest, predictTestSet)

# Confusion matrix for the training set
confusionMatrixTrain <- confusionMatrix(trainSet$targetsTrain, fitted.values(model))
accuracyTrain <- sum(diag(confusionMatrixTrain)) / sum(confusionMatrixTrain)
accuracyTrain
precisionTrain <- diag(confusionMatrixTrain) / rowSums(confusionMatrixTrain)
precisionTrain
recallTrain <- diag(confusionMatrixTrain) / colSums(confusionMatrixTrain)
recallTrain
f1ScoreTrain <- 2 * (precisionTrain * recallTrain) / (precisionTrain + recallTrain)
f1ScoreTrain

# Confusion matrix for the test set
confusionMatrixTest <- confusionMatrix(trainSet$targetsTest, predictTestSet)
# Calculating accuracy, precision, recall, and F1 score
accuracyTest <- sum(diag(confusionMatrixTest)) / sum(confusionMatrixTest)
accuracyTest
precisionTest <- diag(confusionMatrixTest) / rowSums(confusionMatrixTest)
precisionTest
recallTest <- diag(confusionMatrixTest) / colSums(confusionMatrixTest)
recallTest
f1ScoreTest <- 2 * (precisionTest * recallTest) / (precisionTest + recallTest)
f1ScoreTest

# interpreting the unknown data set (prediction)
head(trainTargets)
head(classifiedData[, 46])
head(predictUnknownSet)

# #Plot
# par(mar = c(5.1, 4.1, 4.1, 2.1))
# par(mfrow = c(2, 2))
# plotIterativeError(model)
# plotRegressionError(predictTestSet[, 2], trainSet$targetsTest[, 2])
# plotROC(fitted.values(model)[, 2], trainSet$targetsTrain[, 2])
# plotROC(predictTestSet[, 2], trainSet$targetsTest[, 2])