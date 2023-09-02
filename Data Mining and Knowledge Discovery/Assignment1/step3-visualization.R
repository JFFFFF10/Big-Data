# Assignment 1 discussion
# INFO411, 2023S1
# SJ
#
# -------------------- SOM VISUALISATION -----------------
#Visualise the SOM model results
# Plot of the training progress - how the node distances have stabilised over time.
## custom palette as per kohonen package (not compulsory)
source('./coolBlueHotRed.R')
#plot(som_model, type = "changes")
# Plot the heatmap for a variable at scaled / normalised values
var <- 1 # Functionary
var_unscaled <- aggregate(
  as.numeric(data_train[, var]),
  by = list(som_model$unit.classif),
  FUN = mean,
  simplify = TRUE
)[, 2]
plot(
  som_model,
  type = "property",
  property = var_unscaled,
  main = names(data_train)[var],
  palette.name = coolBlueHotRed
)
rm(var_unscaled, var)

var <- 2 # FI3O.credit.score
var_unscaled <- aggregate(
  as.numeric(data_train[, var]),
  by = list(som_model$unit.classif),
  FUN = mean,
  simplify = TRUE
)[, 2]
plot(
  som_model,
  type = "property",
  property = var_unscaled,
  main = names(data_train)[var],
  palette.name = coolBlueHotRed
)
rm(var_unscaled, var)

var <- 3 # Rebalance.payback
var_unscaled <- aggregate(
  as.numeric(data_train[, var]),
  by = list(som_model$unit.classif),
  FUN = mean,
  simplify = TRUE
)[, 2]
plot(
  som_model,
  type = "property",
  property = var_unscaled,
  main = names(data_train)[var],
  palette.name = coolBlueHotRed
)
rm(var_unscaled, var)

var <- 4 # credit.refused.in.past.
var_unscaled <- aggregate(
  as.numeric(data_train[, var]),
  by = list(som_model$unit.classif),
  FUN = mean,
  simplify = TRUE
)[, 2]
plot(
  som_model,
  type = "property",
  property = var_unscaled,
  main = names(data_train)[var],
  palette.name = coolBlueHotRed
)
rm(var_unscaled, var)

var <- 6 #Gender
var_unscaled <- aggregate(
  as.numeric(data_train[, var]),
  by = list(som_model$unit.classif),
  FUN = mean,
  simplify = TRUE
)[, 2]
plot(
  som_model,
  type = "property",
  property = var_unscaled,
  main = names(data_train)[var],
  palette.name = coolBlueHotRed
)
rm(var_unscaled, var)

#plot a variable from the original data set (will be uncapped etc.)
# This function produces a menu for multiple heatmaps.
source('./plotHeatMap.R')
plotHeatMap(som_model, classifiedData, variable = 0)
# Use other visualization tools to supplement the analysis
# to look for additional information
functional = with(classifiedData, table(credit.rating, functionary))
functional
barplot(
  functional,
  beside = TRUE,
  col = c("darkgreen", "yellow", "red"),
  main = "Functionary vs Credit Rating",
  sub = "0 = No, 1 = Yes"
)
legend(
  "topright",
  c("Credit Rating A", "Credit Rating B", "Credit Rating
C"),
  fill = c("darkgreen", "yellow", "red"),
  cex = 0.5
)

FI30 = with(classifiedData, table(credit.rating, FI3O.credit.score ))
functional
barplot(
  FI30,
  beside = TRUE,
  col = c("darkgreen", "yellow", "red"),
  main = "FI3O.credit.score vs Credit Rating",
  sub = "0 = No, 1 = Yes"
)
legend(
  "topright",
  c("Credit Rating A", "Credit Rating B", "Credit Rating
C"),
  fill = c("darkgreen", "yellow", "red"),
  cex = 0.5
)

Credit = with(classifiedData, table(credit.rating, credit.refused.in.past.))
Credit
barplot(
  Credit,
  beside = TRUE,
  col = c("darkgreen", "yellow", "red"),
  main = "credit.refused.in.past. vs Credit Rating",
  sub = "0 = No, 1 = Yes"
)
legend(
  "topright",
  c("Credit Rating A", "Credit Rating B", "Credit Rating
C"),
  fill = c("darkgreen", "yellow", "red"),
  cex = 0.5
)

gender = with(classifiedData, table(credit.rating, gender))
gender
barplot(
  gender,
  beside = TRUE,
  col = c("darkgreen", "yellow", "red"),
  main = "gender vs Credit Rating",
  sub = "0 = No, 1 = Yes"
)
legend(
  "topright",
  c("Credit Rating A", "Credit Rating B", "Credit Rating
C"),
  fill = c("darkgreen", "yellow", "red"),
  cex = 0.5
)

rebalance = with(classifiedData, table(credit.rating, re.balanced..paid.back..a.recently.overdrawn.current.acount))
rebalance
barplot(
  rebalance,
  beside = TRUE,
  col = c("darkgreen", "yellow", "red"),
  main = "re.balanced..paid.back.. vs Credit Rating",
  sub = "0 = No, 1 = Yes"
)
legend(
  "topright",
  c("Credit Rating A", "Credit Rating B", "Credit Rating
C"),
  fill = c("darkgreen", "yellow", "red"),
  cex = 0.5
)

# Plot histogram for the five attributes
hist(data$functionary)
hist(data$FI3O.credit.score)
hist(data$re.balanced..paid.back..a.recently.overdrawn.current.acount)
hist(data$credit.refused.in.past.)
hist(data$gender)

# Plot scatter for the five attributes
plot(classifiedData$functionary, classifiedData$credit.rating)
plot(classifiedData$FI3O.credit.score,
     classifiedData$credit.rating)
plot(
  classifiedData$re.balanced..paid.back..a.recently.overdrawn.current.acount,
  classifiedData$credit.rating
)
plot(classifiedData$credit.refused.in.past.,
     classifiedData$credit.rating)
plot(classifiedData$gender, classifiedData$credit.rating)