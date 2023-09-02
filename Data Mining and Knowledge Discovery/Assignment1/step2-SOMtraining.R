# Assignment 1 discussion
# INFO411, 2023S1
# SJ
#
# ------------------- SOM TRAINING ---------------------------
# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728',
                             '#9467bd', '#8c564b', '#e377c2')
                             # Next, I prepare a trained data set and do further analysis using
# visualization to confirm and conclude the findings. Visualization
# also helps for a more in-depth analysis and to gain insight on the
# relationships between the data.
#
# The SOM model is applied on the dataset. The self organising map (SOM)
# is a clustering and data visualisation technique based on a neural
# network viewpoint. The goal of SOM is to find a set of reference vectors
# and to assign each object in the data set to that centroid which provides
# the best appoximation of that object. There is one neuron associated with
# each centroid. The data objects are processed one at a time and the
# closest centroid is updated. SOM imposes a topographic ordering on the
# centroids and nearby centroids are also updated. The processing of the
# points continues until a predefined limit is reached or the centroids
# are not changing much. The final output is a set of centroids that define
# clusters.
#choose the variables with which to train the SOM
#the following selects column 1,2,3,4,6
interestedFeatures <- data[, c(1,2,3,4,6)]
#data_train <- data[, c(1:45)]
data_train <- classifiedData[, c(1:45)]
# now train the SOM using the Kohonen method
data_train_matrix <- as.matrix(scale(data_train))
names(data_train_matrix) <- names(data_train)
require(kohonen)
x_dim=20
y_dim=20
# small_areas <-FALSE
# if (small_areas){
# larger grid for the small areas example (more samples)
som_grid <- somgrid(xdim = x_dim, ydim=y_dim, topo="hexagonal")
#} else {
# som_grid <- somgrid(xdim = x_dim/2, ydim=y_dim/2, topo="hexagonal")
#}
# Train the SOM model!
if (packageVersion("kohonen") < 3){
  system.time(som_model <- som(data_train_matrix,
                               grid=som_grid,
                               rlen=1000,
                               alpha=c(0.9,0.01),
                               n.hood = "circular",
                               keep.data = TRUE ))
}else{
  system.time(som_model <- som(data_train_matrix,
                               grid=som_grid,
                               rlen=1000,
                               alpha=c(0.9,0.01),
                               mode="online",
                               normalizeDataLayers=false,
                               keep.data = TRUE ))
}
summary(som_model)
# rm(som_grid, data_train_matrix)