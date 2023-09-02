# Preprocessing

### LOAD LIBRARIES - install with:
# install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos"))


library(kohonen)
# library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(MASS)
library(Hmisc)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

### DATA PREPARATION

# Census data comes in counts of people per area. 
# To compare areas, we will convert a number of the
# stats collected into percentages. Without this, 
# the primary differentiator between the different 
# areas would be population size.

# Load the data into a data frame
# Get the map of these areas and filter for Dublin areas.

data <- read.csv("./creditworthiness.csv")  


#idcol="functionary"
#names(data_raw)[1] <- "functionary"

#To understand the data more, I use the describe() function to produce a contingency table supplying
#the information about the data set
describe(data)

# Check for missing values
sum(is.na(data))


# Using R base (with the number of bins corresponding to the square root of the number of observations in order to have more bins than the default option):
hist(data$credit.rating,
     xlab = "credit.rating",
     main = "Histogram of credit.rating",
     breaks = sqrt(nrow(data))
) # set number of bins

# A boxplot helps to visualize a quantitative variable by displaying five common location summary (minimum, median, first and third quartiles and maximum) and any observation that was classified as a suspected outlier using the interquartile range (IQR) criterion.
boxplot(data$credit.rating,
        ylab = "credit.rating"
)
ggplot(data) +
  aes(x = "", y = credit.rating) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# The which() function it is possible to extract the row number corresponding to these outliers
out <- boxplot.stats(data$credit.rating)$out
out_ind <- which(data$hwy %in% c(out))
out_ind
# Print the values of the outliers directly on the boxplot with the mtext() function
boxplot(data$credit.rating,
        ylab = "credit.rating",
        main = "Boxplot of credit.rating"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))



# After a quick analysis, it is noted there are rows of records that have not been classified. 
# As such these rows of records cannot be used in the testing of the model for prediction. 
# Hence I decide to excluded these data using a subset function
classifiedData = subset(data, data[, 46] > 0)
unclassifiedData = subset(data, data[, 46] == 0)


# To establish statistical property of various attributes against the credit.rating, I use correlation function 
# to establish their relationship
corTable = abs(cor(classifiedData, y=classifiedData$credit.rating))
corTable

# I reorder the corTable in descending order in order to identify high to low correlation
corTable = corTable[order(corTable, decreasing=TRUE),,drop=FALSE]
head(corTable, 6)





