# --------------------------------------
# Exercise: Clustering
# --------------------------------------

# 2020 Laboratory of Geo-Information Science and Remote Sensing (GRS)
# Wageningen University
# 
# Devis Tuia
# Diego Marcos
# Sylvain Lobry
# Benjamin Kellenberger

# --------------------------------------------------------------------------------------------
# Setup and data preparation for the 2D dataset
# --------------------------------------------------------------------------------------------

# clear previous variables
rm(list=ls())

# import packages or load distmat.R
#library(pracma) # Practical Math operations, like distmat to compute a distance matrix
source('distmat.R')

# Load and visualize the data
Data2D <- read.csv(file="data2d.csv", header=FALSE, sep=",")
plot(Data2D[,1], Data2D[,2] ,xlab="Variable 1",ylab="Variable 2")

# --------------------------------------------------------------------------------------------
# 1. Write your k-means algorithm and test it on a 2D dataset
# --------------------------------------------------------------------------------------------

# 1.1 Initialization of K-means

# Choose K points randomly from the dataset to initialize the cluster centroids.
# Store them as a matrix of size [K, dim] and call it cluCentroids.
# Check out the Matlab function randperm() for this.
K <- 4 #Number of clusters
################################
# R1
# YOUR CODE HERE
init_indeces <- #?
cluCentroids <- #?
################################

# 1.2 Assign each data point to the nearest cluster centroid

# Assign each data point to a cluster centroid.
# You probably want to use the function pdist2().
# Store the assigments in a [N,1] vector called assigned_clusterIDs.
################################
# R2
# YOUR CODE HERE
D <- #? distance matrix. Use distmat()
assigned_clusterIDs <- #? for each data point, find the index of nearest cluster
################################

# this plot allows to see the cluster assignment
plot(Data2D[,1], Data2D[,2], col = palette()[assigned_clusterIDs])
points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)

# 1.3: Update the centroids
################################
# R3
# YOUR CODE HERE
for (i in 1:K) {
  cluCentroids[i,] <- #? upadte the centroids as the average of the assigned data points
}
################################

# 1.4: Write your kmeans and compare it to R's native kmeans
################################
# R3
# YOUR CODE HERE
# Using R's native kmeans
clustering <- #? get the clustering with kmeans()
assigned_clusterIDs <- #?
cluCentroids <- #?
# Using your kmeans implementation
source('my_kmeans.R')
my_clustering <- #? get the clustering with kmeans()
my_assigned_clusterIDs <- #?
my_cluCentroids <- #?
################################

# Plot the clustering results of both algorithms
par(mfrow=c(1, 1))
plot(Data2D[,1], Data2D[,2], col = palette()[assigned_clusterIDs])
points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)

plot(Data2D[,1], Data2D[,2], col = palette()[my_assigned_clusterIDs])
points(my_cluCentroids[,1], my_cluCentroids[,2], col = palette(),pch=19,cex=2)

# --------------------------------------------------------------------------------------------
# Setup and data preparation for the world cities dataset
# --------------------------------------------------------------------------------------------

# clear previous variables
rm(list=ls())

# import packages
# install.packages("xlsx")
require(xlsx)

# load the 8D data into a data frame
AllDataCities <- read.xlsx("oecd_cities_stats.xlsx", sheetName = "OECD.Stat export")

# extract a subset of the data frame to remove unwanted rows and columns
DataCities<-subset(AllDataCities,NA..9!='..' & NA..15!='..',select=c(NA..1,NA..3,NA..5,NA..7,NA..9,NA..11,NA..13,NA..15))

# extract the country codes correspoinding to each city
# note the use of the apply() function and check what it does. You will need it later
Cities<-subset(AllDataCities,NA..9!='..' & NA..15!='..',select=FALSE.)
Cities <- apply(Cities,2,as.character)
CountryCodes<-apply(Cities,1,function(x) substr(strsplit(x,": ")[[1]][1],3,4))

# load a Look-up-table containing the contry codes, names and world regions
CountryLUT <- read.xlsx("oecd_cities_stats.xlsx", sheetName = "Countries")

# extract the coutry names and change the variable names
CountryNames<-matrix(sapply(CountryCodes,function(x) CountryLUT[1,which(x==names(CountryLUT))]))
RegionNames<-matrix(sapply(CountryCodes,function(x) CountryLUT[2,which(x==names(CountryLUT))]))
CityNames<-apply(Cities,1,function(x) strsplit(x,": ")[[1]][2])
VarNames<-c('Population','Youth dep. ratio', 'Old-age dep. ratio','Density','Green area per cap.','Core concentration', 'P2.5 pollution','Unemplyment')
names(DataCities)<-VarNames
rownames(DataCities)<-CityNames

# --------------------------------------------------------------------------------------------
# 2. Visually explore the world cities dataset
# --------------------------------------------------------------------------------------------

# here is some code to plot all pairs of variables
# maybe not the easiest way to see what's going on, although you can if you put some effort
pairs(DataCities,col=sapply(RegionNames,FUN=function(x) which.max(unique(RegionNames) == x)),oma=c(3,3,5,15),pch=19)
par(xpd=TRUE)
legend(0.87, 0.8, as.vector(unique(RegionNames)),  fill=palette() )

# 2.1: Run kmeans on the data
################################
# R5
# YOUR CODE HERE
DataCities<-apply(DataCities,2,as.numeric)
K<-4
clustering <- #? get the clustering of the cities
################################

# Plot the cluster centroids as barplots
par(mfrow=c(2, ceiling(K/2)))
for (i in 1:K) {
  barplot(clustering$centers[i,]-colMeans(clustering$centers),names.arg=VarNames,las=2)
}

# 2.2: Check the regional proportions in each cluster
################################
# R6
# YOUR CODE HERE
par(mfrow=c(2, ceiling(K/2)))
for (i in 1:K) {
  #? pie plot with the proportion of regions per cluster
}
################################

# 2.3: Use Principal Component Analysis to see how varaibles as grouped together
################################
# R7
# YOUR CODE HERE
pca<- #? PCA of DataCities
source('ggbiplot.R')
#? use ggbiplot to visualize. Type ggbiplot to see the code and the options

################################




