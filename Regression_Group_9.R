# --------------------------------------
# Exercise: Regression_Group_9.R
# --------------------------------------

# Machine Learning 2020
# Regression Project
# 21/02/2020
# Group 9
# Rosalie, Busra

# download packages if needed
if (!require('raster')) install.packages('raster')
if (!require('FNN')) install.packages('FNN')
if (!require('ggplot2')) install.packages('ggplot2')

# load packages
library(raster)
library(FNN) #library for KNN
library(ggplot2) #library for plotting

#clear env
rm(list=ls())

###read data###
#Set the directory where you put the data
#setwd("")
#read the csv
data <- as.data.frame(t(read.csv("J_SPARC_one_day.csv")))  
#obtain the wavelenghts
wavelengths <- data[1,]
#remove the first row
data <- data[2:nrow(data),]
#put the right name for the rows
rownames(data) <- c(paste("Sample", seq_len(nrow(data)), sep = "_"))
#and for the columns
colnames(data) <- c("Chlorophyll", "LAI", "FCover", paste("Hyperbands", seq(1:62), sep="_"))

###Visualization histogram of chlorophyll###
graphics.off()
#Use hist() function
hist(data[,1], xlab="Chlorophyl content", ylab="Number of sample points", main="Histogram of Chlorophyll", col="darkgreen")

###Show spectral values (unormalized)###
graphics.off()
sample_id = 11
spectral_val <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val) <- c("Wavelengths", "Val", "ID")
sample_id = 100
spectral_val2 <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val2) <- c("Wavelengths", "Val", "ID")
spectral_val <- rbind(spectral_val, spectral_val2)
ggplot(data=spectral_val, aes(x=Wavelengths, y=Val, color=ID, shape=ID)) + geom_point() + geom_line(linetype = "dashed")

###perform train/test split###
set.seed(1)
split <- runif(nrow(data),0,1)>0.2
train_samples <- subset(data, select = -c(LAI, FCover), split)
test_samples <- subset(data, select= -c(LAI, FCover), !split)

# train_ground_truth <- 
# test_ground_truth <- 


###Apply linear model###
#attach(data)
set.seed(1)
linear_reg <- lm(train_samples$Chlorophyll~., data=train_samples); summary(linear_reg)
pred_test <- predict(linear_reg, test_samples)

# calculate RMSE 
RMSE_linear_test <- sqrt(sum((test_samples$Chlorophyll-pred_test)^2)/length(pred_test))
RMSE_linear_test
# calculate MAE
MAE_linear_test <- mean(abs(test_samples$Chlorophyll-pred_test), na.rm = T)
MAE_linear_test 
# calculate rsq
rss_test <- sum((test_samples$Chlorophyll-pred_test)^2)
tss_test <- sum((test_samples$Chlorophyll - mean(test_samples$Chlorophyll))^2)
rsq_test <- 1- (rss_test/tss_test)
rsq_test

# MAE gives equal weight to all errors, while RMSE gives extra weight to large errors
# These values are RMSE: 5.858875, MAE: 4.825098, R2: 0.8243799. RMSE and MAE are really low, and the R2 is high. 
# These values indicate that relative to the training data, the model performs quite well. It is also shown in the F-statistic
# and p-value 18.86 and 2.2e-16 respectively. 

###normalize data###
data_norm <- data
data_norm[,4:65] <- data_norm[,4:65]/data_norm[,57]  # 57 is the 54th band

###Show spectral values after normalization###
#graphics.off()
sample_id = 11
spectral_val <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data_norm[sample_id,4:ncol(data_norm)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val) <- c("Wavelengths", "Val", "ID")
sample_id = 100
spectral_val2 <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data_norm[sample_id,4:ncol(data_norm)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val2) <- c("Wavelengths", "Val", "ID")
spectral_val <- rbind(spectral_val, spectral_val2)
ggplot(data=spectral_val, aes(x=Wavelengths, y=Val, color=ID, shape=ID)) + geom_point() + geom_line(linetype = "dashed")

# The values are now between 0-1, the pattern is the similar with the original values

###apply linear regression after normalization###
###perform train/test split###
set.seed(1)
split <- runif(nrow(data),0,1)>0.2
train_samples_norm <- subset(data_norm, select = -c(LAI, FCover), split)
test_samples_norm <- subset(data_norm, select = -c(LAI, FCover), !split)
###Apply linear model###
linear_reg_norm <- lm(train_samples_norm$Chlorophyll~.-train_samples_norm$Chlorophyll, data=train_samples_norm); summary(linear_reg_norm)
pred_test_norm <- predict(linear_reg_norm, test_samples_norm)

###Visualize linear model coeffs###
plot(linear_reg_norm$coefficients)

###Make the scatter plot###
lm_scatter <- data.frame(rownames(test_samples), test_samples[,1], unname(pred_test), "LM")
colnames(lm_scatter) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter2 <- data.frame(rownames(test_samples_norm), test_samples_norm[,1], unname(pred_test_norm), "LM (Norm)")
colnames(lm_scatter2) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter <- rbind(lm_scatter, lm_scatter2)
ggplot(data=lm_scatter, aes(x=GT, y=Pred, color=Method, shape=Method)) + geom_point() + geom_abline(slope=1, intercept = 0)

# calculate RMSE 
RMSE_linear_test_norm <- sqrt(sum((test_samples_norm$Chlorophyll-pred_test_norm)^2)/length(pred_test_norm))
RMSE_linear_test_norm
# calculate MAE
MAE_linear_test_norm <- mean(abs(test_samples_norm$Chlorophyll-pred_test_norm), na.rm = T)
MAE_linear_test_norm 
# calculate rsq
rss_test_norm <- sum((test_samples_norm$Chlorophyll-pred_test_norm)^2)
tss_test_norm <- sum((test_samples_norm$Chlorophyll - mean(test_samples_norm$Chlorophyll))^2)
rsq_test_norm <- 1- (rss_test_norm/tss_test_norm)
rsq_test_norm

# The RMSE is 6.783294, MAE is 5.390811, and an R2 of 0.7645888 these values are slightly higher than before. 
# The F-statistic is higher and P-value is the same,hence this indicates that the model is performing well. 
# 27.06, 2.2e-1 respectively

###KNN reg###
knn <- knn.reg(train = train_samples_norm[,2:62], test = test_samples_norm[,2:62], y = train_samples_norm[,1],  k = 12)
pred_test_knn = knn$pred
res <- test_samples_norm[,1] - pred_test_knn

RMSE_knn_normalized <- sqrt(mean(res^2))
RMSE_knn_normalized
MAE_knn_normalized <- mean(abs(res), na.rm=T)
MAE_knn_normalized
rss_knn <- sum(res^2)
tss_knn <- sum((test_samples_norm[,1] - mean(test_samples_norm[,1]))^2)
R2_knn_normalized <- 1- rss_knn/tss_knn
R2_knn_normalized

###find the best number of neighbors###
best_RMSE <- 10000000
best_k <- -1

train_70samples <- train_samples_norm [1:70,]
test_70samples <- train_samples_norm [71:115,]

for (k in seq(3, 20, by=1)){
  knn <- knn.reg(train_70samples[,2:62], test_70samples[,2:62], y = train_70samples[,1],  k)
  pred_test = knn$pred
  res <- test_70samples[,1] - pred_test
  RMSE <- sqrt(mean(res^2))
  if (RMSE < best_RMSE) {
    best_RMSE <- RMSE
    best_k = k
    cat("New best model: ", k , " (", best_RMSE, ")\n")
  }
}

# As a rule of thumb, setting K to the square root of the number of n can lead to better results.
# Higher K might deteriorate the flexibility of the model.

###Apply KNN with the best number of neighbors###
knn <- knn.reg(train = train_samples_norm, test = test_samples_norm, y = train_samples_norm[,1],  k = 12)
pred_test_knn = knn$pred

###Show the scatter plot###
knn_scatter <- data.frame(rownames(test_samples), test_samples[,1], unname(pred_test_knn), "KNN")
colnames(knn_scatter) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter <- data.frame(rownames(test_samples_norm), test_samples_norm[,1], unname(pred_test_norm), "LM")
colnames(lm_scatter) <- c("Sample_id", "GT", "Pred", "Method")
knn_scatter <- rbind(knn_scatter, lm_scatter)
ggplot(data=knn_scatter, aes(x=GT, y=Pred, color=Method, shape=Method)) + geom_point() + geom_abline(slope=1, intercept = 0)


###Apply regression models on image###
raster_img <- stack("./CHRIS.tif")

maxs <- cellStats(raster_img, stat='max')
par(mfrow=c(1,3))
plotRGB(raster_img/maxs*255, r=25,g=15,b=5, main='Input image')

###Apply lm###
img <- as.data.frame(raster_img, xy=TRUE)
colnames(img) <- c("x", "y", paste("Wavelength", wavelengths[1,4:ncol(wavelengths)], sep = "_"))
#Normalize
img[,3:64] <- (img[,3:64]) / (img$Wavelength_879.54)

img1 <- img[,-c(1:2)]
colnames(img1) <-  colnames(train_samples_norm)[2:63]
#Predict
pred_img <- as.data.frame(predict(linear_reg_norm, img1))
#Convert to matrix
mat_img <- matrix(unlist(pred_img), nrow=dim(raster_img)[1], ncol=dim(raster_img)[2],byrow = TRUE)
#Clip values
mat_img[mat_img<0] <- 0
mat_img[mat_img>55] <- 55
img_lm <- raster(mat_img)
plot(img_lm, axes=FALSE, main='Linear regression', asp=1)

#Apply knn
mat_img <- raster::as.matrix(raster_img)
mat_img <- as.data.frame(raster_img, xy=TRUE)
#Normalize
mat_img[,] <- (img[,3:64]) / (img$Wavelength_879.54)
#Give meaningful names to the column
colnames(mat_img) <- colnames(train_samples_norm)[2:62]

#Apply KNN
knn <- knn.reg(train = train_samples_norm[,2:63], test = mat_img[3:64], y = train_samples_norm[,1],  k = 20)
img_knn <- raster(matrix(unlist(knn$pred), nrow=dim(raster_img)[1], ncol=dim(raster_img)[2],byrow = TRUE))
plot(img_knn, axes=FALSE, main='KNN regression')


