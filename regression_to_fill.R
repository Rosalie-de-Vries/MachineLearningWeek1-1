install.packages('Metrics')
library(raster)
library(FNN) #library for KNN
library(ggplot2) #library for plotting

#clear env
rm(list=ls())

###read data###
#Set the directory where you put the data
setwd("C:/Users/Niels/Documents/School/Master/Machine_Learning/W1Project")
#read the csv
data <- as.data.frame(t(read.csv("J_SPARC_one_day.csv")))
#obtain the wavelenghts
wavelengths <- data[1,]
#remove the first row
data <- data[2:nrow(data),]
#put the right name for the rows
rownames(data) <- c(paste("Sample", seq_len(nrow(data)), sep = "_"))
#and for the columns
colnames(data) <- c("Chlorophyll", "LAI", 'FCover',  paste("Wavelength", wavelengths[1,4:ncol(wavelengths)], sep = "_"))



###Visualization histogram of chlorophyll###
graphics.off()
#?
#Use hist() function
par(mfrow=c(1,1))
hist(data[,1], col='darkgreen', xlab='Chlorophyl content', ylab='Number of sample points', xlim=c(0,60))
##Comment: Most of the points have a chlorophyl content of around 35-55.
##55 is the max value of chlorophyl content in the sample points 0-5 is the minimum found.

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

## The cholorphyl content of 11 is 51 and 100 it is 48.5. Their spectral data show a high reflectance in the NIR region
## and a small peak in Green region, which indicates the presence of chlorophyl containing organisms (plants, phytoplanton, algae etc.)
## Sample 11 is had a lower reflectance, but higher reflectance compared to 100. 
## There is however aa difference between FCover and LAI which can be the reason for this difference. 
data[11,1]
data[11,2]
data[11,3]
data[100,1]
data[100,2]
data[100,3]

###perform train/test split###

data_random <- data[sample(nrow(data)),]

train_samples <- data_random[1:108,-c(2:3)]
test_samples <- data_random[109:135,-c(2:3)]

train_ground_truth <- train_samples[,1]
test_ground_truth <- test_samples[,1]

## To obtain training and test data we first randomized the rows of the original dataset.
## It is a rule of thumb to have 80% training data and 20% test data, thus we seperated it that way. 

###Apply linear model###
set.seed(100)
linear_reg <- lm(train_samples$Chlorophyll~. ,data=train_samples)
ln_pred <- predict(linear_reg, test_samples)

tss = sum((test_ground_truth-mean(test_ground_truth))^2)
ssr = sum((test_ground_truth-ln_pred)^2)

RMSE_linear <- sqrt(sum ((test_ground_truth-ln_pred)^2) / length(ln_pred))
MAE_linear <- mean( abs(ln_pred - test_ground_truth), na.rm = TRUE)

R2_linear <- 1-(ssr/tss)


##These values are RMSE: 5.634749, MAE:  4.13594, R2: 0.8580157 The errors are really low, and the R2 is really high. 
##It would indicate that relative to the training data, the model performs quite well. It is also shown in the F-statistic
##and P-Value. (26.57 and 2.2e-16 respectivally)

###normalize data###
data_norm <- data_random
data_norm[,4:65] <- data_random[,4:65]/max(data_random[,54])

###Show spectral values after normalization###
graphics.off()
sample_id = 11
spectral_val <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data_norm[sample_id,4:ncol(data_norm)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val) <- c("Wavelengths", "Val", "ID")
sample_id = 100
spectral_val2 <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data_norm[sample_id,4:ncol(data_norm)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val2) <- c("Wavelengths", "Val", "ID")
spectral_val <- rbind(spectral_val, spectral_val2)
ggplot(data=spectral_val, aes(x=Wavelengths, y=Val, color=ID, shape=ID)) + geom_point() + geom_line(linetype = "dashed")

## The values are now between 0-1, the shape is the same as with the original values. 

###perform train/test split###

train_samples_norm <- data_norm[1:108,-c(2:3)]
test_samples_norm <- data_norm[109:135,-c(2:3)]

train_ground_truth_norm <- train_samples_norm[,1]
test_ground_truth_norm <- test_samples_norm[,1]

###apply linear regression after normalization###
set.seed(100)
linear_reg_norm <- lm(train_samples_norm$Chlorophyll~. ,data=train_samples_norm)
ln_pred_norm <- predict(linear_reg_norm, test_samples_norm)

tss = sum((test_ground_truth_norm-mean(test_ground_truth_norm))^2)
ssr = sum((test_ground_truth_norm-ln_pred_norm)^2)

RMSE_linear <- sqrt(sum ((test_ground_truth_norm-ln_pred_norm)^2) / length(ln_pred_norm))
MAE_linear <- mean( abs(ln_pred_norm - test_ground_truth_norm), na.rm = TRUE)

R2_linear <- 1-(ssr/tss)

## The RMSE is 5.634749, MAE is 4.13594, and an R2 of 0.8580157 These values are approximately the same as before. 
## The F-statistic is slightly higher and P-value is the same, both indicating that the model is performing well. 
## (33.76, 2.2e-16 respectivally.)

###Visualize linear model coeffs###
plot(linear_reg_norm$coefficients)

###Make the scatter plot###
lm_scatter <- data.frame(rownames(test_samples), test_samples[,1], unname(linear_reg$fitted.values), "LM")
colnames(lm_scatter) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter2 <- data.frame(rownames(test_samples), test_samples[,1], unname(linear_reg_norm$fitted.values), "LM (Norm)")
colnames(lm_scatter2) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter <- rbind(lm_scatter, lm_scatter2)
ggplot(data=lm_scatter, aes(x=GT, y=Pred, color=Method, shape=Method)) + geom_point() + geom_abline(slope=1, intercept = 0)


###KNN reg###

train_knn <- train_samples_norm[1:70,]
test_knn <- train_samples_norm[71:108,]

train_ground_truth_knn <- train_ground_truth_norm[1:70]
test_ground_truth_knn <- train_ground_truth_norm[71:108]

knn <- knn.reg(train = train_knn, test=test_knn, y=train_ground_truth_knn, k=5)
pred_test = knn$pred

tss = sum((train_ground_truth_norm-mean(train_ground_truth_knn))^2)
ssr = sum((pred_test-mean(train_ground_truth_knn))^2)

RMSE_knn_normalized = sqrt(mean((test_ground_truth_knn-pred_test)^2))
MAE_knn_normalized <-  mean(abs(test_ground_truth_knn - pred_test), na.rm = TRUE)
R2_knn_normalized <- 1-(ssr/tss)

## We think 3 is the best amount of K, a lower amount of K means the model will be more flexible and we expect that it will have a better performance with 3 compared to a larger amount of K
## Eventually we used K=5 because it has the lowest Error and the highest R2
## RMSE: 1.009249 MAE:0.4525526 ,R2: 0.6860733 The errors are larger than the LM errors and the Normalized LM errors 
  
###find the best number of neighbors###
best_RMSE <- 10000000
best_k <- -1

for (k in seq(1:12)){
  knn <- knn.reg(train = train_knn, test=test_knn, y=train_ground_truth_knn, k=k)
  pred_test = knn$pred
  RMSE <- sqrt(mean((test_ground_truth_knn-pred_test)^2))
  
  if (RMSE < best_RMSE) {
    best_RMSE <- RMSE
    best_k = k
    cat("New best model: ", k , " (", best_RMSE, ")\n")
  }
}
##The range that would be suitable is 1:12, we base the range of K based on the squareroot of n. Which is +/- 12.
## Anything higher would deteriorate the flexibility of the model. 

###Apply KNN with the best number of neighbors###
knn <- knn.reg(train = train_samples_norm, test=test_samples_norm, y=train_ground_truth_norm, k=12)
pred_test = knn$pred

###Show the scatter plot###
lm_scatter <- data.frame(rownames(test_samples), test_samples[,1], unname(linear_reg$fitted.values), "LM")
colnames(lm_scatter) <- c("Sample_id", "GT", "Pred", "Method")

lm_scatter2 <- data.frame(rownames(test_samples), test_samples[,1], unname(linear_reg_norm$fitted.values), "LM (Norm)")
colnames(lm_scatter2) <- c("Sample_id", "GT", "Pred", "Method")

knn_scatter <- data.frame(rownames(test_samples), test_samples[,1], unname(pred_test), "KNN")
colnames(knn_scatter) <- c("Sample_id", "GT", "Pred", "Method")

lm_scatter <- rbind(lm_scatter, lm_scatter2, knn_scatter)
ggplot(data=lm_scatter, aes(x=GT, y=Pred, color=Method, shape=Method)) + geom_point() + geom_abline(slope=1, intercept = 0)

##We might be overfitting the model for KNN. The scatterplot shows that the KNN points are really close to the center line.

###Apply regression models on image###
raster_img <- stack("C:/Users/Niels/Documents/School/Master/Machine_Learning/W1Project/CHRIS.tif")

##The metadata states this: dimensions : 139, 262, 36418, 62  (nrow, ncol, ncell, nlayers)
##                          resolution : 25, 25  (x, y)
##                          4.95 mb

maxs <- cellStats(raster_img, stat='max')
par(mfrow=c(1,3))
plotRGB(raster_img/maxs*255, r=650,g=550,b=450, main='Input image')

###Apply lm###
img <- as.data.frame(raster_img, xy=TRUE)
colnames(img) <- c("x", "y", paste("Wavelength", wavelengths[1,4:ncol(wavelengths)], sep = "_"))

#Normalize
img[,3:64] <- img[,3:64]/max(img[,54])
#Predict
pred_img <- as.data.frame(predict(linear_reg_norm, img))
#Convert to matrix
mat_img <- matrix(unlist(pred_img), nrow=dim(raster_img)[1], ncol=dim(raster_img)[2],byrow = TRUE)
#Clip values
mat_img[mat_img<0] <- 0
mat_img[mat_img>55] <- 55
img_lm <- raster(mat_img)
plot(img_lm, axes=FALSE, main='Linear regression', asp=1)

#Apply knn
mat_img <- raster::as.matrix(raster_img)
#Normalize
mat_img[,] <- mat_img/max(mat_img[,54])
#Give meaningful names to the column
colnames(mat_img) <- c(paste("Wavelength", wavelengths[1,4:ncol(wavelengths)], sep = "_"))
#Apply KNN
knn <- knn.reg(train = train_samples_norm[-c(1)], test=mat_img, y=train_ground_truth_norm, k=12)
img_knn <- raster(matrix(knn$pred, nrow=dim(raster_img)[1], ncol=dim(raster_img)[2],byrow = TRUE))
plot(img_knn, axes=FALSE, main='KNN regression')


