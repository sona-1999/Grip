model_data <- read.table("model_data.csv", header=TRUE, sep=",",
                         colClasses = c( "character",
                                        "numeric", rep("factor",4),"numeric","factor",rep("numeric",7)), na.strings="NA") 
pred_data <- read.table("prediction_data.csv", header=TRUE, sep=",",
                         colClasses = c( "character",
                                         "numeric", rep("factor",3),rep("numeric",7)), na.strings="NA") 

set.seed(12345)
library(dplyr)
library(dummies)
library(rgl)
library(cluster)
library(fpc)
library(caret)
library(factoextra)
library(rpart)
library(nnet)
library(NeuralNetTools)
library(ROCR)
library(rpart.plot)
library(randomForest)
summary(model_data)
###Box plot
ob<-boxplot(model_data[,c("OU_LOS_hrs","BloodPressureDiff","BloodPressureUpper","BloodPressureLower","Pulse","Respirations")],las=2,col=1:6)
###Outlier removal
md1 <- model_data %>%
   dplyr::filter(OU_LOS_hrs < 200 &
                   BloodPressureLower < 110 &
                   BloodPressureUpper < 210 &
                  BloodPressureDiff < 110 &
                  BloodPressureDiff > 15 &
                  Pulse < 130 &
                  Respirations < 25 &
                  Respirations > 11) %>%
  droplevels()
summary(md1)

# Box plot
ob<-boxplot(model_data[,c("OU_LOS_hrs","BloodPressureDiff","BloodPressureUpper","BloodPressureLower","Pulse","Respirations")],las=2,col=1:6)

## Imputed the N/A values
md1$Temperature[is.na(md1$Temperature)] <-median(md1$Temperature, na.rm=TRUE)
apply(is.na(md1),2,sum)
###To remove the column
md2 = subset(md1, select = -c(1,5))


## Dummy variable

md3 <- dummy.data.frame(md2, names=c("Gender","Flipped","PrimaryInsuranceCategory","DRG01"))
## Scaling
md4 <- scale(md3, center=TRUE, scale=TRUE)

###Kmeans 
model_kmeans <- kmeans(md4, centers=3)
#conduct PCA
model_pca <- prcomp(md4, retx=TRUE)
summary(model_pca$x[,1:2])
plot(model_pca$x[,1:2], col=model_kmeans$cluster, pch=model_kmeans$cluster)
model_pca$rotation[,1:2]

# Plot only cluster 1
plot(model_pca$x[,1:2], type="n")
points(model_pca$x[model_kmeans$cluster == 1,1:2], 
       col=model_kmeans$cluster[model_kmeans$cluster== 1], 
       pch=model_kmeans$cluster[model_kmeans$cluster == 1])

# Plot only cluster 2
plot(model_pca$x[,1:2], type="n")
points(model_pca$x[model_kmeans$cluster == 2,1:2], 
       col=model_kmeans$cluster[model_kmeans$cluster== 2], 
       pch=model_kmeans$cluster[model_kmeans$cluster == 2])

# Plot only cluster 3
plot(model_pca$x[,1:2], type="n")
points(model_pca$x[model_kmeans$cluster == 3,1:2], 
       col=model_kmeans$cluster[model_kmeans$cluster== 3], 
       pch=model_kmeans$cluster[model_kmeans$cluster == 3])
#
str(md2)

#response flipped
# factor DRG01, PRIMARY INSURANCE CATEGORY
#CLASSIFICATION
#logistic regression model
colnames(md2)
md5 <- md2 %>%
  dplyr::select("Age","Gender","PrimaryInsuranceCategory","Flipped","DRG01","BloodPressureUpper","BloodPressureLower","BloodPressureDiff","Pulse","PulseOximetry","Respirations","Temperature")                          
summary(md5$Flipped)
##partitioning into train and test
train_rows <- createDataPartition(md5$Flipped,
                                       p = 0.3,
                                       list=FALSE)
train <- md5[train_rows,]
test <- md5[-train_rows,]

summary(train)
summary(test)
set.seed(12345)
# no outliers or na values found
###aplying the model
my_lr <- glm(Flipped ~ ., data=train,
                  family=binomial("logit"))
#Finding relevant variable
summary(my_lr)

summary(train$Flipped)

#confusion matrix
my_lr_predict <- predict(my_lr,
                              newdata=test,
                              type="response")
my_lr_predict_class <- character(length(my_lr_predict))
my_lr_predict_class[my_lr_predict < 0.5] <- "0"
my_lr_predict_class[my_lr_predict >= 0.5] <- "1"
my_cm<-table(test$Flipped, my_lr_predict_class)
my_cm

#calculating misclassification rate
1-sum(diag(my_cm))/sum(my_cm)


#classification tree model
my_rpart <- rpart(Flipped ~ ., data=train)
prp(my_rpart)
my_rpart_predict <- predict(my_rpart, newdata=test, type="class")
my_cm2=table(test$Flipped, my_rpart_predict)
my_cm2

#calculating misclassification rate
1-sum(diag(my_cm2))/sum(my_cm2)

#importance
my_rpart$variable.importance

##Neural network
modelLookup("nnet")
train_dummy <- dummy.data.frame(train, names=c("Gender","PrimaryInsuranceCategory","DRG01"))
flip_preprocess <- preProcess(train_dummy)
train_numeric <- predict(flip_preprocess, train_dummy)
test_dummy <- dummy.data.frame(test, names=c("Gender","PrimaryInsuranceCategory","DRG01"))
test_numeric <- predict(flip_preprocess, test_dummy)
levels(test_numeric$Flipped) <- list(Yes="1",No="0")
levels(test_numeric$Flipped)

my_weights1 <- rep(2, nrow(train_numeric))
my_weights1[train_numeric$Flipped == "0"] <- 1
levels(train_numeric$Flipped) <- list(Yes="1",No="0")
levels(train_numeric$Flipped)
my_nn <- train(Flipped ~ .,
                     data=train_numeric,
                     method="nnet",
                     metric="ROC",
                     weights=my_weights1,
               trControl=trainControl(classProbs=TRUE,
                                      summaryFunction=twoClassSummary))
my_nn_predict <- predict(my_nn, newdata=test_numeric)
my_cm3 <- table(test_numeric$Flipped, my_nn_predict)
my_cm3
plotnet(my_nn,cex_val=0.7)

#importance
garson(my_nn, bar_plot=FALSE)
       
#calculating mis-classification rate
1-sum(diag(my_cm3))/sum(my_cm3)

#Random Forest

train$Flipped <- relevel(train$Flipped, "1")
test$Flipped <- relevel(test$Flipped, "1")
summary(train$Flipped)
summary(test$Flipped)
my_rf <- randomForest(Flipped ~ .,
                            data = train,
                            importance=TRUE)
#importance
my_rf$importance
#confusion matrix
my_predict_rf <- predict(my_rf, newdata=test, type="class")
my_cm4 <- table(test$Flipped, my_predict_rf)
my_cm4
#misclassification rate
1-sum(diag(my_cm4))/sum(my_cm4)


# ROC curve
#logistic
my_lr_predict <- predict(my_lr, test, type="response")
my_lr_pred <- prediction(my_lr_predict,
                           test$Flipped,
                           label.ordering=c("0","1"))
my_lr_perf <- performance(my_lr_pred, "tpr", "fpr")
#classification tree
my_rpart_predict <- predict(my_rpart, test, type="prob")
my_rpart_pred <- prediction(my_rpart_predict[,2],
                              test$Flipped,
                              label.ordering=c("0", "1"))
my_rpart_perf <- performance(my_rpart_pred, "tpr", "fpr")
#neural network
my_nn_predict <- predict(my_nn, test_numeric, type="prob")
my_nn_pred <- prediction(my_nn_predict[,1],
                         test_numeric$Flipped,
                         label.ordering=c("No", "Yes"))
my_nn_perf <- performance(my_nn_pred, "tpr", "fpr")

#random Forest
my_rf_predict <- predict(my_rf, newdata=test, type="prob")
my_rf_pred <- prediction(my_rf_predict[,1],
                           test$Flipped,
                           label.ordering=c("0", "1"))
my_rf_perf <- performance(my_rf_pred, "tpr", "fpr")

plot(my_lr_perf, col=1)
plot(my_rpart_perf, col=2, add=TRUE)
plot(my_nn_perf, col=3, add=TRUE)
plot(my_rf_perf, col=4, add=TRUE)
legend(0.7,0.6,c("LR", "CT","NN","RF"), col=1:4, lwd=3)

###Visualization
my_Flipped_Gender_table <- table(md5$Gender, md5$Flipped)
mosaicplot(my_Flipped_Gender_table, color=c(8:9), xlab="Gender",
           ylab="Flipped",main="Flipped V/S Gender")

my_Flipped_DRG01_table <- table(md5$DRG01, md5$Flipped)
mosaicplot(my_Flipped_DRG01_table, color=c(3:4), xlab="DRG01",
           ylab="Flipped",main="Flipped v/s DRG01")

boxplot(md5$BloodPressureLower ~ md5$Flipped,xlab="Flipped",ylab="Blood Pressure Lower",col=1:2, main="Blood Pressure Lower V/s Flipped")

########## Prediction ########
summary(pred_data)
str(pred_data)
#imputing na values
apply(is.na(pred_data),2,sum)
pred_data$BloodPressureUpper[is.na(pred_data$BloodPressureUpper)] <-
  median(pred_data$BloodPressureUpper, na.rm=TRUE)
pred_data$BloodPressureDiff[is.na(pred_data$BloodPressureDiff)] <-
  median(pred_data$BloodPressureDiff, na.rm=TRUE)
pred_data$PulseOximetry[is.na(pred_data$PulseOximetry)] <-
  median(pred_data$Respirations, na.rm=TRUE)
pred_data$Respirations[is.na(pred_data$Respirations)] <-
  median(pred_data$Respirations, na.rm=TRUE)
pred_data$Temperature[is.na(pred_data$Temperature)] <-
  median(pred_data$Temperature, na.rm=TRUE)
pred_data$Pulse[is.na(pred_data$Pulse)] <-
  median(pred_data$Pulse, na.rm=TRUE)
apply(is.na(pred_data),2,sum)


#prediction
my_lr_predict1 <- predict(my_lr,
                         newdata=pred_data,
                         type="response")
my_lr_predict1
#create table
my_table<-data.frame(pred_data$ObservationRecordKey,my_lr_predict1)
colnames(my_table)
rownames(my_table) <- my_table[,1]
drop<-
View(my_table)
### creating csv file
write.csv(my_table,"prediction.csv", row.names = FALSE)
