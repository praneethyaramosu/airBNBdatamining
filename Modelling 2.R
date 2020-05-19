library(ROCR)
library(class)
library(tree)
library(RTextTools)
library(maxent) #low-memory multinomial logistic regression, short for "maximum entropy"
library(SnowballC)
library(ggplot2)
library(randomForest)
library(gbm)
library(ISLR)
set.seed(12345)
data = read.csv("train300.csv")
test = read.csv("test300.csv")
data$X = NULL
test$X = NULL


data$host_has_profile_pic = as.factor(data$host_has_profile_pic)
data$host_identity_verified = as.factor(data$host_identity_verified)
data$host_is_superhost = as.factor(data$host_is_superhost)
data$instant_bookable = as.factor(data$instant_bookable)
data$is_location_exact = as.factor(data$is_location_exact)
data$require_guest_phone_verification = as.factor(data$require_guest_phone_verification)
data$require_guest_profile_picture = as.factor(data$require_guest_profile_picture)
data$requires_license = as.factor(data$requires_license)
data$high_booking_rate = as.factor(data$high_booking_rate)
data$bathrooms = ifelse(is.na(data$bathrooms),1,data$bathrooms)
data$bedrooms = ifelse(is.na(data$bedrooms),1,data$bedrooms)
data$beds = ifelse(is.na(data$beds),1,data$beds)
data$property_type = as.character(data$property_type)
data$property_type = ifelse(is.na(data$property_type),"Apartment",data$property_type)
data$property_type = as.factor(data$property_type)


train <- sample(nrow(data),.7*nrow(data))
test_size <- nrow(data)-length(train)

#rf
rf.mod <- randomForest(high_booking_rate~.,data=data,subset=train,mtry=10,ntree=1000,importance=TRUE)
rf_preds <- predict(rf.mod,newdata=data[-train,],type = "prob")
pred_log = prediction(rf_1,data$high_booking_rate[-train])
accuracy.valid = performance(pred_log,measure = "acc")
best = which.max(slot(accuracy.valid,"y.values")[[1]])
max.accuracy = slot(accuracy.valid,"y.values")[[1]][best]
max.cutoff = slot(accuracy.valid,"x.values")[[1]][best]
rf_acc <- sum(ifelse(rf_preds==data$high_booking_rate[-train],1,0))/test_size
table(rf_preds)

#boosting
data$high_booking_rate = as.numeric(data$high_booking_rate)
data$high_booking_rate = data$high_booking_rate-1
boost.mod <- gbm(high_booking_rate~.,data=data[train,],distribution="bernoulli",n.trees=1000,interaction.depth=4)
boost_preds <- predict(boost.mod,newdata=data[-train,],type='response',n.trees=1000)
boost_class <- ifelse(boost_preds>.5,1,0)
boost_acc <- sum(ifelse(boost_class==data$high_booking_rate[-train],1,0))/test_size
boost_acc
