setwd("C:/Users/ZXY0117/Desktop")
clean<- read.csv("cleanedata_xyz2.csv")
data=clean[1:100000,]

#setwd("C:/Users/ZXY0117/Desktop/Course_S2/Data Mining And Predictive Analytics(T)/Project/Data")
#airbnb_train_y<- read.csv("airbnb_train_y.csv")
data$high_booking_rate = airbnb_train_y$high_booking_rate

library(randomForest)
library(mlbench)
library(caret)

# Load Dataset
x <- data[,1:142]
y <- data[,143]


#rnadomly split the dataset
set.seed(12345)
train<- sample(nrow(data), 0.7*nrow(data))


# train model

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes



seed <- 7
metric <- "Accuracy"
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:20), .ntree=c(500,1000, 1500, 2000, 2500))
set.seed(seed)

rf.modnew3 <- randomForest(high_booking_rate~availability_30+availability_365+
                             availability_60+
                             availability_90+bedrooms+bathrooms+accommodates+beds+
                             average_beds+cleaning_fee+
                             extra_people + host_response_rate+minimum_nights+
                             maximum_nights+price+security_deposit+ 
                             description_len+access_len+interaction_len+
                             house_rules_len+host_about_len+
                             neighborhood_overview_len+notes_len+space_len+transit_len+
                             property_type+fast_response+host_experience+
                             is_location_exact+instant_bookable+
                             host_is_superhost+cancellation_policy+
                             host_identity_verified+wifi+
                             ACHeater+kitchen+washing+hair+essentials+smokedetector+
                             amenities_len+tv+internet+Uber+Lyft+Metro+bus+
                             subway+market+first_review_year+
                             airport+downtown+restaurant+shop+grocery+park+
                             museum+beach+price_per_person+
                             city_popular2+host_total_listings_count,
                           data=data,subset=train,method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control,importance=TRUE,na.action = na.roughfix)

summary(rf.modnew3)
plot(rf.modnew3)







rf_predsnew3 <- predict(rf.modnew3,newdata=data[-train,])
rf.mod
accc=table(rf_predsnew3,data$high_booking_rate[-train])
(accc[1,1]+accc[2,2])/(accc[1,1]+accc[2,2]+accc[1,2]+accc[2,1])

#plot the variable importances (the average decrease in impurity when splitting across that variable)
importance(rf.modnew3)
varImpPlot(rf.modnew3)   

test_pre_new3=clean[100001:112208,]
rf_preds_test_new3 <- predict(rf.modnew3,newdata=test_pre_new3)

write.csv(classifications_test_new2,"C:\\Users\\ZXY0117\\Desktop\\high_booking_rate_prediction_new2.csv", row.names = TRUE)












