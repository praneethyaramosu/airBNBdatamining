data = trainingcleaned
library(glmnet)
library(e1071)
library(ROCR)
library(class)
library(tree)
set.seed(12345)
data$bathrooms = ifelse(is.na(data$bathrooms),1,data$bathrooms)
data$bedrooms = ifelse(is.na(data$bedrooms),1,data$bedrooms)
data$beds = ifelse(is.na(data$beds),1,data$beds)
data$property_type = ifelse(is.na(data$property_type),"Apartment",data$property_type)

data$availability = as.factor(data$availability)
data$bed_type = as.factor(data$bed_type)
data$cancellation_policy = as.factor(data$cancellation_policy)
data$first_review = as.factor(data$first_review)
data$host_about = as.factor(data$host_about)
data$host_has_profile_pic = as.factor(data$host_has_profile_pic)
data$host_identity_verified = as.factor(data$host_identity_verified)
data$host_is_superhost = as.factor(data$host_is_superhost)
data$host_response_time = as.factor(data$host_response_time)
data$house_rules = as.factor(data$house_rules)
data$instant_bookable = as.factor(data$instant_bookable)
data$is_location_exact = as.factor(data$is_location_exact)
data$maximum_nights = as.factor(data$maximum_nights)
data$property_type = as.factor(data$property_type)
data$requires_license = as.factor(data$requires_license)
data$room_type = as.factor(data$room_type)
data$security_deposit = as.factor(data$security_deposit)
data$require_guest_phone_verification = as.factor(data$require_guest_phone_verification)
data$require_guest_profile_picture = as.factor(data$require_guest_profile_picture)
data$high_booking_rate = as.factor(data$high_booking_rate)


#ridge regression
data_x = model.matrix(high_booking_rate~accommodates+availability+bathrooms+bed_type+bedrooms+beds+cancellation_policy+cleaning_fee+extra_people+first_review+host_about+guests_included+host_has_profile_pic+host_identity_verified+host_is_superhost+host_total_listings_count+host_response_rate+host_response_time+host_experience+house_rules+instant_bookable+is_location_exact+maximum_nights+minimum_nights+price+property_type+require_guest_phone_verification+require_guest_profile_picture+requires_license+room_type+security_deposit,data)
data_y = data$high_booking_rate
train <- sample(nrow(data),.7*nrow(data))
x_train <- data_x[train,]
x_test <- data_x[-train,]

y_train <- data_y[train]
y_test <- data_y[-train]
grid <- 10^seq(10,-2,length=100)
cv.out <- cv.glmnet(x_train, y_train, family="binomial", alpha=1, lambda=grid)
bestlam <- cv.out$lambda.min
pred <- predict(cv.out, s=bestlam, newx = x_test,type="response")
pred_log = prediction(pred,y_test)
accuracy.ridge = performance(pred_log,measure = "acc")
best = which.max(slot(accuracy.ridge,"y.values")[[1]])
max.accuracy.ridge = slot(accuracy.ridge,"y.values")[[1]][best]

#lasso regression
cv.out <- cv.glmnet(x_train, y_train, family="binomial", alpha=0, lambda=grid)
bestlam <- cv.out$lambda.min
pred <- predict(cv.out, s=bestlam, newx = x_test,type="response")
pred_log = prediction(pred,y_test)
accuracy.lasso = performance(pred_log,measure = "acc")
best.lasso = which.max(slot(accuracy.lasso,"y.values")[[1]])
max.accuracy.lasso = slot(accuracy.lasso,"y.values")[[1]][best.lasso]
max.cutoff.lasso = slot(accuracy.lasso,"x.values")[[1]][best.lasso]
