
data<- read.csv("jessicatrain.csv")

colnames(data)
library(randomForest)
library(gbm)

#rnadomly split the dataset
set.seed(12345)
train<- sample(nrow(data), 0.7*nrow(data))
test=data[-train, ]
test_size <- nrow(data)-length(train)


data$high_booking_rate<-as.factor(data$high_booking_rate)
data$extra_people<-as.numeric(data$extra_people)
data$host_response_rate<-as.numeric(data$host_response_rate)
data$host_acceptance_rate<-as.numeric(data$host_acceptance_rate)
data$host_has_profile_pic<-as.factor(data$host_has_profile_pic) 
data$host_identity_verified<-as.factor(data$host_identity_verified)        
data$host_is_superhost<-as.factor(data$host_is_superhost)
data$interaction<-as.factor(data$interaction)
data$bathrooms[is.na(data$bathrooms)]<-1
data$host_acceptance_rate[is.na(data$host_acceptance_rate)]<-0.5

str(data)
rf.mod <- randomForest(high_booking_rate~accommodates+availability_30+availability_365+availability_60+
                         availability_90+bathrooms+bed_type+bedrooms+beds+cancellation_policy+cleaning_fee+
                         extra_people+guests_included+host_acceptance_rate+host_has_profile_pic+host_identity_verified+
                         host_is_superhost+host_listings_count+host_response_rate+host_response_time+host_total_listings_count+
                        instant_bookable+interaction+is_business_travel_ready+is_location_exact+license+price+
                         property_type+require_guest_phone_verification+require_guest_profile_picture+requires_license+room_type+
                         security_deposit+amenities_count+description_count+host_experience+host_verifications_count+minimum_night_range+
                         maximum_night_range+monthly_cheapter+city_popular,
                         data=data,subset=train,mtry=10,importance=TRUE,na.action = na.roughfix)



rf_preds <- predict(rf.mod,newdata=data[-train,])
rf_preds
sum(is.na(rf_preds))
rf_acc <- sum(ifelse(rf_preds==data$high_booking_rate[-train],1,0))/test_size

rf_acc
