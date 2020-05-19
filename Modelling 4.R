airbnb <- read.csv('train_cleaned_h.csv')
airbnb_train_y <- read.csv('raw_data/airbnb_train_y.csv')



data <- airbnb

#convert the data type
data[,c("bed_type", "cancellation_policy", "host_is_superhost","host_has_profile_pic",
        "host_identity_verified", "is_location_exact", "is_business_travel_ready", "fast_response",
        "property_type", "requires_license", "require_guest_profile_picture", "require_guest_phone_verification",
        "room_type", "shared_bathroom", "state", "tv", "instant_bookable", 
        "city_popular", "wifi_ornot", "airconditioner", "kitchen", "essentials",
        "Uber", "Lyft", "Metro","Bikeshare","subway","bus")] <- lapply(
          data[,c("bed_type", "cancellation_policy", "host_is_superhost", "host_has_profile_pic",
                  "host_identity_verified", "is_location_exact", "is_business_travel_ready",
                  "fast_response", "property_type", "requires_license", 
                  "require_guest_profile_picture", "require_guest_phone_verification",
                  "room_type", "shared_bathroom", "state", "tv", "instant_bookable",
                  "city_popular", "wifi_ornot", "airconditioner", "kitchen", "essentials",
                  "Uber", "Lyft", "Metro","Bikeshare","subway","bus")], as.factor)

data[,c("accommodates","availability_30", "availability_90", "availability_60", 
        "availability_365","bedrooms", "cleaning_fee", "description_freq", "extra_people", "first_review",
        "guests_included", "host_experience", "host_response_rate", "house_rules_len", "host_about_len", 
        "host_listings_count", "host_ver_num", "interaction_len",
        "minimum_nights","maximum_nights", "neighborhood_overview_len", "notes_len", "price_miniNights",
        "space_len", "transit_len", "weekly_price", "average_beds","description_len", 
        "amenitiesNum")]  <- lapply(
            data[,c("accommodates","availability_30", "availability_90", "availability_60", 
                    "availability_365","bedrooms", "cleaning_fee", "description_freq", "extra_people", "first_review",
                    "guests_included", "host_experience", "host_response_rate", "house_rules_len", 
                    "host_about_len", "host_listings_count", "host_ver_num", "interaction_len",
                    "minimum_nights","maximum_nights", "neighborhood_overview_len", "notes_len", "price_miniNights",
                    "space_len", "transit_len", "weekly_price", "average_beds","description_len", 
                    "amenitiesNum")], as.numeric)



# train_test_split
data_train_X <- data[1:100000,]
test_X <- data[100001:112208,]

data_train <- cbind(data_train_X, airbnb_train_y$high_booking_rate)
colnames(data_train)[ncol(data_train)] <- 'high_booking_rate'

data_train <- data_train[!is.na(data_train$high_booking_rate),]

# convert the target variable
data_train$high_booking_rate <- as.factor(as.character(data_train$high_booking_rate))





#****************************visualization**************************
bk_rate_price <- aggregate(data_train$price, list(data_train$high_booking_rate), mean)
colnames(bk_rate_price) <- c('High Booking Rate', 'Mean Price')
plot(bk_rate_price)







# train_valid split
set.seed(12345)
test_inst <- sample(nrow(data_train), .3*nrow(data_train))
valid <- data_train[test_inst,]
train <- data_train[-test_inst,]



# use random forest by ranger
library(ranger)
library(dplyr)

# grid search
hyper_grid <- expand.grid(
  mtry       = c(5,6,7),
  node_size  = seq(1,3, by=1),
  sampe_size = c(.632, .7, .8),
  OOB_RMSE   = 0
)


for(i in 1:nrow(hyper_grid)) {
  # train model
  rf_model <- ranger(
    formula = high_booking_rate~accommodates+availability_30+availability_90+availability_60+
      availability_365+bedrooms+beds+cancellation_policy+city+bathrooms+
      cleaning_fee+extra_people+description_freq+guests_included+is_location_exact+is_business_travel_ready+
      host_response_rate+host_experience+host_about_len+house_rules+house_rules_len+maximum_nights+
      minimum_nights+price+neighborhood_overview_len+notes_len+
      property_type+room_type+require_guest_profile_picture+security_deposit+transit_len+tv+weekly_price+fast_response+
      wifi_ornot+airconditioner+average_beds+description_len+amenitiesNum,
    data = train,
    num.trees = 1000,
    mtry = hyper_grid$mtry[i],
    min.node.size = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed = 12345
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(rf_model$prediction.error)
}

# see result
hyper_grid %>% dplyr::arrange(OOB_RMSE) %>% head(10)




# optimal model
optimal_rfmodel <- ranger(
  formula = high_booking_rate~accommodates+availability_30+availability_90+availability_60+
    availability_365+bedrooms+beds+cancellation_policy+city_popular+bathrooms+
    cleaning_fee+extra_people+description_freq+first_review+guests_included+
    is_location_exact+is_business_travel_ready+instant_bookable+interaction_len+
    host_response_rate+host_experience+host_has_profile_pic+host_about_len+host_is_superhost+
    house_rules_len+host_identity_verified+host_listings_count+host_ver_num+
    maximum_nights+minimum_nights+price+price_miniNights+neighborhood_overview_len+notes_len+
    property_type+room_type+require_guest_profile_picture+require_guest_phone_verification+space_len+
    security_deposit+shared_bathroom+transit_len+state+tv+weekly_price+fast_response+
    wifi_ornot+airconditioner+kitchen+essentials+Uber+Lyft+Metro+Bikeshare+subway+
    bus+average_beds+description_len+amenitiesNum,
  data            = train, 
  num.trees       = 1000,
  mtry            = 6,
  min.node.size   = 1,
  sample.fraction = .8,
  importance      = 'impurity'
)


# predict and accuracy
rf_pred <- predict(optimal_rfmodel, data=valid)
rf_acc <- sum(ifelse(rf_pred$predictions == valid$high_booking_rate,1,0))/length(rf_pred$predictions)


sort(importance(optimal_rfmodel), decreasing =TRUE)



# use boosting
library(gbm)

boost_data <- data_train

#needs a numerical target variable
boost_data$high_booking_rate <- as.numeric(as.character(boost_data$high_booking_rate))

#split

boost_data_test <- boost_data[test_inst,]
boost_data_train <- boost_data[-test_inst,]

#interaction.depth refers to the maximum depth of tree allowed
boost_model <- gbm(high_booking_rate~accommodates+availability_30+availability_90+availability_60+
                     availability_365+bedrooms+beds+balcony+cancellation_policy+city_popular+bathrooms+
                     cleaning_fee+extra_people+description_freq+freeparking+first_review+guests_included+
                     is_location_exact+is_business_travel_ready+instant_bookable+interaction_len+
                     host_response_rate+host_experience+host_has_profile_pic+host_about_len+house_rules+host_is_superhost+
                     house_rules_len+host_identity_verified+host_listings_count+host_ver_num+
                     maximum_nights+minimum_nights+price+price_miniNights+neighborhood_overview_len+notes_len+
                     property_type+room_type+require_guest_profile_picture+require_guest_phone_verification+space_len+
                     security_deposit+shared_bathroom+transit_len+state+tv+weekly_price+fast_response+
                     wifi_ornot+airconditioner+kitchen+essentials+Uber+Lyft+Metro+Bikeshare+subway+
                     bus+average_beds+description_len+amenitiesNum,
                   data=boost_data_train,
                   distribution="bernoulli",n.trees=1000,interaction.depth=5)

boost_preds <- predict(boost_model,newdata=boost_data_test,type='response',n.trees=1000)

#classify with a cutoff and compute accuracy
boost_class <- ifelse(boost_preds>.5,1,0)
boost_acc <- sum(ifelse(boost_class==boost_data_test$high_booking_rate ,1,0))/length(boost_class)





















#*************************preditction in test**********************************



# predict for airbnb_test_x data by rf
rf_pred_test <- predict(optimal_rfmodel, data=test_X)

sum(rf_pred_test$predictions ==1)



# predict for airbnb_test_x data by boosting
boost_pred_test <- predict(boost_model,newdata=test_X,type='response',n.trees=1000)
boost_class_test <- ifelse(boost_pred_test>.5,1,0)

sum(boost_class==1)





# submit prediction
submission <- data.frame(rf_pred_test$predictions)
colnames(submission) <- 'high_booking_rate'

write.csv(submission, 'submission_bookingrate_Group8.csv', row.names = TRUE)


# check submission
pppp <- read.csv('submission_bookingrate_Group8.csv')

sum(pppp$high_booking_rate == 1)


