data1 <- airbnb_train_x
data1$X1=NULL
data1$high_booking_rate = airbnb_train_y$high_booking_rate


colnames(data1)
data = data1

colSums(is.na(data))
#remove null
data = subset(data,!is.na(data$accommodates))
data = subset(data,!is.na(data$availability_30))
data = subset(data,!is.na(data$availability_365))
data = subset(data,!is.na(data$availability_60))
data = subset(data,!is.na(data$availability_90))
data = subset(data,!is.na(data$beds))
data = subset(data,!is.na(data$bedrooms))
data= subset(data,!is.na(data$bathrooms))
data=subset(data,!is.na(data$cancellation_policy))
data=subset(data,!is.na(data$city_name))
data=subset(data,!is.na(data$cleaning_fee))
data=subset(data,!is.na(data$description))
data=subset(data,!is.na(data$extra_people))
data=subset(data,!is.na(data$host_is_superhost))
data=subset(data,!is.na(data$host_listings_count))
data=subset(data,!is.na(data$host_response_time))
data=subset(data,!is.na(data$host_since))
data=subset(data,!is.na(data$instant_bookable))
data=subset(data,!is.na(data$price))
data=subset(data,!is.na(data$property_type))
data=subset(data,!is.na(data$requires_license))
data=subset(data,!is.na(data$room_type))
data=subset(data,!is.na(data$maximum_nights))
data=subset(data,!is.na(data$minimum_nights))
data=subset(data,!is.na(data$host_identity_verified))
data=subset(data,!is.na(data$host_response_rate))
data=subset(data,!is.na(data$is_location_exact))

#accomadates

#availbility

##person/per bed
data$accommodates=as.numeric(data$accommodates)
data$beds=as.numeric(data$beds)
data$bed_person = data$accommodates/data$beds
table(data$bed_person)
data = subset(data,!is.na(data$bed_person))
data = subset(data, data$bed_person!='Inf')

#share_bathroom
data$bathrooms=as.numeric(data$bathrooms)
data$bedrooms=as.numeric(data$bedrooms)
data$share_bathroom = ifelse(data$bathrooms<data$bedrooms,1,0)
table(data$share_bathroom)
data=subset(data,!is.na(data$share_bathroom))


##reducing factors of cancellation policy

table(data$cancel_policy )
data$cancel_policy = ifelse((data$cancellation_policy=="no_refunds") | (data$cancellation_policy=="strict") | (data$cancellation_policy=="super_strict_30") | (data$cancellation_policy=="super_strict_60"),1,ifelse(data$cancellation_policy=='moderate',2,3))
data=subset(data,!is.na(data$cancel_policy))

###city

table(data$city_name)
data$city_popular = data$city_name
data=subset(data,!is.na(data$city_popular))
table(data$city_popular)
##data$city_popular = ifelse(data$city_name %in% c("Nashville", "Los Angeles","San Diego","Washington DC","New Orleans","San Francisco","New York","Seattle","Chicago","Boston","Austin","Portland","Denver","Santa Cruz","Oakland","Asheville"),data$city_name,'Other')
###cleaning fee column
data$cleaning_fee = substr(data$cleaning_fee,2,1000)
table(data$cleaning_fee)
data=subset(data,!is.na(data$cleaning_fee))
table(data$cleaning_fee)
##description
description_len = c()

for (i in 1:nrow(data))
{
  m = length(strsplit(data$description[i], " ")[[1]])
  description_len = c(description_len,m)
  
}

data$description_len = description_len

data=subset(data,!is.na(data$description_len))
table(data$description_len)
##extra people column
data$extra_people = substr(data$extra_people,2,1000)
table(data$extra_people)
data=subset(data,!is.na(data$extra_people))
table(data$extra_people)


##host

data$host_is_superhost = ifelse(data$host_is_superhost=='TRUE',1,0)
table(data$host_is_superhost)
data=subset(data,!is.na(data$host_is_superhost))
table(data$host_is_superhost)
#host listing count
table(data$host_listings_count)
data=subset(data,!is.na(data$host_listings_count))
table(data$host_listings_count)
##response
data=subset(data,!is.na(data$host_response_time))
data$host_response = ifelse(data$host_response_time=='within an hour',1,ifelse(data$host_response_time=='within a few hours',2,3))
table(data$host_response)
data=subset(data,!is.na(data$host_response ))

#host experience
host_since_year = as.numeric(substr(data$host_since,1,4))
data$host_experience = 2019 - host_since_year
data=subset(data,!is.na(data$host_experience))
table(data$host_experience)
##instant bookaable
data=subset(data,!is.na(data$instant_bookable))
data$instant_bookable = ifelse(data$instant_bookable=='TRUE',1,0)
table(data$instant_bookable)
data=subset(data,!is.na(data$instant_bookable))
table(data$instant_bookable)
###price per person
data$price = gsub("\\$","",data$price)
data$price = gsub(",","",data$price)
data$price = as.numeric(data$price)
data$price_person = data$price/data$accommodates
table(data$price_person)
data=subset(data,!is.na(data$price_person))
data=subset(data, data$price_person!='Inf')
table(data$price_person)
#property type

table(data$property_type)
data=subset(data,!is.na(data$property_type))
data$property_newtype = ifelse(data$property_type=='Apartment',1,ifelse(data$property_type=='House',2,ifelse(data$property_type=='Condominium',3,ifelse(data$property_type=='Townhouse',4,ifelse(data$property_type=='Loft',5,6)))))
table(data$property_newtype)
data=subset(data,!is.na(data$property_newtype))
#resquire license
data=subset(data,!is.na(data$requires_license))
data$requires_license = ifelse(data$requires_license=='TRUE',1,0)
table(data$requires_license)
data=subset(data,!is.na(data$requires_license))
#room type
data=subset(data,!is.na(data$room_type))
table(data$room_type)

#maximum night
data=subset(data,!is.na(data$maximum_nights))
data$maximum_nights = as.numeric(data$maximum_nights)
data$minimum_nights = as.numeric(data$minimum_nights)
data$maximum_night_range = ifelse(data$maximum_nights<=7,'1',
                                  ifelse(data$maximum_nights<=14,'2',
                                         ifelse(data$maximum_nights<=30,'3',
                                                ifelse(data$maximum_nights<=60,'4',
                                                       ifelse(data$maximum_nights<=90,'5',
                                                              ifelse(data$maximum_nights<=180,'6',
                                                                     ifelse(data$maximum_nights<=360,'7',
                                                                            ifelse(data$maximum_nights<=720,'8',
                                                                                   ifelse(data$maximum_nights<=1080,'9', '10')))))))))
table(data$maximum_night_range)
data=subset(data,!is.na(data$maximum_night_range))
##minimum night
data=subset(data,!is.na(data$minimum_nights))
data$minimum_night_range = ifelse(data$minimum_nights<=3,'1',
                                  ifelse(data$minimum_nights<=7,'2',
                                         ifelse(data$minimum_nights<=10,'3',
                                                ifelse(data$minimum_nights<=14,'4',
                                                       ifelse(data$minimum_nights<=30,'5',
                                                              ifelse(data$minimum_nights<=60,'6','7'))))))
table(data$minimum_night_range)
data=subset(data,!is.na(data$minimum_night_range))
table(data$minimum_night_range)
##average bathroom
data$avg_bathrooms=data$bathrooms/data$accommodates
table(data$avg_bathrooms)
data=subset(data,!is.na(data$avg_bathrooms))

#create a host_identity_verifie dummy variable(=1 if its verified)
table(data$host_identity_verified)
data$host_identity_verified=ifelse(data$host_identity_verified=="TRUE",1,0)
data=subset(data,!is.na(data$host_identity_verified))
table(data$host_identity_verified)
##location is exact 
data$is_location_exact=ifelse(data$is_location_exact=="TRUE",1,0)
data=subset(data,!is.na(data$is_location_exact))
table(data$is_location_exact)
#host response rate
data$host_response_rate = as.numeric(gsub("%.*","",data$host_response_rate))
data$host_response_rate=data$host_response_rate /100
data=subset(data,!is.na(data$host_response_rate))
table(data$host_response_rate)



colnames(data)
house2 = data[,c(2,4,5,6,7,15,20,26,27,28,32,38,41,58,59,71,72,73,74,75,76,77,78,79,80,81,82,70)]

##any null
colSums(is.na(house2))

hotel_data2=house2
###
table(hotel_data2$city_popular)
hotel_data2$room_type=ifelse(hotel_data2$room_type=='Entire home/apt',1,ifelse(hotel_data2$room_type=='Private room',2,3))


table(hotel_data2$room_type)
hotel_data2$city_popular=ifelse(hotel_data2$city_popular=='Asheville',1,
                                ifelse(hotel_data2$city_popular=='Austin',2,
                                       ifelse(hotel_data2$city_popular=='Boston',3,
                                              ifelse(hotel_data2$city_popular=='Chicago',4,
                                                     ifelse(hotel_data2$city_popular=='Denver',5,
                                                            ifelse(hotel_data2$city_popular=='Los Angeles',6,
                                                                   ifelse(hotel_data2$city_popular=='Nashville',7,
                                                                          ifelse(hotel_data2$city_popular=='New Orleans',8,
                                                                                 ifelse(hotel_data2$city_popular=='New York',9,
                                                                                        ifelse(hotel_data2$city_popular=='Oakland',10,
                                                                                               ifelse(hotel_data2$city_popular=='Portland',11,
                                                                                                      ifelse(hotel_data2$city_popular=='San Diego',12,
                                                                                                             ifelse(hotel_data2$city_popular=='San Francisco',13,
                                                                                                                    ifelse(hotel_data2$city_popular=='Santa Cruz',14,
                                                                                                                           ifelse(hotel_data2$city_popular=='Seattle',15,16
                                                                                                                                  
                                                                                                                           )))))))))))))))




##factor
hotel_data2$availability_30 = as.numeric(hotel_data2$availability_30)
hotel_data2$availability_60 = as.numeric(hotel_data2$availability_60)
hotel_data2$availability_90 = as.numeric(hotel_data2$availability_90)
hotel_data2$availability_365 = as.numeric(hotel_data2$availability_365)
hotel_data2$bed_person = as.numeric(hotel_data2$bed_person)
hotel_data2$host_experience = as.numeric(hotel_data2$host_experience)
hotel_data2$accommodates = as.numeric(hotel_data2$accommodates)
hotel_data2$cleaning_fee = as.numeric(hotel_data2$cleaning_fee)
hotel_data2$extra_people = as.numeric(hotel_data2$extra_people)
hotel_data2$description_len = as.numeric(hotel_data2$description_len)
hotel_data2$price_person = as.numeric(hotel_data2$price_person)
hotel_data2$host_listings_count = as.numeric(hotel_data2$host_listings_count)
hotel_data2$avg_bathrooms =  as.numeric(hotel_data2$avg_bathrooms )
hotel_data2$host_response_rate = as.numeric(hotel_data2$host_response_rate)


hotel_data2$host_is_superhost = as.numeric(hotel_data2$host_is_superhost)
hotel_data2$host_response = as.numeric(hotel_data2$host_response)
hotel_data2$instant_bookable = as.numeric(hotel_data2$instant_bookable)
hotel_data2$room_type = as.numeric(hotel_data2$room_type)
hotel_data2$property_newtype = as.numeric(hotel_data2$property_newtype)
##hotel_data2$high_booking_rate = as.factor(hotel_data2$high_booking_rate)
hotel_data2$requires_license = as.numeric(hotel_data2$requires_license)
hotel_data2$maximum_night_range = as.numeric(hotel_data2$maximum_night_range)
hotel_data2$minimum_night_range=as.numeric(hotel_data2$minimum_night_range)
hotel_data2$cancel_policy = as.numeric(hotel_data2$cancel_policy)
hotel_data2$city_popular = as.numeric(hotel_data2$city_popular)
hotel_data2$share_bathroom = as.numeric(hotel_data2$share_bathroom)
hotel_data2$host_identity_verified = as.numeric(hotel_data2$host_identity_verified)
hotel_data2$is_location_exact = as.numeric(hotel_data2$is_location_exact )

colSums(is.na(hotel_data2))


hotel_data2 = subset(hotel_data2,!is.na(hotel_data2$cleaning_fee))
sum(is.na(hotel_data2$cleaning_fee))
colnames(hotel_data2)



##xg boost model
library(xgboost)

####model1 <- xgboost(data = hotel_data2[train_inds,0:26], label = hotel_data2$high_booking_rate[train_inds], max.depth = 15, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic") `
##y_pred <- predict(model1, data.matrix(hotel_data2[-train_inds,0:26]))


##xgboost
highbooking = hotel_data2$high_booking_rate
label=as.integer(hotel_data2$high_booking_rate)
hotel_data2$high_booking_rate = NULL
n = nrow(hotel_data2)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(hotel_data2[train.index,])
train.label = label[train.index]
test.data = as.matrix(hotel_data2[-train.index,])
test.label = label[-train.index]

xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)
##Define the parameters for multinomial classification
##num_class = length(levels(highbooking))
params = list(
  booster="gbtree",
  eta=0.001, #learning_rate
  max_depth=5, #Maximum depth of a tree.
  gamma=3,#Minimum loss reduction
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=2
)

# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit


# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)

class = ifelse(xgb.pred[,2]>0.5,1,0)

label2=label[-train.index]
label2=as.numeric(label2)
# Calculate the final accuracy
result = sum(ifelse(class==label2,1,0) )/length(class)
result



##boosting
train_inds <- sample(nrow(hotel_data2), .7*nrow(hotel_data2))
test_size <- nrow(hotel_data2)-length(train_inds)


set.seed(1)
boost.mod <- gbm(high_booking_rate~.,data=hotel_data2[train_inds,],distribution="bernoulli",n.trees=1000,interaction.depth=4)

boost_preds <- predict(boost.mod,newdata=hotel_data2[-train_inds,],type='response',n.trees=1000)

boost_class <- ifelse(boost_preds>.5,1,0)
boost_acc <- sum(ifelse(boost_class==hotel_data2$high_booking_rate[-train_inds],1,0))/test_size
boost_acc ##0.79


##rf
library(randomForest)
library(gbm)

library(ISLR)

rf.mod <- randomForest(high_booking_rate~accommodates+availability_60+availability_30+availability_365+availability_90+cleaning_fee+extra_people+host_is_superhost+host_listings_count+instant_bookable+requires_license+description_len+bed_person+share_bathroom+cancel_policy+city_popular+host_response+host_experience+price_person+property_newtype+room_type+minimum_night_range+maximum_night_range+avg_bathrooms+host_response_rate+is_location_exact+host_identity_verified,data=hotel_data2,subset=train_inds,mtry=20,ntree=1000,importance=TRUE)
rf_preds <- predict(rf.mod,newdata=hotel_data2[-train_inds,])
rf_acc <- sum(ifelse(rf_preds==hotel_data2$high_booking_rate[-train_inds],1,0))/test_size

rf.mod
rf_acc 



