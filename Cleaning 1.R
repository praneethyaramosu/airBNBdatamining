library(dplyr)

data = airbnb_train_x
data$high_booking_rate = airbnb_train_y$high_booking_rate

#removing first row
data$X1 = NULL

#removing null rows from accomodates column
data = subset(data,!is.na(data$accommodates))

#converting availability columns to factors
data$availability_30 = data$availability_30/30
data$availability_30 = ifelse(data$availability_30>=0.5,"HIGH","LOW")
data$availability_60 = data$availability_60/60
data$availability_60 = ifelse(data$availability_60>=0.5,"HIGH","LOW")
data$availability_90 = data$availability_90/90
data$availability_90 = ifelse(data$availability_90>=0.5,"HIGH","LOW")
data$availability_365 = data$availability_365/365
data$availability_365 = ifelse(data$availability_365>=0.5,"HIGH","LOW")

#making a total availability column by counting if number of "HIGH"s > number of "LOWS"
count_30 = ifelse(data$availability_30=='HIGH',1,0)
count_60 = ifelse(data$availability_60=='HIGH',1,0)
count_90 = ifelse(data$availability_90=='HIGH',1,0)
count_365 = ifelse(data$availability_365=='HIGH',1,0)
count_availability = count_30 + count_60 + count_90 + count_365
count_availability = count_availability/4
data$availability = ifelse(count_availability>=0.5,"HIGH","LOW")
data = data[,c(1:7,71,8:70)]
data$availability = as.factor(data$availability)

#rounding number of bathrooms
data$bathrooms = round(data$bathrooms)

#making bed type a factor
data$bed_type = ifelse(data$bed_type == "Real Bed",data$bed_type,"OTHER")
data$bed_type = as.factor(data$bed_type)

#reducing factors of cancellation policy
data$cancellation_policy = ifelse((data$cancellation_policy=="no_refunds") | (data$cancellation_policy=="strict") | (data$cancellation_policy=="super_strict_30") | (data$cancellation_policy=="super_strict_60"), "STRICT", "LINEANT")
data$cancellation_policy = as.factor(data$cancellation_policy)

#cleaning fee column
data$cleaning_fee = substr(data$cleaning_fee,2,1000)
data$cleaning_fee[is.na(data$cleaning_fee)] = 0
data$cleaning_fee = as.numeric(data$cleaning_fee)
data = subset(data,!is.na(data$cleaning_fee))

#experiences offered is none for all rows

#extra people column
data$extra_people = substr(data$extra_people,2,1000)
data$extra_people = as.numeric(data$extra_people)

#host_about column
data$host_about = ifelse(is.na(data$host_about),0,1)
data$host_about = as.factor(data$host_about)

#cleaning first review column
first_review_year = substr(data$first_review,1,4)
data$first_review = ifelse((first_review_year == "2016") | (first_review_year == "2017") | (first_review_year == "2018") | (first_review_year == "2015"), "NEW","OLD")
data$first_review = as.factor(data$first_review)

#host acceptance rate has too many nulls, cant be used

#host has profile pic does not seem to be an important factor

data$host_identity_verified = as.factor(data$host_identity_verified)

#host_listings_count may not be an important factor, if we decide to use it, it has 142 nulls
data = subset(data,!is.na(data$host_listings_count))

#making a host_experience column (number of years)
host_since_year = as.numeric(substr(data$host_since,1,4))
data$host_experience = 2019 - host_since_year
data = data[,c(1:35,72,36:71)]

#using mice package to predict the NA values in columns host_response_rate and host_response_time
data$host_has_profile_pic = as.factor(data$host_has_profile_pic)
data$host_response_time = as.factor(data$host_response_time)
data$host_is_superhost = as.factor(data$host_is_superhost)
data$host_response_rate = as.numeric(gsub("%.*","",data$host_response_rate))


host_data = data[,c("host_has_profile_pic","host_identity_verified","host_is_superhost","host_total_listings_count","host_experience","host_response_rate","host_response_time")]
library(mice)
init = mice(host_data, maxit=0) 
meth = init$method
predM = init$predictorMatrix
meth[c("host_has_profile_pic")] = ""
meth[c("host_identity_verified")] = ""
meth[c("host_is_superhost")] = ""
meth[c("host_total_listings_count")] = ""
meth[c("host_experience")] = ""
meth[c("host_response_rate")] = "pmm"
meth[c("host_response_time")] = "polyreg"
set.seed(103)
imputed = mice(host_data, method=meth, predictorMatrix=predM, m=5)
imputed = complete(imputed)
host_data = imputed
write.csv(host_data,"D:\\Proneeth\\DATA MINING\\host_data.csv", row.names = TRUE)


data$host_response_rate = host_data$host_response_rate
data$host_response_time = host_data$host_response_time
data$host_response_time = as.factor(data$host_response_time)

#house_rules
data$house_rules = ifelse(is.na(data$house_rules),0,1)
data$house_rules = as.factor(data$house_rules)

#instant bookable
data$instant_bookable = as.factor(data$instant_bookable)

#interaction
data$interaction = ifelse(is.na(data$interaction),0,1)

#is_business_travel_ready has way too many nulls, probably cant be used.

#is_location_exact
data$is_location_exact = as.factor(data$is_location_exact)

#maximum_nights
data$maximum_nights = ifelse(data$maximum_nights <=10,"FEW",ifelse(data$maximum_nights<=100,"LOT","UNLIMITED"))

#price column
data$price = gsub("\\$","",data$price)
data$price = gsub(",","",data$price)
data$price = as.numeric(data$price)

#property type column
data$property_type = ifelse((data$property_type == "Townhouse") | (data$property_type == "Tiny house") ,"House",data$property_type)
data$property_type = ifelse(data$property_type == "Loft","Apartment",data$property_type)
data$property_type = ifelse((data$property_type != "Apartment") & (data$property_type != "Condominium") & (data$property_type != "House") , "OTHER",data$property_type )
data$property_type = as.factor(data$property_type)

#about 90% of guest_phone_verification and guest_requires_profile_picture values are FALSE, so it might not be an effective predictor

#requires_lisence
data$requires_license = as.factor(data$requires_license)

#room type column
data$room_type = as.factor(data$room_type)

#security deposit
data$security_deposit = ifelse(is.na(data$security_deposit) | (data$security_deposit == "$0.00"),"NO","YES")
data$security_deposit = as.factor(data$security_deposit)

#square_feet column has almost all nulls, so cant be used.

#most transits are specified, so almost 90% are 1's. Probably not a good predictor

write.csv(data,"D:\\Proneeth\\DATA MINING\\cleaneddata.csv", row.names = TRUE)


