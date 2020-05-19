train_X<-read.csv("airbnb_train_x.csv")
train_Y<-read.csv("airbnb_train_y.csv")
data<-train_X
data$high_booking_rate<-train_Y$high_booking_rate


# Accomondates
# 1. change to charactor
data$accommodates<-as.character(data$accommodates)
unique(data$accommodates)
data$accommodates<-ifelse(data$accommodates!="t",data$accommodates,NA)
unique(data$accommodates)
data<-subset(data,!is.na(data$accommodates))
unique(data$accommodates)
data$accommodates<-as.integer(data$accommodates)




# Amenities - count number of amenitie

data$amenities <-as.character(data$amenities )
amenities_counts = c()

for (i in 1:nrow(data))
{
  m = strsplit(data$amenities[i], ",")
  amenities_counts  = c(amenities_counts ,lengths(m))
}
data$amenities_count<-amenities_counts
data$amenities<-NULL


## Avilability 30,60,90,365 
data$availability_30<-as.factor(data$availability_30)
unique(data$availability_30)
data$availability_30<-ifelse(data$availability_30!="t"|data$availability_30!="f",data$availability_30,NA)
data$availability_30<-data$availability_30/30
data$availability_30<-ifelse(data$availability_30>=0.5,"HIGH","LOW")
data$availability_30<-as.factor(data$availability_30)

data$availability_60<-as.factor(data$availability_60)
data$availability_60<-ifelse(data$availability_60!="Santa Monica"|data$availability_60!="California"|data$availability_60!="United States",data$availability_60,NA)
data$availability_60<-data$availability_60/60
data$availability_60<-ifelse(data$availability_60>=0.5,"HIGH","LOW")
data$availability_60<-as.factor(data$availability_60)

data$availability_90<-as.factor(data$availability_90)
data$availability_90<-ifelse(data$availability_90!="Winni",data$availability_90,NA)
data$availability_90<-data$availability_90/90
data$availability_90<-ifelse(data$availability_90>=0.5,"HIGH","LOW")
data$availability_90<-as.factor(data$availability_90)


data$availability_365<-as.factor(data$availability_365)
data$availability_365<-ifelse(data$availability_365!="Winni",data$availability_365,NA)
data$availability_365<-data$availability_365/365
data$availability_365<-ifelse(data$availability_365>=0.5,"HIGH","LOW")
data$availability_365<-as.factor(data$availability_365)



data$cancellation_policy<-ifelse((data$cancellation_policy=="no_refunds") | (data$cancellation_policy=="strict") | (data$cancellation_policy=="super_strict_30") | (data$cancellation_policy=="super_strict_60"), "STRICT", "LINEANT")
data$cancellation_policy<-as.factor(data$cancellation_policy)

data$bed_type<-as.character(data$bed_type)
data$bed_type<-ifelse(data$bed_type == "Real Bed",data$bed_type,"OTHER")
data$bed_type<-as.factor(data$bed_type)



data$bedrooms<-as.numeric(data$bedrooms)
data$bedrooms[is.na(data$bedrooms)]<-1

data$beds<-as.numeric(data$beds)

## Remove $ -cleaning fee NA - inpute with average $66
data$cleaning_fee<-as.character(data$cleaning_fee)
data$cleaning_fee<-gsub('[//$]','',data$cleaning_fee)
data$cleaning_fee<-as.numeric(data$cleaning_fee)
data$cleaning_fee[is.na(data$cleaning_fee)]<-mean(data$cleaning_fee[!is.na(data$cleaning_fee)])

#experiences_offered
#description
data$description=as.character(data$description)

description_count <- c()

for (i in 1:nrow(data))
{
  m = length(strsplit(data$description[i], " ")[[1]])
  description_count = c(description_count,m)
  
}

data$description_count<- description_count
data$description_count<-as.numeric(data$description_count)



#guests_included: inpute 0 with 1
str(data$guests_included)
sum(data$guests_included==0)

data$guests_included[data$guests_included==0]<-1

# host_acceptance_rate_removed too many NA
# host_has_profile_pic
data$host_has_profile_pic<-gsub('"',"",data$host_has_profile_pic)
data$host_has_profile_pic<-ifelse(data$host_has_profile_pic=="t"|data$host_has_profile_pic=="1",1,0)
data$host_has_profile_pic<-as.factor(data$host_has_profile_pic)
#host_identity_verified
data$host_identity_verified<-as.character(data$host_identity_verified)
data$host_identity_verified<-ifelse(data$host_identity_verified=="t"|data$host_identity_verified=="1",1,0)
data$host_identity_verified<-as.factor(data$host_identity_verified)
#hhost_is_superhost
data$host_is_superhost<-as.character(data$hhost_is_superhost)
data$host_is_superhost<-ifelse(data$host_is_superhost=="t",1,0)
data$host_is_superhost<-as.factor(data$host_is_superhost)


#host_listings_count
data$host_listings_count<-as.character(data$host_listings_count)
data$host_listings_count<-as.numeric(data$host_listings_count)
data$host_listings_count[is.na(data$host_listings_count)]<-1
data$host_listings_count[data$host_listings_count==0]<-1
data$host_listings_count<-as.numeric(data$host_listings_count)

#host_response_rate
data$host_response_rate<-as.character(data$host_response_rate)
data$host_response_rate<-as.numeric(sub("%", "",data$host_response_rate,fixed=TRUE))/100
data$host_response_rate
data$host_response_rate[is.na(data$host_response_rate)]<-mean(data$host_response_rate[!is.na(data$host_response_rate)])

#host_response_time

data$host_response_time=ifelse(data$host_response_time=="a few days or more","Slow",
                               ifelse(data$host_response_time=="within a day"|data$host_response_time=="f","Fair",
                                      ifelse(data$host_response_time=="within a few hours","Fast",
                                             ifelse(data$host_response_time=="within an hour","Exellent","Unknown"))))

data$host_response_time<-as.factor(data$host_response_time)
data$host_response_time
#host_since
data$host_since<-as.numeric(substr(data$host_since,1,4))
data$host_experience = 2019 - data$host_since
data$host_experience[is.na(data$host_experience)]<-mean(data$host_experience[!is.na(data$host_experience)])


#host_total_listings_count
data$host_total_listings_count<-as.character(data$host_total_listings_count)
data$host_total_listings_count<-as.numeric(data$host_total_listings_count)
data$host_total_listings_count[is.na(data$host_total_listings_count)]<-1

# host_verifications
data$host_verifications<-as.character(data$host_verifications)

host_verifications_count = c()

for (i in 1:nrow(data))
{
  m = strsplit(data$host_verifications[i], ",")
  host_verifications_count  = c(host_verifications_count ,lengths(m))
}

data$host_verifications_count<-host_verifications_count 

#house_rules
data$house_rules = ifelse(is.na(data$house_rules),0,1)
data$house_rules = as.factor(data$house_rules)

#instant_bookable
data$instant_bookable<-as.character(data$instant_bookable)


data$instant_bookable<-ifelse(data$instant_bookable!="$100.00"|data$instant_bookable!="$200.00"|data$instant_bookable!="",data$instant_bookable,NA)
data$instant_bookable<-as.factor(data$instant_bookable)

#interaction -categorical
data$interaction<-as.character(data$interaction)
data$interaction <- ifelse(nchar(data$interaction)>1,1,0)
data$interaction<-as.factor(data$interaction)

#is_business_travel_ready
data$is_business_travel_ready
data$is_business_travel_ready<-as.character(data$is_business_travel_ready)
data$is_business_travel_ready<- ifelse(nchar(data$is_business_travel_ready)>3,NA,data$is_business_travel_ready)
sum(is.na(data$is_business_travel_ready)) ### Only have 9 NA - WERID
data = subset(data,!is.na(data$is_business_travel_ready))

data$is_business_travel_ready<-as.factor(data$is_business_travel_ready)
#is_location_exact  _ NO NEED CLEAN

#license - 1/0
data$license<-as.character(data$license)
data$license<- ifelse(nchar(data$license)>1,1,0)
data$license<-as.factor(data$license)
#maximum_nights

data$minimum_nights[is.na(data$minimum_nights)]<-1
data$minimum_night_range <- ifelse(data$minimum_nights==1,'1',
                                   ifelse(data$minimum_nights<=3,'2',
                                          ifelse(data$minimum_nights<=7,'3',
                                                 ifelse(data$minimum_nights<=14,'4',
                                                        ifelse(data$minimum_nights<=30,'5',
                                                               ifelse(data$minimum_nights<=60,'6','7'))))))


data$minimum_night_range<-as.factor(data$minimum_night_range)
# MAXIMUN
data$maximum_nights[is.na(data$maximum_nights)]<-1
data$maximum_night_range = ifelse(data$maximum_nights<=7,'1',
                                  ifelse(data$maximum_nights<=14,'2',
                                         ifelse(data$maximum_nights<=30,'3',
                                                ifelse(data$maximum_nights<=60,'4',
                                                       ifelse(data$maximum_nights<=90,'5',
                                                              ifelse(data$maximum_nights<=180,'6',
                                                                     ifelse(data$maximum_nights<=360,'7',
                                                                            ifelse(data$maximum_nights<=720,'8',
                                                                                   ifelse(data$maximum_nights<=1080,'9', '10')))))))))


data$maximum_nights=NULL
data$maximum_night_range<-as.factor(data$maximum_night_range)

#monthly_price
data$monthly_price<-as.character(data$monthly_price)
data$monthly_price<-gsub('[//$]','',data$monthly_price)
data$monthly_price<-as.numeric(data$monthly_price)

#Price
data$price<-as.character(data$price)
data$price<-gsub('[//$]','',data$price)
data$price<-as.numeric(data$price)

#####   DROP NA
data<-subset(data,!is.na(data$price))
data$monthly<-data$monthly_price/30
data$cheapter<-data$monthly-data$price
data$monthly_cheapter<-ifelse(data$cheapter<0,1,0)
data$monthly_cheapter[is.na(data$monthly_cheapter)]<-0
data$monthly<-NULL
data$cheapter<-NULL

####property_type

data$property_type<-as.character(data$property_type)

data$property_type<-ifelse(data$property_type=="House"|data$property_type=="Townhouse"|data$property_type=="Villa","Big house",data$property_type)
unique(data$property_type)
data$property_type<-ifelse(data$property_type=="Guesthouse"|data$property_type=="Barn"|data$property_type=="Cottage"|data$property_type=="In-law"|data$property_type=="Tiny house"|data$property_type=="Bungalow","Small house",data$property_type)
unique(data$property_type)
data$property_type<-ifelse(data$property_type== "Loft"|data$property_type== "Apartment"|data$property_type== "Condominium"|data$property_type== "Guest suite","APT",data$property_type)
unique(data$property_type)
data$property_type<-ifelse(data$property_type== "Tent"|data$property_type== "Lighthouse"|data$property_type== "Plane"|data$property_type== "Train"|data$property_type== "Nature lodge"|data$property_type== "Earth house"|data$property_type== "Earth House"|data$property_type== "Tipi"|data$property_type== "Hut"|data$property_type== "Casa particular (Cuba)"|data$property_type== "Cave"|data$property_type== "Yurt"|data$property_type== "Cabin"|data$property_type== "Treehouse"|data$property_type== "Camper/RV"|data$property_type== "Boat","FUN Living",data$property_type)
unique(data$property_type)
data$property_type<-ifelse(data$property_type== "Resort"|data$property_type== "Island"|data$property_type== "Farm stay"|data$property_type== "Timeshare"|data$property_type== "Castle"|data$property_type== "Chalet"|data$property_type== "Vacation home","Reort",data$property_type)                          
unique(data$property_type)
data$property_type<-ifelse(data$property_type== "Bed and breakfast"|data$property_type== "Bed & Breakfast"|data$property_type== "Dorm"|data$property_type== "Hostel","Eco_Living", data$property_type)                             
unique(data$property_type)
data$property_type<-ifelse(data$property_type== "Hotel"|data$property_type==  "Aparthotel"|data$property_type== "Serviced apartment"|data$property_type== "Boutique hotel" ,"HOTEL",data$property_type) 
unique(data$property_type)
data$property_type<-ifelse(data$property_type!="Big house"&data$property_type!="Small house"&data$property_type!="APT"&data$property_type!="FUN Living"&data$property_type!="Eco_Living"&data$property_type!="HOTEL"&data$property_type!="Reort","OTHER",data$property_type)
unique(data$property_type)                                                                                                                    
#requires_license


data$requires_license<-as.factor(data$requires_license)

#room_type


data$room_type<-as.character(data$room_type)
data$room_type<-as.factor(data$room_type)

#security_deposit

data$security_deposit<-as.character(data$security_deposit)
data$security_deposit<-gsub('[//$]','',data$security_deposit)
data$security_deposit<-as.numeric(data$security_deposit)
data$security_deposit[is.na(data$security_deposit)]<-0

## City_name
data$city_name <- as.character(data$city_name)

data$city_popular <- ifelse(data$city_name %in% 
                              c("Nashville", "Los Angeles","San Diego","Washington DC",
                                "New Orleans","San Francisco","New York","Seattle","Chicago",
                                "Boston","Austin","Portland","Denver","Santa Cruz","Oakland",
                                "Asheville"),data$city_name,'Other')

data$city_popular=as.factor(data$city_popular)

write.csv(data,"C:/Users/Owner/Desktop/T- DataMining &PredictiveAnalytics/Project/jessicatrain.csv")
