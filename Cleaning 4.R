setwd('/Users/yuhaibo/Downloads/758T/project/')

library(plyr)
library(stringr)


# import the data
airbnb_train_x <- read.csv('raw_data/airbnb_train_x.csv')
airbnb_test_x <- read.csv('raw_data/airbnb_test_x.csv')



# concat the training and test data
airbnb_x <- rbind(airbnb_train_x, airbnb_test_x)

# make copy
data_all <- airbnb_x






# *************************************Data Cleaning*******************************

# amenity
data_all$amenities <- gsub('"',"",data_all$amenities)

Wifi_ornot <- c()
airconditioner <- c()
tv <- c()
kitchen <- c()
essentials <- c()
l <- c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$amenities[i], ",")
  if (("Wireless Internet" %in% m[[1]])|("Wifi" %in% m[[1]]))
    Wifi_ornot = c(Wifi_ornot,1)
  else
    Wifi_ornot = c(Wifi_ornot,0)
  if (("Air conditioning" %in% m[[1]])|("Heating" %in% m[[1]]))
    airconditioner = c(airconditioner ,1)
  else
    airconditioner = c(airconditioner ,0)
  if ("Free parking" %in% m[[1]])
    freeparking = c(freeparking,1)
  else
    freeparking = c(freeparking,0)
  if ("balcony" %in% m[[1]])
    Balcony = c(Balcony ,1)
  else
    Balcony  = c(Balcony ,0)
  if ("Cable TV" %in% m[[1]])
    tv = c(tv ,1)
  else
    tv  = c(tv ,0)
  if ("Kitchen" %in% m[[1]])
    kitchen = c(kitchen,1)
  else
    kitchen = c(kitchen,0)
  if ("Essentials" %in% m[[1]])
    essentials= c(essentials,1)
  else
    essentials = c(essentials,0)
  l <- c(length(m[[1]]), l)
  
}

data_all$wifi_ornot <- Wifi_ornot
data_all$airconditioner <- airconditioner
data_all$tv <- tv
data_all$kitchen <- kitchen
data_all$essentials <- essentials
data_all$amenitiesNum <- l


# availability
data_all$availability_30 <- as.numeric(as.character(data_all$availability_30))
data_all$availability_30[is.na(data_all$availability_30)] <- round(median(data_all$availability_30, na.rm = TRUE))


data_all$availability_365 <- as.numeric(as.character(data_all$availability_365))
data_all$availability_365[is.na(data_all$availability_365)] <- round(median(data_all$availability_365, na.rm = TRUE))


data_all$availability_60 <- as.numeric(as.character(data_all$availability_60))
data_all$availability_60[is.na(data_all$availability_60)] <- round(median(data_all$availability_60, na.rm = TRUE))


data_all$availability_90 <- as.numeric(as.character(data_all$availability_90))
data_all$availability_90[is.na(data_all$availability_90)] <- round(median(data_all$availability_90, na.rm = TRUE))



# bedrooms 
table(data_all$bedrooms)
data_all$bedrooms <- as.numeric(as.character(data_all$bedrooms))
data_all$bedrooms[is.na(data_all$bedrooms)] <- round(median(data_all$bedrooms, na.rm = TRUE))



# bathroom
table(data_all$bathrooms)
data_all$bathrooms <- as.numeric(as.character(data_all$bathrooms))
data_all$bathrooms[is.na(data_all$bathrooms)] <- round(median(data_all$bathrooms, na.rm = TRUE))



# accomodates
table(data_all$accommodates)
data_all$accommodates <- as.numeric(as.character(data_all$accommodates))
data_all$accommodates[is.na(data_all$accommodates)] <- round(median(data_all$accommodates, na.rm = TRUE))



# beds
table(data_all$beds)
data_all$beds <- as.numeric(as.character(data_all$beds))
data_all$beds[is.na(data_all$beds)] <- round(median(data_all$beds, na.rm = TRUE))


#create a new variable indicating whether there are shared bathrooms or not(=1 means yes)
data_all$shared_bathroom <- ifelse(data_all$bedrooms/data_all$bathrooms <= 1, 0, 1)
sum(is.na(data_all$shared_bathroom))
table(data_all$shared_bathroom)

data_all$shared_bathroom[is.na(data_all$shared_bathroom)] <- 0


#create new average variables
data_all$average_beds <- data_all$beds/data_all$accommodates


# bed_type
table(data_all$bed_type)
data_all$bed_type <- as.character(data_all$bed_type)
data_all$bed_type <- ifelse(str_detect(data_all$bed_type,"100%"),"Real Bed" ,data_all$bed_type)
data_all$bed_type <- ifelse(str_detect(data_all$bed_type,"81%"),"Real Bed" ,data_all$bed_type)
data_all$bed_type[data_all$bed_type == ''] <- 'Real Bed'
sum(is.na(data_all$bed_type))



#reducing factors of cancellation policy
table(data_all$cancellation_policy)
data_all$cancellation_policy <- as.character(data_all$cancellation_policy)

data_all$cancellation_policy <- ifelse(str_detect(data_all$cancellation_policy,"1"),
                                       "strict" ,data_all$cancellation_policy)
data_all$cancellation_policy <- ifelse(str_detect(data_all$cancellation_policy,"2"),
                                       "strict" ,data_all$cancellation_policy)
data_all$cancellation_policy <- ifelse(str_detect(data_all$cancellation_policy,"5"),
                                       "strict" ,data_all$cancellation_policy)



#cleaning fee column
#if the values of cleaning_fee are NA,we assume they are 0 
data_all$cleaning_fee <- substr(data_all$cleaning_fee,2,1000)
data_all$cleaning_fee <- as.numeric(data_all$cleaning_fee)
data_all$cleaning_fee[is.na(data_all$cleaning_fee)] <- 0



#extra people column
data_all$extra_people <- substr(data_all$extra_people,2,1000)
data_all$extra_people <- as.numeric(data_all$extra_people)
sum(is.na(data_all$extra_people))



# first review
first_review_year<-substr(data_all$first_review,1,4)

first_review_year <- as.numeric(first_review_year)
first_review_year[is.na(first_review_year)] <- median(first_review_year, na.rm = TRUE)

data_all$first_review <- 2019-first_review_year

  


# guests included
sum(data_all$guests_included == 0)

data_all$guests_included[data_all$guests_included == 0] <- 1



# host_has_profile_pic
table(data_all$host_has_profile_pic)

data_all$host_has_profile_pic <- ifelse(data_all$host_has_profile_pic == 'f', 0, 1)


#host_identity_verifie
table(data_all$host_identity_verified)

data_all$host_identity_verified[!(data_all$host_identity_verified == 'f' |
                                 data_all$host_identity_verified == 't') ] <- 't'

data_all$host_identity_verified <- ifelse(data_all$host_identity_verified=='t',1,0)



#host_is_superhost
table(data_all$host_is_superhost)

data_all$host_is_superhost[!(data_all$host_is_superhost == 'f' |
                                    data_all$host_is_superhost == 't') ] <- 'f'

data_all$host_is_superhost <- ifelse(data_all$host_is_superhost=='t',1,0)



# host_listings_count
data_all$host_listings_count <- as.numeric(data_all$host_listings_count)
sum(is.na(data_all$host_listings_count))


# host_verifications
data_all$host_verifications <- as.character(data_all$host_verifications)

host_ver_num = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$host_verifications[i], ",")[[1]])
  host_ver_num = c(host_ver_num,m)
  
}

data_all$host_ver_num <- host_ver_num




#instant_bookable
table(data_all$instant_bookable)

data_all$instant_bookable <- ifelse(data_all$instant_bookable=='t',1,0)



#intercation
data_all$interaction <- as.character(data_all$interaction)
interaction_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$interaction[i], " ")[[1]])
  interaction_len = c(interaction_len,m)
  
}

data_all$interaction_len <- interaction_len





#is_location_exact
table(data_all$is_location_exact)
data_all$is_location_exact <- ifelse(data_all$is_location_exact=='f',0,1)




#making a host_experience column (number of years)
host_since_year <- as.numeric(substr(data_all$host_since,1,4))
host_since_year[is.na(host_since_year)] <- round(median(host_since_year, na.rm = TRUE))

data_all$host_experience <- 2019 - host_since_year
data_all$host_since <- NULL


#host response rate
data_all$host_response_rate <- as.numeric(gsub("%.*","",data_all$host_response_rate))
sum(is.na(gsub("%.*","",data_all$host_response_rate)))

data_all$host_response_rate[is.na(data_all$host_response_rate)] <- round(median(data_all$host_response_rate, na.rm = TRUE))

data_all$host_response_rate <- data_all$host_response_rate / 100



# house rules
data_all$house_rules <- as.character(data_all$house_rules)

house_rules_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$house_rules[i], " ")[[1]])
  house_rules_len = c(house_rules_len,m)
  
}

data_all$house_rules_len <- house_rules_len




#host about
data_all$host_about <- as.character(data_all$host_about)

host_about_len <- c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$host_about[i], " ")[[1]])
  host_about_len = c(host_about_len,m)
  
}

data_all$host_about_len <- host_about_len





#host response time
#if the host responses within one hour,we think its a fast response
table(data_all$host_response_time)
data_all$fast_response <- ifelse(data_all$host_response_time=="within an hour",1,
                           ifelse(data_all$host_response_time=="within a few hours",2,3))


# is business_travel_ready
table(data_all$is_business_travel_ready)
data_all$is_business_travel_ready <- ifelse(data_all$is_business_travel_ready == 't', 't', 'f')



# maximum nights
is.numeric(data_all$maximum_nights)
sum(is.na(data_all$maximum_nights))

data_all$maximum_nights <- ifelse(data_all$maximum_nights <= 365,data_all$maximum_nights, 365)

data_all$maximum_nights[is.na(data_all$maximum_nights)] <- round(median(data_all$maximum_nights, na.rm = TRUE))


# minimum nights
is.numeric(data_all$minimum_nights)
sum(is.na(data_all$minimum_nights))

data_all$minimum_nights[is.na(data_all$minimum_nights)] <- round(median(data_all$minimum_nights, na.rm = TRUE))

data_all$minimum_nights <- ifelse(data_all$minimum_nights <= 365,data_all$minimum_nights,365)





#neighborhood overview
data_all$neighborhood_overview <- as.character(data_all$neighborhood_overview)

neighborhood_overview_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$neighborhood_overview[i], " ")[[1]])
  neighborhood_overview_len = c(neighborhood_overview_len,m)
  
}

data_all$neighborhood_overview_len <- neighborhood_overview_len


#notes
data_all$notes <- as.character(data_all$notes)

notes_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$notes[i], " ")[[1]])
  notes_len = c(notes_len,m)
  
}

data_all$notes_len <- notes_len






#price column
#create data average price column
data_all$price <- gsub("\\$","",data_all$price)
data_all$price <- gsub(",","",data_all$price)
data_all$price <- as.numeric(data_all$price)

data_all$price[is.na(data_all$price)] <- round(median(data_all$price, na.rm = TRUE))



# new column, price*minimum_nights
data_all$price_miniNights <- data_all$price * data_all$minimum_nights



#property type column
table(data_all$property_type)
data_all$property_type <- as.character(data_all$property_type)

data_all$property_type<-ifelse(data_all$property_type=="House"|data_all$property_type=="Townhouse"|
                                 data_all$property_type=="Villa","Big house",data_all$property_type)

data_all$property_type<-ifelse(data_all$property_type=="Guesthouse"|data_all$property_type=="Barn"|
                                 data_all$property_type=="Cottage"|data_all$property_type=="In-law"|
                                 data_all$property_type=="Tiny house"|data_all$property_type=="Bungalow",
                               "Small house",data_all$property_type)

data_all$property_type<-ifelse(data_all$property_type== "Loft"|data_all$property_type== "Apartment"|
                                 data_all$property_type== "Condominium"|data_all$property_type== "Guest suite",
                               "APT",data_all$property_type)

data_all$property_type<-ifelse(data_all$property_type== "Tent"|data_all$property_type== "Lighthouse"|
                                 data_all$property_type== "Plane"|data_all$property_type== "Train"|
                                 data_all$property_type== "Nature lodge"|data_all$property_type== "Earth house"|
                                 data_all$property_type== "Earth House"|data_all$property_type== "Tipi"|
                                 data_all$property_type== "Hut"|data_all$property_type== "Casa particular (Cuba)"|
                                 data_all$property_type== "Cave"|data_all$property_type== "Yurt"|
                                 data_all$property_type== "Cabin"|data_all$property_type== "Treehouse"|
                                 data_all$property_type== "Camper/RV"|data_all$property_type== "Boat",
                               "FUN Living",data_all$property_type)

data_all$property_type<-ifelse(data_all$property_type== "Resort"|data_all$property_type== "Island"|
                                 data_all$property_type== "Farm stay"|data_all$property_type== "Timeshare"|
                                 data_all$property_type== "Castle"|data_all$property_type== "Chalet"|
                                 data_all$property_type== "Vacation home","Reort",data_all$property_type)                          

data_all$property_type<-ifelse(data_all$property_type== "Bed and breakfast"|data_all$property_type== 
                                 "Bed & Breakfast"|data_all$property_type== "Dorm"|data_all$property_type==
                                 "Hostel","Eco_Living", data_all$property_type)                             

data_all$property_type<-ifelse(data_all$property_type== "Hotel"|data_all$property_type==  "Aparthotel"|
                                 data_all$property_type== "Serviced apartment"|data_all$property_type== 
                                 "Boutique hotel" ,"HOTEL",data_all$property_type) 

data_all$property_type<-ifelse(data_all$property_type!="Big house"&data_all$property_type!=
                                 "Small house"&data_all$property_type!="APT"&data_all$property_type!=
                                 "FUN Living"&data_all$property_type!="Eco_Living"&data_all$property_type!=
                                 "HOTEL"&data_all$property_type!="Reort","OTHER",data_all$property_type)





#requires_license
table(data_all$requires_license)
data_all$requires_license <- ifelse(data_all$requires_license == 't', 1, 0)


# require_guest_profile_picture
table(data_all$require_guest_profile_picture)
data_all$require_guest_profile_picture <- ifelse(data_all$require_guest_profile_picture == 't', 1, 0)


# require_guest_phone_verification
table(data_all$require_guest_phone_verification)

data_all$require_guest_phone_verification <- ifelse(data_all$require_guest_phone_verification == 't', 1, 0)


# room_type
table(data_all$room_type)
data_all$room_type <- as.character(data_all$room_type)

data_all$room_type[data_all$room_type == ''] <- 'Entire home/apt'



# security_deposit
data_all$security_deposit <- substr(data_all$security_deposit,2,1000)
data_all$security_deposit <- as.numeric(data_all$security_deposit)

data_all$security_deposit[is.na(data_all$security_deposit)] <- 0



#space
data_all$space <- as.character(data_all$space)
space_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$space[i], " ")[[1]])
  space_len = c(space_len,m)
  
}

data_all$space_len <- space_len






# state
table(data_all$state)

data_all$state <- tolower(data_all$state)

data_all$state <- ifelse(!(data_all$state == 'ca' | data_all$state == 'co' |
                             data_all$state == 'dc' | data_all$state == 'il' | 
                             data_all$state == 'la' | data_all$state == 'ma' |
                             data_all$state == 'nc' | data_all$state == 'ny' |
                             data_all$state == 'or' | data_all$state == 'tn' | 
                             data_all$state == 'tx' | data_all$state == 'wa'),
                         'OTHER', data_all$state)


# transit
data_all$transit <- as.character(data_all$transit)
data_all$transit <- gsub('"',"",data_all$transit)

Uber <- c()
Lyft <- c()
Metro <- c()
Bikeshare <- c()
subway <- c()
bus <- c()


transit_len = c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$transit[i], " ")[[1]]
  if ("Uber" %in% m)
    Uber = c(Uber,1)
  else
    Uber  = c(Uber,0)
  if ("Lyft" %in% m)
    Lyft = c(Lyft,1)
  else
    Lyft  = c(Lyft,0)
  if ("Metro" %in% m)
    Metro = c(Metro,1)
  else
    Metro = c(Metro,0)
  if ("Bike share" %in% m)
    Bikeshare= c(Bikeshare,1)
  else
    Bikeshare = c(Bikeshare,0)
  if ("subway" %in% m)
    subway= c(subway,1)
  else
    subway = c(subway,0)
  if ("bus" %in% m)
    bus = c(bus,1)
  else
    bus = c(bus,0)
  transit_len = c(transit_len,length(m))
  
}

data_all$Uber <- Uber
data_all$Lyft <- Lyft
data_all$Metro <- Metro
data_all$Bikeshare <- Bikeshare
data_all$subway <- subway
data_all$bus <- bus

data_all$transit_len <- transit_len




# weekly_price
data_all$weekly_price<-gsub('\\$','',data_all$weekly_price)
data_all$weekly_price<-gsub('.\\.','',data_all$weekly_price)
data_all$weekly_price <- as.numeric(data_all$weekly_price)

sum(is.na(data_all$weekly_price))
data_all$weekly_price <- ifelse(is.na(data_all$weekly_price), data_all$price * 7,data_all$weekly_price)



#description
data_all$description=as.character(data_all$description)

description_len <- c()
description_freq <- c()

wds <- c('private', 'new', 'free')

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$description[i], " ")[[1]]
  l = length(m)
  wds_freq = sum(m %in% wds)
  
  description_len = c(description_len,l)
  description_freq = c(description_freq, wds_freq)
}

data_all$description_len <- description_len
data_all$description_freq <- description_freq




#city
data_all$city_name <- as.character(data_all$city_name)

data_all$city_name <- tolower(data_all$city_name)
data_all$city_name <- trimws(data_all$city_name)
sort(table(data_all$city_name))

data_all$city_popular <- ifelse(data_all$city_name %in% 
                                     c("nashville", "los angeles","san diego","washington dc",
                                       "new orleans","san francisco","new york","seattle","chicago",
                                       "boston","austin","portland","denver","oakland",
                                       "asheville", "santa cruz"),data_all$city_name,'Other')




# save the data
write.csv(data_all, 'train_cleaned_h.csv', row.names = TRUE)


















## *******************************EDA***************************************
########### not the right version

hist(airbnb_train$accommodates)
summary(airbnb_train$bathrooms)
table(airbnb_train$bedrooms)

barplot(table(airbnb_train$city))
table(airbnb_train$cancellation_policy,airbnb_train$high_booking_rate)

# property type frequency
property_type <- data.frame(table(airbnb_train$property_type))
property_type_top <- property_type[order(property_type$Freq, decreasing = TRUE),][1:5,]
barplot(property_type_top$Freq, names=property_type_top$Var1)

# room type
barplot(table(airbnb_train$room_type))

# price vs bedroom
airbnb_train[which(airbnb_train$bedrooms == 'within a day'), 'bedrooms'] = mean(airbnb_train$bedrooms)
boxplot(airbnb_train$price~airbnb_train$bedrooms)

# high booking rate vs price
aggregate(airbnb_train$price, list(airbnb_train$high_booking_rate), mean)

# scores vs pirce,     cannot see direct relationship by plot
plot(aggregate(airbnb_train$review_scores_rating, list(airbnb_train$price), mean))



