library(plyr)
library(stringr)
setwd("C:/Users/ZXY0117/Desktop/Course_S2/Data Mining And Predictive Analytics(T)/Project/Data")
# import the data
airbnb_train_x <- read.csv('airbnb_train_x.csv')
airbnb_test_x <- read.csv('airbnb_test_x.csv')


# concat the training and test data
airbnb_x <- rbind(airbnb_train_x, airbnb_test_x)

# make copy
data_all <- airbnb_x


# *************************************Data Cleaning*******************************


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
#table(data_all$bathrooms)
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
data_all$shared_bathroom=as.factor(data_all$shared_bathroom)

#create new average variables
data_all$average_beds <- data_all$beds/data_all$accommodates
sum(is.na(data_all$average_beds))
data_all$average_beds=as.numeric(data_all$average_beds)

# bed_type
table(data_all$bed_type)
data_all$bed_type <- as.character(data_all$bed_type)
data_all$bed_type <- ifelse(str_detect(data_all$bed_type,"100%"),"Real Bed" ,data_all$bed_type)
data_all$bed_type <- ifelse(str_detect(data_all$bed_type,"81%"),"Real Bed" ,data_all$bed_type)
data_all$bed_type[data_all$bed_type == ''] <- 'Real Bed'
sum(is.na(data_all$bed_type))


#reducing factors of cancellation policy
data_all$cancellation_policy <- as.character(data_all$cancellation_policy)

data_all$cancellation_policy <- ifelse((data_all$cancellation_policy=="no_refunds") | 
                                         (data_all$cancellation_policy=="strict") | 
                                         (data_all$cancellation_policy=="super_strict_30") | 
                                         (data_all$cancellation_policy=="super_strict_60"), "3",
                                       ifelse(data_all$cancellation_policy=="moderate",2,1))
data_all$cancellation_policy <- as.factor(data_all$cancellation_policy)

#cleaning fee column
#if the values of cleaning_fee are NA,we assume they are 0 
data_all$cleaning_fee <- substr(data_all$cleaning_fee,2,1000)
data_all$cleaning_fee <- as.numeric(data_all$cleaning_fee)
data_all$cleaning_fee[is.na(data_all$cleaning_fee)] <- 0



#extra people column
data_all$extra_people <- substr(data_all$extra_people,2,1000)
data_all$extra_people <- as.numeric(data_all$extra_people)
sum(is.na(data_all$extra_people))



#create a host_identity_verifie dummy variable(=1 if its verified)
table(data_all$host_identity_verified)
data_all$host_identity_verified <- ifelse(data_all$host_identity_verified=='t',1,0)
data_all$host_identity_verified=as.factor(data_all$host_identity_verified)

#create a host_is_superhost dummy variable(=1 if its super host)
table(data_all$host_is_superhost)
data_all$host_is_superhost <- ifelse(data_all$host_is_superhost=='t',1,0)
data_all$host_is_superhost =as.factor(data_all$host_is_superhost)

#create a instant_bookable dummy variable(=1 if its instance bookable)
table(data_all$instant_bookable)
data_all$instant_bookable <- ifelse(data_all$instant_bookable=='t',1,0)
data_all$instant_bookable =as.factor(data_all$instant_bookable )


#create is_location_exact dummy variable(=1 if the location is exact)
table(data_all$is_location_exact)
data_all$is_location_exact <- ifelse(data_all$is_location_exact=='t',1,0)
data_all$is_location_exact=as.factor(data_all$is_location_exact)



#making a host_experience column (number of years)
host_since_year <- as.numeric(substr(data_all$host_since,1,4))
host_since_year[is.na(host_since_year)] <- round(median(host_since_year, na.rm = TRUE))

data_all$host_experience <- 2019 - host_since_year
data_all$host_experience =as.numeric(data_all$host_experience)

#making a first_review_since (number of years)
first_review_year <- as.numeric(substr(data_all$first_review,1,4))
first_review_year[is.na(first_review_year)] <- round(median(first_review_year, na.rm = TRUE))

data_all$first_review_year<- 2019 - first_review_year
sum(is.na(data_all$first_review_year))
data_all$first_review_year =as.numeric(data_all$first_review_year)



#host response rate
data_all$host_response_rate <- as.numeric(gsub("%.*","",data_all$host_response_rate))
sum(is.na(gsub("%.*","",data_all$host_response_rate)))

data_all$host_response_rate[is.na(data_all$host_response_rate)] <- round(median(data_all$host_response_rate, na.rm = TRUE))

data_all$host_response_rate <- data_all$host_response_rate / 100
data_all$host_response_rate=as.numeric(data_all$host_response_rate)


#host response time
#if the host responses within one hour,we think its a fast response
table(data_all$host_response_time)
data_all$fast_response <- ifelse(data_all$host_response_time=="within an hour",1,
                                 ifelse(data_all$host_response_time=="within a few hours",2,3))
data_all$fast_response=as.factor(data_all$fast_response)

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






#price column
#create data average price column
data_all$price <- gsub("\\$","",data_all$price)
data_all$price <- gsub(",","",data_all$price)
data_all$price <- as.numeric(data_all$price)

data_all$price[is.na(data_all$price)] <- round(median(data_all$price, na.rm = TRUE))
data_all$price=as.numeric(data_all$price)



#property type column
table(data_all$property_type)
data_all$property_type <- as.character(data_all$property_type)

data_all$property_type <- ifelse((data_all$property_type == "Townhouse") | 
                                   (data_all$property_type == "Tiny house"),
                                 "House",data_all$property_type)
data_all$property_type <- ifelse(data_all$property_type == "Loft",
                                 "Apartment",data_all$property_type)
data_all$property_type <- ifelse((data_all$property_type != "Apartment") & 
                                   (data_all$property_type != "Condominium") & 
                                   (data_all$property_type != "House"),
                                 "OTHER",data_all$property_type )




#requires_license
table(data_all$requires_license)
data_all$requires_license <- ifelse(data_all$requires_license == 't', 1, 0)
data_all$requires_license=as.factor(data_all$requires_license)




# security_deposit
data_all$security_deposit <- substr(data_all$security_deposit,2,1000)
data_all$security_deposit <- as.numeric(data_all$security_deposit)

data_all$security_deposit[is.na(data_all$security_deposit)] <- 0

#amenities
data_all$amenities=as.character(data_all$amenities)

amenities_len <- c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$amenities[i], " ")[[1]])
  amenities_len = c(amenities_len,m)
  
}

data_all$amenities_len <- amenities_len
data_all$amenities_len=as.numeric(data_all$amenities_len)



#description
data_all$description=as.character(data_all$description)

description_len <- c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$description[i], " ")[[1]])
  description_len = c(description_len,m)
  
}

data_all$description_len <- description_len
data_all$description_len=as.numeric(data_all$description_len)

#access
data_all$access=as.character(data_all$access)
access_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$access[i], " ")[[1]])
  access_len = c(access_len,m)
  
}

data_all$access_len = access_len
data_all$access_len=as.numeric(data_all$access_len)

#intercation
data_all$interaction=as.character(data_all$interaction)
interaction_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$interaction[i], " ")[[1]])
  interaction_len = c(interaction_len,m)
  
}

data_all$interaction_len = interaction_len
data_all$interaction_len=as.numeric(data_all$interaction_len)

#house rules
data_all$house_rules=as.character(data_all$house_rules)
house_rules_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$house_rules[i], " ")[[1]])
  house_rules_len = c(house_rules_len,m)
  
}

data_all$house_rules_len = house_rules_len
data_all$house_rules_len=as.numeric(data_all$house_rules_len)

#host about
data_all$host_about=as.character(data_all$host_about)
host_about_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$host_about[i], " ")[[1]])
  host_about_len = c(host_about_len,m)
  
}

data_all$host_about_len = host_about_len
data_all$host_about_len=as.numeric(data_all$host_about_len)


#neighborhood overview
data_all$neighborhood_overview=as.character(data_all$neighborhood_overview)
neighborhood_overview_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$neighborhood_overview[i], " ")[[1]])
  neighborhood_overview_len = c(neighborhood_overview_len,m)
  
}

data_all$neighborhood_overview_len = neighborhood_overview_len
data_all$neighborhood_overview_len=as.numeric(data_all$neighborhood_overview_len)
table(data_all$neighborhood_overview_len)

#notes
data_all$notes=as.character(data_all$notes)
notes_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$notes[i], " ")[[1]])
  notes_len = c(notes_len,m)
  
}

data_all$notes_len = notes_len
data_all$notes_len=as.numeric(data_all$notes_len)

#space
data_all$space=as.character(data_all$space)
space_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$space[i], " ")[[1]])
  space_len = c(space_len,m)
  
}

data_all$space_len = space_len
data_all$space_len=as.numeric(data_all$space_len)


#trainsit
data_all$transit=as.character(data_all$transit)
transit_len = c()

for (i in 1:nrow(data_all))
{
  m = length(strsplit(data_all$transit[i], " ")[[1]])
  transit_len = c(transit_len,m)
  
}

data_all$transit_len = transit_len
data_all$transit_len=as.numeric(data_all$transit_len)



#city
data_all$city_name <- as.character(data_all$city_name)

data_all$city_popular <- ifelse(data_all$city_name %in% 
                                  c("Nashville", "Los Angeles","San Diego","Washington DC",
                                    "New Orleans","San Francisco","New York","Seattle","Chicago",
                                    "Boston","Austin","Portland","Denver","Santa Cruz","Oakland",
                                    "Asheville"),data_all$city_name,'Other')

data_all$city_popular=as.factor(data_all$city_popular)

data_all$city<- as.character(data_all$city)
table(data_all$city)
data_all$city_popular2 <- ifelse(data_all$city %in% 
                                  c("Washington", "San Diego","Redondo Beach","Rowland Heights",
                                    "Pasadena","Oakland","New York","New Orleans","Nashville",
                                    "Marina del Rey","Manhattan Beach","Malibu","Long Island CIty",
                                    "Long Beach","Glendale"
                                     ),data_all$city,'Other')

data_all$city_popular2=as.factor(data_all$city_popular2)

#add new dummy variables

#amenities
data_all$amenities <- gsub('"',"",data_all$amenities)
Wifi = c()
ACHeater = c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$amenities[i], ",")
  if (("Wireless Internet" %in% m[[1]])|("Wifi" %in% m[[1]]))
    Wifi = c(Wifi,1)
  else
    Wifi= c(Wifi,0)
  if (("Air conditioning" %in% m[[1]])|("Heating" %in% m[[1]]))
    ACHeater  = c(ACHeater ,1)
  else
    ACHeater  = c(ACHeater ,0)
  
}

data_all$wifi = Wifi
data_all$ACHeater = ACHeater 
data_all$wifi =as.factor(data_all$wifi)
data_all$ACHeater=as.factor(data_all$ACHeater)



kitchen = c()
freeparking = c()
washing=c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$amenities[i], ",")
  if (("Washer" %in% m[[1]])|("Dryer" %in% m[[1]]))
    washing = c(washing,1)
  else
    washing = c(washing,0)
  if ("Kitchen" %in% m[[1]])
    kitchen = c(kitchen,1)
  else
    kitchen = c(kitchen,0)
  if ("Free parking" %in% m[[1]])
    freeparking = c(freeparking,1)
  else
    freeparking = c(freeparking,0)
  
}
data_all$kitchen  = kitchen 
data_all$freeparking= freeparking
data_all$washing= washing
data_all$kitchen=as.factor(data_all$kitchen )
data_all$freeparking=as.factor(data_all$freeparking)
data_all$kitchen=as.factor(data_all$kitchen)


animal= c()
hair = c()
smokedetector=c()
essentials=c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$amenities[i], ",")
  if (("Dog" %in% m[[1]])|("Pets" %in% m[[1]]))
    animal = c(animal ,1)
  else
    animal  = c(animal ,0)
  if (("Hair dryer" %in% m[[1]])|("Shampoo" %in% m[[1]]))
    hair = c(hair,1)
  else
    hair = c(hair,0)
  if ("Smoke detector" %in% m[[1]])
    smokedetector= c(smokedetector,1)
  else
    smokedetector = c(smokedetector,0)
  if ("Essentials" %in% m[[1]])
    essentials= c(essentials,1)
  else
    essentials = c(essentials,0)
  
}
data_all$animal = animal 
data_all$hair= hair
data_all$smokedetector= smokedetector
data_all$essentials= essentials

data_all$animal=as.factor(data_all$animal)
data_all$hair=as.factor(data_all$hair)
data_all$essentials=as.factor(data_all$essentials)
data_all$smokedetector=as.factor(data_all$smokedetector)

tv= c()
internet = c()
coffeemaker=c()
cookingbasics=c()
microwave=c()
refrigerator=c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$amenities[i], ",")
  if ("Cable TV" %in% m[[1]])
    tv = c(tv ,1)
  else
    tv  = c(tv ,0)
  if ("Coffee maker" %in% m[[1]])
    coffeemaker = c(coffeemaker ,1)
  else
    coffeemaker  = c(coffeemaker ,0)
  if ("Refrigerator" %in% m[[1]])
    refrigerator = c(refrigerator,1)
  else
    refrigerator = c(refrigerator,0)
  if ("Internet" %in% m[[1]])
    internet= c(internet,1)
  else
    internet = c(internet,0)
  if ("Microwave" %in% m[[1]])
    microwave= c(microwave,1)
  else
    microwave = c(microwave,0)
  if ("Cooking basics" %in% m[[1]])
    cookingbasics = c(cookingbasics,1)
  else
    cookingbasics = c(cookingbasics,0)
}
data_all$tv=tv
data_all$internet= internet
data_all$coffeemaker= coffeemaker
data_all$cookingbasics= cookingbasics
data_all$microwave= microwave
data_all$refrigerator= refrigerator


data_all$tv=as.factor(data_all$tv)
data_all$internet=as.factor(data_all$internet)
data_all$coffeemaker=as.factor(data_all$coffeemaker)
data_all$cookingbasics=as.factor(data_all$cookingbasics)
data_all$microwave=as.factor(data_all$microwave)
data_all$refrigerator=as.factor(data_all$refrigerator)

Balcony= c()
garden= c()


for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$amenities[i], ",")
  if (("Garden" %in% m[[1]])|("backyard" %in% m[[1]]))
    garden = c(garden ,1)
  else
    garden = c(garden ,0)
}

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$amenities[i], ",")
  if ("balcony" %in% m[[1]])
    Balcony = c(Balcony ,1)
  else
    Balcony  = c(Balcony ,0)
}


data_all$balcony=Balcony
data_all$garden= garden

data_all$balcony=as.factor(data_all$balcony)
data_all$garden=as.factor(data_all$garden)


#transit
data_all$transit <- gsub('"',"",data_all$transit)
Uber= c()
Lyft= c()
Metro =c()
Bikeshare=c()
subway=c()
bus=c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$transit[i], " ")
  if ("Uber" %in% m[[1]])
    Uber = c(Uber,1)
  else
    Uber  = c(Uber,0)
  if ("Lyft" %in% m[[1]])
    Lyft = c(Lyft,1)
  else
    Lyft  = c(Lyft,0)
  if ("Metro" %in% m[[1]])
    Metro = c(Metro,1)
  else
    Metro = c(Metro,0)
  if ("Bike share" %in% m[[1]])
    Bikeshare= c(Bikeshare,1)
  else
    Bikeshare = c(Bikeshare,0)
  if ("subway" %in% m[[1]])
    subway= c(subway,1)
  else
    subway = c(subway,0)
  if ("bus" %in% m[[1]])
    bus = c(bus,1)
  else
    bus = c(bus,0)
}
data_all$Uber=Uber
data_all$Lyft= Lyft
data_all$Metro= Metro
data_all$Bikeshare= Bikeshare
data_all$subway= subway
data_all$bus= bus

data_all$Uber=as.factor(data_all$Uber)
data_all$Lyft=as.factor(data_all$Lyft)
data_all$Metro=as.factor(data_all$Metro)
data_all$Bikeshare=as.factor(data_all$Bikeshare)
data_all$subway=as.factor(data_all$subway)
data_all$bus=as.factor(data_all$bus)

ptrans= c()
airport= c()
downtown=c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$transit[i], " ")
  if ("public transportation" %in% m[[1]])
    ptrans = c(ptrans,1)
  else
    ptrans  = c(ptrans,0)
  if ("airport" %in% m[[1]])
    airport = c(airport,1)
  else
    airport  = c(airport,0)
  if ("downtown" %in% m[[1]])
    downtown = c(downtown,1)
  else
    downtown  = c(downtown,0)
}
data_all$ptrans= ptrans
data_all$airport=airport
data_all$downtown=downtown

data_all$ptrans=as.factor(data_all$ptrans)
data_all$airport=as.factor(data_all$airport)
data_all$downtown=as.factor(data_all$downtown)


#neighborhood_overview
data_all$neighborhood_overview <- gsub('"',"",data_all$neighborhood_overview)
restaurant= c()
shop=c()
grocery =c()
park=c()
bar=c()
beach=c()
museum=c()

for (i in 1:nrow(data_all))
{
  m = strsplit(data_all$neighborhood_overview[i], " ")
  if ("restaurant" %in% m[[1]])
    restaurant = c(restaurant,1)
  else
    restaurant  = c(restaurant,0)
  if ("museum" %in% m[[1]])
    museum = c(museum,1)
  else
    museum  = c(museum,0)
  if ("shop" %in% m[[1]])
    shop = c(shop,1)
  else
    shop  = c(shop,0)
  if ("grocery" %in% m[[1]])
    grocery = c(grocery,1)
  else
    grocery = c(grocery,0)
  if ("park" %in% m[[1]])
    park= c(park,1)
  else
    park = c(park,0)
  if ("bar" %in% m[[1]])
    bar= c(bar,1)
  else
    bar= c(bar,0)
  if ("beach" %in% m[[1]])
    beach = c(beach,1)
  else
    beach = c(beach,0)
}
data_all$restaurant=restaurant
data_all$shop=shop
data_all$grocery= grocery
data_all$park= park
data_all$bar=bar
data_all$beach=beach
data_all$museum=museum

data_all$restaurant=as.factor(data_all$restaurant)
data_all$shop=as.factor(data_all$shop)
data_all$grocery=as.factor(data_all$grocery)
data_all$park=as.factor(data_all$park)
data_all$bar=as.factor(data_all$bar)
data_all$beach=as.factor(data_all$beach)
data_all$museum=museum

#average

data_all$price_per_person=data_all$price/data_all$accommodates
sum(is.na(data_all$price_per_person))
data_all$price_per_person=as.numeric(data_all$price_per_person)

#data_all$price_per_guest=data_all$price/data_all$guests_included
#sum(is.na(data_all$price_per_guest))
#data_all$price_per_guest=as.numeric(data_all$price_per_guest)



#market
table(data_all$market)
data_all$market <- ifelse(data_all$market %in% 
                                  c("Chicago ", "Los Angeles","New York","Austin",
                                    "Denver","San Francisco","Nashville","Portland","Seattle",
                                    "Boston","D.C.","New Orleans","San Diego"),data_all$market,'Other')

data_all$market=as.factor(data_all$market)

#host_total_listings_count
is.numeric(data_all$host_total_listings_count)
data_all$host_total_listings_count=as.numeric(data_all$host_total_listings_count)
sum(is.na(data_all$host_total_listings_count))
table(data_all$host_total_listings_count)

#state
table(data_all$state)
data_all$state <- ifelse(data_all$state %in% 
                            c("CO", "MA","DC","TN",
                              "TX","IL","NC","NY","WA",
                              "CA","LA","OR"),data_all$state,'Other')
data_all$state=as.factor(data_all$state)
# save the data
write.csv(data_all,"C:\\Users\\ZXY0117\\Desktop\\cleanedata_xyz2.csv", row.names = TRUE)

data_all=clean
