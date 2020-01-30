library(readxl)
library(dplyr)
library(Amelia)
library(ggmap)
library(qmap)
library(leaflet)
df_review <- read.csv("Airbnb_BOS_reviews.csv",stringsAsFactors = FALSE)# comment
df_listing <- read.csv("Airbnb_BOS_listings.csv",stringsAsFactors = FALSE)
str(df_review)
#removing blanks & nulls from review
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
df_review2<- df_review %>% filter(!(is.na(comments)|comments==""))
str(df_review2)
df_review2$date2<-as.Date(df_review2$date,format="%m/%d/%Y") #defining what is the desired format of your date
#print(df_review2$date2)

missmap(df_review2,main = 'Missing Data',col = c('yellow','blue'),legend = FALSE)
missmap(df_listing,main = 'Missing Data',col = c('yellow','blue'),legend = FALSE)
str(df_listing)
#Chicago_map<-  qmap("chicago", zoom = 14, color = "bw", legend = "topleft")

#leaflet map for airbnb clusters as ggmap has API requirements
leaflet(df_listing) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions(),popup = paste0("<b> Name: </b>", df_listing$name , "<br/><b> Host Name: </b>", df_listing$host_name, "<br> <b> Price: </b>", df_listing$price, "<br/><b> Room Type: </b>", df_listing$room_type, "<br/><b> Property Type: </b>", df_listing$property_type
  ))
