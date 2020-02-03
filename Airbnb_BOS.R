library(readxl)
library(dplyr)
library(Amelia)
library(ggmap)
library(qmap)
library(leaflet)
library(ggthemes)
df_review <- read.csv("Airbnb_BOS_reviews.csv",stringsAsFactors = FALSE)
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

#Demand vs Pricing in Boston by Property Type 
#aggregation --avg_price= max(df_listing$price,na.rm = TRUE) condition was not working without aggregate func hence split it into various steps using columnbind on two dataframes,...to get an aggregated list of demand and records by property type
df_listing$price <- as.numeric(gsub('[$,]', '', df_listing$price))
str(df_listing)
#grouping off the low freq types by creating function
group_prop <- function(property_type){
  if (property_type=='Villa' | property_type=='Guesthouse' | property_type=='Dorm' | property_type=='Camper/RV' | property_type=='Other' | property_type=='Entire Floor'){
    return('Other/Villa/Dorm/Guesthouse/RV')
  }else{
    return(property_type)
  }
}
df_listing$property_type <- sapply(df_listing$property_type,group_prop)
table(df_listing$property_type)
#summarising by groups
df_list_agg<- df_listing %>%group_by(df_listing$property_type) %>% summarise(n())
df_agg_2<- aggregate(price~property_type,df_listing,mean)
df_list_agg <- cbind(df_list_agg, mean= df_agg_2$price)
df_list_agg<- df_list_agg[-1,]
print(df_list_agg)

#ggplots  --+ facet_grid(Category~.) 
p1<- ggplot(df_list_agg,aes(x=df_list_agg$mean,y=df_list_agg$`n()`))+ geom_point(aes(color=df_list_agg$`df_listing$property_type`),size=3,alpha=0.6)  + labs(y= "Demand", x = "Average Price of Units")+ggtitle("Price vs Demand in Boston by Property type")+theme_economist_white()
print(p1)

#Correlation of Ratings given to Host Information
str(df_listing)
df_listing_corr<- df_listing[,c(23,26,27,29,33,35,36,61,77,80)]
str(df_listing_corr)
df_listing_corr$host_response_time<- as.factor(df_listing_corr$host_response_time) 
df_listing_corr$host_has_profile_pic<- as.factor(df_listing_corr$host_has_profile_pic) 
df_listing_corr$host_is_superhost<- as.factor(df_listing_corr$host_is_superhost)
df_listing_corr$host_since<- as.Date(df_listing_corr$host_since,format="%m/%d/%Y")
df_listing_corr$host_response_rate<- as.numeric(gsub('[%]', '', df_listing_corr$host_response_rate))
str(df_listing_corr)


