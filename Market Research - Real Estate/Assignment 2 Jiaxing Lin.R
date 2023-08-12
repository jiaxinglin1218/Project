#Load the required data and package
library(tidyverse)
library(lubridate)
library(ggplot2)
library(factoextra)
sales<-read.csv("NYC_TRANSACTION_DATA.CSV")
class<-read.csv("BUILDING_CLASS.CSV")
Nbh<-read.csv("NEIGHBORHOOD.CSV")

#FIlter out data that is not useful SUch as Sale price = 0 or Square feet = 0;
sales<- sales %>% filter(SALE_PRICE != 0)
sales<- sales %>% filter(GROSS_SQUARE_FEET != 0)

sales<- sales %>%
  rename(BUILDING_CODE_ID = BUILDING_CLASS_FINAL_ROLL)

sales <- merge(x=sales,y=class,by ="BUILDING_CODE_ID",all.x = TRUE)

#Add an additional column Sale years
sales$sale_year<-year(sales$SALE_DATE[1:625033])

#Only show residential building type
sales<- sales %>% filter(TYPE == 'RESIDENTIAL')

#Part 1: 

#Finding NEIGHBORHOOD_ID for our assigned area
Nbh<- Nbh %>% filter(NEIGHBORHOOD_NAME == "HARLEM-CENTRAL");
view(Nbh)

#Filter DATAset to assigned Area ----  "HARLEM-CENTRAL" NEIGHBORHOOD_ID = 118
sales1<- sales %>% filter(NEIGHBORHOOD_ID==118)

#Group the data by year, and summarize to show the average price of 1 square foot
#of real estate. List the results.

Part1Q1<- group_by(sales1,sale_year)%>%
  summarise(SALE_PRICE = mean(SALE_PRICE/GROSS_SQUARE_FEET))

#Get the five-number summary (minimum, 1st quantile, median, 3rd quantile, maximum) 
#for both sale price and gross square footage for residential properties in your neighborhood since 2009

#filter out sale year that is earlier than 2009
sales2<- sales1 %>% filter(sale_year >= 2009)


summary(sales2$SALE_PRICE)
summary(sales2$GROSS_SQUARE_FEET)

#What is the correlation between sale price and gross square feet for residential properties in your neighborhood since 2009?

cor(sales2$GROSS_SQUARE_FEET,sales2$SALE_PRICE)

#Part 2:

#Calculate the following metrics.
sales3<- sales %>% filter(sale_year >= 2009)

#Median sale price for residential properties since 2009
kpi1<-group_by(sales3,NEIGHBORHOOD_ID)%>%
  summarise(Median_sale_price = median(SALE_PRICE))

#Number of sales for residential properties since 2009
kpi2<-group_by(sales3,NEIGHBORHOOD_ID)%>%
  summarise(Number_of_sale = sum(RESIDENTIAL_UNITS))

#Standard deviation of sales of residential properties since 2009

kpi3<-group_by(sales3,NEIGHBORHOOD_ID)%>%
  summarise(Sd_of_sale = sd(RESIDENTIAL_UNITS))

#Perform k-means clustering of neighborhoods based on the KPIs above. Describe the cluster that your neighborhood belongs to.

#First step: Merge all three KPI into a single dataset
K <- merge(kpi1, kpi2, by = "NEIGHBORHOOD_ID")
K1 <- merge(K, kpi3, by = "NEIGHBORHOOD_ID")

#Remove Nbh_ID because it is a unique identifier
K2 <- K1[,2:4]
K2 <- scale(K2)

#change the row name as Neighborhood_ID
rownames(K2) <- K1$NEIGHBORHOOD_ID

#Remove missing value inside our data set
K2 <- na.omit(K2)

#Find the optimal number of cluster
fviz_nbclust(x=K2, FUNcluster = kmeans, method="wss", k.max=25)

#based on Elbow method that 6 is the best number of clusters.
km6 <- kmeans(x=K2, centers=6)
km6

#Graph the K-means cluster, The nbh_ID I got assigned is 118, and It is in cluster 1
fviz_cluster(object=km6, data = K2)


km6$cluster
km6$centers
km6$totss
km6$withinss
km6$tot.withinss
km6$betweenss
km6$size


#Part3 :
#Choose one other neighborhood, and test the hypothesis that, starting in 2009, the average residential property costs more,
#less, or a different amount in your neighborhood (Note: this requires a t-test.)
sales4<- sales %>% filter(sale_year >= 2009)


X <- (sales4 %>% filter(NEIGHBORHOOD_ID==118))$SALE_PRICE
Y <- (sales4 %>% filter(NEIGHBORHOOD_ID==238))$SALE_PRICE

length(X)
length(Y)
#Unpaired

t.test(X, Y, paired=F, conf.level=0.95, 
       mu=0, alternative = "greater")





