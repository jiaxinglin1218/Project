#Load the required data and package
library(tidyverse)
library(lubridate)
library(ggplot2)
sales<-read.csv("NYC_TRANSACTION_DATA.CSV")
class<-read.csv("BUILDING_CLASS.CSV")
Nbh<-read.csv("NEIGHBORHOOD.CSV")

#Finding NEIGHBORHOOD_ID for our assigned area
Nbh<- Nbh %>% filter(NEIGHBORHOOD_NAME == "HARLEM-CENTRAL");
view(Nbh)

#Filter DATAset to assigned Area ----  "HARLEM-CENTRAL" NEIGHBORHOOD_ID = 118
sale<- sales %>% filter(NEIGHBORHOOD_ID==118)

#FIlter out data that is not useful SUch as Sale price = 0 or Square feet = 0;
sale<- sale %>% filter(SALE_PRICE != 0)
sale<- sale %>% filter(GROSS_SQUARE_FEET != 0)

#Add an addtional column Sale years due to the KPI we were interested in needed to use sale years
sale$sale_year<-year(sale$SALE_DATE[1:3882])

#Join sales data with building class data;
sale<- sale %>%
          rename(BUILDING_CODE_ID = BUILDING_CLASS_FINAL_ROLL)

sale1 <- merge(x=sale,y=class,by ="BUILDING_CODE_ID",all.x = TRUE)

#Define out 3 KPI: Average sales per year, Average sales per building type, and sum of building type sold in each year
#KPI 1
sale2<- group_by(sale1,sale_year)%>%
        summarise(SALE_PRICE = mean(SALE_PRICE))
y<-1000000
sale2$SALE_PRICE<- sale2$SALE_PRICE/y

ggplot(data=sale2, aes(x=sale_year, y=SALE_PRICE)) +
  ggtitle("Average Sales Per Year")+
  labs(y = "Sale Price (M)", x = "Sale Date (Year)")+
  theme(
    plot.title = element_text(color = "#0099f9", size = 15, face = "bold", hjust = 0.5))+
  geom_line(color = "#0099f9",size = 1)+
  geom_point(color = "#0099f9",size = 3)

#KPI2
sale3<- group_by(sale1,DESCRIPTION)%>%
  summarise(SALE_PRICE = mean(SALE_PRICE))
y<-1000000
sale3$SALE_PRICE<- sale3$SALE_PRICE/y

sale3 <- sale3[order(sale3$SALE_PRICE, decreasing = TRUE),]
sale3 <- sale3[1:10,]

ggplot(sale3) +
  geom_col(aes(SALE_PRICE,DESCRIPTION), fill = "#076fa2", width = 0.6)+
  scale_y_discrete(expand = expansion(add = c(0, 0.5)))+
  ggtitle("Top 10 Average Sales Per Building Class")+
  labs(y = "Building Class", x = "Sales Price (M)")+
  theme(
    plot.title = element_text(color = "#076fa2", size = 15, face = "bold", hjust = 0.5))

#KPI3
sale4<- group_by(sale1,sale_year)%>%
  summarise(UNITS = sum(RESIDENTIAL_UNITS),
            type = "Resident")

sale5<- group_by(sale1,sale_year)%>%
  summarise(UNITS = sum(COMMERCIAL_UNITS),
            type = "Commercial")

sale6 <- merge(sale4, sale5, by = c("sale_year", "UNITS", "type"), all = TRUE)

ggplot(sale6)+
  geom_line(mapping = aes(x=sale_year,y=UNITS,color=type),size =1)+
  guides(color = guide_legend(title = "Unit Type"))+
  ggtitle("Unit Type Sold In Each Year")+
  labs(y = "Amount of Unit Sold", x = "Sale Year")+
  theme(plot.title = element_text(color = "#076fa2", size = 15, face = "bold", hjust = 0.5))

#If real estate companies, on average, earn a commission of 5 cents per dollar on total sales,
#what is the total revenue earned in your neighborhood over the last year of data?
#If your company achieves 12.5% market penetration in your neighborhood, what would its revenue be?

#First Step: Filter out our assigned area and Only keep last year of data
Q3<- sales %>% filter(NEIGHBORHOOD_ID==118)
Q3$sale_year<-year(Q3$SALE_DATE[1:16806])
Q3<- Q3 %>% filter(sale_year == 2021)

#Second Step: Calculate Sum of Sale Price and Multiple by 0.05
sum<-sum(Q3$SALE_PRICE)
Total_commission = sum*0.05
Total_commission

#Third Step:Apply Market Penetration
Market = sum*0.125
Total_commission1 = Market*0.05
Total_commission1




