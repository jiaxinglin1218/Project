#Load the required data and package
library(tidyverse)
library(lubridate)
library(forecast)
sales<-read.csv("NYC_TRANSACTION_DATA.CSV")
class<-read.csv("BUILDING_CLASS.CSV")
#FIlter out data that is not useful SUch as Sale price = 0 or Square feet = 0;
sales<- sales %>% filter(SALE_PRICE != 0)
sales<- sales %>% filter(GROSS_SQUARE_FEET != 0)

sales<- sales %>%
  rename(BUILDING_CODE_ID = BUILDING_CLASS_FINAL_ROLL)

sales <- merge(x=sales,y=class,by ="BUILDING_CODE_ID",all.x = TRUE)

#Add an additional column Sale years
sales$sale_year<-year(sales$SALE_DATE[1:625033])

#Only show residential building type,nbhID,and beyond 2009
sales<- sales %>% filter(TYPE == 'RESIDENTIAL')

sales<- sales %>% filter(NEIGHBORHOOD_ID==118)
sales<- sales %>% filter(sale_year >= 2009)

#Part 1
sales$BUILDING_CODE_ID <- factor(sales$BUILDING_CODE_ID, ordered=F)
model1<- lm(SALE_PRICE~YEAR_BUILT+BUILDING_CODE_ID+GROSS_SQUARE_FEET+RESIDENTIAL_UNITS, data = sales)
summary(model1)

#Part 2
sales$SALE_DATE<-as.Date(sales$SALE_DATE)
sales1 <- aggregate(sales$SALE_PRICE, 
                             by = list(Year = format(sales$SALE_DATE, "%Y"), Quarter = quarters(sales$SALE_DATE)), 
                             FUN = sum)
sales1 <- sales1 %>%
  arrange(Year, Quarter)


x_ts <- ts(sales1$x, start = c(2009,1), frequency = 4)
x_ts

plot(x_ts)
model <- ets(y=x_ts, model="AAA")
summary(model)

pred <- predict(model, 8)
pred

plot(pred)
autoplot(pred)

model1 <- ets(y=x_ts, model="ANA")
summary(model1)

pred1 <- predict(model1, 8)
pred1

plot(pred1)
autoplot(pred1)

#part3
result<-data.frame(pred1)
write.csv(result,file = 'Part3.csv',row.names = TRUE)

sales<-read.csv("NYC_TRANSACTION_DATA.CSV")
class<-read.csv("BUILDING_CLASS.CSV")
#FIlter out data that is not useful SUch as Sale price = 0 or Square feet = 0;
sales<- sales %>% filter(SALE_PRICE != 0)
sales<- sales %>% filter(GROSS_SQUARE_FEET != 0)

sales<- sales %>%
  rename(BUILDING_CODE_ID = BUILDING_CLASS_FINAL_ROLL)

sales <- merge(x=sales,y=class,by ="BUILDING_CODE_ID",all.x = TRUE)

#Add an additional column Sale years
sales$sale_year<-year(sales$SALE_DATE[1:625033])

#Only show residential building type,nbhID,and beyond 2009
sales<- sales %>% filter(TYPE == 'COMMERCIAL')

sales<- sales %>% filter(NEIGHBORHOOD_ID==118)
sales<- sales %>% filter(sale_year >= 2009)

sales2<- group_by(sales,sale_year)%>%
  summarise(SALE_PRICE = mean(SALE_PRICE/GROSS_SQUARE_FEET))

sales2
