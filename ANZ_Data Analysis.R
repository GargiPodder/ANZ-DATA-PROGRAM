library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(knitr)
library(rpart)
library(ggplot2)
library(ggpubr)
library(modelr)
library(sp)
library(leaflet)
library(geosphere)



#file import

library(readxl)
ANZ_DATA <- read_excel("C:/Users/User/Downloads/ANZ%20synthesised%20transaction%20dataset.xlsx; filename%2A.xlsx")
View(ANZ_DATA)


str(ANZ_DATA)
summary(ANZ_DATA)

############Data Issues
##### (1) Date format=> extract date from date and check unique customers or not
##### (2) Extraction=> convert the extraction to char and extract date and time of transaction
##### (3) customer and merchant lat and long=> separate the lat and long
##### (4) find the outliers and while plotting remove them
##### (5) find which details contain the NA values 
##### (6) customer's location

#changing date format 
ANZ_DATA$date<-as.Date(ANZ_DATA$date)

#extracting time and weekday from extraction
ANZ_DATA$extraction<-as.character(ANZ_DATA$extraction)

ANZ_DATA$hour<-hour(as.POSIXct(substr(ANZ_DATA$extraction,12,19),format="%H:%M:%S"))

ANZ_DATA$weekday<-weekdays(ANZ_DATA$date)

#checking date range
Daterange<-seq(min(ANZ_DATA$date),max(ANZ_DATA$date),by=1)
Daterange[!Daterange %in% ANZ_DATA$date]

#we dont have any transaction on 16-08-2018

#separating latitude and longitude

df<-ANZ_DATA[,c("long_lat","merchant_long_lat")]
df<- df %>% separate("long_lat",c("c_long","c_lat"),sep = ' ')
df<- df %>% separate("merchant_long_lat",c("m_long","m_lat"),sep = ' ')
df<-data.frame(sapply(df, as.numeric))
df

ANZ_DATA<-cbind(ANZ_DATA,df)
View(ANZ_DATA)

#we are considering there is unique account id to every customer id
ANZ_DATA%>%select(account,customer_id)%>%unique()%>%nrow()
#since there is 100 customer we have unique account attached to each customer

#checking NA and Unique values

sapply(ANZ_DATA, function(x)sum(is.na(x)))
#there is some NA values in merchants probably those transactions which are not purchased
# that is payment, salary etc
sapply(ANZ_DATA, function(x)length(unique(x)))
#we can conclude many people have brought from same merchant

#############we are filtering out the person who is not from australia
table(ANZ_DATA$c_long)
table(ANZ_DATA$c_lat)
boxplot(ANZ_DATA$c_long)
boxplot(ANZ_DATA$c_lat)

#Let's see what is the transactions of the person outside of australia
ANZ_DATA$txn_description[which(ANZ_DATA$c_long==255.00)]



ANZ_DATA_AUS<-ANZ_DATA %>%
  filter(!(c_long>113 & c_long<154 & c_lat>(-44) & c_lat<(-10)))
length(unique(ANZ_DATA_AUS$customer_id))
#so there is only one person who is outside of australia
#we are not removing that person since the transactions were made in australia


#finding out transaction type
table(ANZ_DATA$txn_description)


#the 
#first we are calculating transaction based on purchase
#hence we are filtering out INTER BANK,PAY/SALARY, PAYMENT,PHONE

df1<-ANZ_DATA %>% filter(merchant_id !='')
table(df1$txn_description)
#it can be seen that POS and SALES_POS are done by merchant
#so we remove all other transaction description 
hist(df1$amount[!df1$amount %in% boxplot.stats(df1$amount)$out],xlab = "Amount",main ="Histogram of Purchase Transactions Amount" )


#average transaction amount
df1<-ANZ_DATA %>%
  group_by(txn_description) %>%
  summarise(average=round(mean(amount,na.rm = TRUE),3))
df1  

ggplot(df1,aes(x=txn_description,y=average))+geom_col(fill="dark green")+
  theme_cleveland()+labs(x="Transaction Method",y="Average Transaction Amount",title = "Average Transaction Amount in each Transaction Method")+
  geom_text(aes(label=average),vjust=-0.2)

df2<-ANZ_DATA %>%
  group_by(customer_id) %>%
  summarise(average_trans=round(n()/3,3))
warnings()
df2

ggplot(df2,aes(average_trans))+geom_histogram(bins = 20,fill="dark green")+theme_cleveland()+
  labs(x="Monthly Average Transactions",y="Number of Customers",title="Monthly Average Transactions")

#segment the data by week
df3<-ANZ_DATA %>%
  select(date,weekday) %>%
  group_by(date,weekday)%>%
  summarise(daily_avg=n())%>%
  group_by(weekday) %>%
  summarise(average_volume=mean(daily_avg, na.rm = TRUE))

df3$weekday <- factor(df3$weekday, levels=c( "Monday","Tuesday","Wednesday",
                                             "Thursday","Friday","Saturday","Sunday"))
df3
ggplot(df3,aes(x=weekday, y=average_volume)) +geom_point(color="orange")+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by weekday') +
  labs(x='Weekday',y='Transaction volume')+theme_pubclean()+ylim(100,160)                                              

#segment the data by day 
df4<-ANZ_DATA %>%
  select(date,hour) %>%
  group_by(date,hour)%>%
  summarise(daily_avg=n())%>%
  group_by(hour) %>%
  summarise(average_volume=mean(daily_avg, na.rm = TRUE))

ggplot(df4,aes(x=hour, y=average_volume)) +geom_point(colour="red")+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by day') +
  labs(x='Hours in a day',y='Transaction volume')+theme_pubclean() +expand_limits(y=0)


#challenge: what conclusion can be drawn from location
# we have both location of merchant and customer in case of purchase
#hence we see average dist between merchant and customer
#but we remove the customer outside of australia

df_csmp <- ANZ_DATA %>%filter(!(txn_description %in% c('PAY/SALARY',"INTER BANK", "PHONE BANK","PAYMEN
T")))

df_temp <- df_csmp %>%
  filter (c_long >113 & c_long <154 & c_lat > (-44) & c_lat < (-10))
dfloc = df_temp [,c("c_long", "c_lat","m_long", "m_lat")]
dfloc<- data.frame(sapply(dfloc, as.numeric))
dfloc$dst <- distHaversine(dfloc[, 1:2], dfloc[, 3:4]) / 1000
hist(dfloc$dst[dfloc$dst<100], main = "Distance between customer and merchants",xlab= 'Distance
(km)' )


