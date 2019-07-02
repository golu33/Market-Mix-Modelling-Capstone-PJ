######################Capstone TiTans########################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(MASS)
library(car)
library(Hmisc)
library(DataCombine)
library(DAAG)

#setwd("C:/PGDDS/Capstone Project")
#to be set to source working directory with all csv files in it 

# Load Dataset in working directory 
eleckrt_consumer <- read.csv("ConsumerElectronics.csv",stringsAsFactors = FALSE)

# Let's modify the name 
colnames(eleckrt_consumer) <- c("FSN_ID","Order_date","Year","Month","Order_id","Order_item_id","gmv","Units","deliverybdays","deliverycdays", "payment_mode","SLA","cust_id","pincode","P_super_category","P_analytic_category","P_sub_category","P_analytic_vertical","MRP","Procurement_SLA")

# structure of dataset 
str(eleckrt_consumer)
summary(eleckrt_consumer)

sapply(eleckrt_consumer, function(x) sum(is.na(x)))
#cust_id, pincode and gmv contains 4904 NA valaues.

sapply(eleckrt_consumer, function(x) length(which(x=="")))
# There are no empty cells

# data sanity Check : Data should be from july-2015 to June -2016,Let's remove other data from eleckrt_consumer dataset
eleckrt_consumer <- subset(eleckrt_consumer, !(Month ==5 & Year==2015|Month ==6 & Year==2015|Month ==7 & Year==2016))

## Convert the dataset into weekly levels. 

# Lubridate package 

#library(lubridate)

eleckrt_consumer$Order_date <- date(eleckrt_consumer$Order_date)

# create new column week of year 

eleckrt_consumer$week_no <- week(eleckrt_consumer$Order_date)


eleckrt_consumer$week_no<- ifelse(eleckrt_consumer$week_no<=26 & eleckrt_consumer$Year==2016,eleckrt_consumer$week_no+53,eleckrt_consumer$week_no)

eleckrt_consumer$week_no<-eleckrt_consumer$week_no-26

summary(eleckrt_consumer$MRP)

# removing rows with NA values
eleckrt_consumer <- na.omit(eleckrt_consumer)

# not considering free products
eleckrt_consumer <- subset(eleckrt_consumer,MRP!=0)

sum(is.na(eleckrt_consumer))

eleckrt_consumer <- cbind(eleckrt_consumer[,-c(5,6,13,14)],
                 sapply(eleckrt_consumer[,c(5,6,13,14)],as.character) )  

# Since 80% of the variable has 'No Expected delay'(\\N)
# will assume these scenarios as zero delay.

eleckrt_consumer$deliverybdays[eleckrt_consumer$deliverybdays=='\\N'] <- '0'
eleckrt_consumer$deliverycdays[eleckrt_consumer$deliverycdays=='\\N'] <- '0'
eleckrt_consumer$deliverybdays <- as.integer(eleckrt_consumer$deliverybdays)
eleckrt_consumer$deliverycdays <- as.integer(eleckrt_consumer$deliverycdays)

# . . . . Strip Spaces ----
eleckrt_consumer$P_super_category <- gsub(" +","",eleckrt_consumer$P_super_category)
eleckrt_consumer$P_analytic_category <- gsub(" +","",eleckrt_consumer$P_analytic_category)
eleckrt_consumer$P_sub_category <- gsub(" +","",eleckrt_consumer$P_sub_category)
eleckrt_consumer$P_analytic_vertical <- gsub(" +","",eleckrt_consumer$P_analytic_vertical)

# gmv should not be more than MRP*units since we can offer discounts but 
# not charge higher

eleckrt_consumer <- subset(eleckrt_consumer, (MRP*Units)>=gmv)

# . . . . Generate Discount ----

eleckrt_consumer$listprice<-eleckrt_consumer$gmv/eleckrt_consumer$Units
  
eleckrt_consumer$discount <- (((eleckrt_consumer$MRP*eleckrt_consumer$Units) - eleckrt_consumer$gmv)/(eleckrt_consumer$MRP*eleckrt_consumer$Units)) * 100


#eleckrt_consumer$gmv[which(eleckrt_consumer$gmv==0)] <- 1

# . . . .  Payment Type ----
eleckrt_consumer$COD     = as.integer(eleckrt_consumer$payment_mode=='COD')
eleckrt_consumer$Prepaid = as.integer(eleckrt_consumer$payment_mode!='COD')

str(eleckrt_consumer)
summary(eleckrt_consumer)
dim(eleckrt_consumer)

# ***************************************************************************
#                   LOAD DATA ---- Media & Inv Data ----
# ***************************************************************************
# . . . .   ProductList ----
productList <- read.csv("product_list.csv", stringsAsFactors = FALSE,na.strings=c('\\N'))

# . . . .   Media Investment ----
mediaInv_data  <- read.csv("media_spend.csv", stringsAsFactors = FALSE)

# . . . .   Monthly NPS ----
monthlyNPS_data <- read.csv("nps_score.csv", stringsAsFactors = FALSE )

# . . . .  Special Sale Event ----

specialSale_data <- read.csv("specialdaysale.csv", stringsAsFactors = FALSE)


# . . . .   ProductList ----
productList <- na.omit(productList)

# . . . . . . . .  Missing Values ----
mediaInv_data[is.na(mediaInv_data)] <- 0   # zero investment


dys <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')

weekdays <- data.frame('days'=dys, Year=year(dys),Month = month(dys))

weekdays$week_no <- week(weekdays$days)

weekdays$week_no<- ifelse(weekdays$week_no<=26 & weekdays$Year==2016,weekdays$week_no+53,weekdays$week_no)

weekdays$week_no<-weekdays$week_no-26

weekdays$nweek <- rep(1,length(dys))

#weekdays_gr<-group_by(weekdays,week_no)

weekdays<- data.frame(weekdays %>% group_by(week_no,Month) %>%summarise(nweeks<-sum(nweek)))

weekdays$fract_week<-weekdays$nweeks/7

# convert montly spend to weekly
mediaInv_data <- cbind(Month=mediaInv_data[,c(2)],mediaInv_data[,-c(1,2)]/4.30)

# Add weekly information
mediaInvestment_weekly <- merge(weekdays,mediaInv_data, by='Month', 
                                all.x = TRUE)

# Convert media Investment at weekly granularity
# pro-rate weekly investment as per the ratio of its days span over adjacent months
mediaInvestment_weekly <- 
  data.frame(
    mediaInvestment_weekly %>% 
      group_by(week_no) %>% 
      summarise(TotalInvestment = sum(Total.Investment*fract_week),
                TV = sum(TV*fract_week), 
                Digital=sum(Digital*fract_week),
                Sponsorship = sum(Sponsorship*fract_week), 
                ContentMarketing = sum(Content.Marketing*fract_week),
                OnlineMarketing = sum(Online.marketing*fract_week), 
                Affiliates = sum(Affiliates*fract_week),
                SEM = sum(SEM*fract_week), 
                Radio = sum(Radio*fract_week), 
                Other = sum(Other*fract_week))
  )


# . . . .   Monthly NPS ----
monthlyNPS_weekly   <- merge(weekdays, monthlyNPS_data, by='Month', all.x = TRUE)

monthlyNPS_weekly   <- as.data.frame(monthlyNPS_weekly %>% group_by(., week_no) %>% 
                                       summarise(., NPS = mean(NPS)))


#monthlyNPS_weekly   <- as.data.frame(monthlyNPS_weekly %>% group_by(., week_no) %>% 
                                #       summarise(., NPS = mean(NPS)))

View(specialSale_data)
str(specialSale_data)
#specialSale_data$Month <- month(as.Date(specialSale_data$Order_date))
specialSale_data$Month <- month(as.Date(specialSale_data$Day))

#specialSale_data$week_no<-week(specialSale_data$Order_date)
specialSale_data$week_no<-week(specialSale_data$Day)

#specialSale_data$Year<-year(specialSale_data$Order_date)
specialSale_data$Year<-year(specialSale_data$Day)

specialSale_data$week_no<- ifelse(specialSale_data$week_no<=26& specialSale_data$Year==2016,specialSale_data$week_no+53,specialSale_data$week_no)

specialSale_data$week_no<-specialSale_data$week_no-26

specialSale_data$sale_day<- 1

specialSale_data_weekly<-specialSale_data

specialSale_data_weekly <-group_by(specialSale_data_weekly,week_no)

specialSale_data_weekly<-summarise(specialSale_data_weekly,Noofsaledays=sum(sale_day))

## Let's divide the dataset into 3 dataframes based on category.

GamingAccessory <-eleckrt_consumer[eleckrt_consumer$P_sub_category=="GamingAccessory" ,] 

CameraAccessory<-eleckrt_consumer[eleckrt_consumer$P_sub_category=="CameraAccessory",]

HomeAudio<-eleckrt_consumer[eleckrt_consumer$P_sub_category=="HomeAudio",]


#Getting Weekly data for all the three

GamingAccessory$SLA <- as.integer(GamingAccessory$SLA)

GamingAccessory_weekly <-group_by(GamingAccessory,week_no)

GamingAccessory_weekly <-summarise(GamingAccessory_weekly,TotalGMV=sum(gmv),product_mrp=mean(MRP), 
                                    units=sum(Units),
                                    discount=mean(discount),
                                    listprice=mean(listprice),
                                    sla=mean(SLA), 
                                    procurement_sla=mean(Procurement_SLA),
                                    COD=sum(COD),
                                    Prepaid = sum(Prepaid))

#tackling shelf inflation

GamingAccessory_weekly$shelfinflation<- 0

GamingAccessory_weekly$mvg_avg<- 0

no_of_row<-nrow(GamingAccessory_weekly)

temp<-GamingAccessory_weekly$listprice

for(i in 2:no_of_row) 
  {
    GamingAccessory_weekly$shelfinflation[i] <- temp[i]/temp[i-1]
  }

#Imputing first value with mean

GamingAccessory_weekly$shelfinflation[1]=mean(GamingAccessory_weekly$shelfinflation)

#Calculating for Moving Average

for(i in 4:no_of_row) 
{
  GamingAccessory_weekly$mvg_avg[i] <- (GamingAccessory_weekly$listprice[i-1]+GamingAccessory_weekly$listprice[i-2]+GamingAccessory_weekly$listprice[i-3])/3
}

#merging with weekly special sale data

GamingAccessory_weekly_sale <- merge(GamingAccessory_weekly, specialSale_data_weekly, by = 'week_no', 
                                all.x = TRUE)

# . . . . . . . .  Missing ValuES set to 0 ----

GamingAccessory_weekly_sale$Noofsaledays[is.na(GamingAccessory_weekly_sale$Noofsaledays)] <- 0  

View(GamingAccessory_weekly_sale)
#----------Camera Accesory analysis begins------------------------#

CameraAccessory$SLA <- as.integer(CameraAccessory$SLA)

CameraAccessory_weekly <-group_by(CameraAccessory,week_no)

CameraAccessory_weekly <-summarise(CameraAccessory_weekly,TotalGMV=sum(gmv),product_mrp=mean(MRP), 
                                   units=sum(Units),
                                   discount=mean(discount),
                                   listprice=mean(listprice),
                                   sla=mean(SLA), 
                                   procurement_sla=mean(Procurement_SLA),
                                   COD=sum(COD),
                                   Prepaid = sum(Prepaid))

#tackling shelf inflation

CameraAccessory_weekly$shelfinflation<- 0

CameraAccessory_weekly$mvg_avg<- 0

no_of_row<-nrow(CameraAccessory_weekly)

temp<-CameraAccessory_weekly$listprice

for(i in 2:no_of_row) 
{
  CameraAccessory_weekly$shelfinflation[i] <- temp[i]/temp[i-1]
}

#Imputing first value with mean

CameraAccessory_weekly$shelfinflation[1]=mean(CameraAccessory_weekly$shelfinflation)

#Calculating for Moving Average

for(i in 4:no_of_row) 
{
  CameraAccessory_weekly$mvg_avg[i] <- (CameraAccessory_weekly$listprice[i-1]+CameraAccessory_weekly$listprice[i-2]+CameraAccessory_weekly$listprice[i-3])/3
}

#merging with weekly special sale data

CameraAccessory_weekly_sale <- merge(CameraAccessory_weekly, specialSale_data_weekly, by = 'week_no', 
                                     all.x = TRUE)

# . . . . . . . .  Missing ValuES set to 0 ----

CameraAccessory_weekly_sale$Noofsaledays[is.na(CameraAccessory_weekly_sale$Noofsaledays)] <- 0 
View(CameraAccessory_weekly_sale)

#----------Home Audio analysis begins------------------------#

HomeAudio$SLA <- as.integer(HomeAudio$SLA)

HomeAudio_weekly <-group_by(HomeAudio,week_no)

HomeAudio_weekly <-summarise(HomeAudio_weekly,TotalGMV=sum(gmv),product_mrp=mean(MRP), 
                                   units=sum(Units),
                                   discount=mean(discount),
                                   listprice=mean(listprice),
                                   sla=mean(SLA), 
                                   procurement_sla=mean(Procurement_SLA),
                                   COD=sum(COD),
                                   Prepaid = sum(Prepaid))

#tackling shelf inflation

HomeAudio_weekly$shelfinflation<- 0

HomeAudio_weekly$mvg_avg<- 0

no_of_row<-nrow(HomeAudio_weekly)

temp<-HomeAudio_weekly$listprice

for(i in 2:no_of_row) 
{
  HomeAudio_weekly$shelfinflation[i] <- temp[i]/temp[i-1]
}

#Imputing first value with mean

HomeAudio_weekly$shelfinflation[1]=mean(HomeAudio_weekly$shelfinflation)

#Calculating for Moving Average

for(i in 4:no_of_row) 
{
  HomeAudio_weekly$mvg_avg[i] <- (HomeAudio_weekly$listprice[i-1]+HomeAudio_weekly$listprice[i-2]+HomeAudio_weekly$listprice[i-3])/3
}

#merging with weekly special sale data

HomeAudio_weekly_sale <- merge(HomeAudio_weekly, specialSale_data_weekly, by = 'week_no', 
                                     all.x = TRUE)

# . . . . . . . .  Missing ValuES set to 0 ----

HomeAudio_weekly_sale$Noofsaledays[is.na(HomeAudio_weekly_sale$Noofsaledays)] <- 0 
View(HomeAudio_weekly_sale)

# ***************************************************************************
#                   MERGING DATA ----
# ***************************************************************************

# . . . .   Merge MediaInvestment & NPS ----
media_nps <- merge(mediaInvestment_weekly, monthlyNPS_weekly, by = 'week_no', 
                   all.x = TRUE)
plot1 <- ggplot(media_nps,aes(TotalInvestment,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Total Advertising Investment Vs NPS") + labs(x = " Total Investment", y = "NPS")
plot1

plot2 <- ggplot(media_nps,aes(week_no,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise NPS variation") + labs(x = "Week", y = "NPS")
plot2

plot2_1 <- ggplot(media_nps,aes(week_no,TotalInvestment))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise Total Investment variation") + labs(x = "Week", y = "Total Investment")
plot2_1

plot3 <- ggplot(media_nps,aes(TV,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("TV Advertising Investment Vs NPS") + labs(x = " TV Investment", y = "NPS")
plot3

plot4 <- ggplot(media_nps,aes(Digital,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Digital Advertising Investment Vs NPS") + labs(x = " Digital Investment", y = "NPS")
plot4

plot5 <- ggplot(media_nps,aes(Sponsorship,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Sponsorship Investment Vs NPS") + labs(x = " Sponsorship Investment", y = "NPS")
plot5

plot6 <- ggplot(media_nps,aes(Radio,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Radio Investment Vs NPS") + labs(x = " Radio Investment", y = "NPS")
plot6

plot7 <- ggplot(media_nps,aes(ContentMarketing,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Content Marketing Investment Vs NPS") + labs(x = " Content Marketing Investment", y = "NPS")
plot7

plot8 <- ggplot(media_nps,aes(OnlineMarketing,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Online Marketing Investment Vs NPS") + labs(x = " Online Marketing Investment", y = "NPS")
plot8

plot9 <- ggplot(media_nps,aes(Affiliates,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Affiliates Investment Vs NPS") + labs(x = " Affiliates Investment", y = "NPS")
plot9

plot10 <- ggplot(media_nps,aes(SEM,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("SEM Investment Vs NPS") + labs(x = " SEM Investment", y = "NPS")
plot10


# . . . .   Merge Sales & NPS data

GamingAccessory_mrgd_weekly <- merge(GamingAccessory_weekly_sale, media_nps, by = 'week_no', all.x = TRUE)

View(GamingAccessory_mrgd_weekly)

plot11 <- ggplot(GamingAccessory_mrgd_weekly,aes(week_no,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of GMV for Gaming Accesories") + labs(x = " Week", y = "GMV")
plot11

plot12 <- ggplot(GamingAccessory_mrgd_weekly,aes(week_no,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Units sold for Gaming Accesories") + labs(x = " Week", y = "Units Sold")
plot12

plot13 <- ggplot(GamingAccessory_mrgd_weekly,aes(TotalGMV,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" GMV vs NPS") + labs(x = " GMV of Gaming Accesories ", y = "NPS")
plot13

plot14 <- ggplot(GamingAccessory_mrgd_weekly,aes(units,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Units sold vs NPS") + labs(x = " Units sold Gaming Accesories", y = "NPS")
plot14

plot23 <- ggplot(GamingAccessory_mrgd_weekly,aes(discount,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Discount Given vs  Total GMV") + labs(x = " Discount Given", y = "Total GMV for Gaming Accesories")
plot23

plot24 <- ggplot(GamingAccessory_mrgd_weekly,aes(discount,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Discount Given vs Units sold") + labs(x = " Discount Given", y = " Units sold for Gaming Accesories")
plot24

plot35 <- ggplot(GamingAccessory_mrgd_weekly,aes(listprice,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" List Price vs  Total GMV") + labs(x = " Avg List Price", y = "Total GMV for Gaming Accesories")
plot35

plot36 <- ggplot(GamingAccessory_mrgd_weekly,aes(listprice,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" List Price vs Units sold") + labs(x = " Avg List Price", y = " Units sold for Gaming Accesories")
plot36

plot29 <- ggplot(GamingAccessory_mrgd_weekly,aes(product_mrp,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. MRP vs  Total GMV") + labs(x = " Avg. MRP", y = "Total GMV for Gaming Accesories")
plot29

plot30 <- ggplot(GamingAccessory_mrgd_weekly,aes(product_mrp,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. MRP vs Units sold") + labs(x = " Avg. MRP", y = " Units sold for Gaming Accesories")
plot30

plot41 <- ggplot(GamingAccessory_mrgd_weekly,aes(week_no,shelfinflation))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Shelf Inflation for Gaming Accesories") + labs(x = " Week", y = "Shelf Inflation")
plot41

plot42 <- ggplot(GamingAccessory_mrgd_weekly,aes(week_no,mvg_avg))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Moving Avg of List Price for Gaming Accesories") + labs(x = " Week", y = "Mvg Avg Of List Price")
plot42

#FActoring in Special Sales

plot51 <- ggplot(GamingAccessory_mrgd_weekly,aes(Noofsaledays,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("No of Sale days vs GMV") + labs(x = " No Of Sale Days", y = "GMV")
plot51

plot52 <- ggplot(GamingAccessory_mrgd_weekly,aes(Noofsaledays,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("No of Sale days vs Units Sold") + labs(x = " No of Sale days", y = "Units Sold")
plot52

#EDA for Camera Accessory  begins

CameraAccessory_mrgd_weekly <- merge(CameraAccessory_weekly_sale, media_nps, by = 'week_no', all.x = TRUE)

View(CameraAccessory_mrgd_weekly)

plot15 <- ggplot(CameraAccessory_mrgd_weekly,aes(week_no,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of GMV for Camera Accesories") + labs(x = " Week", y = "GMV")
plot15

plot16 <- ggplot(CameraAccessory_mrgd_weekly,aes(week_no,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Units sold for Camera Accesories") + labs(x = " Week", y = "Units sold")
plot16

plot17 <- ggplot(CameraAccessory_mrgd_weekly,aes(TotalGMV,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" GMV vs NPS") + labs(x = " GMV of Camera Accesories", y = "NPS")
plot17

plot18 <- ggplot(CameraAccessory_mrgd_weekly,aes(units,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Units sold vs NPS") + labs(x = " Units sold of Camera Accesories", y = "NPS")
plot18

plot25 <- ggplot(CameraAccessory_mrgd_weekly,aes(discount,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Discount Given vs  Total GMV") + labs(x = "Discount Given", y = "Total GMV for Camera Accessories")
plot25

plot26 <- ggplot(CameraAccessory_mrgd_weekly,aes(discount,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Discount Given vs Units sold") + labs(x = "Discount Given ", y = " Units sold for Camera Accesories")
plot26

plot37 <- ggplot(CameraAccessory_mrgd_weekly,aes(listprice,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. List Price vs  Total GMV") + labs(x = "Avg. List Price", y = "Total GMV for Camera Accessories")
plot37

plot38 <- ggplot(CameraAccessory_mrgd_weekly,aes(listprice,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg List Price vs Units sold") + labs(x = "Avg. List Price ", y = " Units sold for Camera Accesories")
plot38

plot31 <- ggplot(CameraAccessory_mrgd_weekly,aes(product_mrp,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. MRP vs  Total GMV") + labs(x = " Avg. MRP", y = "Total GMV for Camera Accessories")
plot31

plot32 <- ggplot(CameraAccessory_mrgd_weekly,aes(product_mrp,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. MRP vs Units sold") + labs(x = " Avg. MRP", y = " Units sold for Gaming Accesories")
plot32

plot43 <- ggplot(CameraAccessory_mrgd_weekly,aes(week_no,shelfinflation))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Shelf Inflation for Camera Accesories") + labs(x = " Week", y = "Shelf Inflation")
plot43

plot44 <- ggplot(CameraAccessory_mrgd_weekly,aes(week_no,mvg_avg))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Moving Avg of List Price for Camera Accesories") + labs(x = " Week", y = "Mvg Avg Of List Price")
plot44

#FActoring in Special Sales

plot53 <- ggplot(CameraAccessory_mrgd_weekly,aes(Noofsaledays,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("No of Sale days vs GMV") + labs(x = " No Of Sale Days", y = "GMV")
plot53

plot54 <- ggplot(CameraAccessory_mrgd_weekly,aes(Noofsaledays,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("No of Sale days vs Units Sold") + labs(x = " No of Sale days", y = "Units Sold")
plot54

#EDA for Home Audio begins

HomeAudio_mrgd_weekly <- merge(HomeAudio_weekly_sale, media_nps, by = 'week_no', all.x = TRUE)

View(HomeAudio_mrgd_weekly)

plot19 <- ggplot(HomeAudio_mrgd_weekly,aes(week_no,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of GMV for HomeAudio Category") + labs(x = " Week", y = "GMV")
plot19

plot20 <- ggplot(HomeAudio_mrgd_weekly,aes(week_no,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Units sold for HomeAudio Category") + labs(x = " Week", y = "Units sold")
plot20

plot21 <- ggplot(HomeAudio_mrgd_weekly,aes(TotalGMV,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" GMV vs NPS") + labs(x = " GMV of HomeAudio Category", y = "NPS")
plot21

plot22 <- ggplot(HomeAudio_mrgd_weekly,aes(units,NPS))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Units sold vs NPS") + labs(x = " Units sold of HomeAudio Category", y = "NPS")
plot22

plot27 <- ggplot(HomeAudio_mrgd_weekly,aes(discount,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Discount Given vs  Total GMV") + labs(x = " Discount Given", y = "Total GMV for Home Audio")
plot27

plot28 <- ggplot(HomeAudio_mrgd_weekly,aes(discount,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Discount Given vs Units sold") + labs(x = " Discount Given", y = " Units sold for Home Audio")
plot28

plot39 <- ggplot(HomeAudio_mrgd_weekly,aes(listprice,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. List Price vs  Total GMV") + labs(x = "  Avg. List Price", y = "Total GMV for Home Audio")
plot39

plot40 <- ggplot(HomeAudio_mrgd_weekly,aes(listprice,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("  Avg. List Price vs Units sold") + labs(x = "  Avg. List Price", y = " Units sold for Home Audio")
plot40

plot33 <- ggplot(HomeAudio_mrgd_weekly,aes(product_mrp,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. MRP vs  Total GMV") + labs(x = " Avg. MRP", y = "Total GMV for Home Audio")
plot33

plot34 <- ggplot(HomeAudio_mrgd_weekly,aes(product_mrp,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. MRP vs Units sold") + labs(x = " Avg. MRP", y = " Units sold for Home Audio")
plot34

plot45 <- ggplot(HomeAudio_mrgd_weekly,aes(week_no,shelfinflation))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Shelf Inflation for Home Audio Products") + labs(x = " Week", y = "Shelf Inflation")
plot45

plot46 <- ggplot(HomeAudio_mrgd_weekly,aes(week_no,mvg_avg))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("Week wise variation of Moving Avg of List Price for Home Audio Products") + labs(x = " Week", y = "Mvg Avg Of List Price")
plot46

#FActoring in Special Sales

plot55 <- ggplot(HomeAudio_mrgd_weekly,aes(Noofsaledays,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("No of Sale days vs GMV") + labs(x = " No Of Sale Days", y = "GMV")
plot55

plot56 <- ggplot(HomeAudio_mrgd_weekly,aes(Noofsaledays,units))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle("No of Sale days vs Units Sold") + labs(x = " No of Sale days", y = "Units Sold")
plot56

summary(HomeAudio_mrgd_weekly$mvg_avg)
######################Linear Modelling#######################

#Gaming Accessory

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(GamingAccessory_mrgd_weekly), 0.7*nrow(GamingAccessory_mrgd_weekly))
# generate the train data set
train = GamingAccessory_mrgd_weekly[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =GamingAccessory_mrgd_weekly[-trainindices,]

# Build model 1 containing all variables
ga_model_1 <-lm(TotalGMV~.,data=train)
summary(ga_model_1)

step <- stepAIC(ga_model_1, direction="both")

# All other variables except the below need to be considered

ga_model_2 <- lm(TotalGMV ~ week_no + units + discount + procurement_sla + COD + 
                   Prepaid + shelfinflation + mvg_avg + TotalInvestment + Sponsorship + 
                   Radio + NPS, data = train)
summary(ga_model_2)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ga_model_2)

ga_model_3 <- lm(TotalGMV ~ week_no + discount + procurement_sla + COD + 
                   Prepaid + shelfinflation + mvg_avg + TotalInvestment + Sponsorship + 
                   Radio + NPS, data = train)
summary(ga_model_3)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ga_model_3)

ga_model_4 <- lm(TotalGMV ~ week_no + discount + procurement_sla + COD + 
                   Prepaid + shelfinflation + mvg_avg + TotalInvestment + Sponsorship + 
                   Radio, data = train)
summary(ga_model_4)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ga_model_4)

ga_model_5 <- lm(TotalGMV ~ discount + procurement_sla + COD + 
                   Prepaid + shelfinflation + mvg_avg + TotalInvestment + Sponsorship + 
                   Radio, data = train)
summary(ga_model_5)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ga_model_5)

ga_model_6 <- lm(TotalGMV ~ discount + procurement_sla + COD + 
                   Prepaid + mvg_avg + TotalInvestment + Sponsorship + 
                   Radio , data = train)
summary(ga_model_6)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ga_model_6)

ga_model_7 <- lm(TotalGMV ~ discount + procurement_sla + COD + 
                   Prepaid + TotalInvestment + Sponsorship + 
                   Radio , data = train)
summary(ga_model_7)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ga_model_7)

#Multiple R-squared:  0.9904,	Adjusted R-squared:  0.9881 

# predicting the results in test dataset
Predict_1 <- predict(ga_model_7,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)

# Cross validation
crooss_val <- cv.lm(data = GamingAccessory_mrgd_weekly, form.lm = ga_model_7, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#6.32e+10
##############################################################################################################
# Elasticity Analysis

mdl_ea <- ga_model_7


# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Linear Model") +xlab("Variables")


#*********************************************************************

plot_result7 <- ggplot(GamingAccessory_mrgd_weekly, aes(x=GamingAccessory_mrgd_weekly$TotalGMV)) + xlab("Total GMV") + ylab("Payment Type") +ggtitle(" Payment Type and GMV")+
  geom_line(aes(y = GamingAccessory_mrgd_weekly$COD, colour = "COD")) + 
  geom_line(aes(y = GamingAccessory_mrgd_weekly$Prepaid, colour = "Prepaid")) 
plot_result7 


plot_result8 <- ggplot(GamingAccessory_mrgd_weekly, aes(x=GamingAccessory_mrgd_weekly$TotalGMV)) + xlab("Total GMV") + ylab("Invesstment Type") +ggtitle(" Investment Type and GMV")+
  geom_line(aes(y = GamingAccessory_mrgd_weekly$TotalInvestment, colour = "TotalInvestment")) + 
  geom_line(aes(y = GamingAccessory_mrgd_weekly$Sponsorship, colour = "Sponsorship")) +
  geom_line(aes(y = GamingAccessory_mrgd_weekly$Radio, colour = "Radio"))
plot_result8 

plot_result9 <- ggplot(GamingAccessory_mrgd_weekly,aes(Radio,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Radio Investment vs  Total GMV") + labs(x = "  Radio Investment", y = "Total GMV for Gaming Accessory")
plot_result9

plot_result10 <- ggplot(GamingAccessory_mrgd_weekly,aes(Sponsorship,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Sponsorship Investment vs  Total GMV") + labs(x = "  Sponsorship Investment", y = "Total GMV for Gaming Accessory")
plot_result10

plot_result11 <- ggplot(GamingAccessory_mrgd_weekly,aes(TotalInvestment,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Total Investment vs  Total GMV") + labs(x = "  Total Investment", y = "Total GMV for Gaming Accessory")
plot_result11

plot_result12 <- ggplot(GamingAccessory_mrgd_weekly,aes(discount,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Total GMV vs  Discount") + labs(x = "  Discount", y = "Total GMV for Gaming Accessory")
plot_result12

plot_result13 <- ggplot(GamingAccessory_mrgd_weekly,aes(procurement_sla,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Total GMV vs  Procurement SLA") + labs(x = "  Procurement SLA", y = "Total GMV for Gaming Accessory")
plot_result13

gaming_accessory_corr = cor(GamingAccessory_mrgd_weekly)
library(corrplot)
corrplot(gaming_accessory_corr)
#Camera Accessory

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(CameraAccessory_mrgd_weekly), 0.7*nrow(CameraAccessory_mrgd_weekly))
# generate the train data set
train = CameraAccessory_mrgd_weekly[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =CameraAccessory_mrgd_weekly[-trainindices,]

# Build model 1 containing all variables
ca_model_1 <-lm(TotalGMV~.,data=train)
summary(ca_model_1)

step <- stepAIC(ca_model_1, direction="both")

ca_model_2 <- lm(TotalGMV ~ units + discount + listprice + sla + procurement_sla + 
                   COD + Prepaid + shelfinflation + Noofsaledays + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Other + NPS, data = train)
summary(ca_model_2)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_2)


ca_model_3 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + Prepaid + shelfinflation + Noofsaledays + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Other + NPS, data = train)
summary(ca_model_3)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_3)

ca_model_4 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + Prepaid + shelfinflation + Noofsaledays + TotalInvestment + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Affiliates + SEM + Other + NPS, data = train)
summary(ca_model_4)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_4)

ca_model_5 <- lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + shelfinflation + Noofsaledays + TotalInvestment + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Affiliates + SEM + Other + NPS, data = train)
summary(ca_model_5)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_5)

ca_model_6 <- lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + Noofsaledays + TotalInvestment + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Affiliates + SEM + Other + NPS, data = train)
summary(ca_model_6)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_6)

ca_model_7 <- lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + Noofsaledays + TotalInvestment + 
                  TV + Digital + Sponsorship + OnlineMarketing + 
                   Affiliates + SEM + Other, data = train)
summary(ca_model_7)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_7)

ca_model_8 <- lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + Noofsaledays + TotalInvestment + 
                 Digital + Sponsorship + OnlineMarketing + 
                   Affiliates + SEM + Other, data = train)
summary(ca_model_8)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_8)

ca_model_9 <- lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + Noofsaledays + TotalInvestment + 
                   Digital + Sponsorship + OnlineMarketing + 
                   Affiliates + Other, data = train)
summary(ca_model_9)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_9)

ca_model_10 <- lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + Noofsaledays + TotalInvestment + 
                   Digital + Sponsorship + 
                   Affiliates + Other, data = train)
summary(ca_model_10)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_10)

ca_model_11 <- lm(TotalGMV ~ units + discount + listprice + COD + Noofsaledays + TotalInvestment + 
                    Digital + Sponsorship + 
                    Affiliates + Other, data = train)
summary(ca_model_11)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_11)

ca_model_12 <- lm(TotalGMV ~ units + discount + listprice + Noofsaledays + TotalInvestment + 
                    Digital + Sponsorship + 
                    Affiliates + Other, data = train)
summary(ca_model_12)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_12)

ca_model_13 <- lm(TotalGMV ~ units + discount + listprice + TotalInvestment + 
                    Digital + Sponsorship + 
                    Affiliates + Other,  data = train)
summary(ca_model_13)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_13)

ca_model_14 <- lm(TotalGMV ~ units + discount + listprice + TotalInvestment + 
                    Digital + Affiliates + Other,  data = train)
summary(ca_model_14)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_14)

ca_model_15 <- lm(TotalGMV ~ units + discount + listprice +  
                    Digital + Affiliates + Other,  data = train)
summary(ca_model_15)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_15)

ca_model_16 <- lm(TotalGMV ~ units + discount + listprice +  
                    Digital + Affiliates ,  data = train)
summary(ca_model_16)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_16)

ca_model_17 <- lm(TotalGMV ~ units + discount + listprice + Affiliates ,  data = train)
summary(ca_model_17)


## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_17)


ca_model_18 <- lm(TotalGMV ~ units + discount + listprice ,  data = train)
summary(ca_model_18)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ca_model_18)

#Multiple R-squared:  0.9695,	Adjusted R-squared:  0.9667

# predicting the results in test dataset
Predict_1 <- predict(ca_model_18,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.9425

# Cross validation
crooss_val <- cv.lm(data = CameraAccessory_mrgd_weekly, form.lm = ca_model_18, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#7.44e+11

plot_result4 <- ggplot(CameraAccessory_mrgd_weekly,aes(listprice,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. List Price vs  Total GMV") + labs(x = "  Avg. List Price", y = "Total GMV for Camera Accessories")
plot_result4

plot_result5 <- ggplot(CameraAccessory_mrgd_weekly,aes(discount,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Discount vs  Total GMV") + labs(x = "  Discount", y = "Total GMV for Camera Accessories")
plot_result5

plot_result6 <- ggplot(CameraAccessory_mrgd_weekly,aes(CameraAccessory_mrgd_weekly$units,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" No.of units vs  Total GMV") + labs(x = "No.of Units", y = "Total GMV for Camera Accessories")
plot_result6

##############################################################################################################
# Elasticity Analysis

mdl_ea <- ca_model_18


# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Linear Model") +xlab("Variables")

#************************************************************************#

#Home Audio

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(HomeAudio_mrgd_weekly), 0.7*nrow(HomeAudio_mrgd_weekly))
# generate the train data set
train = HomeAudio_mrgd_weekly[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =HomeAudio_mrgd_weekly[-trainindices,]

# Build model 1 containing all variables
ha_model_1 <-lm(TotalGMV~.,data=train)
summary(ha_model_1)

step <- stepAIC(ha_model_1, direction="both")

ha_model_2 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                   sla + procurement_sla + COD + Prepaid + shelfinflation + 
                   mvg_avg + Noofsaledays + TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other + NPS, data = train)

summary(ha_model_2)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_2)

ha_model_3 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                   sla + procurement_sla + COD + Prepaid + shelfinflation + 
                   mvg_avg + TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other + NPS, data = train)

summary(ha_model_3)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_3)

ha_model_4 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                   sla + procurement_sla + COD + Prepaid + 
                   mvg_avg + TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other + NPS, data = train)

summary(ha_model_4)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_4)

ha_model_5 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                   procurement_sla + COD + Prepaid + 
                   mvg_avg + TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other + NPS, data = train)

summary(ha_model_5)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_5)

ha_model_6 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                   procurement_sla + COD + Prepaid + 
                   TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other + NPS, data = train)

summary(ha_model_6)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_6)


ha_model_7 <- lm(TotalGMV ~ week_no + product_mrp + discount + listprice + 
                   procurement_sla + COD + Prepaid + 
                   TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other + NPS, data = train)

summary(ha_model_7)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_7)

ha_model_8 <- lm(TotalGMV ~ week_no + product_mrp + discount + listprice + 
                   procurement_sla + COD + Prepaid + 
                   TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other, data = train)

summary(ha_model_8)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_8)

ha_model_9 <- lm(TotalGMV ~ week_no + discount + listprice + 
                   procurement_sla + COD + Prepaid + 
                   TotalInvestment + TV + Digital + 
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other, data = train)

summary(ha_model_9)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_9)

ha_model_10 <- lm(TotalGMV ~ week_no + discount + listprice + 
                   procurement_sla + COD + Prepaid + 
                   TotalInvestment + TV +  
                   Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                   SEM + Other, data = train)

summary(ha_model_10)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_10)

ha_model_11 <- lm(TotalGMV ~ week_no + discount + listprice + 
                    procurement_sla + COD + Prepaid + 
                    TotalInvestment + TV +  
                    Sponsorship + ContentMarketing  + Affiliates + 
                    SEM + Other, data = train)

summary(ha_model_11)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_11)

ha_model_12 <- lm(TotalGMV ~ week_no + discount + listprice + 
                    procurement_sla + COD + Prepaid + 
                    TotalInvestment +   
                    Sponsorship + ContentMarketing  + Affiliates + 
                    SEM + Other, data = train)

summary(ha_model_12)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_12)

ha_model_13 <- lm(TotalGMV ~ week_no + discount + listprice + 
                    procurement_sla + COD + Prepaid + 
                    TotalInvestment +   
                    Sponsorship + ContentMarketing  + Affiliates + 
                    Other, data = train)

summary(ha_model_13)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_13)

ha_model_14 <- lm(TotalGMV ~ discount + listprice + 
                    procurement_sla + COD + Prepaid + 
                    TotalInvestment +   
                    Sponsorship + ContentMarketing  + Affiliates + 
                    Other, data = train)

summary(ha_model_14)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_14)

ha_model_15 <- lm(TotalGMV ~ discount + listprice + 
                    procurement_sla + COD + Prepaid + 
                    TotalInvestment +   
                    Sponsorship + Affiliates + 
                    Other, data = train)

summary(ha_model_15)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_15)

ha_model_16 <- lm(TotalGMV ~ discount + listprice + 
                     COD + Prepaid + 
                    TotalInvestment +   
                    Sponsorship + Affiliates + 
                    Other, data = train)

summary(ha_model_16)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_16)

ha_model_17 <- lm(TotalGMV ~ discount + listprice + 
                    COD + Prepaid + 
                    TotalInvestment +   
                    Sponsorship +  
                    Other, data = train)

summary(ha_model_17)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_17)

ha_model_18 <- lm(TotalGMV ~ discount + listprice + 
                    COD + Prepaid + 
                    TotalInvestment +   
                    Other, data = train)

summary(ha_model_18)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_18)

ha_model_19 <- lm(TotalGMV ~ discount + listprice + 
                    COD + Prepaid + 
                    Other, data = train)

summary(ha_model_19)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_19)

ha_model_20 <- lm(TotalGMV ~ discount + listprice + 
                    COD + Prepaid , data = train)

summary(ha_model_20)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(ha_model_20)

#Multiple R-squared:  0.9965,	Adjusted R-squared:  0.9961

# predicting the results in test dataset
Predict_1 <- predict(ha_model_20,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.99

# Cross validation
crooss_val <- cv.lm(data = HomeAudio_mrgd_weekly, form.lm = ha_model_20, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#9.25e+10


plot_result1 <- ggplot(HomeAudio_mrgd_weekly,aes(listprice,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Avg. List Price vs  Total GMV") + labs(x = "  Avg. List Price", y = "Total GMV for Home Audio")
plot_result1


plot_result2 <- ggplot(HomeAudio_mrgd_weekly,aes(discount,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Total GMV vs  Discount") + labs(x = "  Discount", y = "Total GMV for Home Audio")
plot_result2


# plot_result3 <- ggplot(HomeAudio_mrgd_weekly,aes(COD,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" COD vs  Total GMV") + labs(x = "  COD", y = "Total GMV for Home Audio")
# plot_result3
# 
# plot_result3 <- ggplot(HomeAudio_mrgd_weekly,aes(Prepaid,TotalGMV))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(" Prepaid vs  Total GMV") + labs(x = "  Prepaid", y = "Total GMV for Home Audio")
# plot_result3

plot_result3 <- ggplot(HomeAudio_mrgd_weekly, aes(x=HomeAudio_mrgd_weekly$TotalGMV)) + xlab("Total GMV") + ylab("Payment Type") +ggtitle(" Payment Type and GMV")+
  geom_line(aes(y = HomeAudio_mrgd_weekly$COD, colour = "COD")) + 
  geom_line(aes(y = HomeAudio_mrgd_weekly$Prepaid, colour = "Prepaid")) 
plot_result3  

##############################################################################################################
# Elasticity Analysis

mdl_ea <- ga_model_7


# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Linear Model") +xlab("Variables")


#*********************************************************************


###################################### Distributed Lag Modelling ###############################################

HomeAudio_mrgd_weekly_dlag <- HomeAudio_mrgd_weekly
GamingAccessory_mrgd_weekly_dlag <- GamingAccessory_mrgd_weekly
CameraAccessory_mrgd_weekly_dlag <- CameraAccessory_mrgd_weekly

################### Home Audio Accessory

dlag_model_1 <- slide(HomeAudio_mrgd_weekly_dlag, Var = "TotalGMV",slideBy = -1)
View(dlag_model_1)
dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -2)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -3)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -4)


dlag_model <- na.omit(dlag_model_1)

dlag_model <- scale(dlag_model)
dlag_model <- data.frame(dlag_model)

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(dlag_model)

trainindices= sample(1:nrow(dlag_model), 0.7*nrow(dlag_model))
# generate the train data set
train = dlag_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =dlag_model[-trainindices,]

# Build model 1 containing all variables
ha_dlag_mdl_1 <-lm(TotalGMV~.,data=train)
summary(ha_dlag_mdl_1)

step <- stepAIC(ha_dlag_mdl_1, direction="both")

ha_dlag_mdl_2 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                      Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_mdl_2)
vif(ha_dlag_mdl_2)

ha_dlag_mdl_3 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates +  Radio + 
                      Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_mdl_3)
vif(ha_dlag_mdl_3)

ha_dlag_mdl_4 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates +  Radio + 
                      Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_mdl_4)
vif(ha_dlag_mdl_4)

ha_dlag_mdl_5 <- lm(TotalGMV ~ product_mrp + units + discount + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates +  Radio + 
                      Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_mdl_5)
vif(ha_dlag_mdl_5)

ha_dlag_mdl_6 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates +  Radio + 
                      Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_mdl_6)
vif(ha_dlag_mdl_6)


ha_dlag_mdl_7 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates +  Radio + 
                      Other + NPS + TotalGMV.1 + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_7)
vif(ha_dlag_mdl_7)

ha_dlag_mdl_8 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates +  Radio + 
                      Other + TotalGMV.1 + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_8)
vif(ha_dlag_mdl_8)


ha_dlag_mdl_9 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                      sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                      Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing +Affiliates +  Radio + 
                      Other + TotalGMV.1 + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_9)
vif(ha_dlag_mdl_9)

ha_dlag_mdl_10 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                       Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                       ContentMarketing +Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_10)
vif(ha_dlag_mdl_10)

ha_dlag_mdl_11 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + Prepaid + shelfinflation + mvg_avg + 
                       Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                       ContentMarketing +Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_11)
vif(ha_dlag_mdl_11)

ha_dlag_mdl_12 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + shelfinflation + mvg_avg + 
                       Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                       ContentMarketing +Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_12)
vif(ha_dlag_mdl_12)

ha_dlag_mdl_13 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + shelfinflation + Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                       ContentMarketing +Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_13)
vif(ha_dlag_mdl_13)

ha_dlag_mdl_14 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                       ContentMarketing +Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_14)
vif(ha_dlag_mdl_14)

ha_dlag_mdl_15 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + TotalInvestment + TV + Digital + Sponsorship + 
                       ContentMarketing +Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_15)
vif(ha_dlag_mdl_15)

ha_dlag_mdl_16 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + TotalInvestment + TV + Digital + Sponsorship + 
                       Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_16)
vif(ha_dlag_mdl_16)

ha_dlag_mdl_17 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       sla + TotalInvestment + Digital + Sponsorship + 
                       Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_17)
vif(ha_dlag_mdl_17)

ha_dlag_mdl_18 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       TotalInvestment + Digital + Sponsorship + 
                       Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_18)
vif(ha_dlag_mdl_18)

ha_dlag_mdl_18 <- lm(TotalGMV ~ product_mrp + units + listprice + 
                       TotalInvestment + Digital + Sponsorship + 
                       Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_18)
vif(ha_dlag_mdl_18)

ha_dlag_mdl_19 <- lm(TotalGMV ~  units + listprice + 
                       TotalInvestment + Digital + Sponsorship + 
                       Affiliates +  Radio + 
                       Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_19)
vif(ha_dlag_mdl_19)

ha_dlag_mdl_20 <- lm(TotalGMV ~  units + listprice + 
                       Digital + Sponsorship + 
                       Affiliates +  Radio + Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_20)
vif(ha_dlag_mdl_20)

ha_dlag_mdl_21 <- lm(TotalGMV ~  units + listprice + 
                       Digital + Sponsorship + 
                       Affiliates + Other + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_21)
vif(ha_dlag_mdl_21)


ha_dlag_mdl_22 <- lm(TotalGMV ~  units + listprice + 
                       Digital + Sponsorship + 
                       Affiliates + TotalGMV.2 , data = train)

summary(ha_dlag_mdl_22)
vif(ha_dlag_mdl_22)

ha_dlag_mdl_23 <- lm(TotalGMV ~  units + listprice + 
                       Digital + Sponsorship + 
                       TotalGMV.2 , data = train)

summary(ha_dlag_mdl_23)
vif(ha_dlag_mdl_23)

ha_dlag_mdl_24 <- lm(TotalGMV ~  units + listprice + 
                       Digital + Sponsorship , data = train)

summary(ha_dlag_mdl_24)
vif(ha_dlag_mdl_24)

ha_dlag_mdl_25 <- lm(TotalGMV ~  units + listprice + Sponsorship , data = train)

summary(ha_dlag_mdl_25)
vif(ha_dlag_mdl_25)

ha_dlag_mdl_26 <- lm(TotalGMV ~  units + listprice  , data = train)

summary(ha_dlag_mdl_26)
vif(ha_dlag_mdl_26)

# predicting the results in test dataset
Predict_1 <- predict(ha_dlag_mdl_26,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#.9917

# Cross validation
crooss_val <- cv.lm(data = dlag_model, form.lm = ha_dlag_mdl_26, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#0.0907
##############################################################################################################
# Elasticity Analysis

mdl_ea <-ha_dlag_mdl_26

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Distributed Lag Model") +xlab("Variables")


#*********************************************************************

################### Camera Accessory

View(CameraAccessory_mrgd_weekly_dlag)
dlag_model_1 <- slide(CameraAccessory_mrgd_weekly_dlag, Var = "TotalGMV",slideBy = -1)
View(dlag_model_1)
dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -2)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -3)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -4)


dlag_model <- na.omit(dlag_model_1)

dlag_model <- scale(dlag_model)
dlag_model <- data.frame(dlag_model)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(dlag_model)

trainindices= sample(1:nrow(dlag_model), 0.7*nrow(dlag_model))
# generate the train data set
train = dlag_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =dlag_model[-trainindices,]

# Build model 1 containing all variables
ca_dlag_mdl_1 <-lm(TotalGMV~.,data=train)
summary(ca_dlag_mdl_1)

step <- stepAIC(ca_dlag_mdl_1, direction="both")

ca_dlag_mdl_2 <- lm(TotalGMV ~ week_no + discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                      Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                      NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_2)
vif(ca_dlag_mdl_2)

ca_dlag_mdl_3 <- lm(TotalGMV ~ week_no + discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                      Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                      NPS + TotalGMV.3 + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_3)
vif(ca_dlag_mdl_3)

ca_dlag_mdl_4 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                      Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                      NPS + TotalGMV.3 + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_4)
vif(ca_dlag_mdl_4)

ca_dlag_mdl_5 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment +  Digital + 
                      Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                      NPS + TotalGMV.3 + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_5)
vif(ca_dlag_mdl_5)


ca_dlag_mdl_6 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment +  Digital + 
                      Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                      NPS + TotalGMV.3 + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_6)
vif(ca_dlag_mdl_6)

ca_dlag_mdl_7 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment +Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                      NPS + TotalGMV.3 + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_7)
vif(ca_dlag_mdl_7)

ca_dlag_mdl_8 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment +Sponsorship + ContentMarketing + Affiliates + Other + 
                      NPS + TotalGMV.3 + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_8)
vif(ca_dlag_mdl_8)

ca_dlag_mdl_9 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + shelfinflation + TotalInvestment +Sponsorship + ContentMarketing + Affiliates + Other + 
                      NPS + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_9)
vif(ca_dlag_mdl_9)

ca_dlag_mdl_10 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + TotalInvestment +Sponsorship + ContentMarketing + Affiliates + Other + 
                       NPS + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_10)
vif(ca_dlag_mdl_10)

ca_dlag_mdl_11 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + Sponsorship + ContentMarketing + Affiliates + Other + 
                       NPS + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_11)
vif(ca_dlag_mdl_11)

ca_dlag_mdl_12 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + ContentMarketing + Affiliates + Other + 
                       NPS + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_12)
vif(ca_dlag_mdl_12)

ca_dlag_mdl_12 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + ContentMarketing + Other + 
                       NPS + TotalGMV.4 , data = train)

summary(ca_dlag_mdl_12)
vif(ca_dlag_mdl_12)

ca_dlag_mdl_13 <- lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + Other + NPS + TotalGMV.4, data = train)

summary(ca_dlag_mdl_13)
vif(ca_dlag_mdl_13)

# predicting the results in test dataset
Predict_1 <- predict(ca_dlag_mdl_13,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#.95

# Cross validation
crooss_val <- cv.lm(data = dlag_model, form.lm = ca_dlag_mdl_13, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#0.224

##############################################################################################################
# Elasticity Analysis

mdl_ea <-ca_dlag_mdl_13

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Distributed Lag Model") +xlab("Variables")


#*********************************************************************


################### Gaming Accessory

View(GamingAccessory_mrgd_weekly_dlag)
dlag_model_1 <- slide(GamingAccessory_mrgd_weekly_dlag, Var = "TotalGMV",slideBy = -1)
View(dlag_model_1)
dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -2)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -3)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -4)


dlag_model <- na.omit(dlag_model_1)

dlag_model <- scale(dlag_model)
dlag_model <- data.frame(dlag_model)

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(dlag_model)

trainindices= sample(1:nrow(dlag_model), 0.7*nrow(dlag_model))
# generate the train data set
train = dlag_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =dlag_model[-trainindices,]

# Build model 1 containing all variables
ga_dlag_mdl_1 <-lm(TotalGMV~.,data=train)
summary(ga_dlag_mdl_1)

step <- stepAIC(ga_dlag_mdl_1, direction="both")

ga_dlag_mdl_2 <-lm(TotalGMV ~ week_no + discount + listprice + procurement_sla + 
                     COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                     Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                     NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4,data=train)
summary(ga_dlag_mdl_2)
vif(ga_dlag_mdl_2)

ga_dlag_mdl_3 <-lm(TotalGMV ~ week_no + discount + listprice + procurement_sla + 
                     COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                     Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                     NPS + TotalGMV.3 + TotalGMV.4,data=train)
summary(ga_dlag_mdl_3)
vif(ga_dlag_mdl_3)

ga_dlag_mdl_4 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                     COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                     Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                     NPS + TotalGMV.3 + TotalGMV.4,data=train)
summary(ga_dlag_mdl_4)
vif(ga_dlag_mdl_4)

ga_dlag_mdl_5 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                     COD + Prepaid + shelfinflation + TotalInvestment + Digital + 
                     Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                     NPS + TotalGMV.3 + TotalGMV.4,data=train)
summary(ga_dlag_mdl_5)
vif(ga_dlag_mdl_5)


ga_dlag_mdl_6 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                     COD + Prepaid + shelfinflation + TotalInvestment + Digital + 
                     Sponsorship + ContentMarketing + Affiliates + Other + 
                     NPS + TotalGMV.3 + TotalGMV.4,data=train)
summary(ga_dlag_mdl_6)
vif(ga_dlag_mdl_6)


ga_dlag_mdl_7 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                     COD + Prepaid + shelfinflation + TotalInvestment +  
                     Sponsorship + ContentMarketing + Affiliates + Other + 
                     NPS + TotalGMV.3 + TotalGMV.4,data=train)
summary(ga_dlag_mdl_7)
vif(ga_dlag_mdl_7)

ga_dlag_mdl_8 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                     COD + Prepaid + shelfinflation + TotalInvestment +  
                     Sponsorship + ContentMarketing + Affiliates + Other + 
                     NPS + TotalGMV.4,data=train)
summary(ga_dlag_mdl_8)
vif(ga_dlag_mdl_8)

ga_dlag_mdl_9 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                     COD + Prepaid +  TotalInvestment +  
                     Sponsorship + ContentMarketing + Affiliates + Other + 
                     NPS + TotalGMV.4,data=train)
summary(ga_dlag_mdl_9)
vif(ga_dlag_mdl_9)

ga_dlag_mdl_10 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid +  Sponsorship + ContentMarketing + Affiliates + Other + 
                      NPS + TotalGMV.4,data=train)
summary(ga_dlag_mdl_10)
vif(ga_dlag_mdl_10)

ga_dlag_mdl_11 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + ContentMarketing + Affiliates + Other + 
                      NPS + TotalGMV.4,data=train)
summary(ga_dlag_mdl_11)
vif(ga_dlag_mdl_11)

ga_dlag_mdl_12 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                      COD + Prepaid + ContentMarketing + Other + 
                      NPS + TotalGMV.4,data=train)
summary(ga_dlag_mdl_12)
vif(ga_dlag_mdl_12)

# predicting the results in test dataset
Predict_1 <- predict(ga_dlag_mdl_12,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#.94


# Cross validation
crooss_val <- cv.lm(data = dlag_model, form.lm = ga_dlag_mdl_12, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#0.0773
##############################################################################################################
# Elasticity Analysis

mdl_ea <-ga_dlag_mdl_12

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Distributed Lag Model") +xlab("Variables")


#*********************************************************************


###################################### Multiplicative Modelling ###############################################

#Gaming Accessory

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(GamingAccessory_mrgd_weekly)

#replacing 0 with 0.00001 
#log(0) is not defined
GamingAccessory_mrgd_weekly[GamingAccessory_mrgd_weekly == 0] <- 0.00001

#taking log for multiplicative model
GamingAccessory_mrgd_weekly <- log(GamingAccessory_mrgd_weekly)


trainindices= sample(1:nrow(GamingAccessory_mrgd_weekly), 0.7*nrow(GamingAccessory_mrgd_weekly))
# generate the train data set
train = GamingAccessory_mrgd_weekly[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =GamingAccessory_mrgd_weekly[-trainindices,]

# Build model 1 containing all variables
ga_mul_model_1 <-lm(TotalGMV~.,data=train)
summary(ga_mul_model_1)

step <- stepAIC(ga_mul_model_1, direction="both")

# All other variables except the below need to be considered

ga_mul_model_2 <- lm(TotalGMV ~ product_mrp + units + discount + listprice + Prepaid + 
                   shelfinflation + mvg_avg + TotalInvestment + Digital + OnlineMarketing + 
                   Affiliates + SEM, data = train)
summary(ga_mul_model_2)
vif(ga_mul_model_2)

#vif of affiliates is very high but its p value is ignificant low
#so removing online marketing having high vif and high p vale

ga_mul_model_3 <- lm(TotalGMV ~ product_mrp + units + discount + listprice + Prepaid + 
                       shelfinflation + mvg_avg + TotalInvestment + Digital + 
                       Affiliates + SEM, data = train)
summary(ga_mul_model_3)
vif(ga_mul_model_3)

#total investment is having high vif and high p value 
#removing total investment
ga_mul_model_4 <- lm(TotalGMV ~ product_mrp + units + discount + listprice + Prepaid + 
                       shelfinflation + mvg_avg + Digital + 
                       Affiliates + SEM, data = train)
summary(ga_mul_model_4)
vif(ga_mul_model_4)

#prepaid paraamter has high vif and high p-value
#removing prepaid

ga_mul_model_5 <- lm(TotalGMV ~ product_mrp + units + discount + listprice + 
                       shelfinflation + mvg_avg + Digital + 
                       Affiliates + SEM, data = train)
summary(ga_mul_model_5)
vif(ga_mul_model_5)

#removing product mrp based on high p-value
ga_mul_model_6 <- lm(TotalGMV ~ units + discount + listprice + 
                       shelfinflation + mvg_avg + Digital + 
                       Affiliates + SEM, data = train)
summary(ga_mul_model_6)
vif(ga_mul_model_6)

#removing discount based on p-value
ga_mul_model_7 <- lm(TotalGMV ~ units + listprice + 
                       shelfinflation + mvg_avg + Digital + 
                       Affiliates + SEM, data = train)
summary(ga_mul_model_7)
vif(ga_mul_model_7)

#removing digital based on high p-value
ga_mul_model_8 <- lm(TotalGMV ~ units + listprice + 
                       shelfinflation + mvg_avg + 
                       Affiliates + SEM, data = train)
summary(ga_mul_model_8)
vif(ga_mul_model_8)

#removing SEM based on high p-value
ga_mul_model_9 <- lm(TotalGMV ~ units + listprice + 
                       shelfinflation + mvg_avg + 
                       Affiliates, data = train)
summary(ga_mul_model_9)
vif(ga_mul_model_9)

#removing shelfinflation based on high p-value
ga_mul_model_10 <- lm(TotalGMV ~ units + listprice + mvg_avg + 
                       Affiliates, data = train)
summary(ga_mul_model_10)
vif(ga_mul_model_10)

#reemoving mvg_avg based on high p-value
ga_mul_model_11 <- lm(TotalGMV ~ units + listprice + Affiliates, data = train)
summary(ga_mul_model_11)
vif(ga_mul_model_11)

#reemoving affiliates based on high p-value
ga_mul_model_12 <- lm(TotalGMV ~ units + listprice , data = train)
summary(ga_mul_model_12)
vif(ga_mul_model_12)

# predicting the results in test dataset
Predict_1 <- predict(ga_mul_model_12,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
rsquared
#0.99

# Cross validation
crooss_val <- cv.lm(data = GamingAccessory_mrgd_weekly, form.lm = ga_mul_model_12, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#7.77e-05

##############################################################################################################
# Elasticity Analysis

mdl_ea <-ga_mul_model_12

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Multiplicative Model") +xlab("Variables")


###############################################################################################

#Home Audio Accessory

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(HomeAudio_mrgd_weekly)

#replacing 0 with 0.00001 
#log(0) is not defined
HomeAudio_mrgd_weekly[HomeAudio_mrgd_weekly == 0] <- 0.00001

#taking log for multiplicative model
HomeAudio_mrgd_weekly <- log(HomeAudio_mrgd_weekly)


trainindices= sample(1:nrow(HomeAudio_mrgd_weekly), 0.7*nrow(HomeAudio_mrgd_weekly))
# generate the train data set
train = HomeAudio_mrgd_weekly[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =HomeAudio_mrgd_weekly[-trainindices,]

# Build model 1 containing all variables
ha_mul_model_1 <-lm(TotalGMV~.,data=train)
summary(ha_mul_model_1)

step <- stepAIC(ha_mul_model_1, direction="both")

# All other variables except the below need to be considered
ha_mul_model_2 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                       sla + procurement_sla + COD + mvg_avg + Noofsaledays +  
                       Sponsorship + ContentMarketing + NPS, data = train)
summary(ha_mul_model_2)
vif(ha_mul_model_2)

ha_mul_model_3 <- lm(TotalGMV ~ week_no + units + discount + listprice + 
                       sla + procurement_sla + COD + mvg_avg + Noofsaledays +  
                       Sponsorship + ContentMarketing + NPS, data = train)
summary(ha_mul_model_3)
vif(ha_mul_model_3)


ha_mul_model_4 <- lm(TotalGMV ~ week_no + units + discount + listprice + 
                       sla + COD + mvg_avg + Noofsaledays +  
                       Sponsorship + ContentMarketing + NPS, data = train)
summary(ha_mul_model_4)
vif(ha_mul_model_4)

ha_mul_model_5 <- lm(TotalGMV ~ week_no + units + listprice + 
                       sla + COD + mvg_avg + Noofsaledays +  
                       Sponsorship + ContentMarketing + NPS, data = train)
summary(ha_mul_model_5)
vif(ha_mul_model_5)


ha_mul_model_6 <- lm(TotalGMV ~ week_no + units + listprice + 
                       sla + COD + mvg_avg + Noofsaledays +  
                       Sponsorship + ContentMarketing , data = train)
summary(ha_mul_model_6)
vif(ha_mul_model_6)


ha_mul_model_7 <- lm(TotalGMV ~ week_no + units + listprice + 
                       sla + mvg_avg + Noofsaledays +  
                       Sponsorship + ContentMarketing , data = train)
summary(ha_mul_model_7)
vif(ha_mul_model_7)

ha_mul_model_8 <- lm(TotalGMV ~ week_no + units + listprice + 
                       sla + mvg_avg + Sponsorship , data = train)
summary(ha_mul_model_8)
vif(ha_mul_model_8)


ha_mul_model_9 <- lm(TotalGMV ~ week_no + units + listprice + 
                       sla + mvg_avg  , data = train)
summary(ha_mul_model_9)
vif(ha_mul_model_9)

ha_mul_model_10 <- lm(TotalGMV ~ week_no + units + listprice +  mvg_avg  , data = train)
summary(ha_mul_model_10)
vif(ha_mul_model_10)

ha_mul_model_11 <- lm(TotalGMV ~ units + listprice +  mvg_avg  , data = train)
summary(ha_mul_model_11)
vif(ha_mul_model_11)


ha_mul_model_12 <- lm(TotalGMV ~ units + listprice   , data = train)
summary(ha_mul_model_11)
vif(ha_mul_model_12)

# predicting the results in test dataset
Predict_1 <- predict(ha_mul_model_12,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.98

# Cross validation
crooss_val <- cv.lm(data = HomeAudio_mrgd_weekly, form.lm = ha_mul_model_12, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#2.71e-05
##############################################################################################################
# Elasticity Analysis

mdl_ea <-ha_mul_model_12

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multiplicative Model") +xlab("Variables")


###############################################################################################

#Camera Accessory

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(CameraAccessory_mrgd_weekly)

#replacing 0 with 0.00001 
#log(0) is not defined
CameraAccessory_mrgd_weekly[CameraAccessory_mrgd_weekly == 0] <- 0.00001

#taking log for multiplicative model
CameraAccessory_mrgd_weekly <- log(CameraAccessory_mrgd_weekly)


trainindices= sample(1:nrow(CameraAccessory_mrgd_weekly), 0.7*nrow(CameraAccessory_mrgd_weekly))
# generate the train data set
train = CameraAccessory_mrgd_weekly[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =CameraAccessory_mrgd_weekly[-trainindices,]

# Build model 1 containing all variables
ca_mul_model_1 <-lm(TotalGMV~.,data=train)
summary(ca_mul_model_1)

step <- stepAIC(ca_mul_model_1, direction="both")

# All other variables except the below need to be considered
ca_mul_model_2 <- lm(TotalGMV ~ product_mrp + units + listprice + procurement_sla + 
                       mvg_avg + Noofsaledays + TV + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates, data = train)
summary(ca_mul_model_2)
vif(ca_mul_model_2)

#vif of onlinemarketing is very high
#removing onlinemarketing based on vif value
ca_mul_model_3 <- lm(TotalGMV ~ product_mrp + units + listprice + procurement_sla + 
                       mvg_avg + Noofsaledays + TV + Sponsorship + ContentMarketing + Affiliates, data = train)
summary(ca_mul_model_3)
vif(ca_mul_model_3)

#affilates have high vif and high p-value
ca_mul_model_4 <- lm(TotalGMV ~ product_mrp + units + listprice + procurement_sla + 
                       mvg_avg + Noofsaledays + TV + Sponsorship + ContentMarketing, data = train)
summary(ca_mul_model_4)
vif(ca_mul_model_4)

#removing contentmarketing based on p -value
ca_mul_model_5 <- lm(TotalGMV ~ product_mrp + units + listprice + procurement_sla + 
                       mvg_avg + Noofsaledays + TV + Sponsorship, data = train)
summary(ca_mul_model_5)
vif(ca_mul_model_5)

#removing mvg_avg based on p -value
ca_mul_model_6 <- lm(TotalGMV ~ product_mrp + units + listprice + procurement_sla + Noofsaledays + TV + Sponsorship, data = train)
summary(ca_mul_model_6)
vif(ca_mul_model_6)

#removing Noofsaledays based on p -value
ca_mul_model_7 <- lm(TotalGMV ~ product_mrp + units + listprice + procurement_sla+ TV + Sponsorship, data = train)
summary(ca_mul_model_7)
vif(ca_mul_model_7)

#removing product_mrp based on p -value
ca_mul_model_8 <- lm(TotalGMV ~ units + listprice + procurement_sla+ TV + Sponsorship, data = train)
summary(ca_mul_model_8)
vif(ca_mul_model_8)

#removing sponsorship based on p -value
ca_mul_model_9 <- lm(TotalGMV ~ units + listprice + procurement_sla+ TV, data = train)
summary(ca_mul_model_9)
vif(ca_mul_model_9)

#removing procurement sla based on p -value
ca_mul_model_10 <- lm(TotalGMV ~ units + listprice + TV, data = train)
summary(ca_mul_model_10)
vif(ca_mul_model_10)


# predicting the results in test dataset
Predict_1 <- predict(ca_mul_model_10,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.99

# Cross validation
crooss_val <- cv.lm(data = CameraAccessory_mrgd_weekly, form.lm = ca_mul_model_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#3.51e-05


##############################################################################################################
# Elasticity Analysis

mdl_ea <-ca_mul_model_10

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multiplicative Model") +xlab("Variables")


###############################################################################################


###################################### Multiplicative+Distributed Lag Modelling ###############################################

HomeAudio_mrgd_weekly_dlag <- HomeAudio_mrgd_weekly
GamingAccessory_mrgd_weekly_dlag <- GamingAccessory_mrgd_weekly
CameraAccessory_mrgd_weekly_dlag <- CameraAccessory_mrgd_weekly

################### Home Audio Accessory

View(HomeAudio_mrgd_weekly_dlag)
dlag_model_1 <- slide(HomeAudio_mrgd_weekly_dlag, Var = "TotalGMV",slideBy = -1)
View(dlag_model_1)
dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -2)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -3)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -4)


dlag_model <- na.omit(dlag_model_1)

dlag_model <- scale(dlag_model)
dlag_model <- data.frame(dlag_model)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(dlag_model)

trainindices= sample(1:nrow(dlag_model), 0.7*nrow(dlag_model))
# generate the train data set
train = dlag_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =dlag_model[-trainindices,]

# Build model 1 containing all variables
ha_dlag_model_1 <-lm(TotalGMV~.,data=train)
summary(ha_dlag_model_1)

step <- stepAIC(ha_dlag_model_1, direction="both")

# All other variables except the below need to be considered

ha_dlag_model_2 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                        Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_2)
vif(ha_dlag_model_2)

#removing sponsorship based on very high vif
ha_dlag_model_3 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TotalInvestment + TV + Digital + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                        Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_3)
vif(ha_dlag_model_3)

#removing SEM based on high vif
ha_dlag_model_4 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TotalInvestment + TV + Digital + 
                        ContentMarketing + OnlineMarketing + Affiliates + Radio + 
                        Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_4)
vif(ha_dlag_model_4)

#removing Online Marketing based on high vif
ha_dlag_model_4 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TotalInvestment + TV + Digital + 
                        ContentMarketing + OnlineMarketing + Affiliates + Radio + 
                        Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_4)
vif(ha_dlag_model_4)

#removing online marketing based on high vif
ha_dlag_model_5 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TotalInvestment + TV + Digital + 
                        ContentMarketing + Affiliates + Radio + 
                        Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_5)
vif(ha_dlag_model_5)

#removing content marketing based on high vif
ha_dlag_model_6 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TotalInvestment + TV + Digital + 
                        Affiliates + Radio + 
                        Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_6)
vif(ha_dlag_model_6)

#removing radio based on high vif
ha_dlag_model_7 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TotalInvestment + TV + Digital + 
                        Affiliates + Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_7)
vif(ha_dlag_model_7)

#removing Total Investment based on high vif 
ha_dlag_model_8 <- lm(TotalGMV ~ week_no + product_mrp + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TV + Digital + 
                        Affiliates + Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_8)
vif(ha_dlag_model_8)

#removing product_mrp bassed on high vif
ha_dlag_model_9 <- lm(TotalGMV ~ week_no + units + discount + listprice + 
                        sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                        Noofsaledays + TV + Digital + 
                        Affiliates + Other + NPS + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_9)
vif(ha_dlag_model_9)

#all the variables are having low vif value
#now removing NPS based on high p-value
ha_dlag_model_10 <- lm(TotalGMV ~ week_no + units + discount + listprice + 
                         sla + procurement_sla + Prepaid + shelfinflation + mvg_avg + 
                         Noofsaledays + TV + Digital + 
                         Affiliates + Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_10)
vif(ha_dlag_model_10)

#removing mvg_avg based on high p-value
ha_dlag_model_11 <- lm(TotalGMV ~ week_no + units + discount + listprice + 
                         sla + procurement_sla + Prepaid + shelfinflation + 
                         Noofsaledays + TV + Digital + 
                         Affiliates + Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_11)
vif(ha_dlag_model_11)

#removing week-no 
ha_dlag_model_12 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + Prepaid + shelfinflation + 
                         Noofsaledays + TV + Digital + 
                         Affiliates + Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

summary(ha_dlag_model_12)
vif(ha_dlag_model_12)

#removing TotalGMV.3 
ha_dlag_model_13 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + Prepaid + shelfinflation + 
                         Noofsaledays + TV + Digital + 
                         Affiliates + Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_model_13)
vif(ha_dlag_model_13)

#removing Prepaid based on high p- value 
ha_dlag_model_14 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + shelfinflation + 
                         Noofsaledays + TV + Digital + 
                         Affiliates + Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_model_14)
vif(ha_dlag_model_14)

#removing TV based on high p- value 
ha_dlag_model_15 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + shelfinflation + 
                         Noofsaledays + Digital + 
                         Affiliates + Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_model_15)
vif(ha_dlag_model_15)

#removing Affiliates based on high p- value 
ha_dlag_model_16 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + shelfinflation + 
                         Noofsaledays + Digital + 
                         Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_model_16)
vif(ha_dlag_model_16)

#removing Noofsaledays based on high p- value 
ha_dlag_model_17 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + shelfinflation + 
                         Digital + Other + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_model_17)
vif(ha_dlag_model_17)

#removing Other based on high p- value 
ha_dlag_model_18 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + shelfinflation + 
                         Digital + TotalGMV.1 + TotalGMV.2 + TotalGMV.4, data = train)

summary(ha_dlag_model_18)
vif(ha_dlag_model_18)

#removing TotalGMV.4 based on high p- value 
ha_dlag_model_19 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + shelfinflation + 
                         Digital + TotalGMV.1 + TotalGMV.2, data = train)

summary(ha_dlag_model_19)
vif(ha_dlag_model_19)

#removing TotalGMV.2 based on high p- value 
ha_dlag_model_20 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + procurement_sla + shelfinflation + 
                         Digital + TotalGMV.1, data = train)

summary(ha_dlag_model_20)
vif(ha_dlag_model_20)

#removing procurement_sla based on high p- value 
ha_dlag_model_21 <- lm(TotalGMV ~ units + discount + listprice + 
                         sla + shelfinflation + 
                         Digital + TotalGMV.1, data = train)

summary(ha_dlag_model_21)
vif(ha_dlag_model_21)

#removing sla based on high p- value 
ha_dlag_model_22 <- lm(TotalGMV ~ units + discount + listprice + 
                         shelfinflation + Digital + TotalGMV.1, data = train)

summary(ha_dlag_model_22)
vif(ha_dlag_model_22)

#removing discount based on high p- value 
ha_dlag_model_23 <- lm(TotalGMV ~ units + listprice + 
                         shelfinflation + Digital + TotalGMV.1, data = train)

summary(ha_dlag_model_23)
vif(ha_dlag_model_23)

#removing shelfinflation based on high p- value 
ha_dlag_model_24 <- lm(TotalGMV ~ units + listprice + Digital + TotalGMV.1, data = train)

summary(ha_dlag_model_24)
vif(ha_dlag_model_24)

#removing TotalGMV.1 based on high p- value 
ha_dlag_model_25 <- lm(TotalGMV ~ units + listprice + Digital, data = train)

summary(ha_dlag_model_25)
vif(ha_dlag_model_25)

#removing Digital based on high p- value 
ha_dlag_model_26 <- lm(TotalGMV ~ units + listprice, data = train)

summary(ha_dlag_model_26)
vif(ha_dlag_model_26)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.004659   0.012830   0.363    0.719    
# units       0.989966   0.012347  80.182  < 2e-16 ***
#   listprice   0.140250   0.014010  10.011 6.44e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.07226 on 29 degrees of freedom
# Multiple R-squared:  0.9959,	Adjusted R-squared:  0.9956 
# F-statistic:  3517 on 2 and 29 DF,  p-value: < 2.2e-16

# predicting the results in test dataset
Predict_1 <- predict(ha_dlag_model_26,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.9917


# Cross validation
crooss_val <- cv.lm(data = dlag_model, form.lm = ha_dlag_model_26, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#6.62e-05
##############################################################################################################
# Elasticity Analysis

mdl_ea <-ha_dlag_model_26

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multiplicative+Distributed Lag Model") +xlab("Variables")


###############################################################################################


################### Camera Accessory

View(CameraAccessory_mrgd_weekly_dlag)
dlag_model_1 <- slide(CameraAccessory_mrgd_weekly_dlag, Var = "TotalGMV",slideBy = -1)
View(dlag_model_1)
dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -2)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -3)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -4)


dlag_model <- na.omit(dlag_model_1)

dlag_model <- scale(dlag_model)
dlag_model <- data.frame(dlag_model)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(dlag_model)

trainindices= sample(1:nrow(dlag_model), 0.7*nrow(dlag_model))
# generate the train data set
train = dlag_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =dlag_model[-trainindices,]

# Build model 1 containing all variables
ca_dlag_model_1 <-lm(TotalGMV~.,data=train)
summary(ca_dlag_model_1)

step <- stepAIC(ca_dlag_model_1, direction="both")

# All other variables except the below need to be considered

ca_dlag_model_2 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + TotalInvestment + TV + Digital + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                        Other + NPS, data = train)

summary(ca_dlag_model_2)
vif(ca_dlag_model_2)

# removing based on TotalInvestment
ca_dlag_model_3 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + TV + Digital + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                        Other + NPS, data = train)

summary(ca_dlag_model_3)
vif(ca_dlag_model_3)

# removing based on Other
ca_dlag_model_4 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + TV + Digital + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + NPS, data = train)

summary(ca_dlag_model_4)
vif(ca_dlag_model_4)


# removing Digital based on vif
ca_dlag_model_5 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + TV + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + NPS, data = train)

summary(ca_dlag_model_5)
vif(ca_dlag_model_5)

# removing Affiliates based on vif
ca_dlag_model_6 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + TV + Sponsorship + 
                        ContentMarketing + OnlineMarketing + SEM + Radio + NPS, data = train)

summary(ca_dlag_model_6)
vif(ca_dlag_model_6)

# removing ContentMarketing based on vif
ca_dlag_model_7 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + TV + Sponsorship + OnlineMarketing + SEM + Radio + NPS, data = train)

summary(ca_dlag_model_7)
vif(ca_dlag_model_7)

# removing TV based on high p-value
ca_dlag_model_8 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + Sponsorship + OnlineMarketing + SEM + Radio + NPS, data = train)

summary(ca_dlag_model_8)
vif(ca_dlag_model_8)

# removing SEM based on high p-value
ca_dlag_model_9 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                        Noofsaledays + Sponsorship + OnlineMarketing + Radio + NPS, data = train)

summary(ca_dlag_model_9)
vif(ca_dlag_model_9)

# removing OnlineMarketing based on high p-value
ca_dlag_model_10 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                         Noofsaledays + Sponsorship + Radio + NPS, data = train)

summary(ca_dlag_model_10)
vif(ca_dlag_model_10)

# removing Radio based on high p-value
ca_dlag_model_11 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                         Noofsaledays + Sponsorship + NPS, data = train)

summary(ca_dlag_model_11)
vif(ca_dlag_model_11)

# removing Noofsaledays based on high p-value
ca_dlag_model_12 <- lm(TotalGMV ~ units + discount + listprice + sla + COD + shelfinflation + 
                         Sponsorship + NPS, data = train)

summary(ca_dlag_model_12)
vif(ca_dlag_model_12)

#removing COD based on high p-value
ca_dlag_model_13 <- lm(TotalGMV ~ units + discount + listprice + sla + shelfinflation + 
                         Sponsorship + NPS, data = train)

summary(ca_dlag_model_13)
vif(ca_dlag_model_13)

#removing shelfinflation based on high p-value
ca_dlag_model_14 <- lm(TotalGMV ~ units + discount + listprice + sla + 
                         Sponsorship + NPS, data = train)

summary(ca_dlag_model_14)
vif(ca_dlag_model_14)

#removing Sponsorship based on high p-value
ca_dlag_model_15 <- lm(TotalGMV ~ units + discount + listprice + sla + NPS, data = train)

summary(ca_dlag_model_15)
vif(ca_dlag_model_15)

#removing NPS based on high p-value
ca_dlag_model_16 <- lm(TotalGMV ~ units + discount + listprice + sla, data = train)

summary(ca_dlag_model_16)
vif(ca_dlag_model_16)

#removing sla based on high p-value
ca_dlag_model_17 <- lm(TotalGMV ~ units + discount + listprice, data = train)

summary(ca_dlag_model_17)
vif(ca_dlag_model_17)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.001096   0.033895   0.032    0.974    
# units       0.949935   0.037187  25.545  < 2e-16 ***
#   discount    0.272499   0.054420   5.007 2.49e-05 ***
#   listprice   0.535874   0.060209   8.900 8.66e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.194 on 29 degrees of freedom
# Multiple R-squared:  0.9622,	Adjusted R-squared:  0.9583 
# F-statistic:   246 on 3 and 29 DF,  p-value: < 2.2e-16

# predicting the results in test dataset
Predict_1 <- predict(ca_dlag_model_17,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.94

# Cross validation
crooss_val <- cv.lm(data = dlag_model, form.lm = ca_dlag_model_17, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#1.29e-05
##############################################################################################################
# Elasticity Analysis

mdl_ea <-ca_dlag_model_17

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative+Distributed Lag Model") +xlab("Variables")


###############################################################################################


################### Gaming Accessory

View(GamingAccessory_mrgd_weekly_dlag)
dlag_model_1 <- slide(GamingAccessory_mrgd_weekly_dlag, Var = "TotalGMV",slideBy = -1)
View(dlag_model_1)
dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -2)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -3)

dlag_model_1 <- slide(dlag_model_1, Var = "TotalGMV",slideBy = -4)


dlag_model <- na.omit(dlag_model_1)

dlag_model <- scale(dlag_model)
dlag_model <- data.frame(dlag_model)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
View(dlag_model)

trainindices= sample(1:nrow(dlag_model), 0.7*nrow(dlag_model))
# generate the train data set
train = dlag_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =dlag_model[-trainindices,]

# Build model 1 containing all variables
ga_dlag_model_1 <-lm(TotalGMV~.,data=train)
summary(ga_dlag_model_1)

step <- stepAIC(ga_dlag_model_1, direction="both")

ga_dlag_model_2 <-lm(TotalGMV ~ week_no + discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                       Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                       NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)

#removing week+no
ga_dlag_model_3 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TotalInvestment + TV + Digital + 
                       Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                       NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)
summary(ga_dlag_model_3)
vif(ga_dlag_model_3)

#removing Total Invetsment based on high vif
ga_dlag_model_4 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TV + Digital + 
                       Sponsorship + ContentMarketing + Affiliates + Radio + Other + 
                       NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)
summary(ga_dlag_model_4)
vif(ga_dlag_model_4)


#removing Content Marketing based on high vif
ga_dlag_model_5 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TV + Digital + 
                       Sponsorship + Affiliates + Radio + Other + 
                       NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)
summary(ga_dlag_model_5)
vif(ga_dlag_model_5)

#removing Other based on high vif
ga_dlag_model_6 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TV + Digital + 
                       Sponsorship + Affiliates + Radio + 
                       NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)
summary(ga_dlag_model_6)
vif(ga_dlag_model_6)

#removing Affiliates based on high p-vaalue
ga_dlag_model_7 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TV + Digital + 
                       Sponsorship + Radio + 
                       NPS + TotalGMV.2 + TotalGMV.3 + TotalGMV.4, data = train)
summary(ga_dlag_model_7)
vif(ga_dlag_model_7)

#removing TotalGmc.3 based on high p-vaalue
ga_dlag_model_8 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TV + Digital + 
                       Sponsorship + Radio + 
                       NPS + TotalGMV.2 + TotalGMV.4, data = train)
summary(ga_dlag_model_8)
vif(ga_dlag_model_8)

#removing TotalGmv.2 based on high p-vaalue
ga_dlag_model_9 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                       COD + Prepaid + shelfinflation + TV + Digital + 
                       Sponsorship + Radio + NPS + TotalGMV.4, data = train)
summary(ga_dlag_model_9)
vif(ga_dlag_model_9)

#removing ShelfInflation based on high p-vaalue
ga_dlag_model_10 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                        COD + Prepaid + TV + Digital + 
                        Sponsorship + Radio + NPS + TotalGMV.4, data = train)
summary(ga_dlag_model_10)
vif(ga_dlag_model_10)

#removing TV based on high p-vaalue
ga_dlag_model_11 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                        COD + Prepaid + Digital + 
                        Sponsorship + Radio + NPS + TotalGMV.4, data = train)
summary(ga_dlag_model_11)
vif(ga_dlag_model_11)

#removing Sponsorship based on high p-vaalue
ga_dlag_model_12 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                        COD + Prepaid + Digital + Radio + NPS + TotalGMV.4, data = train)
summary(ga_dlag_model_12)
vif(ga_dlag_model_12)

#removing Radio based on high p-vaalue
ga_dlag_model_13 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                        COD + Prepaid + Digital + NPS + TotalGMV.4, data = train)
summary(ga_dlag_model_13)
vif(ga_dlag_model_13)

#removing TotalGMV.4 based on high p-vaalue
ga_dlag_model_14 <-lm(TotalGMV ~ discount + listprice + procurement_sla + 
                        COD + Prepaid + Digital + NPS, data = train)
summary(ga_dlag_model_14)
vif(ga_dlag_model_14)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -0.03885    0.01166  -3.330   0.0026 ** 
#   discount        -0.07797    0.01521  -5.126 2.41e-05 ***
#   listprice        0.32939    0.02194  15.015 2.52e-14 ***
#   procurement_sla -0.11111    0.01285  -8.649 3.98e-09 ***
#   COD              0.92115    0.01806  51.016  < 2e-16 ***
#   Prepaid          0.50689    0.01314  38.590  < 2e-16 ***
#   Digital         -0.07582    0.01171  -6.473 7.34e-07 ***
#   NPS             -0.11445    0.01775  -6.448 7.83e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06591 on 26 degrees of freedom
# Multiple R-squared:  0.9964,	Adjusted R-squared:  0.9954 
# F-statistic:  1028 on 7 and 26 DF,  p-value: < 2.2e-16

# predicting the results in test dataset
Predict_1 <- predict(ga_dlag_model_14,test[,-1])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.98


# Cross validation
crooss_val <- cv.lm(data = dlag_model, form.lm = ga_dlag_model_14, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#0.592
##############################################################################################################
# Elasticity Analysis

mdl_ea <-ga_dlag_model_14

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory- Multiplicative+Distributed Lag Model") +xlab("Variables")


###############################################################################################

#---------------------------------------Koyck model-------------------------------------------------

# Please check for gaming accessory, TotalInvestment,TV,Digital,Sponsorship,OnlineMarketing,Affiliates, SEM are correlated. 
# if i am removing any of them, all the other var are becoming insignificant. 
# plz check it and tell me where am i going wrong.
# Same problem is coming for camera acc as well as home audio

HomeAudio_mrgd_weekly_kyk <- HomeAudio_mrgd_weekly
GamingAccessory_mrgd_weekly_kyk <- GamingAccessory_mrgd_weekly
CameraAccessory_mrgd_weekly_kyk <- CameraAccessory_mrgd_weekly

Gaming_kyk <- slide(GamingAccessory_mrgd_weekly_kyk, Var = "TotalGMV",slideBy = -1)
Gaming_kyk <- na.omit(Gaming_kyk)
Gaming_kyk <- scale(Gaming_kyk)
Gaming_kyk <- data.frame(Gaming_kyk)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset

trainindices= sample(1:nrow(Gaming_kyk), 0.7*nrow(Gaming_kyk))
# generate the train data set
train = Gaming_kyk[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =Gaming_kyk[-trainindices,]

# Build model 1 containing all variables
ga_kyk_model_1 <-lm(TotalGMV~.,data=train)    
summary(ga_kyk_model_1)

step <- stepAIC(ga_kyk_model_1, direction="both")  
ga_kyk_model_2 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + COD + Prepaid + 
                      shelfinflation + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                      Other + NPS + TotalGMV.1,data=train)  
summary(ga_kyk_model_2)
vif(ga_kyk_model_2)


#removing total investment based on high vif value
ga_kyk_model_3 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + COD + Prepaid + 
                      shelfinflation + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                      Other + NPS + TotalGMV.1,data=train)  
summary(ga_kyk_model_3)
vif(ga_kyk_model_3)

#removing Other based on high vif value
ga_kyk_model_4 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + COD + Prepaid + 
                      shelfinflation + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                      NPS + TotalGMV.1,data=train)  
summary(ga_kyk_model_4)
vif(ga_kyk_model_4)

#removing COD based on high vif value
ga_kyk_model_5 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + Prepaid + 
                      shelfinflation + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Radio + 
                      NPS + TotalGMV.1,data=train)  
summary(ga_kyk_model_5)
vif(ga_kyk_model_5)

#removing Online Marketing based on high vif value
ga_kyk_model_6 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + Prepaid + 
                      shelfinflation + TV + Digital + Sponsorship + 
                      ContentMarketing + Affiliates + SEM + Radio + 
                      NPS + TotalGMV.1,data=train)  
summary(ga_kyk_model_6)
vif(ga_kyk_model_6)

#removing Digital Marketing based on high vif value
ga_kyk_model_7 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + Prepaid + 
                      shelfinflation + TV + Sponsorship + 
                      ContentMarketing + Affiliates + SEM + Radio + 
                      NPS + TotalGMV.1,data=train)  
summary(ga_kyk_model_7)
vif(ga_kyk_model_7)

#removing Content Marketing based on high vif value
ga_kyk_model_8 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + Prepaid + 
                      shelfinflation + TV + Sponsorship + 
                      Affiliates + SEM + Radio + NPS + TotalGMV.1,data=train)  
summary(ga_kyk_model_8)
vif(ga_kyk_model_8)

#aall vif are low now
#removing TotalGMV.1 based on high p-value
ga_kyk_model_9 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + Prepaid + 
                      shelfinflation + TV + Sponsorship + 
                      Affiliates + SEM + Radio + NPS,data=train)  
summary(ga_kyk_model_9)
vif(ga_kyk_model_9)

#removing affiliates based on high p-value
ga_kyk_model_10 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + Prepaid + 
                      shelfinflation + TV + Sponsorship + SEM + Radio + NPS,data=train)  
summary(ga_kyk_model_10)
vif(ga_kyk_model_10)

#removing NPS based on high p-value
ga_kyk_model_11 <-lm(TotalGMV ~ product_mrp + units + listprice + sla + Prepaid + 
                       shelfinflation + TV + Sponsorship + SEM + Radio,data=train)  
summary(ga_kyk_model_11)
vif(ga_kyk_model_11)

#removing product_mrp based on high p-value
ga_kyk_model_12 <-lm(TotalGMV ~ units + listprice + sla + Prepaid + 
                       shelfinflation + TV + Sponsorship + SEM + Radio,data=train)  
summary(ga_kyk_model_12)
vif(ga_kyk_model_12)

#removing shelf_inflation based on high p-value
ga_kyk_model_13 <-lm(TotalGMV ~ units + listprice + sla + Prepaid + 
                       TV + Sponsorship + SEM + Radio,data=train)  
summary(ga_kyk_model_13)
vif(ga_kyk_model_13)

#removing sla based on high p-value
ga_kyk_model_14 <-lm(TotalGMV ~ units + listprice + Prepaid + 
                       TV + Sponsorship + SEM + Radio,data=train)  
summary(ga_kyk_model_14)
vif(ga_kyk_model_14)

# predicting the results in test dataset
Predict_1 <- predict(ga_kyk_model_14,test[,-2])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.97

# Cross validation
crooss_val <- cv.lm(data = Gaming_kyk, form.lm = ga_kyk_model_14, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#0.0654

##############################################################################################################
# Elasticity Analysis

mdl_ea <- ga_kyk_model_14

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Koyck Model") +xlab("Variables")

####################################################################################


######### Home Audio #########
HomeAudio_kyk <- slide(HomeAudio_mrgd_weekly_kyk, Var = "TotalGMV",slideBy = -1)
HomeAudio_kyk <- na.omit(HomeAudio_kyk)
HomeAudio_kyk <- scale(HomeAudio_kyk)
HomeAudio_kyk <- data.frame(HomeAudio_kyk)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset

trainindices= sample(1:nrow(HomeAudio_kyk), 0.7*nrow(HomeAudio_kyk))
# generate the train data set
train = HomeAudio_kyk[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =HomeAudio_kyk[-trainindices,]

# Build model 1 containing all variables
ha_kyk_model_1 <-lm(TotalGMV~.,data=train)    
summary(ha_kyk_model_1)

step <- stepAIC(ha_kyk_model_1, direction="both")  
ha_kyk_model_2 <-lm(TotalGMV ~ week_no + units + discount + listprice + COD + Prepaid + 
                      mvg_avg + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Other + 
                      NPS,data=train)  

summary(ha_kyk_model_2)
vif(ha_kyk_model_2)

#removing Week_no
ha_kyk_model_3 <-lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + 
                      mvg_avg + TotalInvestment + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Other + 
                      NPS,data=train)  

summary(ha_kyk_model_3)
vif(ha_kyk_model_3)

#removing Total Investment based on high vif value
ha_kyk_model_4 <-lm(TotalGMV ~ units + discount + listprice + COD + Prepaid + 
                      mvg_avg + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Other + 
                      NPS,data=train)  

summary(ha_kyk_model_4)
vif(ha_kyk_model_4)

#removing COD based on high vif value
ha_kyk_model_5 <-lm(TotalGMV ~ units + discount + listprice + Prepaid + 
                      mvg_avg + TV + Digital + Sponsorship + 
                      ContentMarketing + OnlineMarketing + Affiliates + SEM + Other + 
                      NPS,data=train)  

summary(ha_kyk_model_5)
vif(ha_kyk_model_5)

#removing OnlineMarketing based on high vif value
ha_kyk_model_6 <-lm(TotalGMV ~ units + discount + listprice + Prepaid + 
                      mvg_avg + TV + Digital + Sponsorship + 
                      ContentMarketing + Affiliates + SEM + Other + 
                      NPS,data=train)  

summary(ha_kyk_model_6)
vif(ha_kyk_model_6)

#removing Digital based on high vif value
ha_kyk_model_7 <-lm(TotalGMV ~ units + discount + listprice + Prepaid + 
                      mvg_avg + TV + Sponsorship + 
                      ContentMarketing + Affiliates + SEM + Other + 
                      NPS,data=train)  

summary(ha_kyk_model_7)
vif(ha_kyk_model_7)

#removing ContentMarketing based on high vif value
ha_kyk_model_8 <-lm(TotalGMV ~ units + discount + listprice + Prepaid + 
                      mvg_avg + TV + Sponsorship + 
                      Affiliates + SEM + Other + NPS,data=train)  

summary(ha_kyk_model_8)
vif(ha_kyk_model_8)


#all vif values are low now
#removing Other based on high p-value
ha_kyk_model_9 <-lm(TotalGMV ~ units + discount + listprice + Prepaid + 
                      mvg_avg + TV + Sponsorship + Affiliates + SEM + NPS,data=train)  

summary(ha_kyk_model_9)
vif(ha_kyk_model_9)

#removing mvg_avg based on high p-value
ha_kyk_model_10 <-lm(TotalGMV ~ units + discount + listprice + Prepaid + 
                      TV + Sponsorship + Affiliates + SEM + NPS,data=train)  

summary(ha_kyk_model_10)
vif(ha_kyk_model_10)

#removing NPS based on high p-value
ha_kyk_model_11 <-lm(TotalGMV ~ units + discount + listprice + Prepaid + 
                       TV + Sponsorship + Affiliates + SEM,data=train)  

summary(ha_kyk_model_11)
vif(ha_kyk_model_11)

#removing Prepaid based on high p-value
ha_kyk_model_12 <-lm(TotalGMV ~ units + discount + listprice + 
                       TV + Sponsorship + Affiliates + SEM,data=train)  

summary(ha_kyk_model_12)
vif(ha_kyk_model_12)

#removing TV based on high p-value
ha_kyk_model_13 <-lm(TotalGMV ~ units + discount + listprice + 
                       Sponsorship + Affiliates + SEM,data=train)  

summary(ha_kyk_model_13)
vif(ha_kyk_model_13)

#removing Affiliates based on high p-value
ha_kyk_model_14 <-lm(TotalGMV ~ units + discount + listprice + Sponsorship + SEM,data=train)  

summary(ha_kyk_model_14)
vif(ha_kyk_model_14)

#removing discount based on high p-value
ha_kyk_model_15 <-lm(TotalGMV ~ units + listprice + Sponsorship + SEM,data=train)  

summary(ha_kyk_model_15)
vif(ha_kyk_model_15)

#removing sponsorship based on high p-value
ha_kyk_model_16 <-lm(TotalGMV ~ units + listprice + SEM,data=train)  

summary(ha_kyk_model_16)
vif(ha_kyk_model_16)

#removing SEM based on high p-value
ha_kyk_model_17 <-lm(TotalGMV ~ units + listprice,data=train)  

summary(ha_kyk_model_17)
vif(ha_kyk_model_17)


# predicting the results in test dataset
Predict_1 <- predict(ha_kyk_model_17,test[,-2])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.99

# Cross validation
crooss_val <- cv.lm(data = HomeAudio_kyk, form.lm = ha_kyk_model_17, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#0.00862
##############################################################################################################
# Elasticity Analysis

mdl_ea <- ha_kyk_model_17

# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")

####################################################################################

######### Camera Accessory #########
CamAccess_kyk <- slide(CameraAccessory_mrgd_weekly_kyk, Var = "TotalGMV",slideBy = -1)
CamAccess_kyk <- na.omit(CamAccess_kyk)
CamAccess_kyk <- scale(CamAccess_kyk)
CamAccess_kyk <- data.frame(CamAccess_kyk)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset

trainindices= sample(1:nrow(CamAccess_kyk), 0.7*nrow(CamAccess_kyk))
# generate the train data set
train = CamAccess_kyk[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =CamAccess_kyk[-trainindices,]

# Build model 1 containing all variables
ca_kyk_model_1 <-lm(TotalGMV~.,data=train)    
summary(ha_kyk_model_1)

step <- stepAIC(ca_kyk_model_1, direction="both")  
ca_kyk_model_2 <-lm(TotalGMV ~ week_no + units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TotalInvestment + TV + 
                      Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                      Affiliates + SEM + Radio + Other + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_2)
vif(ca_kyk_model_2)

#removing week_no based on high vif value
ca_kyk_model_3 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TV + 
                      Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                      Affiliates + SEM + Radio + Other + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_3)
vif(ca_kyk_model_3)

#removing Other based on high vif value
ca_kyk_model_4 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TV + 
                      Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                      Affiliates + SEM + Radio + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_4)
vif(ca_kyk_model_4)

#removing Affiliates on high vif value
ca_kyk_model_5 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TV + 
                      Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                      SEM + Radio + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_5)
vif(ca_kyk_model_5)

#removing Digital on high vif value
ca_kyk_model_6 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TV + 
                      Sponsorship + ContentMarketing + OnlineMarketing + 
                      SEM + Radio + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_6)
vif(ca_kyk_model_6)

#removing ContentMarketing on high vif value
ca_kyk_model_7 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TV + 
                      Sponsorship + OnlineMarketing + 
                      SEM + Radio + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_7)
vif(ca_kyk_model_7)

#all vif values are low
#removing SEM on high p-value
ca_kyk_model_8 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TV + 
                      Sponsorship + OnlineMarketing + Radio + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_8)
vif(ca_kyk_model_8)

#removing OnlineMarketing on high p-value
ca_kyk_model_9 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      COD + shelfinflation + Noofsaledays + TV + 
                      Sponsorship + Radio + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_9)
vif(ca_kyk_model_9)

#removing COD on high p-value
ca_kyk_model_10 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                      shelfinflation + Noofsaledays + TV + 
                      Sponsorship + Radio + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_10)
vif(ca_kyk_model_10)

#removing Radio on high p-value
ca_kyk_model_11 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                       shelfinflation + Noofsaledays + TV + 
                       Sponsorship + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_11)
vif(ca_kyk_model_11)

#removing TV on high p-value
ca_kyk_model_12 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                       shelfinflation + Noofsaledays + Sponsorship + NPS + TotalGMV.1,data=train)  

summary(ca_kyk_model_12)
vif(ca_kyk_model_12)

#removing TotalGMV.1n on high p-value
ca_kyk_model_13 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                       shelfinflation + Noofsaledays + Sponsorship + NPS ,data=train)  

summary(ca_kyk_model_13)
vif(ca_kyk_model_13)

#removing shelfinflation on high p-value
ca_kyk_model_14 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + 
                       Noofsaledays + Sponsorship + NPS ,data=train)  

summary(ca_kyk_model_14)
vif(ca_kyk_model_14)

#removing Noofsaledays on high p-value
ca_kyk_model_15 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + Sponsorship + NPS ,data=train)  

summary(ca_kyk_model_15)
vif(ca_kyk_model_15)

#removing Sponsorship on high p-value
ca_kyk_model_16 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + Sponsorship + NPS ,data=train)  

summary(ca_kyk_model_16)
vif(ca_kyk_model_16)

#removing NPS on high p-value
ca_kyk_model_17 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla + Sponsorship,data=train)  

summary(ca_kyk_model_17)
vif(ca_kyk_model_17)

#removing Sponsorship on high p-value
ca_kyk_model_18 <-lm(TotalGMV ~ units + discount + listprice + procurement_sla,data=train)  

summary(ca_kyk_model_18)
vif(ca_kyk_model_18)

#removing procurement_sla on high p-value
ca_kyk_model_19 <-lm(TotalGMV ~ units + discount + listprice,data=train)  

summary(ca_kyk_model_19)
vif(ca_kyk_model_19)

#removing discount on high p-value
ca_kyk_model_19 <-lm(TotalGMV ~ units + listprice,data=train)  

summary(ca_kyk_model_19)
vif(ca_kyk_model_19)


# predicting the results in test dataset
Predict_1 <- predict(ca_kyk_model_19,test[,-2])
test$test_GMV <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$TotalGMV,test$test_GMV)
rsquared <- cor(test$TotalGMV,test$test_GMV)^2
View(rsquared)
#0.98

# Cross validation
crooss_val <- cv.lm(data = CamAccess_kyk, form.lm = ca_kyk_model_19, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
attr(crooss_val,'ms')
#0.0926

##############################################################################################################
# Elasticity Analysis

mdl_ea <- ca_kyk_model_19


# estimating the elasticity coefficients

elasticity <- function(var){
  
  temp_elast <-as.numeric(mdl_ea$coefficients[var]*mean(train[,var])/mean(train$TotalGMV))
  
  return(temp_elast)
} 

var_list <- list()

for(i in 2:length(mdl_ea$coefficients)){
  
  var_list[i-1] <-elasticity(names(mdl_ea$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(mdl_ea$coefficients[2:length(mdl_ea$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Koyck Model") +xlab("Variables")

#################################################################################################