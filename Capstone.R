
# ***************************************************************************
#             MARKET MIX  MODELLING
#             
#       PGDDA ( IIIT Bangalore )
#       April 2017
#       AtchiReddy (atchireddi@gmail.com)
#       Armando    ()
#       Santosh Francis ()
#       Anandh Rati ()
#             
# ***************************************************************************


# Objective :

# ElecKart is an e-commerce firm specialising in electronic products. Over the 
# last one year, they had spent a significant amount of money in marketing. 
# Occasionally, they had also offered big-ticket promotions (similar to the Big 
# Billion Day). They are about to create a marketing budget for the next year 
# which includes spending on commercials, online campaigns, and pricing & promotion 
# strategies. The CFO feels that the money spent over last 12 months on marketing 
# was not sufficiently impactful and that they can either cut on the budget or 
# reallocate it  optimally across marketing levers to improve the revenue response




# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)



# ***************************************************************************
#                   PROCs ----
# ***************************************************************************
nweek <- function(x, format="%Y-%m-%d", origin){
  if(missing(origin)){
    as.integer(format(strptime(x, format=format), "%W"))
  }else{
    x <- as.Date(x, format=format)
    o <- as.Date(origin, format=format)
    w <- as.integer(format(strptime(x, format=format), "%w"))
    2 + as.integer(x - o - w) %/% 7
  }
}



# ***************************************************************************
#                   LOAD DATA ---- Transaction Data ----
# ***************************************************************************
# Make sure you are in current directory as in R-file is in. Should I do a commit?yes..

ce_data <- read.csv('./input/ConsumerElectronics.csv',stringsAsFactors = FALSE)

str(ce_data)



# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************

head(ce_data)

# . . . .   Outlier Treatment ----
# Remove orders before July'15 and after June'16
ce_data$order_date <- format(as.POSIXct(ce_data$order_date,format='%Y-%m-%d'),
                             format='%Y-%m-%d')
ce_data$order_date <- as.Date(ce_data$order_date, format = "%Y-%m-%d")

ce_data <- subset(ce_data, order_date > "2015-6-30" & order_date < "2016-7-1")


# . . . .   Missing Values ----

# Since 80% of the variable has NAs Omit 'deliverybday' & 'deliverycdays'
# Removed Pincode, as there seems to be some data quality issues with this variable
ce_data <- ce_data[,-c(9,10)]

summary(ce_data)

ce_data <- na.omit(ce_data)   # 4904 missing values, can be ignored


# . . . .   Correct Data Types ----

# 'order_id', 'order_item_id', 'cust_id', 'pincode' are qualitative data
#  having numeric values, let's convert them to character type

ce_data <- cbind(ce_data[,-c(5,6,11,12)],
                 sapply(ce_data[,c(5,6,11,12)],as.character) )   # operate on interested columns



# ***************************************************************************
#                   FEATURE ENGINEERING ----
# ***************************************************************************

# create week,  week numbers start from min 'order date'
# . . . . Week Numbers ----
dates <- as.Date(
  gsub(" .*","",ce_data$order_date)
)
ce_data$week <- nweek(dates,origin = as.Date("2015-07-01"))


# . . . . Days, weeks, Month ----
# will compute Month, week, and no.of days per week (month, week)
# 
dys <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=dys, Month = month(dys), 
                       week = nweek(dys,origin = as.Date("2015-07-01")),
                       nweek = rep(1,length(dys)))
weekdays <- data.frame(weekdays %>% group_by(Month,week) %>% summarize(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7


# . . . . Strip Spaces ----
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)


# . . . . Generate Discount ----
ce_data$discount <- ((ce_data$product_mrp - ce_data$gmv)/ce_data$product_mrp) * 100



# ***************************************************************************
#                   LOAD DATA ---- Media & Inv Data ----
# ***************************************************************************
# . . . .   ProductList ----
productList_data      <- 
  read.csv("ProductList.csv", stringsAsFactors = FALSE, 
           na.strings=c('\\N'))

# . . . .   Media Investment ----
mediaInvestment_data  <- 
  read.csv("MediaInvestment.csv", stringsAsFactors = FALSE)

# . . . .  Special Sale Event ----

specialSale_data      <- 
  read.csv("SpecialSale.csv", stringsAsFactors = FALSE)

# . . . .   Monthly NPS ----
monthlyNPS_data       <- 
  read.csv("MonthlyNPSscore.csv", stringsAsFactors = FALSE )



# ***************************************************************************
#                   DATA PREPARATION ----
# ***************************************************************************

# . . . .   ProductList ----
str(productList_data)
productList_data <- na.omit(productList_data)

# . . . . . . . .  Correct Data types ----
productList_data$Frequency <- as.integer(productList_data$Frequency)
summary(productList_data)

# . . . .   Media Investment ----
str(mediaInvestment_data)
summary(mediaInvestment_data)

# . . . . . . . .  Missing Values ----
mediaInvestment_data[is.na(mediaInvestment_data)] <- 0   # zero investment

# . . . . . . . .  Convert to weekly data ----
# convert montly spend to weekly
mediaInvestment_data <- cbind(Month=mediaInvestment_data[,c(2)],
                              mediaInvestment_data[,-c(1,2)]/4.30)
# Add weekly information
mediaInvestment_weekly <- merge(weekdays,mediaInvestment_data, by='Month', all.x = TRUE)

# Convert media Investment at weekly granularity
# pro-rate weekly investment as per the ratio of its days span over adjacent months
mediaInvestment_weekly <- data.frame(mediaInvestment_weekly %>% group_by(week) %>% 
                                              summarize(TotalInvestment = sum(Total.Investment*fracDays),
                                              TV = sum(TV*fracDays), 
                                              Digital=sum(Digital*fracDays),
                                              Sponsorship = sum(Sponsorship*fracDays), 
                                              ContentMarketing = sum(Content.Marketing*fracDays),
                                              OnlineMarketing = sum(Online.marketing*fracDays), 
                                              Affiliates = sum(Affiliates*fracDays),
                                              SEM = sum(SEM*fracDays), 
                                              Radio = sum(Radio*fracDays), 
                                              Other = sum(Other*fracDays))
                            )

# . . . .   SPecialSale ----
str(specialSale_data)
specialSale_data$Date     <- as.Date(specialSale_data$Date, format = "%m/%d/%Y")
specialSale_data$week     <- nweek(specialSale_data$Date,origin = as.Date("2015-07-01"))

summary(specialSale_data)
unique(specialSale_data$week)
#Subsetting unique holiday weeks
specialSale_data            <- specialSale_data[!duplicated(specialSale_data$week),]      
specialSale_data$Sales.Name <- gsub(" +","",specialSale_data$Sales.Name)    # remove spaces

# . . . .   Monthly NPS ----
str(monthlyNPS_data)
monthlyNPS_data$Date <- as.Date(monthlyNPS_data$Date, format = "%m/%d/%Y")
monthlyNPS_data$Month <- month(ymd(monthlyNPS_data$Date))
monthlyNPS_weekly   <- merge(weekdays, monthlyNPS_data, by='Month', all.x = TRUE)
# Average weekly NPS for the weeks span over adjacent months
monthlyNPS_weekly   <- data.frame(monthlyNPS_weekly %>% group_by(week) %>%
                                              summarize(NPS = mean(NPS)))


# ***************************************************************************
#                   WEEKLY DATA AGGREGATION ----
# ***************************************************************************
ce_data_weekly <-  ce_data %>% 
  group_by(product_analytic_category,
           product_analytic_sub_category,
           product_analytic_vertical,
           week) %>% 
  summarize(gmv=sum(gmv), 
            product_mrp=mean(product_mrp), 
            units=sum(units),
            discount=mean(discount),
            sla=mean(sla), 
            procurement_sla=mean(product_procurement_sla))

ce_data_weekly <- as.data.frame(ce_data_weekly)   # type cast to data.frame


# ***************************************************************************
#                   MERGING DATA ----
# ***************************************************************************

# . . . .   Merge MediaInvestment & NPS ----
media_nps <- merge(mediaInvestment_weekly, monthlyNPS_weekly, by = 'week', all.x = TRUE)


# . . . .   Merge Sales & SaleDays
data <- merge(ce_data_weekly, specialSale_data[,-1], by = 'week', all.x = TRUE)
data$Sales.Name[is.na(data$Sales.Name)] <- "No sale"


#. . . .   Merge Data & Media_NPS
data <- merge(data, media_nps, by = 'week', all.x = TRUE)


# Discount on Products
data$discount_mrp <- as.integer(data$gmv/data$units)
data$discount     <- (1-(data$discount_gmv/data$product_mrp))*100


# ***************************************************************************
#           CREATE A NEW DATASET WITH ONLY THE IMP VARIABLES ----
# ***************************************************************************

write.csv(data, file = "eleckart.csv",row.names=FALSE)

camera_accessory_data <- subset(data, product_analytic_sub_category=="CameraAccessory")
home_audio_data       <- subset(data, product_analytic_sub_category=="HomeAudio")
gaming_accessory_data <- subset(data, product_analytic_sub_category=="GamingAccessory")



# ***************************************************************************
#                        LINEAR MODEL : Camera_accessory ----
# ***************************************************************************

indices=sample(1:nrow(eleckart),0.7*nrow(eleckart))
train=camera_accessory_data[indices,]
test=camera_accessory_data[-indices,]

# Modelling the Advertising Effects
# model_1 <- lm(gmv~ .,data=eleckart)
# summary(model_1)
# vif(model_1)