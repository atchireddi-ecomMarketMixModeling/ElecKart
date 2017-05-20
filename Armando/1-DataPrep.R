# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(Hmisc)   # describe

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

ce_data <- read.csv('./data/ConsumerElectronics.csv',stringsAsFactors = FALSE)

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

max(ce_data$product_mrp)

#NA Values
sapply(ce_data, function(x) sum(is.na(x)))
#Removed NA values from GMV
ce_data <- na.omit(ce_data)
ce_data <- subset(ce_data, product_mrp != 0)
warning()
# Lets add a couple of variables to the CE data. List Price from GMV and Promotion which is
# the discount offered

#....List Price variable
ce_data$List_Price <- as.integer(ce_data$gmv / ce_data$units)

#....Promotion Variable
ce_data$Promotion <- as.numeric((ce_data$product_mrp - ce_data$List_Price) / ce_data$product_mrp)

#....Here we have created a Pricing categorical variable
ce_data$mrp_category[ce_data$product_mrp == 0] <- "Free"
ce_data$mrp_category[ce_data$product_mrp >= 150001] <- "Luxury"
ce_data$mrp_category[ce_data$product_mrp >= 80001 & ce_data$product_mrp <= 150000] <- "Premium"
ce_data$mrp_category[ce_data$product_mrp >= 30001 & ce_data$product_mrp <= 80000] <- "Mid"
ce_data$mrp_category[ce_data$product_mrp > 0 & ce_data$product_mrp <= 30000] <- "Lower"

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
weekdays <- data.frame(weekdays %>% group_by(Month,week) %>% summarise(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7


# . . . . Strip Spaces ----
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)




# ***************************************************************************
#                   LOAD DATA ---- Media & Inv Data ----
# ***************************************************************************
# . . . .   ProductList ----
productList_data      <- 
  read.csv("./data/ProductList.csv", stringsAsFactors = FALSE, 
           na.strings=c('\\N'))

# . . . .   Media Investment ----
mediaInvestment_data  <- 
  read.csv("./data/MediaInvestment.csv", stringsAsFactors = FALSE)

# . . . .  Special Sale Event ----

specialSale_data      <- 
  read.csv("./data/SpecialSale.csv", stringsAsFactors = FALSE)

# . . . .   Monthly NPS ----
monthlyNPS_data       <- 
  read.csv("./data/MonthlyNPSscore.csv", stringsAsFactors = FALSE )

# . . . .   Holiday List ----
holiday_list      <- 
  read.csv("./data/HolidayList.csv", stringsAsFactors = FALSE)


# ***************************************************************************
#                   DATA PREPARATION ----
# ***************************************************************************
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
                                       summarise(TotalInvestment = sum(Total.Investment*fracDays),
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
sale_days <- as.data.frame(table(specialSale_data$week))
names(sale_days) <- c("week", "sale_days")

#Created sale days here and this will be used in the final merge, because we will be needing the number of sale days


# . . . .   HolidayList ----
holiday_list$Date     <- as.Date(holiday_list$Date, format = "%m/%d/%Y")
holiday_list$week     <- nweek(holiday_list$Date,origin = as.Date("2015-07-01"))

holiday_days <- as.data.frame(table(holiday_list$week))
names(holiday_days) <- c("week", "holidays")

#Added a new KPI holiday days here and this will be used in the final merge, this is different than sale days


# . . . .   Monthly NPS ----
str(monthlyNPS_data)
monthlyNPS_data$Date <- as.Date(monthlyNPS_data$Date, format = "%m/%d/%Y")
monthlyNPS_data$Month <- month(ymd(monthlyNPS_data$Date))
monthlyNPS_weekly   <- merge(weekdays, monthlyNPS_data, by='Month', all.x = TRUE)
# Average weekly NPS for the weeks span over adjacent months
monthlyNPS_weekly   <- monthlyNPS_weekly %>% group_by(., week) %>% 
  summarise(., NPS = mean(NPS))

# ***************************************************************************
#                   CREATING ADSTOCK ----
# ***************************************************************************

#Creating a Dataset for just the sales/gmv data to be used as input for creating media Adstock
gmv_weekly <-  ce_data %>% 
  group_by(week) %>% 
  summarise(gmv=sum(gmv))
write.csv(gmv_weekly, file = "sales.csv",row.names=FALSE)


# . . . .   Media Adstock ----
media_adstk      <- 
  read.csv("./data/media_adstock.csv", stringsAsFactors = FALSE)


# ***************************************************************************
#                   WEEKLY DATA AGGREGATION ----
# ***************************************************************************

#Weekly aggregation of ce_data

ce_data_weekly <-  ce_data %>% 
  group_by(mrp_category,
           product_analytic_sub_category,
           week) %>% 
  summarise(gmv=sum(gmv),
            product_mrp=mean(product_mrp), 
            list_price=mean(List_Price), 
            units=sum(units),
            Promotion=mean(Promotion),
            sla=mean(sla), 
            procurement_sla=mean(product_procurement_sla))


ce_data_weekly <- as.data.frame(ce_data_weekly)   # type cast to data.frame
summary(ce_data_weekly)




#Converting dummy variables for mrp_category
fact1<- as.data.frame(model.matrix(~ce_data_weekly[, 1], data = ce_data_weekly))
fact1 <- fact1[,-1]
names(fact1) <- c("cat_luxury", "cat_mid", "cat_premium")
# Merge dummy variables with the main data frame
ce_data_weekly <- cbind(ce_data_weekly[,-10], fact1)
# ***************************************************************************
#                   MERGING DATA ----
# ***************************************************************************

# . . . .   Merge MediaInvestment & NPS ----
media_nps <- merge(media_adstk, monthlyNPS_weekly, by = 'week', all.x = TRUE)


# . . . .   Merge Sales & SaleDays
data <- merge(ce_data_weekly, sale_days, by = 'week', all.x = TRUE)
data$sale_days[is.na(data$sale_days)] <- 0 #no sale days in that week

# . . . .   Merge data & holidays
data <- merge(data, holiday_days, by = 'week', all.x = TRUE)
data$holidays[is.na(data$holidays)] <- 0 #no sale days in that week

#. . . .   Merge Data & Media_NPS
data <- merge(data, media_nps, by = 'week', all.x = TRUE)

#Backing up the data
data_bkp <- data


#Removing mrp_category
#Keeping weeks, because I am thinking of using that to create our Training & Test Datasets
data <- data[,-c(2)]
quantile(data$product_mrp)


# ***************************************************************************
#           CREATE A NEW DATASET WITH ONLY THE IMP VARIABLES ----
# ***************************************************************************

camera_accessory_data <- subset(data, product_analytic_sub_category=="CameraAccessory")
home_audio_data       <- subset(data, product_analytic_sub_category=="HomeAudio")
gaming_accessory_data <- subset(data, product_analytic_sub_category=="GamingAccessory")

# . . . . Data Cleanup ----
# remove sub_category column
camera_accessory_data <- camera_accessory_data[,-2]
home_audio_data <- home_audio_data[,-2]
gaming_accessory_data <- gaming_accessory_data[,-2]


# . . . . Save Intrim Data ----
write.csv(data, file = "./intrim/eleckart.csv",row.names=FALSE)
write.csv(camera_accessory_data, file = './intrim/cameraAccessory.csv',row.names = FALSE)
write.csv(home_audio_data, file = './intrim/homeAudio.csv',row.names = FALSE)
write.csv(gaming_accessory_data, file = './intrim/gamingAccessory.csv',row.names = FALSE)
