# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)



# ok copied these...now how yo push these ?

# here is the manthr...
#  git status  tell me which are new files and which are modified files. 
#  git pull
#  git add <file name for new files>
#  git commit <file name (or) *(this is dangerous)> I do selective commit for each file
#   git push.
# for all these steps, I open shell from tools

# lets commit modified files for now..

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
#                   LOAD DATA ---- Transaction Data
# ***************************************************************************
setwd("~/Analytics Course - Upgrad/Capstone Project/ElectKart/Input")
ce_data <- read.csv('ConsumerElectronics.csv',stringsAsFactors = FALSE)

str(ce_data)

# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************

head(ce_data)

# . . . .   Outlier Treatment ----
# Remove orders before July'15 and after June'16
ce_data$order_date <- format(as.POSIXct(ce_data$order_date,format='%Y-%m-%d'),format='%Y-%m-%d')
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
dates <- as.Date(
  gsub(" .*","",ce_data$order_date)
)

ce_data$week <- nweek(dates,origin = as.Date("2015-07-01"))

# replace spaces
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)

# Create a new variable discount
ce_data$discount <- ((ce_data$product_mrp - ce_data$gmv)/ce_data$product_mrp) * 100

# ***************************************************************************
#                   LOAD DATA ---- Media & Inv Data
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


# . . . .   SPecialSale ----
str(specialSale_data)
specialSale_data$Date          <- as.Date(specialSale_data$Date, format = "%m/%d/%Y")
specialSale_data$week <- nweek(specialSale_data$Date,origin = as.Date("2015-07-01"))
summary(specialSale_data)
unique(specialSale_data$week)
specialSale_data<- specialSale_data[!duplicated(specialSale_data$week),]      #Subsetting unique holiday weeks


# . . . .   Monthly NPS ----
str(monthlyNPS_data)
monthlyNPS_data$Date <- as.Date(monthlyNPS_data$Date, format = "%m/%d/%Y")
monthlyNPS_data$Month <- month(ymd(monthlyNPS_data$Date))

# ***************************************************************************
#                   WEEKLY DATA AGGREGATION ----
# ***************************************************************************
ce_data_weekly <-  ce_data %>% 
  group_by(product_analytic_category,
           product_analytic_sub_category,
           product_analytic_vertical,
           Month,
           week) %>% 
  summarize(gmv=sum(gmv), 
            product_mrp=sum(product_mrp), 
            units=sum(units),
            discount=sum(discount),
            sla=mean(sla), 
            procurement_sla=mean(product_procurement_sla))


# ***************************************************************************
#                   MERGING DATA ----
# ***************************************************************************


# . . . .   Merge MediaInvestment & NPS ----
media_nps <- merge(mediaInvestment_data, monthlyNPS_data[,-1], by = 'Month', all.x = TRUE)
# . . . .   Make the data daily ----
media_nps <- cbind(Month=media_nps[,c(1)],
                   media_nps[,-c(1,2)]/4.30)

# . . . .   Merge Sales & SaleDays
data <- merge(ce_data_weekly, specialSale_data[,-1], by = 'week', all.x = TRUE)
data$Sales.Name[is.na(data$Sales.Name)] <- "No sale"


#. . . .   Merge Data & Media_NPS
data <- merge(data, media_nps, by = 'Month', all.x = TRUE)

# Converting the varible type into 'factor'
data$Month <- as.factor(data$Month)
data$week <- as.factor(data$week)
data$product_analytic_category <- as.factor(data$product_analytic_category)
data$product_analytic_sub_category <- as.factor(data$product_analytic_sub_category)
data$product_analytic_vertical <- as.factor(data$product_analytic_category)
# Discount on Products
data$discount <- ((data$product_mrp - data$gmv)/(data$product_mrp) * 100)
data$Holiday.Sale <- ifelse(data$Sales.Name == "No Sale", 0, 1)

# ***************************************************************************
#           CREATE A NEW DATASET WITH ONLY THE IMP VARIABLES ----
# ***************************************************************************

write.csv(data, file = "eleckart.csv",row.names=FALSE)


# ***************************************************************************
#                        LINEAR MODEL ----
# ***************************************************************************

indices=sample(1:nrow(eleckart),0.7*nrow(eleckart))
train=data[indices,]
test=data[-indices,]

#Modelling the Advertising Effects
model_1 <- lm(units~ .,data=eleckart)
summary(model_1)
vif(model_1)