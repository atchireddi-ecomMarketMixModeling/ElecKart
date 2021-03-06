
#' ---
#' title: 'MarketMixModeling DataPreparation'
#' author: 'Atchireddy chavva'
#' output: pdf_document
#' ---


#' \vspace{8pt}
#' ## **Goal:**
#' \vspace{8pt}
#' ElecKart is an e-commerce firm specialising in electronic products. Over the 
#' last one year, they had spent a significant amount of money in marketing. 
#' Occasionally, they had also offered big-ticket promotions (similar to the Big 
#' Billion Day). They are about to create a marketing budget for the next year 
#' which includes spending on commercials, online campaigns, and pricing & promotion 
#' strategies. The CFO feels that the money spent over last 12 months on marketing 
#' was not sufficiently impactful and that they can either cut on the budget or 
#' reallocate it  optimally across marketing levers to improve the revenue response
#' \vspace{8pt}



# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
#+ warning=FALSE, message=FALSE
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(Hmisc)   # describe


# ***************************************************************************
#                   PROCs ----
# ***************************************************************************
# Function to compute the week number w.r.t origin date.
# It takes data and orgin in Date format as arguments.
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


#' \newpage
# ***************************************************************************
#                   LOAD DATA ---- Transaction Data ----
# ***************************************************************************
# Make sure you are in current directory as in R-file is in. Should I do a commit?yes..

ce_data <- read.csv('./input/ConsumerElectronics.csv',stringsAsFactors = FALSE)
#' \vspace{8pt}
str(ce_data)
#' \vspace{8pt}



# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************


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

ce_data <- na.omit(ce_data)   # 4904 missing values, can be ignored


# . . . .   Correct Data Types ----

# 'order_id', 'order_item_id', 'cust_id', 'pincode' are qualitative data
#  having numeric values, let's convert them to character type

ce_data <- cbind(ce_data[,-c(5,6,11,12)],
                 sapply(ce_data[,c(5,6,11,12)],as.character) )  
# operate on interested columns



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
weekdays <- data.frame(weekdays %>% 
                         group_by(Month,week) %>% 
                              summarise(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7


# . . . . Strip Spaces ----
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)


# . . . . Generate Discount ----
ce_data$discount <- ((ce_data$product_mrp - ce_data$gmv)/ce_data$product_mrp) * 100

# . . . .  Payment Type ----
ce_data$COD     = as.integer(ce_data$s1_fact.order_payment_type=='COD')
ce_data$Prepaid = as.integer(ce_data$s1_fact.order_payment_type!='COD')



#' \newpage
# ***************************************************************************
#                   LOAD DATA ---- Media & Inv Data ----
# ***************************************************************************
# . . . .   ProductList ----
productList_data      <- 
  read.csv("./input/ProductList.csv", stringsAsFactors = FALSE, 
           na.strings=c('\\N'))

# . . . .   Media Investment ----
mediaInvestment_data  <- 
  read.csv("./input/MediaInvestment.csv", stringsAsFactors = FALSE)

# . . . .  Special Sale Event ----

specialSale_data      <- 
  read.csv("./input/SpecialSale.csv", stringsAsFactors = FALSE)

# . . . .   Monthly NPS ----
monthlyNPS_data       <- 
  read.csv("./input/MonthlyNPSscore.csv", stringsAsFactors = FALSE )




# ***************************************************************************
#                   DATA PREPARATION ----
# ***************************************************************************

# . . . .   ProductList ----
productList_data <- na.omit(productList_data)

# . . . . . . . .  Correct Data types ----
productList_data$Frequency <- as.integer(productList_data$Frequency)
#' \vspace{8pt}
str(productList_data)
#' \vspace{8pt}

# . . . .   Media Investment ----
str(mediaInvestment_data)
#' \vspace{8pt}

# . . . . . . . .  Missing Values ----
mediaInvestment_data[is.na(mediaInvestment_data)] <- 0   # zero investment


# . . . . . . . .  Convert to weekly data ----
# convert montly spend to weekly
mediaInvestment_data <- cbind(Month=mediaInvestment_data[,c(2)],
                              mediaInvestment_data[,-c(1,2)]/4.30)
# Add weekly information
mediaInvestment_weekly <- merge(weekdays,mediaInvestment_data, by='Month', 
                                all.x = TRUE)

# Convert media Investment at weekly granularity
# pro-rate weekly investment as per the ratio of its days span over adjacent months
mediaInvestment_weekly <- 
  data.frame(
            mediaInvestment_weekly %>% 
                group_by(week) %>% 
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
#' \vspace{8pt}
specialSale_data$Date      <- as.Date(specialSale_data$Day, format = "%m/%d/%Y")
specialSale_data$week      <- nweek(specialSale_data$Date,origin = as.Date("2015-07-01"))
specialSale_data           <- data.frame(table(specialSale_data$week))
colnames(specialSale_data) <- c('week','n_saledays')


# . . . .   Monthly NPS ----
monthlyNPS_weekly   <- merge(weekdays, monthlyNPS_data, by='Month', all.x = TRUE)
monthlyNPS_weekly   <- as.data.frame(monthlyNPS_weekly %>% group_by(., week) %>% 
                                              summarise(., NPS = mean(NPS)))



#' \newpage
# ***************************************************************************
#                   WEEKLY DATA AGGREGATION ----
# ***************************************************************************
ce_data_weekly <-  ce_data %>% 
  group_by(product_analytic_sub_category,
           week) %>% 
  summarise(gmv=sum(gmv), 
            product_mrp=mean(product_mrp), 
            units=sum(units),
            discount=mean(discount),
            sla=mean(sla), 
            procurement_sla=mean(product_procurement_sla),
            COD=sum(COD),
            Prepaid = sum(Prepaid))

ce_data_weekly <- as.data.frame(ce_data_weekly)   # type cast to data.frame



# ***************************************************************************
#                   MERGING DATA ----
# ***************************************************************************

# . . . .   Merge MediaInvestment & NPS ----
media_nps <- merge(mediaInvestment_weekly, monthlyNPS_weekly, by = 'week', 
                   all.x = TRUE)


# . . . .   Merge Sales & SaleDays
data <- merge(ce_data_weekly, specialSale_data, by = 'week', all.x = TRUE)
data[is.na(data$n_saledays),'n_saledays'] = 0
# data$Sales.Name[is.na(data$Sales.Name)] <- "No sale"


#. . . .   Merge Data & Media_NPS
data <- merge(data, media_nps, by = 'week', all.x = TRUE)


# Discount on Products
data$list_mrp     <- as.integer(data$gmv/data$units)
data$discount     <- (1-(data$list_mrp/data$product_mrp))*100


# . . . . Save Intrim Data ----
write.csv(data, file = "./intrim/eleckart.csv",row.names=FALSE)
#' \vspace{8pt}
str(data)
#' \vspace{8pt}


