# ***************************************************************************
#             MARKET MIX  MODELLING
#             
#       PGDDA ( IIIT Bangalore )
#       April 2017
#       AtchiReddy (atchireddi@gmail.com)
#             
#       DATA CLEANING & DATA PREPARATION
#             
# ***************************************************************************

```{r warning=FALSE, message=FALSE}
    
# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
library(lubridate)
library(dplyr)

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
#                   LOAD DATA ----
# ***************************************************************************
ce_data <- read.csv('../input/ConsumerElectronics.csv',stringsAsFactors = FALSE)

str(ce_data)

atchircUtils::naSummary(ce_data)

# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************

head(ce_data)

# . . . .   Missing Values ----
ce_data <- ce_data[,-c(9,10)]   # Omit 'deliverybday' & 'deliverycdays'

ce_data <- na.omit(ce_data)   # 4904 missing values, can be ignored


# . . . .   Outlier Treatment ----
# Remove orders before July'15 and after June'16
ce_data <- ce_data[ce_data$order_date>as.Date('2015-6-30'),]
ce_data <- ce_data[ce_data$order_date<as.Date('2016-7-1'),]

# . . . .   Correct Data Types ----

# 'order_id', 'order_item_id', 'cust_id', 'pincode' are qualitative data
#  having numeric values, let's convert them to character type

ce_data <- cbind(ce_data[,-c(5,6,11,12)],
           sapply(ce_data[,c(5,6,11,12)],as.character) )   # operate on interested columns

# gmv & mrp make non-zero
ce_data$gmv <- ce_data$gmv+1
ce_data$product_mrp <- ce_data$product_mrp+1


# ***************************************************************************
#                   FEATURE ENGINEERING ----
# ***************************************************************************

# create week,  week numbers start from min 'order date'
dates <- as.Date(
            gsub(" .*","",ce_data$order_date)
          )
 
ce_data$week <- atchircUtils::nweek(dates,origin = as.Date("2015-07-01"))

# replace spaces
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)

# compute discount gmv
ce_data$discount_gmv <- as.integer(ce_data$gmv/ce_data$units)

# discount
ce_data$discount  <- 100.0-(ce_data$discount_gmv*100/ce_data$product_mrp)



# ***************************************************************************
#                   WEEKLY DATA AGGREGATION ----
# ***************************************************************************

# Drop 'fsn_id', 'order_data', 'Year', 'Month', 'sl_fact.order_type',  
#  'order_id', 'order_item_id', 'cust_id', 'pincode', 

ce_data <- ce_data[,-c(1,2,3,7,9,15,16,17,18,20)]

str(ce_data)

ce_data_weekly <-  ce_data %>% 
                      group_by(product_analytic_category,
                               product_analytic_sub_category,
                               product_analytic_vertical,
                               Month,
                               week) %>% 
                      summarize(gmv=sum(gmv), 
                                product_mrp=mean(product_mrp), 
                                units=sum(units), 
                                sla=mean(sla), 
                                procurement_sla=mean(product_procurement_sla))

str(ce_data_weekly)



# ***************************************************************************
#                   DATA PREPARATION ----
# ***************************************************************************

# # Create subset for categories 'CameraAccessory', 'HomeAudio', 'GamingAccesory'
# camera_accessory_data <- subset(ce_data, product_analytic_sub_category=="CameraAccessory")
# home_audio_data       <- subset(ce_data, product_analytic_sub_category=="HomeAudio")
# gaming_accessory_data <- subset(ce_data, product_analytic_sub_category=="GamingAccessory")
# 
# 
# # ***************************************************************************
# #                   Save CLEAN DATA ----
# # ***************************************************************************
# 
write.csv(ce_data, '../intrim/ConsumeElectronics.csv', row.names = FALSE)
# write.csv(camera_accessory_data, '../intrim/CameraAccesory.csv')
# write.csv(home_audio_data, '../intrim/HomeAudio.csv')
# write.csv(gaming_accessory_data, '../intrim/GamingAccessory.csv')




# Observations : 
#     1. why -ve values in  'Cust_id' and 'pincode'
#     2. Order_id/cust_id/pincode has any naming convention
#     3. fsn_id has any naming convention
#     4. what is NPS score
#     5. should special sale days be marked in the dataset
#     6. which day to be considered start of week
#     7. Few More Insights in product list Tab
#     8. Elaboration on Media Investment
#     9. product details are given in order dataset, 
#           why aditional documentation,
#    10. How to ratio NPS & media spend to weekly
#    11. gmv vs mrp vs units. ( is gmv gt/lt mrp)
#    12. product_mrp is zero..??
#    
#    
#    
#    
# Data Augmentation : 
#     1. Derive day
#     2. Derive week
#     3. Derive Month
#     4. Mark Special Sale Dates
#     5. 


    
```

