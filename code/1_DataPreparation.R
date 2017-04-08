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
#                   DATA PREPARATION ----
# ***************************************************************************

head(ce_data)

# . . . .   Missing Values ----
ce_data <- ce_data[,-c(9,10)]   # Omit 'deliverybday' & 'deliverycdays'

ce_data <- na.omit(ce_data)   # 4904 missing values, can be ignored

# . . . .   Correct Data Types ----

# 'order_id', 'order_item_id', 'cust_id', 'pincode' are qualitative data
#  having numeric values, let's convert them to character type

ce_data <- cbind(ce_data[,-c(5,6,17,18)],
           sapply(ce_data[,c(5,6,17,18)],as.character) )   # operate on interested columns


# ***************************************************************************
#                   Feature Engineering ----
# ***************************************************************************

# create week,  week numbers start from min 'order date'
dates <- as.Date(
            gsub(" .*","",ce_data$order_date)
          )
min_date <- min(dates)
ce_data$week <- nweek(dates,origin = min_date)



# ***************************************************************************
#                   Save CLEAN DATA ----
# ***************************************************************************



# Observations : 
#     1. why -ve values in  'Cust_id' and 'pincode'
#     2. Order_id/cust_id/pincode has any naming convention
#     3. fsn_id has any naming convention
#     4. what is NPS score
#     5. should special sale days be marked in the dataset
#     6. which day to be considered start of week
#     7. Few More Insights in product list Tab
#     8. Elaboration on Media Investment



# Data Augmentation : 
#     1. Derive day
#     2. Derive week
#     3. Derive Month
#     4. Mark Special Sale Dates
#     5. 


    
```

