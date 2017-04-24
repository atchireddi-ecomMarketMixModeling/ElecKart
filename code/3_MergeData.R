# ***************************************************************************
#             MARKET MIX  MODELLING
#             
#       PGDDA ( IIIT Bangalore )
#       April 2017
#       AtchiReddy (atchireddi@gmail.com)
#             
#       MERGE SALES DATA AND MARKETING DATA
#             
# ***************************************************************************


```{r warning=FALSE, message=FALSE}
    
# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
source('atchircUtils.R')

# ***************************************************************************
#                   LOAD DATA ----
# ***************************************************************************

# prepare sales data
source('./1_DataPreparation.R')

# Prepare Marketing Data
source('./1p1_MarketingDataPreparation.R')


# . . . .   Sales Data ----
sales_data <- read.csv('../intrim/ConsumeElectronics.csv',stringsAsFactors = FALSE)

# . . . .   Marketing Data ----
productList_data <- read.csv('../intrim/productList.csv',stringsAsFactors = FALSE)

specialSale_data <- read.csv('../intrim/specialSale.csv',stringsAsFactors = FALSE)

mediaInvestment_data <- read.csv('../intrim/mediaInvestment.csv',stringsAsFactors = FALSE)

monthlyNPS_data <- read.csv('../intrim/monthlyNPS.csv',stringsAsFactors = FALSE)


# ***************************************************************************
#                   PREPARE DATA ----
# ***************************************************************************

# convert monthly media invest to weekly
# . . . .   Monthly MediaInvestment to Weekly ----
mediaInvestment_data <- cbind(Month=mediaInvestment_data[,c(2)],
                              mediaInvestment_data[,-c(1,2)]/4.3)

# . . . .   Merge MediaInvestment & NPS ----
media_nps_data <- merge(mediaInvestment_data, monthlyNPS_data, by = 'Month', all.x = TRUE)


# . . . .   Merge Sales & ProductList ----
data <- merge(sales_data, productList_data[,-c(2,3)], by.x = 'product_analytic_vertical',by.y = 'Product')

# . . . .   Merge Sales & SaleDays
data <- merge(data, specialSale_data[,-c(1)], by='week')

# . . . .   Merge Sales & media_nps
data <- merge(data, media_nps_data, by='Month')



# ***************************************************************************
#                   SAVE DATA ----
# ***************************************************************************

write.csv(data,'../intrim/dataForModeling.csv',row.names = FALSE)


```