# Load Library ----

# Load Data ----
ce_data <- read.csv('../input/ConsumerElectronics.csv',stringsAsFactors = FALSE)

ce_data <- ce_data[,-c(9,10)]

str(ce_data)

View(ce_data)

# Explore data ----

# 'order_id',  'order_item_id', 'cust_id', 'pincode' variable values though
#  seems numeric in nature, they represent qualitative attributes. 

# ce_data$order_id        <- as.character(ce_data$order_id)
# ce_data$order_item_id   <- as.character(ce_data$order_item_id)
# ce_data$cust_id         <- as.character(ce_data$cust_id)
# ce_data$pincode         <- as.character(ce_data$pincode)
# 
# 
# str(ce_data)
# 
# View(ce_data)


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

