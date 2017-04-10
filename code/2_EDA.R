# ***************************************************************************
#             MARKET MIX  MODELLING
#             
#       PGDDA ( IIIT Bangalore )
#       April 2017
#       AtchiReddy (atchireddi@gmail.com)
#             
#       Exploratory Analysis
#             
# ***************************************************************************

```{r warning=FALSE, message=FALSE}
# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
library(ggplot2)

# ***************************************************************************
#                   LOAD DATA ----
# ***************************************************************************

cam_acess_data <- read.csv('../intrim/CameraAccesory.csv')

# ***************************************************************************
#                   UNIVARIATE ----
# ***************************************************************************

ggplot(ce_data, aes(units)) + geom_bar()


# ***************************************************************************
#                   BI-VARIATE ----
# ***************************************************************************

# No of Units Sold per Month
ggplot(ce_data, aes(x=factor(Month),units)) + geom_bar()

# Revenue(gmv) per category
ggplot(ce_data, aes(x=reorder(product_analytic_category,-gmv),gmv)) + 
      geom_bar(stat = 'identity') + 
      theme(axis.text.x = element_text(angle = 90,hjust = 0))

# Revenue(gmv) per sub_category
ggplot(ce_data, aes(x=reorder(product_analytic_sub_category,-gmv),gmv)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0))

# Revenue(gmv) per vertical
ggplot(ce_data, aes(x=reorder(product_analytic_vertical,-gmv),gmv)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0))

# Revenue(gmv) per pincode
ggplot(ce_data, aes(x=reorder(pincode,-gmv),gmv)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0))

# Revenue(gmv) per Month
ggplot(ce_data, aes(x=reorder(factor(Month),-gmv),gmv)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0))

# Revenue(gmv) per week
ggplot(ce_data, aes(x=reorder(factor(week),-gmv),gmv)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0))

    
```