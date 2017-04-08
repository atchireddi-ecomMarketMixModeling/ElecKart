# ***************************************************************************
#             MARKET MIX  MODELLING
#             
#       PGDDA ( IIIT Bangalore )
#       April 2017
#       AtchiReddy (atchireddi@gmail.com)
#             
#       Marketing DATA PREPARATION
#             
# ***************************************************************************

```{r warning=FALSE, message=FALSE}
    
# ***************************************************************************
#                   LOAD LIRRARY ----
# ***************************************************************************
library(ggplot2)


# ***************************************************************************
#                   LOAD DATA ----
# ***************************************************************************
# Can't use xlsx/xlconnect package which needs 64bit java version

# . . . .   ProductList ----
productList_data      <- 
        read.csv("../input/ProductList.csv", stringsAsFactors = FALSE, 
                 na.strings=c('\\N'))

# . . . .   Media Investment ----
mediaInvestment_data  <- 
        read.csv("../input/MediaInvestment.csv", stringsAsFactors = FALSE)

# . . . .  Special Sale Event ----

specialSale_data      <- 
        read.csv("../input/SpecialSale.csv", stringsAsFactors = FALSE)

# . . . .   Monthly NPS ----
monthlyNPS_data       <- 
        read.csv("../input/MonthlyNPSscore.csv", stringsAsFactors = FALSE )


# ***************************************************************************
#                   DATA PREPARATION ----
# ***************************************************************************

# . . . .   ProductList ----
str(productList_data)
atchircUtils::naSummary(productList_data)
productList_data <- na.omit(productList_data)

# . . . . . . . .  Correct Data types ----
productList_data$Frequency <- as.integer(productList_data$Frequency)

# . . . .   Media Investment ----
str(mediaInvestment_data)

# . . . . . . . .  Missing Values ----
mediaInvestment_data[is.na(mediaInvestment_data)] <- 0   # zero investment

# . . . .   SPecialSale ----
str(specialSale_data)

specialSale_data$SaleOccasion <- as.factor(specialSale_data$SaleOccasion)
specialSale_data$Day          <- as.Date(specialSale_data$Day, format = "%d/%m/%Y")

# . . . .   Monthly NPS ----
str(monthlyNPS_data)

monthlyNPS_data$Month <- as.Date(monthlyNPS_data$Month, format = "%m/%d/%Y")



# ***************************************************************************
#                   EDA ----
# ***************************************************************************

# . . . .   Product List ----    
plt <- ggplot(productList_data, aes(x=reorder(Product,-Frequency),Frequency)) + 
        geom_bar(stat = 'identity') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0)) + 
        labs(x='Products',title='Products Distribution')

plt


# . . . .   Media Investment ----

# Month wise Media Investment 
plt <- ggplot(mediaInvestment_data, aes(Month))
plt <- plt + geom_bar(aes(y=Total.Investment),stat = "identity")
plt <- plt + labs(title="Month wise Marketing Spend")
plt

# Channel wise Media Investment Breakdown
plt <- ggplot(mediaInvestment_data, aes(Month))
plt <- plt + geom_line(aes(y=TV, colour= "TV"), size=1)
plt <- plt + geom_line(aes(y=Digital, colour="Digital"), size=1)
plt <- plt + geom_line(aes(y=Content.Marketing, colour="Content.Marketing"),size=1)
plt <- plt + geom_line(aes(y=Sponsorship, colour="Sponsorship"),size=1)
plt <- plt + geom_line(aes(y=Online.marketing, colour="Online.marketing"),size=1)
plt <- plt + geom_line(aes(y=SEM, colour="SEM"),size=1)
plt <- plt + geom_line(aes(y=Radio, colour="Radio"),size=1)
plt <- plt + geom_line(aes(y=Other, colour="Other"),size=1)
plt <- plt + geom_line(aes(y=Affiliates, colour="Affiliates"),size=1)
plt <- plt + scale_colour_manual("", 
                 breaks = c("TV", "Digital","Content.Marketing","Sponsorship",
                            "Online.marketing","SEM","Radio","Other","Affiliates"),
                 values = c("red", "dark green","blue","brown","cyan","magenta",
                            "orange","black","purple"))
plt + labs(title="Marketing Spend Breakdown")
  

# . . . .   NPS ----
plot(monthlyNPS_data,main="NPS")  
lines(monthlyNPS_data,main="NPS")  



```