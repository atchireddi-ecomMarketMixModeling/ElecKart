#' ---
#' title: 'MarketMixModeling Models'
#' author: 'Atchireddy chavva'
#' output: pdf_document
#' ---



 
 
#+ Load_Library ---- 
#' ## Load Libraries:
#'
#'  Load required libraries. Will use `stepAIC`  from `MASS` package for model pruning.
#' 

#+ warning=FALSE, message=FALSE
library(MASS)
library(stargazer)






#+ Load Data ---- 
#' ## Load Data:
#'
#'  Load blended data from sales and marketing datasets. Data is throughly
#' cleaned, pre-processed for model building. Refer to `DataCleaning.R`
#' script for data preparation steps. For this capstone project we limit
#' our model building focus to `camera_accessory`, `Home_audio` and 
#' `Gaming_accessory` product sub-categories. For simplicity will start
#' Linear model building with numerical features, later will consider 
#' categorical features.
#' \vspace{4pt}   

#+ load_data,
camera_accessory_data_nrm <- read.csv('./intrim/cameraAccessory.csv')
home_audio_data_nrm       <- read.csv('./intrim/homeAudio.csv')
gaming_accessory_data_nrm <- read.csv('./intrim/gamingAccessory')


#' \vspace{4pt}
#' Lets preview the dataset structure. `gmv` gross merchandise values is our target
#' variable, which we would like to maximize with optimal marketing spend across 
#' different marketing levers, `discount` is one of the KPI derived from sales data, 
#' bunch of other features from marketing spend and NPS datasets.
#' \vspace{8pt}


#'
#' \footnotesize
str(camera_accessory_data_nrm)
#' \vspace{12pt}
#' 
#' **Features distribution:** Look at features statistical distributions
stargazer(camera_accessory_data_nrm,type='text')
#' \normalsize


#' \vspace{12pt}
#+ Training and Validation Data ----
#' \vspace{8pt}
#' ## Train and Validation Data:
#' 
#' With one year Sales and Marketing Data, we have 52 obeservations aggreagted
#' at weekly level for each sub-category. Splitting data into training and validation
#' sets would further reduce the training sample size, Moreover the task at hand
#' is to figure out most influential marketing leavers for optimial marketing spend.
#' The goal is to explain the influence of features rather predicting any quantities
#' we can safely utilize the whole dataset for training.







#' \newpage
#+ Model Building ----
#' ## **Model Building - Linear Model:**
#' 
#'
#' \vspace{8pt}  
#'  
#'  ###### Assumptions:
#'  For simplicity, will consider, each sub-category sales affected from overall marketing spend,
#'  where in reality, only a portion of the marketing spend would have been alloted for promoting
#'  a certain product category.




#+ . . . . Camera Accessory ----
#' #### Camera Accesory:
#' 
#+ . . . . . . . . Initial Linear Model ----
#' ###### Initial Linear Model
model_cam1 <- lm(gmv~ .,data=camera_accessory_data_nrm)

#+ . . . . . . . . Auto-Otimize Model ----
#' ###### Auto-Optimize Model
step_cam <- stepAIC(model_cam1, direction = "both",trace=FALSE,k=2)
summary(step_cam)
model_cam2 <- lm(formula = gmv ~ sla + procurement_sla + TV + Digital + Sponsorship + 
                   ContentMarketing + Affiliates + SEM + Radio, data = camera_accessory_data_nrm)
summary(model_cam2)
vif(model_cam2)

#Removing SEM
model_cam3 <- lm(formula = gmv ~ sla + procurement_sla + TV + Digital + Sponsorship + 
                   ContentMarketing + Affiliates + Radio, data = camera_accessory_data_nrm)
summary(model_cam3)
vif(model_cam3)

#Removing ContentMarketing
model_cam4 <- lm(formula = gmv ~ sla + procurement_sla + TV + Digital + Sponsorship + Affiliates + Radio, data = camera_accessory_data_nrm)
summary(model_cam4)
vif(model_cam4)

#Removing Radio & TV
model_cam5 <- lm(formula = gmv ~ sla + procurement_sla + Digital + Sponsorship + Affiliates, data = camera_accessory_data_nrm)
summary(model_cam5)
vif(model_cam5)


#' **Summary**
#' \footnotesize
stargazer(model_cam1,step_cam, align = TRUE, type = 'text',
          title='Linear Regression Results', single.row=TRUE)
#' \normalsize

#' \vspace{8pt}
#' ##### **Understanding Model:**
#' \vspace{8pt}
#' 
#' Linear model could explain 56% of revenue from marketing expenditure, but some 
#' of the significant features like `TV spending` has negative coefficient term. 
#' If we were to explain this, it should mean, `reducing the TV marketing spend would 
#' increase camera Accessory sales`, which doesn't make sense. we can further optimize
#' model by exploring multi-collinearity and pruning features. Hopefully we may end
#' up with a model with better R-Square value and co-efficient terms which makes sense.



#' \newpage
#+ . . . . Gaming Accessory ----
#' #### Gaming Accesory:
#' 
#+ . . . . . . . . Initial Linear Model ----
#' ###### Initial Model
model_ga1 <- lm(gmv~ .,data=gaming_accessory_data_nrm)

#+ . . . . . . . . Auto-Optimize Model ----
#' ###### Auto-Optimize Model
step_ga <- stepAIC(model_ga1, direction = "both",trace=FALSE)

 
#' \footnotesize
stargazer(model_ga1,step_ga, align = TRUE, type = 'text',
          title='Linear Regression Results', single.row=TRUE)
#' \normalsize




#' \newpage
#+ . . . . Home Accessory ----
#' #### Home Accessory
#' 
#+ . . . . . . . . Initial Linear Model ----
#' ###### Initial Model
model_ha1 <- lm(gmv~ .,data=home_audio_data_nrm)

#+ . . . . . . . . Auto-Optimize Model ----
#' ###### Auto-Opitimize Model
step_ha <- stepAIC(model_ha1, direction = "both",trace=FALSE)

#' \normalsize
#' \footnotesize
#' **Summary**
stargazer(model_ha1,step_ha, align = TRUE, type = 'text',
          title='Linear Regression Results', single.row=TRUE)
#' \normalsize





#+ Observations ----
#' ## Observations:
