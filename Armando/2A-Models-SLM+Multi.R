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
#                   LOAD DATA ---- Product Categories ----
# ***************************************************************************
camera_accessory_data <- read.csv('./intrim/cameraAccessory.csv')
home_audio_data <- read.csv('./intrim/homeAudio.csv')
gaming_accessory_data <- read.csv('./intrim/gamingAccessory.csv')

# ***************************************************************************
#                   Create Training & Test Datasets ----
# ***************************************************************************
#Lets divide the Train & Test data. For this we will use the first 36 weeks data as Train set and rest as Test set

#Camera Accessory
cam_train <- subset(camera_accessory_data, week <= 36)
cam_test <- subset(camera_accessory_data, week > 36)
cam_train <- cam_train[,-1]
cam_test <- cam_test[,-1]


#Gaming Accessory
gam_train <- subset(gaming_accessory_data, week <= 36)
gam_test <- subset(gaming_accessory_data, week > 36)
gam_train <- gam_train[,-1]
gam_test <- gam_test[,-1]
                     
#Home Audio
hom_train <- subset(home_audio_data, week <= 36)
hom_test <- subset(home_audio_data, week > 36)
hom_train <- hom_train[,-1]
hom_test <- hom_test[,-1]

# ***************************************************************************
#                   MODELLING ---- Simple Linear Model(additive) ----
# ***************************************************************************


#+ . . . . Camera Accessory ----
#' 
#+ . . . . . . . . Initial Linear Model ----
#' ###### Initial Linear Model
slm_cam1 <- lm(gmv~ .,data=cam_train)

#+ . . . . . . . . Auto-Otimize Model ----
#' ###### Auto-Optimize Model
step_slm_cam <- stepAIC(slm_cam1, direction = "both",trace=FALSE)
summary(step_slm_cam)

#Pruning of Variables to arrive at Final Model
slm_cam2 <- lm(formula = gmv ~ product_mrp + list_price + Promotion + sla + 
                 cat_mid + cat_premium + ContentMarketing + OnlineMarketing + 
                 Affiliates + SEM + NPS, data = cam_train)
summary(slm_cam2)
vif(slm_cam2)

#Removing Affiliates
slm_cam3 <- lm(formula = gmv ~ product_mrp + list_price + Promotion + sla + 
                 cat_mid + cat_premium + ContentMarketing + OnlineMarketing + 
                 SEM + NPS, data = cam_train)
summary(slm_cam3)
vif(slm_cam3)

#Removing List_price
slm_cam4 <- lm(formula = gmv ~ product_mrp + Promotion + sla + 
                 cat_mid + cat_premium + ContentMarketing + OnlineMarketing + 
                 SEM + NPS, data = cam_train)
summary(slm_cam4)
vif(slm_cam4)

#Removing OnlineMarketing
slm_cam5 <- lm(formula = gmv ~ product_mrp + Promotion + sla + 
                 cat_mid + cat_premium + ContentMarketing + 
                 SEM + NPS, data = cam_train)
summary(slm_cam5)
vif(slm_cam5)

#Removing ContentMarketing
slm_cam6 <- lm(formula = gmv ~ product_mrp + Promotion + sla + 
                 cat_mid + cat_premium + 
                 SEM + NPS, data = cam_train)
summary(slm_cam6)
vif(slm_cam6)

#Removing Promotion
slm_cam7 <- lm(formula = gmv ~ product_mrp + sla + 
                 cat_mid + cat_premium + 
                 SEM + NPS, data = cam_train)
summary(slm_cam7)
vif(slm_cam7)

#Removing SLA & SEM, this is our final model
slm_cam8 <- lm(formula = gmv ~ product_mrp + 
                 cat_mid + cat_premium + 
                 NPS, data = cam_train)
summary(slm_cam8)
vif(slm_cam8)

#Test the model on Test Dataset
pred_cam_slm<-predict(slm_cam8,cam_test[,-1])

#Add New column for predicted_gmv
cam_test$predicted_gmv <- pred_cam_slm

#Lets look at the Corr & R2
cor(cam_test$gmv, cam_test$predicted_gmv)
cor(cam_test$gmv, cam_test$predicted_gmv)^2
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#+ . . . . Gaming Accessory ----
#' 
#+ . . . . . . . . Initial Linear Model ----
#' ###### Initial Linear Model
slm_gam1 <- lm(gmv~ .,data=gam_train)

#+ . . . . . . . . Auto-Otimize Model ----
#' ###### Auto-Optimize Model
step_slm_gam <- stepAIC(slm_gam1, direction = "both",trace=FALSE)
summary(step_slm_gam)

#Pruning of Variables to arrive at Final Model
slm_gam2 <- lm(formula = gmv ~ Promotion + cat_mid + sale_days + Sponsorship + 
                 NPS, data = gam_train)
summary(slm_gam2)
vif(slm_gam2)

#Removing Promotion
slm_gam3 <- lm(formula = gmv ~ cat_mid + sale_days + Sponsorship + 
                 NPS, data = gam_train)
summary(slm_gam3)
vif(slm_gam3)

#Removing sale_days, This is our final model
slm_gam4 <- lm(formula = gmv ~ cat_mid + Sponsorship + 
                 NPS, data = gam_train)
summary(slm_gam4)
vif(slm_gam4)

#Test the model on Test Dataset
pred_gam_slm<-predict(slm_gam4,gam_test[,-1])

#Add New column for predicted_gmv
gam_test$predicted_gmv <- pred_gam_slm

#Lets look at the Corr & R2
cor(gam_test$gmv, gam_test$predicted_gmv)
cor(gam_test$gmv, gam_test$predicted_gmv)^2
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#+ . . . . Home Audio ----
#' 
#+ . . . . . . . . Initial Linear Model ----
#' ###### Initial Linear Model
slm_hom1 <- lm(gmv~ .,data=hom_train)

#+ . . . . . . . . Auto-Otimize Model ----
#' ###### Auto-Optimize Model
step_slm_hom <- stepAIC(slm_hom1, direction = "both",trace=FALSE)
summary(step_slm_hom)

#Pruning of Variables to arrive at Final Model
slm_hom2 <- lm(formula = gmv ~ product_mrp + list_price + Promotion + sla + 
                 sale_days + ContentMarketing + Affiliates + SEM + NPS, data = hom_train)
summary(slm_hom2)
vif(slm_hom2)
cor(hom_train$product_mrp, hom_train$list_price)

#Removing NPS & sale_day
slm_hom3 <- lm(formula = gmv ~ product_mrp + list_price + Promotion + sla + 
                 ContentMarketing + Affiliates + SEM, data = hom_train)
summary(slm_hom3)
vif(slm_hom3)

#Removing sla
slm_hom4 <- lm(formula = gmv ~ product_mrp + list_price + Promotion + 
                 ContentMarketing + Affiliates + SEM, data = hom_train)
summary(slm_hom4)
vif(slm_hom4)

#Removing product_mrp
slm_hom5 <- lm(formula = gmv ~ list_price + Promotion + 
                 ContentMarketing + Affiliates + SEM, data = hom_train)
summary(slm_hom5)
vif(slm_hom5)


#Removing SEM
slm_hom6 <- lm(formula = gmv ~ list_price + Promotion + 
                 ContentMarketing + Affiliates, data = hom_train)
summary(slm_hom6)
vif(slm_hom6)

plot(slm_hom6)
abline(slm_hom6)

#Test the model on Test Dataset
pred_hom_slm<-predict(slm_hom6,hom_test[,-1])

#Add New column for predicted_gmv
hom_test$predicted_gmv <- pred_hom_slm

#Lets look at the Corr & R2
cor(hom_test$gmv, hom_test$predicted_gmv)
cor(hom_test$gmv, hom_test$predicted_gmv)^2
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#Log Transformation of Train & Test Data


#Camera Accessory
cam_train_log <- log1p(cam_train)
cam_test_log <- log1p(cam_test)

#Gaming Accessory
gam_train_log <- log1p(gam_train)
gam_test_log <- log1p(gam_test)

#Home Audio
hom_train_log <- log1p(hom_train)
hom_test_log <- log1p(hom_test)




# ***************************************************************************
#                   MODELLING ---- Multiplicative Model ----
# ***************************************************************************

#+ . . . . Camera Accessory ----
#' 
#+ . . . . . . . . Initial Linear Model ----
#' ###### Initial Linear Model
mul_cam1 <- lm(gmv~ .,data=cam_train_log)
#+ . . . . . . . . Auto-Otimize Model ----
#' ###### Auto-Optimize Model
step_mul_cam <- stepAIC(mul_cam1, direction = "both",trace=FALSE)
summary(step_mul_cam)
vif(step_mul_cam)

#Remove product_mrp
mul_cam2 <- lm(formula = gmv ~ list_price + Promotion + cat_luxury + 
                 cat_mid + cat_premium + TV + OnlineMarketing + Affiliates, 
               data = cam_train_log)
summary(mul_cam2)
vif(mul_cam2)

#Remove TV
mul_cam3 <- lm(formula = gmv ~ list_price + Promotion + cat_luxury + 
                 cat_mid + cat_premium + OnlineMarketing + Affiliates, 
               data = cam_train_log)
summary(mul_cam3)
vif(mul_cam3)

#Remove Affiliates
mul_cam4 <- lm(formula = gmv ~ list_price + Promotion + cat_luxury + 
                 cat_mid + cat_premium + OnlineMarketing, 
               data = cam_train_log)
summary(mul_cam4)
vif(mul_cam4)

#Remove cat_premium
mul_cam5 <- lm(formula = gmv ~ list_price + Promotion + cat_mid + 
                 cat_premium + OnlineMarketing, 
               data = cam_train_log)
summary(mul_cam5)
vif(mul_cam5)

#Test the model on Test Dataset
pred_cam_mul<-predict(mul_cam5,cam_test_log[,-1])

#Add New column for predicted_gmv
cam_test_log$predicted_gmv <- pred_cam_mul

#Lets look at the Corr & R2
cor(cam_test_log$gmv, cam_test_log$predicted_gmv)
cor(cam_test_log$gmv, cam_test_log$predicted_gmv)^2
