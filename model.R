#' ---
#' title: 'MarketMixModeling Modeling'
#' author: 'Atchireddy chavva'
#' output: pdf_document
#' ---



 
 
#+ Load_Library ---- 
#' ## Load Libraries:
#'
#'  Load required libraries. Will use `stepAIC`  from `MASS` package for model pruning.
#'  `vif` variation inflation factor from `car` package
#' 

#+ warning=FALSE, message=FALSE
library(MASS)
library(car)
library(stargazer)
source('./code/atchircUtils.R')






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

#+ EDA ----
#'
#' **Marketing Spend Breakdown:**
#' 
#' ![](../output/MarketingSpendBreakdown1.png)



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
#' **Variation Inflation Factor**
knitr::kable(viewModelSummaryVIF(step_cam))
#' \normalsize

#' OBservations:
#' 
#' `Digital` and  `SEM` exhibits multi-collinearity, with `Digital` sligthly highly
#' significant. Lets refer marketing spend breakdown
#' 


#+ . . . . . . . . Prune procurement_sla, sla, ContentMarketing, Radio, Digital  ----
#' **Final Model:**
model_cam2=lm(formula = gmv ~  Sponsorship + SEM + TV +
     Affiliates, data = camera_accessory_data_nrm)
#' \footnotesize
getModelR2(model_cam2)
knitr::kable(viewModelSummaryVIF(model_cam2))
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
knitr::kable(viewModelSummaryVIF(step_ga))
#' \normalsize


#+ . . . . . . . . Prune OnlineMarketing, procurement_sla, NPS, SEM, ContentMarketing ----
#' **Final Model**
model_ga2 <- lm(formula = gmv ~ discount + TV + Digital + 
Sponsorship + Affiliates + 
   Radio, data = gaming_accessory_data_nrm)

#' \footnotesize
knitr::kable(viewModelSummaryVIF(model_ga2))
getModelR2(model_ga2)
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
knitr::kable(viewModelSummaryVIF(step_ha))
#' \normalsize


#+ . . . . . . . . Prune ContentMarketing,TV, NPS, OnlineMarketing, SEm, Radio, Digital, Other, procurement ----
#'
model_ha2 <- lm(formula = gmv ~ discount + sla + 
                  Sponsorship, data = home_audio_data_nrm)
#' \footnotesize
knitr::kable(viewModelSummaryVIF(model_ha2))
getModelR2(model_ha2)
#' \normalsize


#+ Observations ----
#' ## Observations:
#' 










# ***************************************************************************
#                   BACK UP ----
# ***************************************************************************






#+ Model Building:
data_week_bkp <- data_week
data_week <- data_week[,-c(1,19,20,22)]  # Drop week, Total Investment, Units, Prepaid

data_week_nrm <- cbind(gmv=data_week$gmv,as.data.frame(scale(data_week[,-2],center = TRUE)))

mdl      <- lm(gmv~., data=data_week)

#' **Optimized Model:**
step_mdl <- stepAIC(mdl,direction='both',trace=FALSE,k=2)

#' **Summary:**
stargazer(mdl,step_mdl,type = 'text',no.space = TRUE)


# plot(data_week$gmv)
pred_lm <- predict(step_mdl,data_week[,-2])
plot(data_week$gmv)
lines(data_week$gmv)
lines(pred_lm,lty=1,col='magenta')





#+ Regularize Linear Model
x = as.matrix(data_week[,-c(1,2,18)])
y = as.vector(data_week$gmv)

#+ Feature selection with L1 regularization
cv.out <- cv.glmnet(as.matrix(x),as.vector(y),alpha=0)
min_lambda <- cv.out$lambda.min
ridge.mod <- glmnet(x,y,alpha=0,lambda = min_lambda)

plot(cv.out)
ridge.pred <- predict(ridge.mod,s= min_lambda,newx=x)

# MSE 
mean((ridge.pred-y)^2)
actual <- y
predict <- ridge.pred
R2 <- 1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2))
R2

ridge.mod$a0
ridge.mod$beta



#+ Linear with L2 regularization

# min lambda
cv.out <- cv.glmnet(as.matrix(x),as.vector(y),alpha=1)
min_lambda <- cv.out$lambda.min
lasso.mod <- glmnet(x,y,alpha=1,lambda = min_lambda)

plot(cv.out)
lasso.pred <- predict(lasso.mod,s= min_lambda,newx=x)

# MSE 
mean((lasso.pred-y)^2)
actual <- y
predict <- lasso.pred
R2 <- 1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2))
R2

lasso.mod$beta




#+ Model Comparison:
matplot(data_week$week, cbind(data_week$gmv,pred_lm,ridge.pred,lasso.pred),
        type='l',lwd=3,xlab='week',ylab='GMV')
legend("topright", inset=0, legend=c('GMV','LM','LM + L1 ','LM + L2 '),
       lty=c(1:4), col=c(1,2,3,4))


#+ Base Line Sales:
plot(data_week$week,data_week$gmv,lwd=2,xlab='week',ylab='GMV, predicted Base GMV')
lines(data_week$week,data_week$gmv,lwd=2)
abline(step_mdl,col='red',lty=3,lwd=2)
abline(h=ridge.mod$a0,col='green',lty=4,lwd=2)
abline(lasso.mod,col='blue',lty=2,lwd=2)
legend('topright', inset = 0, legend = c('GMV','LM Base GMV','LM + L1 Base GMV','LM + L2 Base GMV'), 
       lty = c(1:4), col=c(1,2,3,4), lwd = 2)






#' \vspace{8pt}
#' \vspace{8pt}
#' **Observations:**
#' Linear model could explain 77% of revenue from promotions, marketing spend on various channels





#+ Multiplicative Model ----
#' \vspace{8pt}
#' ## **Multiplicative Model:**
#' \vspace{8pt}
#' **Model Including All Variables:**
#' \vspace{8pt}
#' will convert multiplicative nature to linear by apply natural log transformation on all KPI
data_week_cpy <- cbind(data_week[,c(1,2)], data_week[,-c(1,2)]+0.00001)
data_week_log <- as.data.frame(log(data_week_cpy))


mdl      <- lm(gmv~., data=data_week_log)  # drop units and Total Investment

#' **Optimized Model:**
step_mdl <- stepAIC(mdl,direction='both',trace=FALSE,k=2)

#' **Summary:**
stargazer(mdl,step_mdl,type = 'text',no.space = TRUE)
viewModelSummaryVIF(step_mdl)

#' \vspace{8pt}
#' \vspace{8pt}
#' **Observations:**
#' Linear model could explain 77% of revenue from promotions, marketing spend on various channels

lm(formula = gmv ~ week + discount + sla + procurement_sla + 
     n_saledays + TV + Digital + OnlineMarketing + Affiliates + 
     SEM + NPS + list_mrp, data = data_week_log)

#+ -Digital ----
mdl2 <- lm(formula = gmv ~ week + discount + sla + procurement_sla + 
             n_saledays + TV + OnlineMarketing + Affiliates + 
             SEM + NPS + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -NPS ----
mdl2 <- lm(formula = gmv ~ week + discount + sla + procurement_sla + 
             n_saledays + TV + OnlineMarketing + Affiliates + 
             SEM + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -OnlineMarketing ----
mdl2 <- lm(formula = gmv ~ week + discount + sla + procurement_sla + 
             n_saledays + TV + Affiliates + 
             SEM + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)


#+ -week ----
mdl2 <- lm(formula = gmv ~  discount + sla + procurement_sla + 
             n_saledays + TV + Affiliates + 
             SEM + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -discount ----
mdl2 <- lm(formula = gmv ~  sla + procurement_sla + 
             n_saledays + TV + Affiliates + 
             SEM + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -discount ----
mdl2 <- lm(formula = gmv ~ n_saledays + TV + Affiliates + discount +
             SEM + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)



#+ -Affiliates ----
mdl2 <- lm(formula = gmv ~ week + discount + sla + procurement_sla + 
             n_saledays + TV + 
             SEM + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -Discount ----
mdl2 <- lm(formula = gmv ~ week + sla + procurement_sla + 
             n_saledays + TV + 
             SEM + list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -SEM ----
mdl2 <- lm(formula = gmv ~ week + sla + procurement_sla + 
             n_saledays + TV + 
             list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -week ----
mdl2 <- lm(formula = gmv ~ sla + procurement_sla + 
             n_saledays + TV + 
             list_mrp, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

#+ -list_mrp ----
mdl2 <- lm(formula = gmv ~ sla + procurement_sla + 
             n_saledays + TV, data = data_week_log)
summary(mdl2)
viewModelSummaryVIF(mdl2)

pred <- predict(step_mdl,data_week_log)
eprep <- exp(pred)
plot(data_week$week,data_week$gmv)
lines(data_week$week,eprep)




#+ Koyck Model ----
#' **Koyck Model:**
#' \vspace{8pt}
data_week_km <- data_week
#data_week_km$gmv_lag1 <- shift(data_week_km$gmv,shiftBy = -1)
str(data_week_km)
#data_week_km <- as.data.frame(data_week_km)
data_week_nrm <- as.data.frame(scale(data_week_km,center = TRUE))

mdl <- lm(gmv~.,data=data_week_nrm)
summary(mdl)
viewModelSummaryVIF(mdl)

step_mdl <- stepAIC(mdl,direction = 'both',trace = FALSE)
summary(step_mdl)
viewModelSummaryVIF(step_mdl)


mdl2 <- lm(formula = gmv ~ product_mrp  + procurement_sla + n_saledays + 
             Digital + Sponsorship + OnlineMarketing + Affiliates + 
             SEM + Radio + Other + NPS + list_mrp, data = data_week_km)
summary(mdl2)
viewModelSummaryVIF(mdl2)


data_week_km <- data %>% group_by(week) %>% 
  summarise(gmv=sum(gmv),product_mrp=mean(product_mrp),
            discount=mean(discount),sla=mean(sla),procurement_sla=mean(procurement_sla),
            n_saledays=mean(n_saledays),NPS=mean(NPS),TV=mean(TV),Digital=mean(Digital),
            Sponsorship=mean(Sponsorship),ContentMarketing=mean(ContentMarketing),
            OnlineMarketing=mean(OnlineMarketing),Affiliates=mean(Affiliates),
            SEM=mean(SEM),Radio=mean(Radio),Other=mean(Other),
            list_mrp=mean(list_mrp))
data_week_km$gmv_lag1 <- shift(data_week_km$gmv,shiftBy = -1)
data_week_km$gmv_lag1[1] <- data_week_km$gmv[1]
data_week_km$gmv_lag2 <- shift(data_week_km$gmv,shiftBy = -2)
data_week_km$gmv_lag2[1] <- data_week_km$gmv[1]
data_week_km$gmv_lag2[2] <- data_week_km$gmv[2]
data_week_km$gmv_lag3 <- shift(data_week_km$gmv,shiftBy = -3)
data_week_km$gmv_lag3[1] <- data_week_km$gmv[1]
data_week_km$gmv_lag3[2] <- data_week_km$gmv[2]
data_week_km$gmv_lag3[3] <- data_week_km$gmv[3]


mdl <- lm(gmv~.,data=data_week_km)
summary(mdl)
viewModelSummaryVIF(mdl)

step_mdl <- stepAIC(mdl,direction = 'both',trace = FALSE)
summary(step_mdl)
viewModelSummaryVIF(step_mdl)




#+ Distributed Lag ----
#' **Distribute Lag Model:**
#' \vspace{8pt}

data_week_km <- data %>% group_by(week) %>% 
  summarise(gmv=sum(gmv),product_mrp=mean(product_mrp),
            discount=mean(discount),sla=mean(sla),procurement_sla=mean(procurement_sla),
            n_saledays=mean(n_saledays),NPS=mean(NPS),TV=mean(TV),Digital=mean(Digital),
            Sponsorship=mean(Sponsorship),ContentMarketing=mean(ContentMarketing),
            OnlineMarketing=mean(OnlineMarketing),Affiliates=mean(Affiliates),
            SEM=mean(SEM),Radio=mean(Radio),Other=mean(Other),
            list_mrp=mean(list_mrp))
data_week_nrm = as.data.frame(scale(data_week_km,center = TRUE))
drtb_data <- cbind(data_week_nrm[,c(1,3)],
                   distributedLag(data_week_nrm[,-c(1,3)])
)


mdl <- lm(gmv~.,data=drtb_data[-c(1,2,3),])
summary(mdl)
viewModelSummaryVIF(mdl)

step_mdl <- stepAIC(mdl,direction = 'both',trace = FALSE)
summary(step_mdl)
viewModelSummaryVIF(step_mdl)





#+ Back Up ----
#pred <- predict(mdl,data_week[,-2])
#step_pred <- predict(step_mdl,data_week[,-2])

#plot(data_week$week,data_week$gmv)
#lines(data_week$week,pred)
#lines(data_week$week,step_pred,lwd=2)





