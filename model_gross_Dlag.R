
#+ warning=FALSE, message=FALSE
library(MASS)
library(car)
library(DataCombine)   # Pair wise correlation
library(stargazer)
library(dplyr)         # Data aggregation
library(glmnet)
source('./code/atchircUtils.R')


data    <- read.csv('./intrim/eleckart.csv')

data_week <- data %>% group_by(week) %>% 
  summarise(gmv=sum(gmv),
            product_mrp=mean(product_mrp),
            discount=mean(discount),
            sla=mean(sla),
            procurement_sla=mean(procurement_sla),
            n_saledays=mean(n_saledays),
            TV=mean(TV),
            Digital=mean(Digital),
            Sponsorship=mean(Sponsorship),
            ContentMarketing=mean(ContentMarketing),
            OnlineMarketing=mean(OnlineMarketing),
            Affiliates=mean(Affiliates),SEM=mean(SEM),
            Radio=mean(Radio),
            Other=mean(Other),
            TotalInvestment=mean(TotalInvestment),
            NPS=mean(NPS),
            list_mrp=mean(list_mrp),
            units=sum(units),
            COD=sum(COD),
            Prepaid=sum(Prepaid))

data_week[,c(8:17)] <- data_week[,c(8:17)]*10000000

# Prune, Insignificant variables
# week, sla, procurement_sla, content.marketing, Total Investment,
# units, radio, digital, product_mrp, prepaid, n_saledays
data_week$chnglist <- c(0,diff(data_week$list_mrp))
data_week$chngdisc <- c(0,diff(data_week$discount))
data_week$chngNPS <- c(0,diff(data_week$NPS))
model_data <- subset(data_week, select= -c(product_mrp,sla,procurement_sla,
                              ContentMarketing,Affiliates,discount,
                              Radio,TotalInvestment,list_mrp,units,NPS,
                              COD,Prepaid))

model_data_org <- model_data

# Log all variables
model_data_lag <- cbind(head(head(model_data,-1)),
                        head(tail(model_data,-1)))
lag <- head(subset(model_data,select = -week),-1)   # chop tail, don't lag week
org <- tail(model_data,-1)   # chop head
colnames(lag) <- sub('^','lag',colnames(lag))
model_data <- cbind(org,lag)

#' \newpage
#' **PROCs:**


#' \vspace{8pt}
#' Set Class definitions
setOldClass('elnet')
setClass(Class = 'atcglmnet',
         representation (
           R2 = 'numeric',
           mdl = 'elnet',
           pred = 'matrix'
         )
)

#' \vspace{8pt}
setOldClass('lm')
setClass(Class = 'atclm',
         representation (
           R2 = 'numeric',
           mdl = 'lm',
           pred = 'matrix'
         )
)


#' \vspace{8pt}
#' Finding min lambda from 1000 iterations
#' \vspace{8pt}
findMinLambda <- function(x,y,alpha,folds) {
  lambda_list <- list()
  for (i in 1:1000) {
    cv.out <- cv.glmnet(as.matrix(x), as.vector(y), alpha=alpha,
                        nfolds=folds)
    lambda_list <- append(lambda_list, cv.out$lambda.min)
  }
  return(min(unlist(lambda_list)))
}


#' \vspace{8pt}
#' Linear Model with Regularization
#' \vspace{8pt}
atcLmReg <- function(x,y,l1l2,folds) {
  # l1l2 = 0 for L1,  1 for L2
  
  if (l1l2) { # Lasso/L2
    min_lambda <- findMinLambda(x,y,1,folds)
  } else { # Ridge/L1
    min_lambda <- findMinLambda(x,y,0,folds)
  }
  mdl        <- glmnet(x,y,alpha=l1l2,lambda = min_lambda)
  pred       <- predict(mdl,s= min_lambda,newx=x)
  
  # MSE 
  mean((pred-y)^2)
  R2 <- 1 - (sum((y-pred )^2)/sum((y-mean(pred))^2))
  return(new('atcglmnet', R2 = R2, mdl=mdl, pred=pred))
}



#' \newpage
#' **Linear Model:**
mdl      <- lm(gmv~., data=model_data)
step_mdl <- stepAIC(mdl,direction = 'both',trace = FALSE)

stargazer(mdl,step_mdl, align = TRUE, type = 'text',
          title='Linear Regression Results', single.row=TRUE)
knitr::kable(viewModelSummaryVIF(step_mdl))
pred_lm <- predict(step_mdl, model_data)


#' \newpage
#+ Regularize Linear Model
#' **Regularized Linear Model:**
x = as.matrix(subset(model_data, select=-gmv))
y = as.vector(model_data$gmv)

ridge_out <- atcLmReg(x,y,0,3)  # x, y, alpha, nfolds
lasso_out <- atcLmReg(x,y,1,3)  # x, y, alpha, nfolds


#' \newpage
#' **Plot Model prediction and base sales:**
plot(model_data$gmv)
lines(model_data$gmv)
lines(pred_lm,col='red',lwd=2)
lines(ridge_out@pred,col='green',lwd=2)
lines(lasso_out@pred,col='blue',lwd=2)
lines(step_mdl$coefficients['(Intercept)']+step_mdl$coefficients['week']*data_week$week,
      lty=2,lwd=2,col='red')
lines(ridge_out@mdl$a0+ridge_out@mdl$beta['week',1]*data_week$week,
      lty=2,lwd=2,col='green')
lines(lasso_out@mdl$a0+lasso_out@mdl$beta['week',1]*data_week$week,
      lty=2,lwd=2,col='blue')
legend('topright',inset=0, legend=c('GMV','LM','LM+L1','LM+L2'),horiz = TRUE,
       lwd = 2, col=c(1:4), cex = 0.5)


#' **Model Coefficients:**

coeff_lm <- as.data.frame(as.matrix(coef(step_mdl)))
coeff_l1 <- as.data.frame(as.matrix(coef(ridge_out@mdl)))                      
coeff_l2 <- as.data.frame(as.matrix(coef(lasso_out@mdl))) 


lm_df=data.frame('x'=rownames(coeff_lm),'y'=coeff_lm)
colnames(lm_df) = c('coeff','lm')
l1_df=data.frame('x'=rownames(coeff_l1),'y'=coeff_l1)
colnames(l1_df)= c('coeff','l1')
l2_df=data.frame('x'=rownames(coeff_l2),'y'=coeff_l2)
colnames(l2_df) <- c('coeff','l2')

smry <- merge(lm_df,l1_df,all = TRUE)
smry <- merge(smry,l2_df,all=TRUE)

print(smry)

ridge_out@R2
lasso_out@R2

# coeff           lm            l1            l2
# 1      (Intercept) 8.276708e+06  1.207175e+07  1.035005e+07
# 2         chngdisc 3.093868e+06  3.222043e+06  3.433271e+06
# 3         chnglist           NA  3.962095e+03  4.459924e+03
# 4          chngNPS           NA -3.241382e+06 -3.578603e+06
# 5        chngOnMar           NA -9.577344e-01 -1.176479e+00
# 6         chngSpon           NA -3.105531e-02 -2.971396e-02
# 7       n_saledays 1.009153e+07  8.803587e+06  9.406754e+06
# 8  OnlineMarketing 6.778385e-01  6.297936e-01  6.829377e-01
# 9            Other           NA  8.637926e-02  1.293520e-01
# 10             SEM           NA  8.986663e-02 -3.682937e-02
# 11     Sponsorship 2.674285e-01  2.285167e-01  2.909754e-01
# 12              TV           NA  3.992118e-01  7.547185e-02
# 13            week 5.904736e+05  4.395925e+05  4.199697e+05


