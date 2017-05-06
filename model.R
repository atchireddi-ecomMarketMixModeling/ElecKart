
```{r, warning=FALSE, message=FALSE, title:'MarketMixModeling_Model',}

# ***************************************************************************
#                   Load Library ----
# ***************************************************************************
library(MASS)


# ***************************************************************************
#                   LOad Data ----
# ***************************************************************************
camera_accessory_data_nrm <- read.csv('./intrim/cameraAccessory.csv')
home_audio_data_nrm <-       read.csv('./intrim/homeAudio.csv')
gaming_accessory_data_nrm <- read.csv('./intrim/gamingAccessory')


# ***************************************************************************
#                        LINEAR MODEL : Camera_accessory ----
# ***************************************************************************

# do we really need to predict something, 
# I feel, we are trying to explain effects of indenpendant variables
# set.seed(100)    
indices=sample(1:nrow(camera_accessory_data_nrm),1.0*nrow(camera_accessory_data_nrm))
train=camera_accessory_data_nrm[indices,]
test=camera_accessory_data_nrm[-indices,]

# Initial Model
model_cam1 <- lm(gmv~ .,data=train)

# summary(model_cam1)
step_cam <- stepAIC(model_cam1, direction = "both",trace=FALSE,k=2)


summary(step_cam)





# ***************************************************************************
#                        LINEAR MODEL : gaming_accessory ----
# ***************************************************************************

indices=sample(1:nrow(home_audio_data_nrm),0.7*nrow(home_audio_data_nrm))
train=home_audio_data_nrm[indices,]
test=home_audio_data_nrm[-indices,]

# Initial Model
model_ga1 <- lm(gmv~ .,data=train)

# summary(model_ga1)
step_ga <- stepAIC(model_ga1, direction = "both",trace=FALSE)

summary(step_ga)






# ***************************************************************************
#                        LINEAR MODEL : home_audio ----
# ***************************************************************************

indices=sample(1:nrow(gaming_accessory_data_nrm),0.7*nrow(gaming_accessory_data_nrm))
train=gaming_accessory_data_nrm[indices,]
test=gaming_accessory_data_nrm[-indices,]

# Initial Model
model_ha1 <- lm(gmv~ .,data=train)

# summary(model_ha1)
step_ha <- stepAIC(model_ha1, direction = "both",trace=FALSE)

summary(step_ha)



```