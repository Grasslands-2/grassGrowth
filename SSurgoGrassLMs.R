setwd("/Volumes/GoogleDrive/Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/Soil Data/SSURGO data")

library(dplyr)
library(caret)
library(tidyr)
library(gt)
library(data.table)
library(MASS)

crops <- read.table("prod_index_soil.txt", header = TRUE, sep = ",")


levels(crops$cropname)

grass <- crops %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Bromegrass-alfalfa hay"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  drop_na() %>%
  droplevels()

#grass_sum <- grass %>%
 # group_by(cropname) %>%
#  summarise(meanYield = round(mean(nonirryield.r),2),
 #           minYield = min(nonirryield.r),
  #          maxYield = max(nonirryield.r),
   #         count = n())

######### Tables for each grass-legume combo with county data ################################


grass %>%
  filter(cropname == "Bluegrass-white clover") %>%
  #group_by(Region) %>%
  summarise(minYield = min(nonirryield.r),
            meanYield = round(mean(nonirryield.r),2),
            maxYield = max(nonirryield.r),
            count = n()) %>%
  gt() %>%
  tab_header(title = "Bluegrass-white clover")

grass %>%
  filter(cropname == "Bromegrass-alfalfa hay") %>%
  #group_by(Region) %>%
  summarise(minYield = min(nonirryield.r),
            meanYield = round(mean(nonirryield.r),2),
            maxYield = max(nonirryield.r),
            count = n()) %>%
  gt()  %>%
  tab_header(title = "Bromegrass-alfalfa hay")


grass %>%
  filter(cropname == "Orchardgrass-alsike") %>%
  #group_by(Region) %>%
  summarise(minYield = min(nonirryield.r),
            meanYield = round(mean(nonirryield.r),2),
            maxYield = max(nonirryield.r),
            count = n()) %>%
  gt()  %>%
  tab_header(title = "Orchardgrass-alsike")

grass %>%
  filter(cropname == "Orchardgrass-red clover") %>%
  #group_by(Region) %>%
  summarise(minYield = min(nonirryield.r),
            meanYield = round(mean(nonirryield.r),2),
            maxYield = max(nonirryield.r),
            count = n()) %>%
  gt()  %>%
  tab_header(title = "Orchardgrass-red clover")

grass %>%
  filter(cropname == "Timothy-alsike") %>%
  #group_by(Region) %>%
  summarise(minYield = min(nonirryield.r),
            meanYield = round(mean(nonirryield.r),2),
            maxYield = max(nonirryield.r),
            count = n()) %>%
  gt()  %>%
  tab_header(title = "Timothy-alsike")

##################################################################
####### Select columns for analysis #############################

#colnames(grass)
#grass <- grass %>%
 # dplyr::select(c(cropname, nonirryield.r, slope.r, airtempa.r, map.r, ffd.r, elev.r, sand, silt, om, ksat, cec, ph, awc, total.depth, Region)) %>%
  #dplyr::rename(yield = nonirryield.r, slope = slope.r, temp = airtempa.r, precip = map.r, elev = elev.r, ffd = ffd.r, depth = total.depth)

#summary(grass)
#ggpairs(data = grass[,c(3:16)])

#set.seed(0731)
#inTrain <- createDataPartition(grass$yield, p = 0.7, list = FALSE)
#train <- grass[inTrain,]
#test<- grass[-inTrain,]

#hist(grass$yield)
#hist(sqrt(grass$yield))

#best.guess <- round(mean(grass$yield),2) 
#RMSE.baseline <- round(sqrt(mean((best.guess-test$yield)^2)),2)

#train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times

#full.model <- train(yield~., data = train, method = "lm", trControl = train.control)
#full.pred <- predict(full.model, test)
#RMSE.full <- round(sqrt(mean((full.pred-test$yield)^2)),3)
#adj.rsquared.full <- round((summary(full.model)$adj.r.squared),3)
#rsquared.full <- round((summary(full.model)$r.squared),3)
#full.model
#plot(full.pred ~ test$yield)
#summary(full.model)
#models <- data.frame(Model = "Full Model", RMSE = RMSE.full, Rsquared = rsquared.full, adj.Rsquared = adj.rsquared.full)
#models

#noRegion <- train(yield~. - Region, data = train, method = "lm", trControl = train.control)
#noRegion.pred <- predict(noRegion, test)
#RMSE.noRegion<- round(sqrt(mean((noRegion.pred-test$yield)^2)),3)
#plot(noRegion.pred ~ test$yield)
#noRegion
#summary(noRegion)

#no.sand <- train(yield~. - sand, data = train, method = "lm", trControl = train.control)
#no.sand
#nosand.pred <- predict(no.sand, test)
#RMSE.nosand <- round(sqrt(mean((nosand.pred-test$yield)^2)),3)
#adj.rsquared.nosand <- round((summary(no.sand)$adj.r.squared),3)
#rsquared.nosand <- round((summary(no.sand)$r.squared),3)

#no.silt <- train(yield~. - silt, data = train, method = "lm", trControl = train.control)
#no.silt
#nosilt.pred <- predict(no.silt, test)
#RMSE.nosilt <- round(sqrt(mean((nosilt.pred-test$yield)^2)),3)
#adj.rsquared.nosilt <- round((summary(no.silt)$adj.r.squared),3)
#rsquared.nosilt <- round((summary(no.silt)$r.squared),3)

######################################################################
##### Remove sand ####################################################

#grass <- grass %>%
 # dplyr::select(-sand) 

#set.seed(0731)
#inTrain <- createDataPartition(grass$yield, p = 0.7, list = FALSE)
#train <- grass[inTrain,]
#test<- grass[-inTrain,]

#full.model <- train(yield~., data = train, method = "lm", trControl = train.control)
#full.pred <- predict(full.model, test)
#RMSE.full <- round(sqrt(mean((full.pred-test$yield)^2)),3)
#adj.rsquared.full <- round((summary(full.model)$adj.r.squared),3)
#rsquared.full <- round((summary(full.model)$r.squared),3)
#models <- data.frame(Model = "Full Model", RMSE = RMSE.full, Rsquared = rsquared.full, adj.Rsquared = adj.rsquared.full)
#models

#as.matrix(car::vif(full.model$finalModel))

############################################################################
#######  remove AWC #######################################################

#grass <- grass %>%
 # dplyr::select(-awc) 

#set.seed(0731)
#inTrain <- createDataPartition(grass$yield, p = 0.7, list = FALSE)
#train <- grass[inTrain,]
#test<- grass[-inTrain,]

#noawc.model <- train(yield~., data = train, method = "lm", trControl = train.control)
#noawc.pred <- predict(noawc.model, test)
#RMSE.noawc <- round(sqrt(mean((noawc.pred-test$yield)^2)),3)
#adj.rsquared.noawc <- round((summary(noawc.model)$adj.r.squared),3)
#rsquared.noawc <- round((summary(noawc.model)$r.squared),3)
#summary(noawc.model)

#as.matrix(car::vif(noawc.model$finalModel))

################################################################################################################
## models will start with: cropname, Region, slope, temp, precip, ffd, elev, silt, om, ksat, k, cec, ph, depth
################################################################################################################
#set.seed(0731)
#inTrain <- createDataPartition(grass$yield, p = 0.75, list = FALSE)
#train <- grass[inTrain,]
#test<- grass[-inTrain,]

#nointerx.lm <- lm(yield~., data = train)
#stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
#stepForward$anova
#length(stepForward$coefficients) #113
#stepForward.model <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
#                             om + ksat + cec + ph + depth + Region + slope:silt + ksat:depth + 
#                             slope:Region + precip:om + precip:Region + cropname:silt + 
#                             ph:Region + temp:om + silt:cec + ksat:cec + slope:ksat + 
#                             precip:ksat + elev:Region + precip:silt + slope:temp + precip:cec + 
#                             precip:ph + cropname:cec + precip:elev + slope:om + cec:ph + 
#                             silt:ph + elev:om + ffd:ksat + cropname:precip + slope:cec + 
#                             cec:Region + silt:Region + temp:ph + om:cec + ksat:Region + 
#                             ffd:ph + om:Region + elev:depth + precip:depth + slope:depth + 
#                             slope:ffd + om:ph + silt:om + elev:cec + elev:ph + silt:depth + 
#                             ffd:om + temp:elev + ffd:elev + ffd:cec + cec:depth + depth:Region + 
#                             ph:depth + ffd:depth + precip:ffd + cropname:slope + slope:precip + 
#                             elev:ksat + cropname:depth + ffd:Region + temp:precip + temp:depth + 
#                             temp:cec,
#                          data = train, method = "lm", trControl = train.control)
#forward.pred <- predict(stepForward.model, test)
#RMSE.forward <- round(sqrt(mean((forward.pred - test$yield)^2)),3)
#adj.rsquared.forward <- round((summary(stepForward.model)$adj.r.squared),3)
#rsquared.forward <- round((summary(stepForward.model)$r.squared),3)
#plot(forward.pred~ test$yield) 
#abline(a = 0, b = 1)
#title("Forward Selection with Region")
#stepForwardLength <- length(coef(stepForward.model$finalModel))
#models <- data.frame(Model = "Region Step Forward", RMSE = RMSE.forward, Rsquared = rsquared.forward, adj.Rsquared = adj.rsquared.forward, no.coef = stepForwardLength)
#models


#allinterx.lm <- lm(yield~.^2,data = train)
#stepBack <- stepAIC(allinterx.lm, direction = "backward")
#stepBack$anova
#length(stepBack$coefficients) #107
#stepBack.mod <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
#                        om + ksat + cec + ph + depth + Region + cropname:slope + 
#                        cropname:ksat + cropname:cec + cropname:depth + slope:temp + 
#                        slope:precip + slope:silt + slope:om + slope:ksat + slope:cec + 
#                        slope:depth + slope:Region + temp:precip + temp:elev + temp:om + 
#                        temp:cec + temp:ph + temp:depth + precip:ffd + precip:elev + 
#                        precip:silt + precip:om + precip:ksat + precip:cec + precip:depth + 
#                        precip:Region + ffd:elev + ffd:om + ffd:ksat + ffd:cec + 
#                        ffd:ph + ffd:depth + ffd:Region + elev:om + elev:ksat + elev:cec + 
#                        elev:ph + elev:depth + elev:Region + silt:om + silt:cec + 
#                        silt:ph + silt:depth + silt:Region + om:cec + om:ph + om:Region + 
#                        ksat:cec + ksat:depth + ksat:Region + cec:ph + cec:depth + 
#                        cec:Region + ph:depth + ph:Region + depth:Region,
#                      data = train, method = "lm", trControl = train.control)
#stepBack.mod
#back.pred <- predict(stepBack.mod, test)
#RMSE.back <- round(sqrt(mean((back.pred - test$yield)^2)),3)
#adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
#rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
#stepBackLength <- length(coef(stepBack.mod$finalModel))
#par(mfrow=c(1,2))
#plot(forward.pred~ test$yield) 
#abline(a = 0, b = 1)
#title("Forward Selection with Region")
#plot(back.pred~ test$yield) 
#abline(a = 0, b = 1)
#title("Backward selection with Region")
#summary(stepBack.mod)
#models <- add_row(models, Model = "Region Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
#                  no.coef = stepBackLength)
#models

####################################################################
#remove region
#grass <- grass %>%
#  dplyr::select(-Region)

#set.seed(0731)
#inTrain <- createDataPartition(grass$yield, p = 0.75, list = FALSE)
#train <- grass[inTrain,]
#test<- grass[-inTrain,]

#nointerx.lm <- lm(yield~., data = train)
#stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
#stepForward$anova
#length(stepForward$coefficients) #78
#stepForward.model <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
#                             om + ksat + cec + ph + depth + slope:silt + ksat:depth + 
#                             slope:temp + silt:cec + cropname:precip + precip:om + precip:cec + 
#                             temp:silt + cec:ph + om:ph + cropname:cec + precip:ph + precip:silt + 
#                             silt:ph + slope:cec + temp:cec + precip:elev + slope:ksat + 
#                             ksat:ph + precip:ksat + ffd:cec + temp:ph + slope:elev + 
#                             silt:om + temp:precip + temp:elev + ph:depth + elev:om + 
#                             ksat:cec + ffd:ksat + slope:om + ffd:om + elev:cec + elev:silt + 
#                             cec:depth + silt:depth + om:cec + slope:depth + silt:ksat + 
#                             cropname:slope + cropname:ksat + slope:precip + elev:ph + 
#                             elev:depth + precip:depth + temp:ksat + temp:ffd + ffd:elev + 
#                             ffd:ph + precip:ffd,
#                           data = train, method = "lm", trControl = train.control)
#forward.pred <- predict(stepForward.model, test)
#RMSE.forward <- round(sqrt(mean((forward.pred - test$yield)^2)),3)
#adj.rsquared.forward <- round((summary(stepForward.model)$adj.r.squared),3)
#rsquared.forward <- round((summary(stepForward.model)$r.squared),3)
#stepForwardLength <- length(coef(stepForward.model$finalModel))
#models <- add_row(models, Model = "No Region Step Forward", RMSE = RMSE.forward, Rsquared = rsquared.forward, adj.Rsquared = adj.rsquared.forward, no.coef = stepForwardLength)
#models


#allinterx.lm <- lm(yield~.^2,data = train)
#stepBack <- stepAIC(allinterx.lm, direction = "backward")
#stepBack$anova
#length(stepBack$coefficients) #78
#stepBack.mod <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
#                        om + ksat + cec + ph + depth + cropname:slope + cropname:ksat + 
#                        cropname:cec + cropname:depth + slope:temp + slope:precip + 
#                        slope:silt + slope:om + slope:ksat + slope:cec + slope:depth + 
#                        temp:precip + temp:ffd + temp:elev + temp:silt + temp:cec + 
#                        temp:ph + temp:depth + precip:ffd + precip:elev + precip:silt + 
#                        precip:om + precip:ksat + precip:cec + precip:ph + precip:depth + 
#                        ffd:elev + ffd:silt + ffd:om + ffd:ksat + ffd:cec + ffd:ph + 
#                        ffd:depth + elev:silt + elev:om + elev:cec + elev:ph + elev:depth + 
#                        silt:om + silt:ksat + silt:cec + silt:ph + silt:depth + om:ph + 
#                        ksat:cec + ksat:ph + ksat:depth + cec:ph + cec:depth + ph:depth,
#                      data = train, method = "lm", trControl = train.control)
#stepBack.mod
#back.pred <- predict(stepBack.mod, test)
#RMSE.back <- round(sqrt(mean((back.pred - test$yield)^2)),3)
#adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
#rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
#stepBackLength <- length(coef(stepBack.mod$finalModel))
#par(mfrow=c(1,2))
#plot(forward.pred~ test$yield) 
#abline(a = 0, b = 1)
#title("Forward Selection without Region")
#plot(back.pred~ test$yield) 
#abline(a = 0, b = 1)
#title("Backward selection without Region")
#summary(stepBack.mod)
#models <- add_row(models, Model = "No Region Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
#                  no.coef = stepBackLength)
#models %>% gt()

#final.mod <- lm(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
#             om + ksat + cec + ph + depth + cropname:slope + cropname:ksat + 
#             cropname:cec + cropname:depth + slope:temp + slope:precip + 
#             slope:silt + slope:om + slope:ksat + slope:cec + slope:depth + 
#             temp:precip + temp:ffd + temp:elev + temp:silt + temp:cec + 
#             temp:ph + temp:depth + precip:ffd + precip:elev + precip:silt + 
#             precip:om + precip:ksat + precip:cec + precip:ph + precip:depth + 
#             ffd:elev + ffd:silt + ffd:om + ffd:ksat + ffd:cec + ffd:ph + 
#             ffd:depth + elev:silt + elev:om + elev:cec + elev:ph + elev:depth + 
#             silt:om + silt:ksat + silt:cec + silt:ph + silt:depth + om:ph + 
#             ksat:cec + ksat:ph + ksat:depth + cec:ph + cec:depth + ph:depth,
#           data = train)

################################################################################################
################# Do this again without bromegrass-alfalfa hay #########################################
###############################################################################################

train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times

grass <- crops %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  filter(cec < 100) %>%
  droplevels()

#### how many acres are accounted for?
## calculate the total acres for all the mukeys
total_acres <- grass %>%
  dplyr::select(c(mukey, muacres)) %>%
  distinct() 
sum(total_acres$muacres)

grass <- crops %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  dplyr::select(c(cropname, nonirryield.r, slope.r, airtempa.r, map.r, ffd.r, elev.r, 
                  silt, om, ksat, cec, ph, total.depth, WI_CCPI_rating, NCCPI_rating)) %>%
  dplyr::rename(yield = nonirryield.r, slope = slope.r, temp = airtempa.r, precip = map.r, 
                elev = elev.r, ffd = ffd.r, depth = total.depth) %>%
  filter(cec < 100) %>% # observations are likely incorrect
  droplevels() %>%
  drop_na()

dim(grass) # 5616 # 13


set.seed(0731)
inTrain <- createDataPartition(grass$yield, p = 0.75, list = FALSE)
train <- grass[inTrain,] # 4217 obs
test<- grass[-inTrain,] # 1403 obs

nointerx.lm <- lm(yield~., data = train)
stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
stepForward$anova
length(stepForward$coefficients) #97
stepForward.model <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
                             om + ksat + cec + ph + depth + Region + slope:silt + slope:temp + 
                             silt:cec + ph:Region + precip:Region + precip:om + temp:om + 
                             precip:cec + precip:silt + slope:ksat + precip:ph + ksat:depth + 
                             precip:ksat + ksat:cec + cropname:silt + depth:Region + temp:silt + 
                             ffd:ksat + slope:Region + cec:depth + silt:depth + ffd:depth + 
                             elev:ph + slope:om + precip:elev + precip:ffd + cec:ph + 
                             silt:ph + temp:cec + slope:cec + om:ph + silt:om + slope:precip + 
                             elev:Region + elev:silt + temp:ph + ffd:cec + temp:Region + 
                             elev:ksat + om:Region + elev:cec + cropname:slope + slope:ffd + 
                             slope:depth + ph:depth + elev:om + temp:elev + cropname:ffd + 
                             ffd:ph + ffd:elev + ffd:Region + ffd:silt + temp:depth + 
                             ffd:om + cropname:om,
                           data = train, method = "lm", trControl = train.control)
stepForward.model
forward.pred <- predict(stepForward.model, test)
RMSE.forward <- round(sqrt(mean((forward.pred - test$yield)^2)),3)
adj.rsquared.forward <- round((summary(stepForward.model)$adj.r.squared),3)
rsquared.forward <- round((summary(stepForward.model)$r.squared),3)
stepForwardLength <- length(coef(stepForward.model$finalModel))
models <- data.frame(Model = "Region Step Forward", RMSE = RMSE.forward, Rsquared = rsquared.forward, adj.Rsquared = adj.rsquared.forward, no.coef = stepForwardLength)
models


allinterx.lm <- lm(yield~.^2,data = train)
stepBack <- stepAIC(allinterx.lm, direction = "backward")
stepBack$anova
length(stepBack$coefficients) #102
stepBack.mod <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
                        om + ksat + cec + ph + depth + Region + cropname:slope + 
                        cropname:ffd + cropname:silt + cropname:om + slope:temp + 
                        slope:precip + slope:ffd + slope:silt + slope:om + slope:ksat + 
                        slope:cec + slope:depth + slope:Region + temp:precip + temp:elev + 
                        temp:silt + temp:cec + temp:ph + temp:depth + temp:Region + 
                        precip:elev + precip:silt + precip:om + precip:ksat + precip:cec + 
                        precip:ph + precip:Region + ffd:elev + ffd:silt + ffd:om + 
                        ffd:ksat + ffd:cec + ffd:ph + ffd:depth + ffd:Region + elev:silt + 
                        elev:om + elev:ksat + elev:cec + elev:ph + elev:depth + elev:Region + 
                        silt:om + silt:cec + silt:ph + silt:depth + silt:Region + 
                        om:ksat + om:ph + ksat:cec + ksat:ph + ksat:depth + cec:ph + 
                        cec:depth + cec:Region + ph:depth + ph:Region + depth:Region,
                      data = train, method = "lm", trControl = train.control)
stepBack.mod
back.pred <- predict(stepBack.mod, test)
RMSE.back <- round(sqrt(mean((back.pred - test$yield)^2)),3)
adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
stepBackLength <- length(coef(stepBack.mod$finalModel))
par(mfrow=c(1,2))
plot(forward.pred~ test$yield) 
abline(a = 0, b = 1)
title("Forward Selection with Region - no Hay")
plot(back.pred~ test$yield) 
abline(a = 0, b = 1)
title("Backward selection with Region - no Hay")
summary(stepBack.mod)
models <- add_row(models, Model = "Region Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
                  no.coef = stepBackLength)
models

# final mod is Region - step backward?
final.mod.Region <- lm(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
                         om + ksat + cec + ph + depth + Region + cropname:slope + 
                         cropname:ffd + cropname:silt + cropname:om + slope:temp + 
                         slope:precip + slope:ffd + slope:silt + slope:om + slope:ksat + 
                         slope:cec + slope:depth + slope:Region + temp:precip + temp:elev + 
                         temp:silt + temp:cec + temp:ph + temp:depth + temp:Region + 
                         precip:elev + precip:silt + precip:om + precip:ksat + precip:cec + 
                         precip:ph + precip:Region + ffd:elev + ffd:silt + ffd:om + 
                         ffd:ksat + ffd:cec + ffd:ph + ffd:depth + ffd:Region + elev:silt + 
                         elev:om + elev:ksat + elev:cec + elev:ph + elev:depth + elev:Region + 
                         silt:om + silt:cec + silt:ph + silt:depth + silt:Region + 
                         om:ksat + om:ph + ksat:cec + ksat:ph + ksat:depth + cec:ph + 
                         cec:depth + cec:Region + ph:depth + ph:Region + depth:Region,
                      data = train)

####################################################################
#remove region
grass <- crops %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  dplyr::select(c(cropname, nonirryield.r, slope.r, airtempa.r, map.r, ffd.r, elev.r, silt, om, ksat, cec, ph, total.depth, WI_CCPI_rating, NCCPI_rating)) %>%
  dplyr::rename(yield = nonirryield.r, slope = slope.r, temp = airtempa.r, precip = map.r, elev = elev.r, ffd = ffd.r, depth = total.depth) %>%
  filter(cec < 100) %>% # observations are likely incorrect
  droplevels() %>%
  drop_na() 

set.seed(0731)
inTrain <- createDataPartition(grass$yield, p = 0.75, list = FALSE)
train <- grass[inTrain,]
test<- grass[-inTrain,]

nointerx.lm <- lm(yield~., data = train)
stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
stepForward$anova
length(stepForward$coefficients) #97
stepForward.model <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
                             om + ksat + cec + ph + depth + WI_CCPI_rating + NCCPI_rating + 
                             silt:NCCPI_rating + slope:ksat + precip:om + slope:temp + 
                             precip:cec + silt:cec + precip:silt + om:ph + temp:precip + 
                             slope:om + precip:ph + slope:NCCPI_rating + precip:ksat + 
                             precip:depth + silt:WI_CCPI_rating + elev:NCCPI_rating + 
                             elev:ph + cropname:silt + cec:ph + temp:cec + ffd:ksat + 
                             silt:depth + silt:ph + cec:depth + precip:elev + depth:NCCPI_rating + 
                             slope:cec + cropname:NCCPI_rating + om:NCCPI_rating + temp:WI_CCPI_rating + 
                             temp:ph + ph:depth + ksat:depth + ffd:silt + silt:om + ksat:ph + 
                             slope:silt + elev:silt + ffd:NCCPI_rating + om:cec + ffd:om + 
                             elev:om + elev:depth + ksat:NCCPI_rating + elev:cec + om:ksat + 
                             temp:elev + slope:elev + temp:NCCPI_rating + temp:silt + 
                             precip:NCCPI_rating + ffd:elev + temp:ffd + ffd:ph + WI_CCPI_rating:NCCPI_rating + 
                             precip:WI_CCPI_rating + cropname:slope + elev:WI_CCPI_rating + 
                             cec:WI_CCPI_rating + cec:NCCPI_rating + ffd:cec + ph:NCCPI_rating + 
                             slope:ph + slope:ffd + slope:precip + cropname:ksat + cropname:om + 
                             ksat:WI_CCPI_rating + elev:ksat + depth:WI_CCPI_rating,
                           data = train, method = "lm", trControl = train.control)
forward.pred <- predict(stepForward.model, test)
RMSE.forward <- round(sqrt(mean((forward.pred - test$yield)^2)),3)
adj.rsquared.forward <- round((summary(stepForward.model)$adj.r.squared),3)
rsquared.forward <- round((summary(stepForward.model)$r.squared),3)
stepForwardLength <- length(coef(stepForward.model$finalModel))
models <- data.frame(Model = "Crop Indices Step Forward", RMSE = RMSE.forward, Rsquared = rsquared.forward, adj.Rsquared = adj.rsquared.forward, no.coef = stepForwardLength)
models


allinterx.lm <- lm(yield~.^2,data = train)
stepBack <- stepAIC(allinterx.lm, direction = "backward")
stepBack$anova
length(stepBack$coefficients) #91
stepBack.mod <- train(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
                        om + ksat + cec + ph + depth + WI_CCPI_rating + NCCPI_rating + 
                        cropname:slope + cropname:silt + cropname:om + cropname:ksat + 
                        cropname:NCCPI_rating + slope:temp + slope:precip + slope:ffd + 
                        slope:elev + slope:silt + slope:om + slope:ksat + slope:cec + 
                        slope:ph + slope:NCCPI_rating + temp:precip + temp:ffd + 
                        temp:elev + temp:silt + temp:cec + temp:ph + temp:depth + 
                        temp:WI_CCPI_rating + temp:NCCPI_rating + precip:elev + precip:silt + 
                        precip:om + precip:ksat + precip:cec + precip:WI_CCPI_rating + 
                        precip:NCCPI_rating + ffd:elev + ffd:om + ffd:cec + ffd:ph + 
                        ffd:depth + elev:silt + elev:om + elev:cec + elev:depth + 
                        elev:WI_CCPI_rating + elev:NCCPI_rating + silt:om + silt:ksat + 
                        silt:cec + silt:ph + silt:depth + silt:WI_CCPI_rating + silt:NCCPI_rating + 
                        om:ksat + om:cec + om:ph + om:NCCPI_rating + ksat:ph + ksat:depth + 
                        ksat:NCCPI_rating + cec:ph + cec:depth + cec:WI_CCPI_rating + 
                        cec:NCCPI_rating + ph:depth + ph:NCCPI_rating + depth:NCCPI_rating + 
                        WI_CCPI_rating:NCCPI_rating,
                      data = train, method = "lm", trControl = train.control)
stepBack.mod
back.pred <- predict(stepBack.mod, test)
RMSE.back <- round(sqrt(mean((back.pred - test$yield)^2)),3)
adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
stepBackLength <- length(coef(stepBack.mod$finalModel))
par(mfrow=c(1,2))
plot(forward.pred~ test$yield) 
abline(a = 0, b = 1)
title("Forward Selection without Region - no Hay")
plot(back.pred~ test$yield) 
abline(a = 0, b = 1)
title("Backward selection without Region - no Hay")
summary(stepBack.mod)
models <- add_row(models, Model = "Crop Indices Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
                  no.coef = stepBackLength)
models %>% gt()

# final mod is no Region - step backward?
final.mod.noHay <- lm(yield ~ cropname + slope + temp + precip + ffd + elev + silt + 
                        om + ksat + cec + ph + depth + cropname:slope + cropname:ffd + 
                        cropname:silt + cropname:om + slope:temp + slope:precip + 
                        slope:ffd + slope:silt + slope:om + slope:ksat + slope:cec + 
                        slope:depth + temp:precip + temp:ffd + temp:elev + temp:silt + 
                        temp:cec + temp:ph + temp:depth + precip:ffd + precip:elev + 
                        precip:silt + precip:om + precip:ksat + precip:cec + ffd:elev + 
                        ffd:om + ffd:ksat + ffd:cec + ffd:ph + ffd:depth + elev:silt + 
                        elev:om + elev:ksat + elev:cec + elev:ph + silt:om + silt:cec + 
                        silt:ph + silt:depth + om:ksat + om:cec + om:ph + ksat:cec + 
                        ksat:ph + ksat:depth + cec:ph + cec:depth + ph:depth,
                data = train)
library(r2pmml)
grass_pmml <- decorate(final.mod.noHay)
r2pmml(grass_pmml, "models/grassInteraction.pmml")

################################################################################################
################################################################################################

# creating a pmml file (no interactions allowed)
# create dummy vars

d <- copy(grass)
#one hot encoding
ddum <- dummyVars("~.", data = d) 
d <- data.table(predict(ddum, newdata = d))

set.seed(0731)
inTrain <- createDataPartition(d$yield, p = 0.75, list = FALSE)
train <- d[inTrain,] # 4214 obs
test<- d[-inTrain,] # 1402 obs

full.lm <- lm(yield~., data = train)
stepBack <- stepAIC(full.lm, direction = "backward")
stepBack$anova
length(stepBack$coefficients) #72
stepBack.mod <- train(yield ~ `cropname.Bluegrass-white clover` + `cropname.Orchardgrass-alsike` + 
                        `cropname.Orchardgrass-red clover` + slope + temp + precip + 
                        ffd + silt + om + ksat + cec + ph + depth,
                      data = train, method = "lm", trControl = train.control)
stepBack.mod
back.pred <- predict(stepBack.mod, test)
RMSE.back <- round(sqrt(mean((back.pred - test$yield)^2)),3)
adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
stepBackLength <- length(coef(stepBack.mod$finalModel))
plot(back.pred~ test$yield) 
abline(a = 0, b = 1)



final.mod <- lm(yield ~ `cropname.Bluegrass-white clover` + `cropname.Orchardgrass-alsike` + 
                        `cropname.Orchardgrass-red clover` + slope + temp + precip + 
                        ffd + silt + om + ksat + cec + ph + depth,,
                      data = d)

library(pmml)
mod_pmml <- pmml(final.mod)
save_pmml(mod_pmml, "GrassNoInteractions.pmml")

#mod.1 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #                           - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #                          - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #                         - slope:elev.r, 
    #                        data = train, method = "lm", trControl = train.control)
#summary(mod.1)

#mod.2 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
   #            - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
      #         - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
       #        - slope:elev.r - elev.r:ksat, 
        #       data = train, method = "lm", trControl = train.control)
#summary(mod.2)

#mod.3 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #            - slope:elev.r - elev.r:ksat - silt:om, 
    #           data = train, method = "lm", trControl = train.control)
#summary(mod.3)

#mod.4 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #            - slope:elev.r - elev.r:ksat - silt:om - precip:ph, 
    #           data = train, method = "lm", trControl = train.control)
#summary(mod.4)

#mod.5 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #            - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth, 
    #           data = train, method = "lm", trControl = train.control)
#summary(mod.5)

#mod.6 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #            - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
    #           - slope:ffd, 
     #          data = train, method = "lm", trControl = train.control)
#summary(mod.6)

#mod.7 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #            - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
    #           - slope:ffd - temp:ksat, 
     #          data = train, method = "lm", trControl = train.control)
#summary(mod.7)

#mod.8 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #            - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
    #           - slope:ffd - temp:ksat - temp:om, 
     #          data = train, method = "lm", trControl = train.control)
#summary(mod.8)

#mod.9 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #            - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
    #           - slope:ffd - temp:ksat - temp:om - precip:depth, 
     #          data = train, method = "lm", trControl = train.control)
#summary(mod.9)

#mod.10 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #              - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #             - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
    #           - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
     #          - slope:ffd - temp:ksat - temp:om - precip:depth - temp:silt, 
      #         data = train, method = "lm", trControl = train.control)
#summary(mod.10)

#mod.11 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #               - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #              - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
   #             - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
    #            - slope:ffd - temp:ksat - temp:om - precip:depth - temp:silt 
     #           - ffd:silt, 
      #          data = train, method = "lm", trControl = train.control)
#summary(mod.11)

#mod.12 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
 #               - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
  #              - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
    #            - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
     #           - slope:ffd - temp:ksat - temp:om - precip:depth - temp:silt 
      #          - ffd:silt - elev.r:ph, 
       #         data = train, method = "lm", trControl = train.control)
#summary(mod.12)
#mod12.pred <- predict(mod.12, test)
#RMSE.mod12 <- round(sqrt(mean((mod12.pred-test$yield)^2)),3)
#adj.rsquared.mod12 <- round((summary(mod.12)$adj.r.squared),3)
#mod12Length <- length(coef(mod.12$finalModel))
#rsquared.mod12 <- round((summary(mod.12)$r.squared),3)
#plot(mod12.pred ~ test$yield)
#abline(a = 0, b = 1)

#mod.13 <- train(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
#                - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
#                - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
 #               - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
  #              - slope:ffd - temp:ksat - temp:om - precip:depth - temp:silt 
    #            - ffd:silt - elev.r:ph - silt:ksat, 
     #           data = train, method = "lm", trControl = train.control)

#summary(mod.13)
#mod13.pred <- predict(mod.13, test)
#RMSE.mod13 <- round(sqrt(mean((mod13.pred-test$yield)^2)),3)
#adj.rsquared.mod13 <- round((summary(mod.13)$adj.r.squared),3)
#mod13Length <- length(coef(mod.13$finalModel))
#rsquared.mod13 <- round((summary(mod.13)$r.squared),3)
#plot(mod13.pred ~ test$yield)
#abline(a = 0, b = 1)

## same model as above mod.13 (final model)
#mod.lm <- lm(yield~.^2 - cropname:slope - cropname:temp - cropname:precip 
#                - cropname:ffd - cropname:elev.r - cropname:silt - cropname:om 
 #               - cropname:ksat - cropname:cec - cropname:ph - cropname:depth
  #              - slope:elev.r - elev.r:ksat - silt:om - precip:ph - om:depth 
   #             - slope:ffd - temp:ksat - temp:om - precip:depth - temp:silt 
    #            - ffd:silt - elev.r:ph - silt:ksat, 
     #           data = train)


#models <- add_row(models, Model = c("All interactions", "Significant Interactions", "Baseline"), RMSE = c(RMSE.allinterx, RMSE.mod13, RMSE.baseline), 
#        Rsquared = c(rsquared.allinterx, rsquared.mod13, NA), adj.Rsquared = c(adj.rsquared.allinterx, adj.rsquared.mod13, NA),
#        no.coef = c(allInterxLength, mod13Length, 0)) %>%
#  arrange(RMSE)
#models %>%
#  gt()

################### FINAL MODEL ###################################################

# model with hay
saveRDS(final.mod.noHay, "models/WIDairyCounties_grass_model.rds")
coef_Grass <- as.data.frame(coef(final.mod.noHay))
write.csv(coef_Grass, file = "models/fullWI_SSURGOgrassMod.csv", quote = FALSE)

###################################################################################
########### ADD PREDICTIONS TO DATASET TO EXAMINE WHERE THE FIT ISN'T GREAT #######

final.mod <- readRDS("models/WIDairyCounties_grass_model.rds")

#grass <- crops %>%
 # filter(cropname == "Bluegrass-white clover"
  #       | cropname == "Bromegrass-alfalfa hay"
   #      | cropname == "Orchardgrass-alsike"
    #     | cropname == "Orchardgrass-red clover"
     #    | cropname == "Timothy-alsike") %>%
#  drop_na() %>%
 # droplevels() %>%
  #dplyr::rename(yield = nonirryield.r, slope = slope.r, elev = elev.r, temp = airtempa.r, precip = map.r, ffd = ffd.r, depth = total.depth)

grass <- crops %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  dplyr::rename(yield = nonirryield.r, slope = slope.r, elev = elev.r, temp = airtempa.r, precip = map.r, ffd = ffd.r, depth = total.depth) %>%
  drop_na(c(yield, slope, elev, temp, precip, ffd, depth, silt, cec, ph)) %>%
  filter(cec < 100) %>%
  droplevels()

pred.ci <- predict(final.mod, grass, interval = "confidence")
colnames(pred.ci) <- c("fit", "lwr.ci", "upr.ci")

pred.pred <- predict(final.mod, grass, interval = "prediction")
colnames(pred.pred) <- c("fit", "lwr.pred", "upr.pred")

grass <- grass %>%
  mutate(prediction = pred.ci[,1],
         lwr.ci = pred.ci[,2],
         upr.ci = pred.ci[,3],
         lwr.pred = pred.pred[,2],
         upr.pred = pred.pred[,3])

colnames(grass)

ggplot(grass, aes(x = yield, y = prediction, color = cropname)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  theme(text = element_text(size = 16))

ggplot(grass, aes(x = yield, y = prediction, color = Region)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  theme(text = element_text(size = 16))

ggplot(grass, aes(x = yield, y = prediction, color = compname)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  theme(text = element_text(size = 16),
        legend.position = "none") 

length(levels(grass$compname))

bad.preds <- grass %>%
  mutate(pred.off = abs(prediction - yield)) %>%
  filter(pred.off > 1.5) %>%
  droplevels()

length(levels(bad.preds$compname))

ggplot(bad.preds, aes(x = yield, y = prediction, color = compname)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  theme(text = element_text(size = 16)) + 
  ylim(1, 5.5)

ggplot(bad.preds, aes(x = yield, y = prediction, color = Region)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  theme(text = element_text(size = 16))

bad.preds <- grass %>%
  mutate(pred.off = abs(prediction - yield)) %>%
  filter(pred.off > 1.5) %>%
  droplevels()

bad_acres <- bad.preds %>%
  dplyr::select(c(Region, county, mukey, musym, compname, comppct.r, muacres)) %>%
  distinct() %>%
  mutate(acres = (comppct.r/100)*muacres)

sum(bad_acres$acres)

ggplot(bad.preds, aes(x = yield, y = prediction, color = compname)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  theme(text = element_text(size = 16)) + 
  ylim(1, 5.5)

ggplot(bad.preds, aes(x = yield, y = prediction, color = Region)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  theme(text = element_text(size = 16))


ggplot(grass, aes(x = yield, y = WI_CCPI_rating, color = cropname))+
  geom_point() 


grass <- grass %>%
  mutate(pred.off = abs(prediction - yield)) %>%
  mutate(bad.pred = case_when(
    pred.off >= 1 ~ "yes",
    pred.off < 1 ~ "no"
  ))

###########################################################################
########## what is leading to the bad predictions? ########################
###########################################################################

ggplot(grass, aes(x = slope, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = slopelenusle.r, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = elev, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = temp, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = precip, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = ksat, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = k, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = cec, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = ph, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = awc, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = depth, fill = bad.pred)) +
  geom_histogram()
ggplot(grass, aes(x = Region, fill = bad.pred)) +
  geom_bar()
ggplot(grass, aes(x = compname, fill = bad.pred)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
## soils that are all wrong when bad = 1 ton/acre:
##  rib, cebana, barronett

#########################################################################3
##########################################################################
##### grass to crop productivity indices

ccpi.lm <- lm(yield ~ WI_CCPI_rating + cropname, data = grass)
summary(ccpi.lm)

# Calculate crop-specific intercepts
intercepts <- c(coef(ccpi.lm)["(Intercept)"],
                coef(ccpi.lm)["(Intercept)"] + coef(ccpi.lm)["cropnameBromegrass-alfalfa hay"],
                coef(ccpi.lm)["(Intercept)"] + coef(ccpi.lm)["cropnameOrchardgrass-alsike"],
                coef(ccpi.lm)["(Intercept)"] + coef(ccpi.lm)["cropnameOrchardgrass-red clover"],
                coef(ccpi.lm)["(Intercept)"] + coef(ccpi.lm)["cropnameTimothy-alsike"])
intercepts

lines.df <- data.frame(intercepts = intercepts,
                       slopes = rep(coef(ccpi.lm)["WI_CCPI_rating"], 5),
                       crops = levels(grass$cropname))

qplot(x = WI_CCPI_rating, y = yield, color = cropname, data = grass, geom = "jitter") + 
  geom_abline(aes(intercept = intercepts, 
                  slope = slopes, 
                  color = crops), data = lines.df)

ccpi.lm.interact <- lm(yield ~ WI_CCPI_rating * cropname, data = grass)
summary(ccpi.lm.interact)
anova(ccpi.lm.2.interact)

# Calculate crop-specific intercepts
intercepts.interact <- c(coef(ccpi.lm.interact)["(Intercept)"],
                         coef(ccpi.lm.interact)["(Intercept)"] + coef(ccpi.lm.interact)["cropnameBromegrass-alfalfa hay"],
                         coef(ccpi.lm.interact)["(Intercept)"] + coef(ccpi.lm.interact)["cropnameOrchardgrass-alsike"],
                         coef(ccpi.lm.interact)["(Intercept)"] + coef(ccpi.lm.interact)["cropnameOrchardgrass-red clover"],
                         coef(ccpi.lm.interact)["(Intercept)"] + coef(ccpi.lm.interact)["cropnameTimothy-alsike"])
intercepts.interact

# Calculate crop-specific slopes
slopes.interact <- c(coef(ccpi.lm.interact)["WI_CCPI_rating"],
                     coef(ccpi.lm.interact)["WI_CCPI_rating"] + coef(ccpi.lm.interact)["WI_CCPI_rating:cropnameBromegrass-alfalfa hay"],
                     coef(ccpi.lm.interact)["WI_CCPI_rating"] + coef(ccpi.lm.interact)["WI_CCPI_rating:cropnameOrchardgrass-alsike"],
                     coef(ccpi.lm.interact)["WI_CCPI_rating"] + coef(ccpi.lm.interact)["WI_CCPI_rating:cropnameOrchardgrass-red clover"],
                     coef(ccpi.lm.interact)["WI_CCPI_rating"] + coef(ccpi.lm.interact)["WI_CCPI_rating:cropnameTimothy-alsike"])
slopes.interact

lines.df.interact <- data.frame(intercepts = intercepts.interact,
                                slopes = slopes.interact,
                                crops = levels(grass$cropname))

qplot(x = WI_CCPI_rating, y = yield, color = cropname, data = grass, geom = "jitter") + 
  geom_abline(aes(intercept = intercepts, 
                  slope = slopes, 
                  color = crops), data = lines.df.interact)

Ncpi.lm <- lm(yield ~ NCCPI_rating + cropname, data = grass)
summary(Ncpi.lm)
anova(Ncpi.lm)

# Calculate crop-specific intercepts
intercepts <- c(coef(Ncpi.lm)["(Intercept)"],
                #coef(Ncpi.lm)["(Intercept)"] + coef(Ncpi.lm)["cropnameBromegrass-alfalfa hay"],
                coef(Ncpi.lm)["(Intercept)"] + coef(Ncpi.lm)["cropnameOrchardgrass-alsike"],
                coef(Ncpi.lm)["(Intercept)"] + coef(Ncpi.lm)["cropnameOrchardgrass-red clover"],
                coef(Ncpi.lm)["(Intercept)"] + coef(Ncpi.lm)["cropnameTimothy-alsike"])
intercepts

lines.df <- data.frame(intercepts = intercepts,
                       slopes = rep(coef(Ncpi.lm)["NCCPI_rating"], 4),
                       crops = levels(grass$cropname))

qplot(x = NCCPI_rating, y = yield, color = cropname, data = grass, geom = "jitter") + 
  geom_abline(aes(intercept = intercepts, 
                  slope = slopes, 
                  color = crops), data = lines.df)

Ncpi.lm.interact <- lm(yield ~ NCCPI_rating * cropname, data = grass)
summary(Ncpi.lm.interact)
anova(Ncpi.lm.interact)

# Calculate crop-specific intercepts
intercepts.interact <- c(coef(Ncpi.lm.interact)["(Intercept)"],
                         #coef(Ncpi.lm.interact)["(Intercept)"] + coef(Ncpi.lm.interact)["cropnameBromegrass-alfalfa hay"],
                         coef(Ncpi.lm.interact)["(Intercept)"] + coef(Ncpi.lm.interact)["cropnameOrchardgrass-alsike"],
                         coef(Ncpi.lm.interact)["(Intercept)"] + coef(Ncpi.lm.interact)["cropnameOrchardgrass-red clover"],
                         coef(Ncpi.lm.interact)["(Intercept)"] + coef(Ncpi.lm.interact)["cropnameTimothy-alsike"])
intercepts.interact

# Calculate crop-specific slopes
slopes.interact <- c(coef(Ncpi.lm.interact)["NCCPI_rating"],
                     #coef(Ncpi.lm.interact)["NCCPI_rating"] + coef(Ncpi.lm.interact)["NCCPI_rating:cropnameBromegrass-alfalfa hay"],
                     coef(Ncpi.lm.interact)["NCCPI_rating"] + coef(Ncpi.lm.interact)["NCCPI_rating:cropnameOrchardgrass-alsike"],
                     coef(Ncpi.lm.interact)["NCCPI_rating"] + coef(Ncpi.lm.interact)["NCCPI_rating:cropnameOrchardgrass-red clover"],
                     coef(Ncpi.lm.interact)["NCCPI_rating"] + coef(Ncpi.lm.interact)["NCCPI_rating:cropnameTimothy-alsike"])
slopes.interact

lines.df.interact <- data.frame(intercepts = intercepts.interact,
                                slopes = slopes.interact,
                                crops = levels(grass$cropname))

qplot(x = NCCPI_rating, y = yield, color = cropname, data = grass, geom = "jitter") + 
  geom_abline(aes(intercept = intercepts, 
                  slope = slopes, 
                  color = crops), data = lines.df.interact)


# Random forest -----------------------------------------------------------

grass <- crops %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  dplyr::select(c(cropname, nonirryield.r, slope.r, airtempa.r, map.r, ffd.r, elev.r, 
                  silt, om, ksat, cec, ph, total.depth, WI_CCPI_rating, NCCPI_rating)) %>%
  dplyr::rename(yield = nonirryield.r, slope = slope.r, temp = airtempa.r, precip = map.r, 
                elev = elev.r, ffd = ffd.r, depth = total.depth) %>%
  filter(cec < 100) %>% # observations are likely incorrect
  droplevels() %>%
  drop_na()

library(randomForest)

set.seed(0731)
inTrain <- createDataPartition(grass$yield, p = 0.75, list = FALSE)
train <- grass[inTrain,]
test<- grass[-inTrain,]

mtry = 14/3
# random forest with 500 trees, try 8 variables at each node
rf_500 <- randomForest(yield ~., data = train, ntree = 500, mtry = 8, importance = TRUE, do.trace = TRUE)
rf_500 # 98.2 var explained
pred_500 <- predict(rf_500, test)
RMSE.500 <- sqrt(mean((pred_500 - test$yield)^2)) #0.15

varImp(rf_500)
varImpPlot(rf_500)
importance(rf_500)
plot(rf_500)

plot(pred_500 ~ test$yield, xlab = "SSURGO Yield", ylab = "Predicted Yield", main = "500 trees")
abline(a = 0, b = 1)
text(x = 2, y = 5, "RMSE = 0.15")

#remove temp
rf_500notemp <- randomForest(yield ~.-temp, data = train, ntree = 500, mtry = 8, importance = TRUE, do.trace = TRUE)
rf_500notemp # 98.2 var explained
pred_500notemp <- predict(rf_500notemp, test)
RMSE.500notemp <- sqrt(mean((pred_500notemp - test$yield)^2)) #0.15

varImp(rf_500)
varImpPlot(rf_500)
importance(rf_500)
plot(rf_500)

plot(pred_500 ~ test$yield, xlab = "SSURGO Yield", ylab = "Predicted Yield", main = "500 trees")
abline(a = 0, b = 1)
text(x = 2, y = 5, "RMSE = 0.15")

rf_250 <- randomForest(yield ~., data = train, ntree = 250, mtry = 8, importance = TRUE, do.trace = TRUE)
rf_250 # 98.1% var explained
pred_250 <- predict(rf_250, test)
RMSE.250 <- sqrt(mean((pred_250 - test$yield)^2)) #0.15

varImp(rf_250)
varImpPlot(rf_250)
importance(rf_250)
plot(rf_250)

plot(pred_250 ~ test$yield, xlab = "SSURGO Yield", ylab = "Predicted Yield", main = "250 trees")
abline(a = 0, b = 1)
text(x = 2, y = 5, "RMSE = 0.16")

rf_200 <- randomForest(yield ~., data = train, ntree = 200, mtry = 8, importance = TRUE, do.trace = TRUE)
rf_200 # 98.1 var explained
pred_200 <- predict(rf_200, test)
RMSE.200 <- sqrt(mean((pred_200 - test$yield)^2)) #0.15

varImp(rf_200)
varImpPlot(rf_250)
importance(rf_250)
plot(rf_250)

rf_150 <- randomForest(yield ~., data = train, ntree = 150, mtry = 8, importance = TRUE, do.trace = TRUE)
rf_150 # 98.1 var explained
pred_150 <- predict(rf_150, test)
RMSE.150 <- sqrt(mean((pred_150 - test$yield)^2)) #0.15

varImp(rf_200)
varImpPlot(rf_250)
importance(rf_250)
plot(rf_150)

plot(pred_250 ~ test$yield, xlab = "SSURGO Yield", ylab = "Predicted Yield", main = "250 trees")
abline(a = 0, b = 1)
text(x = 2, y = 5, "RMSE = 0.16")

rf_100 <- randomForest(yield ~., data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf_100 # 98.1 var explained
pred_100 <- predict(rf_100, test)
RMSE.100 <- sqrt(mean((pred_100 - test$yield)^2)) #0.15

varImp(rf_200)
varImpPlot(rf_250)
importance(rf_250)
plot(rf_250)

plot(pred_250 ~ test$yield, xlab = "SSURGO Yield", ylab = "Predicted Yield", main = "250 trees")
abline(a = 0, b = 1)
text(x = 2, y = 5, "RMSE = 0.16")

rf_75 <- randomForest(yield ~., data = train, ntree = 75, mtry = 8, importance = TRUE, do.trace = TRUE)
rf_75 # 98.0 var explained
pred_75 <- predict(rf_75, test)
RMSE.75 <- sqrt(mean((pred_75 - test$yield)^2)) #0.15

plot(pred_75 ~ test$yield, xlab = "SSURGO Yield", ylab = "Predicted Yield", main = "75 trees")
abline(a = 0, b = 1)
text(x = 2, y = 5, "RMSE = 0.15")

varImpPlot(rf_75)
