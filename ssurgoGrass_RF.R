library(tidyverse)
library(caret)
library(pmml)
library(randomForest)


# load and clean data -----------------------------------------------------

crops <- read.table("prod_index_soil.txt", header = TRUE, sep = ",")

grass <- crops %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  mutate(cropname = recode(cropname, 
                           `Bluegrass-white clover`="Bluegrass-clover",
                           `Orchardgrass-alsike`= "Orchardgrass-clover",
                           `Orchardgrass-red clover` = "Orchardgrass-clover",
                           `Timothy-alsike` = "Timothy-clover")) %>%
  filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope = slope.r, elev = elev.r, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth, WI_CCPI_rating, NCCPI_rating)) %>%
  drop_na()


summary(grass)


grass_sum <- grass %>%
  group_by(cropname) %>%
  summarise(minYld = min(yield),
            meanYld = round(mean(yield),2),
            maxYld = max(yield))

write.csv(grass_sum, "surgoGrassStats.csv", row.names = FALSE, quote = FALSE)

# create training and testing data sets -----------------------------------

set.seed(0731)
inTrain <- createDataPartition(grass$yield, p = 0.75, list = FALSE)
train <- grass[inTrain,] # 4216 obs
test<- grass[-inTrain,] # 1404 obs


# model building ----------------------------------------------------------

# random forest with 500 trees, try 8 variables at each node
rf_500 <- randomForest(yield ~., data = train, ntree = 500, importance = TRUE, do.trace = TRUE)
rf_500 # 97.2 var explained
pred_500 <- predict(rf_500, test)
RMSE.500 <- sqrt(mean((pred_500 - test$yield)^2)) #0.17

varImp(rf_500)
varImpPlot(rf_500)
importance(rf_500)
plot(rf_500)

plot(pred_500 ~ test$yield, xlab = "SSURGO Yield", ylab = "Predicted Yield", main = "Full Mod")
abline(a = 0, b = 1)
text(x = 2, y = 5, "RMSE = 0.17")

# change number of trees ----

rf_150 <- randomForest(yield ~ ., data = train, ntree = 150, importance = TRUE, do.trace = TRUE, keep.inbag = TRUE)
rf_150
pred_150<- predict(rf_150, test)
#pred_150<- predict(rf_150, test, predict.all = TRUE) this creates a prediction df for calculating CI
RMSE.150 <- sqrt(mean((pred_150 - test$yield)^2)) #0.17
plot(pred_150~test$yield)
saveRDS(rf_150, "RF150.rds")
rf150 <- pmml(rf_150)
save_pmml(rf150, "rf150.pmml")

rf_75 <- randomForest(yield ~ ., data = train, ntree = 75, importance = TRUE, do.trace = TRUE, keep.inbag = TRUE)
rf_75
pred_75<- predict(rf_75, test)
#pred_75 <- predict(rf_75, test, predict.all = TRUE) this creates a prediction df for calculating CI
RMSE.75 <- sqrt(mean((pred_75 - test$yield)^2)) #0.18
plot(pred_75~test$yield)
saveRDS(rf_75, "RF75.rds")
rf75<- pmml(rf_75)
save_pmml(rf75, "rf75.pmml")



# 70% CI
# https://stats.stackexchange.com/questions/56895/do-the-predictions-of-a-random-forest-model-have-a-prediction-interval
t(apply(pred.afa$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(0.025, 0.975)))
}))

# get one soil for prediction
afa # from soils df in shinyapp data
pred.afa <- predict(rf_150, afa, predict.all = TRUE)


# 95% CI
t(apply( pred.afa$individual, 1, function(x){ 
  c( mean(x) + c(-1.96,1.96)*sd(x), quantile(x, c(0.025,0.975)) )}))

# https://www.r-bloggers.com/2016/03/confidence-intervals-for-random-forests/
library(devtools)
library(randomForestCI)

# Calculate the Variance
X <- test[,-2]
var_hat <- randomForestInfJack(rf_150, X, calibrate = TRUE)
plot(var_hat)

rf_100 <- randomForest(yield ~ ., data = train, ntree = 100, importance = TRUE, do.trace = TRUE)
rf_100
pred_100<- predict(rf_100, test)
RMSE.100 <- sqrt(mean((pred_100 - test$yield)^2)) #0.18

rf_75 <- randomForest(yield ~ ., data = train, ntree = 75, importance = TRUE, do.trace = TRUE)
rf_75
pred_75<- predict(rf_75, test)
RMSE.75 <- sqrt(mean((pred_75 - test$yield)^2)) #0.17

# tune mtry

mtry_default = (length(colnames(train))-1)/3
mtry = 5

rf_150_5 <- randomForest(yield ~ ., data = train, ntree = 150, mtry = 5, importance = TRUE, do.trace = TRUE)
rf_150_5
pred_150_5 <- predict(rf_150_5, test)
RMSE.150_5 <- sqrt(mean((pred_150_5 - test$yield)^2)) #0.17

rf_100_5 <- randomForest(yield ~ ., data = train, ntree = 100, mtry = 5, importance = TRUE, do.trace = TRUE)
rf_100_5
pred_100_5 <- predict(rf_100_5, test)
RMSE.100_5 <- sqrt(mean((pred_100_5 - test$yield)^2)) #0.18

rf_75_5 <- randomForest(yield ~ ., data = train, ntree = 75, mtry = 5, importance = TRUE, do.trace = TRUE)
rf_75_5
pred_75_5<- predict(rf_75_5, test)
RMSE.75_5 <- sqrt(mean((pred_75_5 - test$yield)^2)) #0.17

mtry = 6

rf_150_6 <- randomForest(yield ~ ., data = train, ntree = 150, mtry = 6, importance = TRUE, do.trace = TRUE)
rf_150_6
pred_150_6 <- predict(rf_150_6, test)
RMSE.150_6 <- sqrt(mean((pred_150_6 - test$yield)^2)) #0.17

rf_100_6 <- randomForest(yield ~ ., data = train, ntree = 100, mtry = 6, importance = TRUE, do.trace = TRUE)
rf_100_6
pred_100_6 <- predict(rf_100_6, test)
RMSE.100_6 <- sqrt(mean((pred_100_6 - test$yield)^2)) #0.18

rf_75_6 <- randomForest(yield ~ ., data = train, ntree = 75, mtry = 6, importance = TRUE, do.trace = TRUE)
rf_75_6
pred_75_6 <- predict(rf_75_6, test)
RMSE.75_6 <- sqrt(mean((pred_75_6 - test$yield)^2)) #0.17

mtry = 7

rf_150_7 <- randomForest(yield ~ ., data = train, ntree = 150, mtry = 7, importance = TRUE, do.trace = TRUE)
rf_150_7
pred_150_7 <- predict(rf_150_7, test)
RMSE.150_7 <- sqrt(mean((pred_150_7 - test$yield)^2)) #0.17

rf_100_7 <- randomForest(yield ~ ., data = train, ntree = 100, mtry = 7, importance = TRUE, do.trace = TRUE)
rf_100_7
pred_100_7 <- predict(rf_100_7, test)
RMSE.100_7 <- sqrt(mean((pred_100_7 - test$yield)^2)) #0.18

rf_75_7 <- randomForest(yield ~ ., data = train, ntree = 75, mtry = 7, importance = TRUE, do.trace = TRUE)
rf_75_7
pred_75_7 <- predict(rf_75_7, test)
RMSE.75_7 <- sqrt(mean((pred_75_7 - test$yield)^2)) #0.17

