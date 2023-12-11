# this script uses a more complete set of soil data and then uses recursive feature elimination to determine best variables

# load libraries
library(tidyverse) # cleans data
library(tidymodels)
#library(randomForest)
library(ranger)
library(recipes)
library(colino)

crops <- read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
  mutate_if(is.character, as.factor)

#summary(crops)

#names(crops)

# clean data--------------
##TODO if I want later, put county, musym and compname back in for as IDs
grass <- crops %>%
  filter(cec < 100,
         nonirryield.r < 10) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(-c(yldunits,cokey,comppct.r,compkind,majcompflag,muacres,mukey, county, musym, compname)) %>%
  rename(yield = nonirryield.r)

summary(grass)

## removing NA---------------------
# remove variables that have significant number of NAs or all zeros
# remove siltco, siltfine, partdensity
grass <- grass %>%
  dplyr::select(-c(siltco, siltfine, partdensity, gypsum, sar))

## examine variance-------------
# examine histograin of each variable
# remove if little to no variance
hist(grass$yield)
hist(grass$slope, breaks = 40)
hist(grass$elev, breaks = 40)
hist(grass$ec, breaks = 40) # remove ec
hist(grass$sand, breaks = 40)
hist(grass$sandvc, breaks = 40)
hist(grass$sandco, breaks = 40)
hist(grass$sandmed, breaks = 40)
hist(grass$sandfine, breaks = 40)
hist(grass$silt, breaks = 40)
hist(grass$clay, breaks = 40)
hist(grass$om, breaks = 40)
hist(grass$ksat, breaks = 40)
hist(grass$cec, breaks = 40)
hist(grass$ph, breaks = 40)
hist(grass$awc, breaks = 40)
hist(grass$frag10, breaks = 40) # remove frag10
hist(grass$frag3, breaks = 40)
hist(grass$total.depth, breaks = 40)

# remove ec, frag10
grass <- grass %>%
  dplyr::select(-c(frag10, ec)) 
names(grass)

# training split---------------
# https://github.com/andrew-couch/Tidy-Tuesday/blob/master/Season%202/Scripts/TidyTuesdayDimensionalityReduction.Rmd
set.seed(0731)
tidy_split <- initial_split(grass, strata = yield)
train <- training(tidy_split)
test <- testing(tidy_split)

# basic dimension reduction---------------
basic_dim_red <- recipe(yield~., data = train) %>%
  #update_role(county, musym, compname, new_role = "ID") %>% # not used in model but used for ID purpose in post hoc explore
  step_impute_mean(all_numeric(), -all_outcomes()) %>% 
  step_nzv(all_numeric(), -all_outcomes()) # remove columns with near zero variance

basic_dim_red %>% prep %>% juice() %>% glimpse()

basic_dim_red %>% prep %>% juice() %>% ncol()
basic_dim_red %>% prep %>% juice() %>% names() # nothing removed for near zero variance


# remove correlated variables------------------------
full_dim_red <- recipe(yield~., data = train) %>%
  #update_role(county, musym, compname, new_role = "ID") %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) 

full_dim_red %>% prep %>% juice() %>% glimpse()

full_dim_red %>% prep %>% juice() %>% ncol() # 16
full_dim_red %>% prep %>% juice() %>% names() # removes sand, silt

# examine correlations---------------
# which features are the most correlated?
recipe(yield~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  prep() %>%
  juice() %>%
  select(-cropname, -yield) %>%
  cor() %>%
  as_tibble(rownames = "features") %>%
  pivot_longer(-features) %>%
  filter(features > name) %>%
  drop_na() %>%
  arrange(desc(abs(value)))

# histogram of the correlated features
recipe(yield ~., data = train)%>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  prep() %>%
  juice() %>%
  select(-cropname, -yield) %>%
  cor() %>%
  as_tibble(rownames = "features") %>%
  pivot_longer(-features) %>%
  filter(features > name) %>%
  drop_na() %>%
  ggplot(aes(x = value)) +
  geom_histogram(color = "white", bins = 40) +
  scale_x_continuous()

# begin making models-------------

# Threshold is A numeric value between 0 and 1 representing the percentile of best scoring features to select. 
# Features with scores that are _larger_ than the specified threshold will be retained.

## global tuning params-----------

# model and engine for rfe
rfe_model <- rand_forest(mode = "regression") %>% 
  set_engine("ranger", importance = "permutation")  # could use importance

# model and engine for tuning
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# base recipe
#base_rec <- recipe(yield ~., data = train) # %>%
# update_role(county, musym, compname, new_role = "ID") %>%
# update_role_requirements("ID", bake = FALSE)

#summary(base_rec)

## recursive feature elimination-------------------
## threshold 0.4------------------

rfe_rec4 <- base_rec <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.4) 

rfe_rec4 %>% #10 var
  prep() %>%
  juice() %>%
  names() # "cropname" "sandco"   "sandmed"  "sandfine" "clay"     "ksat"     "cec"      "ph"       "awc"      "yield"

# recreate the training data with these features
grass4 <- grass %>%
  select(c(cropname,sandco,sandmed,sandfine, clay,ksat,cec,ph,awc,yield))

set.seed(0731)
tidy_split4 <- initial_split(grass4, strata = yield)
train4 <- training(tidy_split4)
test4 <- testing(tidy_split4)

# base recipe
rec4 <- recipe(yield ~., data = train4) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rf4_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec4)

rf4_workflow

set.seed(234)
folds <- vfold_cv(train4, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(train4))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 4)), #sqrt(10) = 3.16
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_res4 <- 
  rf4_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmse4 <- select_best(rf_res4, "rmse")

best_rmse4

final_rf4 <- finalize_model(
  rf_mod,
  best_rmse4
)

final_rf4

final_wf4 <- workflow() %>%
  add_recipe(rec4) %>%
  add_model(final_rf4)

final_res4 <- final_wf4 %>%
  last_fit(tidy_split4)

mod4 <- final_rf4 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = train4
  )

mod4

saveRDS(mod4, "models/pastureRFE4.rds")

pred_df4 <- final_res4 %>%
  collect_predictions()

pred_df4 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

mod4
rmse4 <- sqrt(0.0383)

## threshold 0.5-----------------

rfe_rec5 <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.5) 

rfe_rec5 %>% #9 var
  prep() %>%
  juice() %>%
  names() # "cropname" "sandco"   "sandmed"  "sandfine" "clay"     "ksat"     "cec"      "awc"      "yield"


# recreate the training data with these features
grass5 <- grass %>%
  select(c(cropname,sandco,sandmed,sandfine, clay,ksat,cec,awc,yield))

set.seed(0731)
tidy_split5 <- initial_split(grass5, strata = yield)
train5 <- training(tidy_split5)
test5 <- testing(tidy_split5)

# base recipe
rec5 <- recipe(yield ~., data = train5) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rf5_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec5)

set.seed(234)
folds <- vfold_cv(train5, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(train5))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 4)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_res5 <- 
  rf5_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmse5 <- select_best(rf_res5, "rmse")

final_rf5 <- finalize_model(
  rf_mod,
  best_rmse5
)

final_wf5 <- workflow() %>%
  add_recipe(rec5) %>%
  add_model(final_rf5)

final_res5 <- final_wf5 %>%
  last_fit(tidy_split5)

mod5 <- final_rf5 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = train5
  )

mod5

saveRDS(mod5, "models/pastureRFE5.rds")

pred_df5 <- final_res5 %>%
  collect_predictions()

pred_df5 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

mod5

rmse5 <- sqrt(0.0385)

## threshold 0.6------------------

rfe_rec6 <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.6)

rfe_rec6 %>% #8 var
  prep() %>%
  juice() %>% 
  names() # [1] "cropname" "sandmed"  "sandfine" "clay"     "ksat"     "awc"      "yield" 

# recreate the training data with these features
grass6 <- grass %>%
  select(c(cropname,sandmed,sandfine, clay,ksat,awc,yield))

set.seed(0731)
tidy_split6 <- initial_split(grass6, strata = yield)
train6 <- training(tidy_split6)
test6 <- testing(tidy_split6)

# base recipe
rec6 <- recipe(yield ~., data = train6) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rf6_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec6)

set.seed(234)
folds <- vfold_cv(train6, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(train6))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 3)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_res6 <- 
  rf6_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmse6 <- select_best(rf_res6, "rmse")

final_rf6 <- finalize_model(
  rf_mod,
  best_rmse6
)

final_wf6 <- workflow() %>%
  add_recipe(rec6) %>%
  add_model(final_rf6)

final_res6 <- final_wf6 %>%
  last_fit(tidy_split6)

mod6 <- final_rf6 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = train6
  )

mod6

saveRDS(mod6, "models/pastureRFE6.rds")

pred_df6 <- final_res6 %>%
  collect_predictions()

pred_df6 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

mod6

rmse6 <- sqrt(0.043)

##threshold 0.7-------------
rfe_rec7 <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.7)

rfe_rec7 %>% #6 var
  prep() %>%
  juice() %>%
  names() #  "sandmed"  "sandfine" "clay"     "ksat"     "awc"      "yield"  

# recreate the training data with these features
grass7 <- grass %>%
  select(c(sandmed,sandfine, clay,ksat,awc,yield))

set.seed(0731)
tidy_split7 <- initial_split(grass7, strata = yield)
train7 <- training(tidy_split7)
test7 <- testing(tidy_split7)

# base recipe
rec7 <- recipe(yield ~., data = train7) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rf7_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec7)

set.seed(234)
folds <- vfold_cv(train7, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(train7))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 3)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_res7 <- 
  rf7_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmse7 <- select_best(rf_res7, "rmse")

final_rf7 <- finalize_model(
  rf_mod,
  best_rmse7
)

final_wf7 <- workflow() %>%
  add_recipe(rec7) %>%
  add_model(final_rf7)

final_res7 <- final_wf7 %>%
  last_fit(tidy_split7)

mod7 <- final_rf7 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = train7
  )

mod7

saveRDS(mod7, "models/pastureRFE7.rds")

pred_df7 <- final_res7 %>%
  collect_predictions()

pred_df7 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

mod7

rmse7 <- sqrt(0.172)

## threshold 0.8----------------

rfe_rec8 <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.8)

rfe_rec8 %>% #5 var
  prep() %>%
  juice() %>%
  names() #  "sandfine" "ksat"     "awc"      "yield" 

# recreate the training data with these features
grass8 <- grass %>%
  select(c(clay,ksat,awc,yield))

set.seed(0731)
tidy_split8 <- initial_split(grass8, strata = yield)
train8 <- training(tidy_split8)
test8 <- testing(tidy_split8)

# base recipe
rec8 <- recipe(yield ~., data = train8) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rf8_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec8)

set.seed(234)
folds <- vfold_cv(train8, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(train8))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 3)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_res8 <- 
  rf8_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmse8 <- select_best(rf_res8, "rmse")

final_rf8 <- finalize_model(
  rf_mod,
  best_rmse8
)

final_wf8 <- workflow() %>%
  add_recipe(rec8) %>%
  add_model(final_rf8)

final_res8 <- final_wf8 %>%
  last_fit(tidy_split8)

mod8 <- final_rf8 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = train8
  )

mod8

saveRDS(mod8, "models/pastureRFE8.rds")

pred_df8 <- final_res8 %>%
  collect_predictions()

pred_df8 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

mod8

rmse8 <- sqrt(0.188)

## threshold 0.9----------------

rfe_rec9 <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.9)

rfe_rec9 %>% #3 var
  prep() %>%
  juice() %>%
  names() # "ksat"  "awc"   "yield"

# recreate the training data with these features
grass9 <- grass %>%
  select(c(ksat,awc,yield))

set.seed(0731)
tidy_split9 <- initial_split(grass9, strata = yield)
train9 <- training(tidy_split9)
test9 <- testing(tidy_split9)

# base recipe
rec9 <- recipe(yield ~., data = train9) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rf9_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec9)

set.seed(234)
folds <- vfold_cv(train9, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(train9))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 3)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_res9 <- 
  rf9_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmse9 <- select_best(rf_res9, "rmse")

final_rf9 <- finalize_model(
  rf_mod,
  best_rmse9
)

final_wf9 <- workflow() %>%
  add_recipe(rec9) %>%
  add_model(final_rf9)

final_res9 <- final_wf9 %>%
  last_fit(tidy_split9)

mod9 <- final_rf9 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = train9
  )

mod9

saveRDS(mod9, "models/pastureRFE9.rds")

pred_df9 <- final_res9 %>%
  collect_predictions()

pred_df9 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

mod9

rmse9 <- sqrt(0.373)

## only remove correlated vars (no rfe)--------------------
rfe_recCor <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) 

rfe_recCor %>% #16 var
  prep() %>%
  juice() %>%
  names() #[1] "cropname"    "slope"       "elev"        "sandvc"      "sandco"      "sandmed"     "sandfine"    "clay"       
#[9] "om"          "ksat"        "cec"         "ph"          "awc"         "frag3"       "total.depth" "yield" 

# recreate the training data with these features
grassCor <- grass %>%
  select(c(cropname,slope, elev, sandvc, sandco,sandmed,sandfine, clay,om,
           ksat,cec,ph,awc,frag3,total.depth, yield))

set.seed(0731)
tidy_splitCor <- initial_split(grassCor, strata = yield)
trainCor <- training(tidy_splitCor)
testCor <- testing(tidy_splitCor)

# base recipe
recCor <- recipe(yield ~., data = trainCor) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rfCor_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(recCor)

set.seed(234)
folds <- vfold_cv(trainCor, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(trainCor))) 

rf_grid <- grid_regular(
  mtry(range = c(3, 5)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_resCor <- 
  rfCor_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmseCor <- select_best(rf_resCor, "rmse")

final_rfCor <- finalize_model(
  rf_mod,
  best_rmseCor
)

final_wfCor <- workflow() %>%
  add_recipe(recCor) %>%
  add_model(final_rfCor)

final_resCor <- final_wfCor %>%
  last_fit(tidy_splitCor)

modCor <- final_rfCor %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = trainCor
  )

modCor

saveRDS(modCor, "models/pastureCorRanger.rds")

pred_dfCor <- final_resCor %>%
  collect_predictions()

pred_dfCor %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

modCor

rmseCor <- sqrt(0.0178)

## no variable removal----------------------------

recReg <- recipe(yield ~., data = train) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rfReg_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(recReg)

set.seed(234)
folds <- vfold_cv(train, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(train))) 

rf_grid <- grid_regular(
  mtry(range = c(3, 5)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_resReg <- 
  rfReg_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmseReg <- select_best(rf_resReg, "rmse")

final_rfReg <- finalize_model(
  rf_mod,
  best_rmseReg
)

final_wfReg <- workflow() %>%
  add_recipe(recReg) %>%
  add_model(final_rfReg)

final_resReg <- final_wfReg %>%
  last_fit(tidy_split)

modReg <- final_rfReg %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = train
  )

modReg

saveRDS(modReg, "models/pastureRegRanger.rds")

pred_dfReg <- final_resReg %>%
  collect_predictions()

pred_dfReg %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

modReg

rmseReg <- sqrt(0.0181)

# OG model--------------------------
mod <- readRDS("models/tidyPastureALLWI.rds")
mod$preproc

grassOG <- grass %>%
  select(c(yield, cropname, slope, elev, sand, silt, clay, om, ksat, cec,
           ph, awc, total.depth))

set.seed(0731)
tidy_splitOG <- initial_split(grassOG, strata = yield)
trainOG <- training(tidy_splitOG)
testOG <- testing(tidy_splitOG)

# base recipe
recOG <- recipe(yield ~., data = trainOG) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rfOG_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(recOG)

set.seed(234)
folds <- vfold_cv(trainOG, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(trainOG))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 4)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_resOG <- 
  rfOG_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmseOG <- select_best(rf_resOG, "rmse")

final_rfOG <- finalize_model(
  rf_mod,
  best_rmseOG
)

final_wfOG <- workflow() %>%
  add_recipe(recOG) %>%
  add_model(final_rfOG)

final_resOG <- final_wfOG %>%
  last_fit(tidy_splitOG)

modOG <- final_rfOG %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = trainOG
  )

modOG

saveRDS(modOG, "models/pastureOGRanger.rds")

pred_dfOG <- final_resOG %>%
  collect_predictions()

pred_dfOG %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

modOG

rmseOG <- sqrt(0.018)

# OG + cor model-------------------

rfe_recCorOG <- recipe(yield ~., data = trainOG) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes()) 

rfe_recCorOG %>% #16 var
  prep() %>%
  juice() %>%
  names() 

# recreate the training data with these features
grassCorOG <- grass %>%
  select(c(cropname,slope, elev,silt, clay, om, ksat, cec, ph, total.depth, yield))

set.seed(0731)
tidy_splitCorOG <- initial_split(grassCorOG, strata = yield)
trainCorOG <- training(tidy_splitCorOG)
testCorOG <- testing(tidy_splitCorOG)

# base recipe
recCorOG <- recipe(yield ~., data = trainCorOG) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rfCorOG_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(recCorOG)

set.seed(234)
folds <- vfold_cv(trainCorOG, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(trainCorOG))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 4)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_resCorOG <- 
  rfCorOG_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmseCorOG <- select_best(rf_resCorOG, "rmse")

final_rfCorOG <- finalize_model(
  rf_mod,
  best_rmseCorOG
)

final_wfCorOG <- workflow() %>%
  add_recipe(recCorOG) %>%
  add_model(final_rfCorOG)

final_resCorOG <- final_wfCorOG %>%
  last_fit(tidy_splitCorOG)

modCorOG <- final_rfCorOG %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = trainCorOG
  )

modCorOG

saveRDS(modCorOG, "models/pastureCorOGRanger.rds")

pred_dfCorOG <- final_resCorOG %>%
  collect_predictions()

pred_dfCorOG %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

modCorOG

rmseCorOG <- sqrt(0.0183)

# compare models------------------
mod4
mod4met <- final_res4 %>%
  collect_metrics() %>%
  mutate(model = "0.4") %>%
  bind_cols(rmse4)
mod5
mod5met <- final_res5 %>%
  collect_metrics() %>%
  mutate(model = "0.5") %>%
  bind_cols(rmse5)
mod6
mod6met <- final_res6 %>%
  collect_metrics() %>%
  mutate(model = "0.6") %>%
  bind_cols(rmse6)
mod7
mod7met <- final_res7 %>%
  collect_metrics() %>%
  mutate(model = "0.7") %>%
  bind_cols(rmse7)
mod8
mod8met <- final_res8 %>%
  collect_metrics() %>%
  mutate(model = "0.8") %>%
  bind_cols(rmse8)
mod9
mod9met <- final_res9 %>%
  collect_metrics() %>%
  mutate(model = "0.9") %>%
  bind_cols(rmse9)
modCorMet <- final_resCor %>%
  collect_metrics() %>%
  mutate(model = "Cor") %>%
  bind_cols(rmseCor)
modRegMet <- final_resReg %>%
  collect_metrics() %>%
  mutate(model = "reg") %>%
  bind_cols(rmseReg)
modOGMet <- final_resOG %>%
  collect_metrics() %>%
  mutate(model = "OG") %>%
  bind_cols(rmseOG)
modCorOGmet <- final_resCorOG %>%
  collect_metrics() %>%
  mutate(model = "OG_Cor") %>%
  bind_cols(rmseCorOG)

modmets <- bind_rows(mod4met, mod5met, mod6met, mod7met, mod8met, mod9met, modCorMet, modRegMet, modOGMet, modCorOGmet) 

write.csv(modmets, "modelOutputs/rangerModCompare.csv", row.names = FALSE, quote = TRUE)


modmets %>% 
  filter(.metric =="rmse") %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col()

modmets %>% 
  filter(.metric =="rsq") %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col()

# choose best models------------------

# based on lowest rmse

modmets %>% 
  filter(.metric =="rmse") %>%
  mutate(model = fct_reorder(model, .estimate)) %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col() # model Cor

modmets %>% 
  filter(.metric =="rsq") %>%
  mutate(model = fct_reorder(model, .estimate)) %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col() # cor!

# will remove OG mods
modmets_final <- modmets %>%
  filter(model != "OG_Cor"
         & model != "OG")

modmets_final %>% 
  filter(.metric =="rmse") %>%
  mutate(model = fct_reorder(model, .estimate)) %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col() # model Cor


# feature importance----------------
# cor the cor model because it has the lowest rmse with the fewest vars
trainExplainer <- trainCor %>%
  drop_na(elev)

cor_explainer <- explain_tidymodels(
  modCor,
  data = dplyr::select(trainExplainer, -yield),
  y = as.integer(trainExplainer$yield),
  verbose = FALSE
)

cor_explainer

library(patchwork)
set.seed(1804)
vip_cor <- model_parts(cor_explainer, loss_function = loss_root_mean_square)
plot(vip_cor)


# eliminate sandvc-------------

# recreate the training data with these features
grassCor2 <- grass %>%
  select(c(cropname, ksat, awc, slope, clay, sandfine, sandmed, ph, cec,
           frag3, om, sandco, elev, total.depth, yield))

set.seed(0731)
tidy_splitCor2 <- initial_split(grassCor2, strata = yield)
trainCor2 <- training(tidy_splitCor2)
testCor2 <- testing(tidy_splitCor2)

# base recipe
recCor2 <- recipe(yield ~., data = trainCor2) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rfCor2_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(recCor2)

set.seed(234)
folds <- vfold_cv(trainCor2, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(trainCor2))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 4)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_resCor2 <- 
  rfCor2_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmseCor2 <- select_best(rf_resCor2, "rmse")

final_rfCor2 <- finalize_model(
  rf_mod,
  best_rmseCor2
)

final_wfCor2 <- workflow() %>%
  add_recipe(recCor2) %>%
  add_model(final_rfCor2)

final_resCor2 <- final_wfCor2 %>%
  last_fit(tidy_splitCor2)

modCor2 <- final_rfCor2 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = trainCor2
  )

modCor2

trainExplainer2 <- trainCor2 %>%
  drop_na(elev)

cor2_explainer <- explain_tidymodels(
  modCor2,
  data = dplyr::select(trainExplainer2, -yield),
  y = as.integer(trainExplainer2$yield),
  verbose = FALSE
)

set.seed(1804)
vip_cor2 <- model_parts(cor2_explainer, loss_function = loss_root_mean_square)
vip_cor2 <- model_parts(cor2_explainer)
plot(vip_cor2)

saveRDS(modCor2, "models/pastureCor2Ranger.rds") # final model!

pred_dfCor2 <- final_resCor2 %>%
  collect_predictions()

pred_dfCor2 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

modCor2

rmseCor2 <- sqrt(0.0184)

rmseCor2

modCor2met <- final_resCor2 %>%
  collect_metrics() %>%
  mutate(model = "Cor2") %>%
  bind_cols(rmseCor2)

# eliminate total.depth-----------------------
# recreate the training data with these features
grassCor3 <- grass %>%
  select(c(cropname, ksat, awc, slope, clay, sandfine, sandmed, ph, cec,
           frag3, om, sandco, elev, yield))

set.seed(0731)
tidy_splitCor3 <- initial_split(grassCor3, strata = yield)
trainCor3 <- training(tidy_splitCor3)
testCor3 <- testing(tidy_splitCor3)

# base recipe
recCor3 <- recipe(yield ~., data = trainCor3) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_nzv(all_numeric(), -all_outcomes()) 

rfCor3_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(recCor3)

set.seed(234)
folds <- vfold_cv(trainCor3, strata = yield)

doParallel::registerDoParallel()

floor(sqrt(ncol(trainCor3))) 

rf_grid <- grid_regular(
  mtry(range = c(2, 4)),
  min_n(range = c(4, 6)),
  trees(range = c(50,100)),
  levels = 3
)

rf_resCor3 <- 
  rfCor3_workflow %>% 
  tune_grid(folds,
            grid = rf_grid)

#choose best model
best_rmseCor3 <- select_best(rf_resCor3, "rmse")

final_rfCor3 <- finalize_model(
  rf_mod,
  best_rmseCor3
)

final_wfCor3 <- workflow() %>%
  add_recipe(recCor3) %>%
  add_model(final_rfCor3)

final_resCor3 <- final_wfCor3 %>%
  last_fit(tidy_splitCor3)

modCor3 <- final_rfCor3 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(yield ~ .,
      data = trainCor3
  )

modCor3

trainExplainer3 <- trainCor3 %>%
  drop_na(elev)

cor3_explainer <- explain_tidymodels(
  modCor3,
  data = dplyr::select(trainExplainer3, -yield),
  y = as.integer(trainExplainer3$yield),
  verbose = FALSE
)

set.seed(1804)
vip_cor3 <- model_parts(cor3_explainer, loss_function = loss_root_mean_square)

plot(vip_cor3)

saveRDS(modCor3, "models/pastureCor3Ranger.rds") 

pred_dfCor3 <- final_resCor3 %>%
  collect_predictions()

pred_dfCor3 %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()  +
  geom_abline(slope = 1, intercept = 0, color = 'red')

modCor3

rmseCor3 <- sqrt(0.0186)

modCor3met <- final_resCor3 %>%
  collect_metrics() %>%
  mutate(model = "Cor3") %>%
  bind_cols(rmseCor3)

modmets <- bind_rows(mod4met, mod5met, mod6met, mod7met, mod8met, mod9met, modCorMet, modRegMet, modOGMet, modCorOGmet, modCor2met, modCor3met)

# will remove OG mods
modmets_final <- modmets %>%
  filter(model != "OG_Cor"
         & model != "OG")

modmets_final %>% 
  filter(.metric =="rmse") %>%
  mutate(model = fct_reorder(model, .estimate)) %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col() # model Cor = final model

names(train)
names(trainCor)
names(trainCor2)

