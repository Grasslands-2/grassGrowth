# this script creates the tidy random forest models for grass yield predictions with different data sets
# just 17 wi counties and all wi counties, with and without weather

library(tidyverse) # cleans data
#library(caret) # machine learning package (for creating training and testing data sets)
#library(pmml)
library(tidymodels)
library(randomForest)
library(gt)


# load and clean data -----------------------------------------------------

crops <- read.table("data/prod_index_soil.txt", header = TRUE, sep = ",")

crops %>%
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
  mutate(county = recode(county,
                         "FondDuLac" = "Fond du Lac")) %>%
  drop_na() %>%
  group_by(county, cropname) %>%
  tally()

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
  mutate(county = recode(county,
                         "FondDuLac" = "Fond du Lac")) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope = slope.r, elev = elev.r, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth)) %>%
  drop_na()

grass %>%
  group_by(cropname) %>%
  tally()

#https://juliasilge.com/blog/sf-trees-random-tuning/

set.seed(123)
split <- initial_split(grass, strata = yield)
train <- training(split)
test <- testing(split)

#recipe 
grass_rec <- recipe(yield ~ ., data = train)

grass_prep <- prep(grass_rec)
juiced <- juice(grass_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

doParallel::registerDoParallel()

rf_grid <- grid_regular(
  mtry(range = c(3, 8)),
  min_n(range = c(3, 8)),
  trees(range = c(50,75)),
  levels = 3
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

summary(regular_res %>% collect_metrics() %>% filter(.metric == "rmse"))

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse",
         min_n == 6)  %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(trees, mean, color = mtry)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

metrics <- regular_res %>% collect_metrics()
write.csv(metrics, "tidyTuneMetrics.csv", row.names = FALSE, quote = FALSE)
metrics <- read_csv("tidyTuneMetrics.csv")
#choose best model
best_rmse <- select_best(regular_res, "rmse")

best_rmse

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_rf

# library(vip)
# 
# final_rf %>%
#   set_engine("randomForest") %>%
#   fit(yield ~ .,
#       data = juice(grass_prep)
#   ) %>%
#   vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split)

final_res %>%
  collect_metrics()

pred_df <- final_res %>%
  collect_predictions()

pred_df %>%
  ggplot(aes(yield, .pred)) + 
  geom_point()

mod <- final_rf %>%
  set_engine("randomForest") %>%
  fit(yield ~ .,
      data = train
  )

saveRDS(mod, "tidyPasture.rds")

mod %>%
  predict(test) %>%
  bind_cols(test) %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()

test <- mod %>%
  predict(test) %>%
  bind_cols(test)

mod
rmse <- mean(mod$fit$mse)
RMSE1 <- sqrt(mean((test$.pred - test$yield)^2)) #0.17

mod <- readRDS("tidyPasture.rds")


# add weather -------------------------------------------------------------

# add weather data to counties
ppt <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_ppt_normals.csv")
tmin <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmin_normals.csv")
tmax <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmax_normals.csv")
tmean <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmean_normals.csv")

grass_weather <- crops %>%
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
  mutate(county = recode(county,
                         "FondDuLac" = "Fond du Lac")) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope = slope.r, elev = elev.r, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth, county)) %>%
  left_join(ppt) %>%
  left_join(tmin) %>%
  left_join(tmax) %>%
  dplyr::select(-county) %>%
  drop_na()

#https://juliasilge.com/blog/sf-trees-random-tuning/

set.seed(123)
split <- initial_split(grass_weather, strata = yield)
train <- training(split)
test <- testing(split)

#recipe 
grass_rec <- recipe(yield ~ ., data = train)

grass_prep <- prep(grass_rec)
juiced <- juice(grass_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

doParallel::registerDoParallel()

rf_grid <- grid_regular(
  mtry(range = c(4, 9)),
  min_n(range = c(4, 9)),
  trees(range = c(50,75)),
  levels = 3
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

summary(regular_res %>% collect_metrics() %>% filter(.metric == "rmse"))

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse",
         min_n == 6)  %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(trees, mean, color = mtry)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

metrics <- regular_res %>% collect_metrics()
write.csv(metrics, "tidyTuneMetricsOriginalWeather.csv", row.names = FALSE, quote = FALSE)
metrics <- read_csv("tidyTuneMetricsOriginalWeather.csv")
#choose best model
best_rmse <- select_best(regular_res, "rmse")

best_rmse

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_rf

# library(vip)
# 
# final_rf %>%
#   set_engine("randomForest") %>%
#   fit(yield ~ .,
#       data = juice(grass_prep)
#   ) %>%
#   vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split)

final_res %>%
  collect_metrics()

pred_df <- final_res %>%
  collect_predictions()

pred_df %>%
  ggplot(aes(yield, .pred)) + 
  geom_point()

mod <- final_rf %>%
  set_engine("randomForest") %>%
  fit(yield ~ .,
      data = train
  )

saveRDS(mod, "tidyPastureWithWeather.rds")

mod %>%
  predict(test) %>%
  bind_cols(test) %>%
  ggplot(aes(x = yield, y = .pred)) +
  geom_point()

test <- mod %>%
  predict(test) %>%
  bind_cols(test)

mod
rmse <- mean(mod$fit$mse)
RMSE2 <- sqrt(mean((test$.pred - test$yield)^2)) #0.17

# all WI model ------------------------------------------------------------


# upload soil data from all of WI
soil <- read.table("data/WI_Grass_Soil.txt", sep = ",", header = TRUE)
summary(soil)
levels(as.factor(soil$cropname))

wiGrass <- soil %>%
  filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(cropname) %>%
  tally()

wiGrass2 <- soil %>%
  filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(county) %>%
  tally()

grass <- soil %>%
  filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth)) %>%
  drop_na()

set.seed(123)
split <- initial_split(grass, strata = yield)
train <- training(split)
test <- testing(split)

#recipe 
grass_rec <- recipe(yield ~ ., data = train)

grass_prep <- prep(grass_rec)
juiced <- juice(grass_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

doParallel::registerDoParallel()

rf_grid <- grid_regular(
  mtry(range = c(3, 8)),
  min_n(range = c(3, 8)),
  trees(range = c(50,75)),
  levels = 3
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

summary(regular_res %>% collect_metrics() %>% filter(.metric == "rmse"))

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse",
         min_n == 6)  %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(trees, mean, color = mtry)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

metrics <- regular_res %>% collect_metrics()
write.csv(metrics, "tidyTuneMetrics.csv", row.names = FALSE, quote = FALSE)
write.csv(metrics, "tidyTuneMetricsAllWI.csv", row.names = FALSE, quote = FALSE)
metrics <- read_csv("tidyTuneMetrics.csv")
metricsRMSE <- metrics %>%
  filter(.metric == "rmse")
metricsFull <- read_csv("tidyTuneMetricsAllWI.csv")
metricsFullrmse <- metricsFull %>%
  filter(.metric == "rmse")
#choose best model
best_rmse <- select_best(regular_res, "rmse")

best_rmse

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_rf

final_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split)

final_res %>%
  collect_metrics()

pred_df <- final_res %>%
  collect_predictions()

pred_df %>%
  ggplot(aes(yield, .pred)) + 
  geom_point()

rmse <- sqrt(mean((pred_df$.pred - pred_df$yield)^2))

mod <- final_rf %>%
  set_engine("randomForest", importance = TRUE) %>%
  fit(yield ~ .,
      data = train
  )

saveRDS(mod, "models/tidyPastureALLWI.rds")

# all WI with weather -----------------------------------------------------

soil <- read.table("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data/cropdata/WI_Grass_Soil.txt", sep = ",", header = TRUE)
soil_dup <- soil %>%
  filter(county == "MilwaukeeWaukesha"
         | county == "KenoshaRacine"
         | county == "CalumetManitowoc") %>%
  mutate(county = recode(county,
                         "MilwaukeeWaukesha" = "Milwaukee",
                         "KenoshaRacine" = "Kenosha",
                         "CalumetManitowoc" = "Calumet")) %>%
  mutate_if(is.character, as.factor)
soil <- soil %>%
  filter(county != "CalumetManitowoc") %>%
  bind_rows(soil_dup) %>%
  mutate(county = recode(county,
                         "MilwaukeeWaukesha" = "Waukesha", 
                         "KenoshaRacine" = "Racine",
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse"))

# add weather data to counties
ppt <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_ppt_normals.csv")
tmin <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmin_normals.csv")
tmax <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmax_normals.csv")
tmean <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmean_normals.csv")

# upload soil data from all of WI
levels(as.factor(soil$cropname))
levels(as.factor(soil$county))
grass <- soil %>%
  filter(cec < 100) %>%
  # mutate(county = recode(county,
  #                        "FondDuLac" = "Fond du Lac")) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth, county)) %>%
  left_join(ppt) %>%
  left_join(tmin) %>%
  left_join(tmax) %>%
  dplyr::select(-county) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

summary(grass)

set.seed(123)
split <- initial_split(grass, strata = yield)
train <- training(split)
test <- testing(split)

#recipe 
grass_rec <- recipe(yield ~ ., data = train)

grass_prep <- prep(grass_rec)
juiced <- juice(grass_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

doParallel::registerDoParallel()

rf_grid <- grid_regular(
  mtry(range = c(4, 9)),
  min_n(range = c(4, 9)),
  trees(range = c(50,75)),
  levels = 3
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

summary(regular_res %>% collect_metrics() %>% filter(.metric == "rmse"))

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse",
         min_n == 6)  %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(trees, mean, color = mtry)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

metrics <- regular_res %>% collect_metrics()
write.csv(metrics, "tidyTuneMetricsAllWI_weather.csv", row.names = FALSE, quote = FALSE)
metricsRMSE <- metrics %>%
  filter(.metric == "rmse")

#choose best model
best_rmse <- select_best(regular_res, "rmse")

best_rmse

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_rf

# library(vip)
# 
# final_rf %>%
#   set_engine("randomForest") %>%
#   fit(yield ~ .,
#       data = juice(grass_prep)
#   ) %>%
#   vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split)

final_res %>%
  collect_metrics()

pred_df <- final_res %>%
  collect_predictions()

pred_df %>%
  ggplot(aes(yield, .pred)) + 
  geom_point()

rmse4 <- sqrt(mean((pred_df$.pred - pred_df$yield)^2))

mod <- final_rf %>%
  set_engine("randomForest") %>%
  fit(yield ~ .,
      data = train
  )

saveRDS(mod, "tidyPastureALLWI_weather.rds")


# add county instead of weather to full model -----------------------------

soil <- read.table("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data/cropdata/WI_Grass_Soil.txt", sep = ",", header = TRUE)
soil_dup <- soil %>%
  filter(county == "MilwaukeeWaukesha"
         | county == "KenoshaRacine"
         | county == "CalumetManitowoc") %>%
  mutate(county = recode(county,
                         "MilwaukeeWaukesha" = "Milwaukee",
                         "KenoshaRacine" = "Kenosha",
                         "CalumetManitowoc" = "Calumet")) %>%
  mutate_if(is.character, as.factor)
soil <- soil %>%
  filter(county != "CalumetManitowoc") %>%
  bind_rows(soil_dup) %>%
  mutate(county = recode(county,
                         "MilwaukeeWaukesha" = "Waukesha", 
                         "KenoshaRacine" = "Racine",
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse")) %>%
  mutate_if(is.character, as.factor)

summary(soil)

grass <- soil %>%
  filter(cec < 100) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth, county)) %>%
  drop_na()

summary(grass)

set.seed(123)
split <- initial_split(grass, strata = yield)
train <- training(split)
test <- testing(split)

#recipe 
grass_rec <- recipe(yield ~ ., data = train)

grass_prep <- prep(grass_rec)
juiced <- juice(grass_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(grass_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

doParallel::registerDoParallel()

rf_grid <- grid_regular(
  mtry(range = c(3, 8)),
  min_n(range = c(3, 9)),
  trees(range = c(50,75)),
  levels = 3
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)
## cannot handle categorical variable with more than 53 levels
# summary(regular_res %>% collect_metrics() %>% filter(.metric == "rmse"))
# 
# regular_res %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   mutate(min_n = factor(min_n)) %>%
#   ggplot(aes(mtry, mean, color = min_n)) +
#   geom_line(alpha = 0.5, size = 1.5) +
#   geom_point() +
#   labs(y = "RMSE")
# 
# regular_res %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse",
#          min_n == 6)  %>%
#   mutate(mtry = factor(mtry)) %>%
#   ggplot(aes(trees, mean, color = mtry)) +
#   geom_line(alpha = 0.5, size = 1.5) +
#   geom_point() +
#   labs(y = "RMSE")
# 
# metrics <- regular_res %>% collect_metrics()
# write.csv(metrics, "tidyTuneMetricsAllWI_weather.csv", row.names = FALSE, quote = FALSE)
# metricsRMSE <- metrics %>%
#   filter(.metric == "rmse")
# 
# #choose best model
# best_rmse <- select_best(regular_res, "rmse")
# 
# best_rmse
# 
# final_rf <- finalize_model(
#   tune_spec,
#   best_rmse
# )
# 
# final_rf
# 
# # library(vip)
# # 
# # final_rf %>%
# #   set_engine("randomForest") %>%
# #   fit(yield ~ .,
# #       data = juice(grass_prep)
# #   ) %>%
# #   vip(geom = "point")
# 
# final_wf <- workflow() %>%
#   add_recipe(grass_rec) %>%
#   add_model(final_rf)
# 
# final_res <- final_wf %>%
#   last_fit(split)
# 
# final_res %>%
#   collect_metrics()
# 
# pred_df <- final_res %>%
#   collect_predictions()
# 
# pred_df %>%
#   ggplot(aes(yield, .pred)) + 
#   geom_point()
# 
# rmse4 <- sqrt(mean((pred_df$.pred - pred_df$yield)^2))
# 
# mod <- final_rf %>%
#   set_engine("randomForest") %>%
#   fit(yield ~ .,
#       data = train
#   )
# 
# saveRDS(mod, "tidyPastureALLWI_weather.rds")
