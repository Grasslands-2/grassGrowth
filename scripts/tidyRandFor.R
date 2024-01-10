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
# check tidyPasture
mod1 <- readRDS("models/tidyPasture.rds")
mod1$preproc

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
  filter(yield < 10) %>%
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
#write.csv(metrics, "tidyTuneMetrics.csv", row.names = FALSE, quote = FALSE)
write.csv(metrics, "modelOutputs/tidyTuneMetricsAllWI.csv", row.names = FALSE, quote = FALSE)
metrics <- read_csv("modelOutputs/tidyTuneMetricsALLWI.csv")
metricsRMSE <- metrics %>%
  filter(.metric == "rmse")
metricsFull <- read_csv("modelOutputs/tidyTuneMetricsAllWI.csv")
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
# check is this the same as tidyPasture.rds
mod2 <- readRDS("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/GrazeScapeModelFiles/tidyPastureALLWI.rds")
mod2

predFull <- mod2 %>%
  predict(grass) %>%
  mutate(full = .pred)
predNoCec <- mod %>%
  predict(grass) %>%
  mutate(noCec = .pred)

testPreds = bind_cols(predNoCec, predFull)
ggplot(testPreds, aes(x = full, y = noCec)) +
  geom_point()

## vip of full model------------------------------

full_mod <- readRDS("models/tidyPastureALLWI.rds")

trainx <- train %>%
  dplyr::select(-c(yield))

library(DALEXtra)
full_explainer <- 
  explain_tidymodels(
    full_mod, 
    data = trainx, 
    y = train$yield,
    label = "random forest",
    verbose = FALSE
  )

set.seed(1804)
vip_rf <- model_parts(full_explainer, loss_function = loss_root_mean_square)
plot(vip_rf)

ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name, 
                      "after permutations\n(higher indicates more important)")
  
  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% 
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable)) 
  if(length(obj) > 1) {
    p <- p + 
      facet_wrap(vars(label)) +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss, color = label),
                 size = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 size = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    
  }
  p +
    theme(legend.position = "none") +
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL)
}

ggplot_imp(vip_rf)

obj <- list(vip_rf)
metric_name <- attr(obj[[1]], "loss_name")
metric_lab <- paste(metric_name, 
                    "after permutations\n(higher indicates more important)")

full_vip <- bind_rows(obj) %>%
  filter(variable != "_baseline_")

perm_vals <- full_vip %>% 
  filter(variable == "_full_model_") %>% 
  group_by(label) %>% 
  summarise(dropout_loss = mean(dropout_loss))

full_vip2 <- full_vip %>%
  filter(variable != "_full_model_") %>% 
  mutate(variable = fct_reorder(variable, dropout_loss)) %>%
  group_by(variable) %>%
  summarise(meanLoss = mean(dropout_loss),
            count = n(),
            se = sd(dropout_loss)/sqrt(count))

ggplot(data = full_vip2, aes(meanLoss, variable)) +
  geom_boxplot(fill = "#91CBD765", alpha = 0.4) +
  geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
             size = 0.8, lty = "dotdash", alpha = 0.9)

%>%
  ggplot(aes(dropout_loss, variable)) 

p <- p + 
    geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
               size = 1.4, lty = 2, alpha = 0.7) +
    geom_boxplot(fill = "#91CBD765", alpha = 0.4)
  
p +
  theme(legend.position = "none") +
  labs(x = metric_lab, 
       y = NULL,  fill = NULL,  color = NULL)

# remove CEC --------------------------------------------------------------

grass <- soil %>%
  #filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, silt,
                  clay, om, ksat, ph, awc, total.depth)) %>%
  filter(yield < 10) %>%
  drop_na()

set.seed(123)
split <- initial_split(grass, strata = yield)
train <- training(split)
test <- testing(split)

hist(test$yield)
hist(train$yield)
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
  trees(range = c(50,100)),
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
         min_n == 3)  %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(trees, mean, color = mtry)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

metrics <- regular_res %>% collect_metrics()
#write.csv(metrics, "tidyTuneMetrics.csv", row.names = FALSE, quote = FALSE)
write.csv(metrics, "modelOutputs/tidyTuneMetricsAllWI_noCEC.csv", row.names = FALSE, quote = FALSE)
metricsFull <- read_csv("modelOutputs/tidyTuneMetricsALLWI.csv")
metrics <- read_csv("modelOutputs/tidyTuneMetricsAllWI_noCEC.csv")
metricsRMSE <- metrics %>%
  filter(.metric == "rmse")
metricsRMSE
metricsFull <- read_csv("modelOutputs/tidyTuneMetricsAllWI_noCEC.csv")
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

mod
saveRDS(mod, "models/tidyPastureALLWInoCec.rds")
# check is this the same as tidyPasture.rds
mod <- readRDS("models/tidyPastureALLWInoCec.rds")

pred <- mod %>%
  predict(test) %>%
  bind_cols(test)

ggplot(pred, aes(x = yield, y = .pred)) +
  geom_point()

rmse = round(sqrt(mean((pred$.pred - pred$yield)^2)),3)
rmse/mean(grass$yield)

rss <- sum((pred$.pred - pred$yield) ^ 2)
tss <- sum((pred$yield - mean(pred$yield)) ^ 2)
rsq <- 1 - rss/tss

grass <- soil %>%
  #filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, silt,
                  clay, om, cec, ksat, ph, awc, total.depth)) %>%
  filter(yield < 10) %>%
  drop_na()

pred2 <- mod2 %>%
  predict(grass)


preds = bind_cols(noCec = pred$.pred, full = pred2$.pred)

plot(preds$noCec ~ preds$full)

## variable importances and partial dependance plots
cbind(rownames(mod$fit$importance), mod$fit$importance) %>% 
  as_tibble() %>%
  select(predictor= V1, IncNodePurity)%>% 
  mutate(IncNodePurity= as.numeric(IncNodePurity)) %>% 
  arrange(desc((IncNodePurity))) %>%
  gt() %>%
  tab_options(data_row.padding = px(6)) 

## vip with no cec model---------------------------

nocec_mod <- readRDS("models/tidyPastureALLWInoCec.rds")

noCec_explainer <- 
  explain_tidymodels(
    nocec_mod, 
    data = train, 
    y = train$yield,
    label = "random forest",
    verbose = FALSE
  )

set.seed(1804)
vip_rf <- model_parts(noCec_explainer, loss_function = loss_root_mean_square)
plot(vip_rf)

ggplot_imp(vip_rf)

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

# including more soil data--------------------

# crops <- read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
#   mutate_if(is.character, as.factor)
# 
# summary(crops)
# 
# 
# # ##TODO remove sar, gypsum, ec, siltco and siltfine and partdensity (too many NAs)
# # crops <- crops %>%
# #   dplyr::select(-c(sar, gypsum, ec, siltco, siltfine, partdensity))
# # 
# # summary(crops)
# # highCEC <- crops %>%
# #   filter(cec > 100) %>%
# #   droplevels()
# # 
# # summary(highCEC)
# # 
# # wormet <- highCEC %>% filter(compname == "Wormet") %>% droplevels()
# # summary(wormet)
# # veedum <- highCEC %>% filter(compname == "Veedum") %>% droplevels()
# # summary(veedum)
# 
# names(crops)
# grass <- crops %>%
#   filter(cec < 100,
#          nonirryield.r < 10) %>%
#   mutate_if(is.character, as.factor) %>%
#   dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, sandvc, sandco, sandmed, sandfine, silt,
#                   siltco, siltfine, clay, om, partdensity, ksat, gypsum, sar, ec, cec, ph, awc, total.depth, frag3, frag10)) 
# 
# summary(grass)
# 
# # https://github.com/andrew-couch/Tidy-Tuesday/blob/master/Season%202/Scripts/TidyTuesdayDimensionalityReduction.Rmd
# set.seed(0731)
# tidy_split <- initial_split(grass, strata = yield)
# train <- training(tidy_split)
# test <- testing(tidy_split)
# 
# # basic dimension reduction
# basic_dim_red <- recipe(yield~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) # remove columns with near zero variance
# 
# basic_dim_red %>% prep %>% juice()
# 
# basic_dim_red %>% prep %>% juice() %>% ncol()
# 
# # remove correlated variables
# full_dim_red <- recipe(yield~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) %>%
#   step_corr(all_numeric(), -all_outcomes())
# 
# full_dim_red %>% prep %>% juice()
# 
# full_dim_red %>% prep %>% juice() %>% ncol()
# 
# # cor_tune <- range(0.8,1)
# # tune_dim_red <- recipe(yield~., data = train) %>%
# #   step_impute_mean(all_numeric(), -all_outcomes()) %>%
# #   step_nzv(all_numeric(), -all_outcomes()) %>%
# #   step_corr(all_numeric(), -all_outcomes(), threshold = tune("corr_tune"))
# # 
# # tune_dim_red %>% prep %>% juice()
# # 
# # full_dim_red %>% prep %>% juice() %>% ncol()
# 
# # which features are the most correlated?
# recipe(yield~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   prep() %>%
#   juice() %>%
#   select(-cropname, -yield) %>%
#   cor() %>%
#   as_tibble(rownames = "features") %>%
#   pivot_longer(-features) %>%
#   filter(features > name) %>%
#   drop_na() %>%
#   arrange(desc(abs(value)))
# 
# # histogram if the correlated features
# recipe(yield ~., data = train)%>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   prep() %>%
#   juice() %>%
#   select(-cropname, -yield) %>%
#   cor() %>%
#   as_tibble(rownames = "features") %>%
#   pivot_longer(-features) %>%
#   filter(features > name) %>%
#   drop_na() %>%
#   ggplot(aes(x = value)) +
#   geom_histogram(color = "white") +
#   scale_x_continuous(labels = scales::label_percent())
# 
# 
# rf_model <- rand_forest(
#   mtry = tune(),
#   trees = tune(),
#   min_n = tune()
# ) %>%
#   set_mode("regression") %>%
#   set_engine("randomForest")
# 
# 
# # manually inspect VIP and use intuition to choose features
# vip_model <- rand_forest(trees = 60) %>%
#   set_mode("regression") %>%
#   set_engine("ranger", importance = "impurity") %>%
#   fit(yield ~., data = full_dim_red %>% prep %>% juice())
# 
# vip::vip(vip_model)
# 
# vip::vi(vip_model) %>%
#   filter(Importance > 0)
# 
# # recursive feature elimination
# install.packages("remotes")
# remotes::install_github("stevenpawley/recipesSelection")
# library(recipeselectors)
# rfe_model <- rand_forest(mode = "regression") %>% set_engine("ranger", importance = "permutation")  # could use permutation
#   
# rfe_rec5 <- recipe(yield ~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) %>%
#   step_corr(all_numeric(), -all_outcomes()) %>%
#   step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.5) # could tune the threshold
#   
# rfe_rec6 <- recipe(yield ~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) %>%
#   step_corr(all_numeric(), -all_outcomes()) %>%
#   step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.6)
# 
# rfe_rec7 <- recipe(yield ~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) %>%
#   step_corr(all_numeric(), -all_outcomes()) %>%
#   step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.7)
# 
# 
# rfe_rec8 <- recipe(yield ~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) %>%
#   step_corr(all_numeric(), -all_outcomes()) %>%
#   step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.8)
# 
# rfe_rec9 <- recipe(yield ~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) %>%
#   step_corr(all_numeric(), -all_outcomes()) %>%
#   step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.9)
# 
# rfe_rec5 %>% #9 var
#   prep() %>%
#   juice()
# 
# rfe_rec6 %>% #8 var
#   prep() %>%
#   juice()
# 
# rfe_rec7 %>% #6 var
#   prep() %>%
#   juice()
# 
# rfe_rec8 %>% #5 var
#   prep() %>%
#   juice()
# 
# rfe_rec9 %>% #3 var
#   prep() %>%
#   juice()
# 
# tune_spec <- rand_forest(
#   mtry = tune(),
#   trees = tune(),
#   min_n = tune()
# ) %>%
#   set_mode("regression") %>%
#   set_engine("ranger")
# 
# 
# #https://juliasilge.com/blog/sf-trees-random-tuning/
# 
# set.seed(123)
# split <- initial_split(grass, strata = yield)
# train <- training(split)
# test <- testing(split)
# 
# library(recipeselectors)
# rfe_model <- rand_forest(mode = "regression") %>% set_engine("ranger", importance = "permutation")  # could use permutation
# 
# #recipe 
# rfe_rec5 <- recipe(yield ~., data = train) %>%
#   step_impute_mean(all_numeric(), -all_outcomes()) %>%
#   step_nzv(all_numeric(), -all_outcomes()) %>%
#   step_corr(all_numeric(), -all_outcomes()) %>%
#   step_select_vip(all_predictors(), outcome = "yield", model = rfe_model, threshold = 0.5) # could tune the threshold
# 
# grass_prep <- prep(rfe_rec5)
# juiced <- juice(grass_prep)
# 
# tune_spec <- rand_forest(
#   mtry = tune(),
#   trees = tune(),
#   min_n = tune()
#  ) %>%
#    set_mode("regression") %>%
#    set_engine("ranger")
# 
# tune_wf <- workflow() %>%
#   add_recipe(rfe_rec5) %>%
#   add_model(tune_spec)
# 
# # train hyperparameters
# set.seed(234)
# folds <- vfold_cv(train)
# 
# doParallel::registerDoParallel()
# 
# rf_grid <- grid_regular(
#   mtry(range = c(3, 4)),
#   min_n(range = c(4, 5)),
#   trees(range = c(50,75)),
#   levels = 2
# )
# 
# set.seed(456)
# regular_res <- tune_grid(
#   tune_wf,
#   resamples = folds,
#   grid = rf_grid
# )
# 
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
# write.csv(metrics, "tidyTuneMetrics.csv", row.names = FALSE, quote = FALSE)
# metrics <- read_csv("tidyTuneMetrics.csv")
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
# mod <- final_rf %>%
#   set_engine("randomForest") %>%
#   fit(yield ~ .,
#       data = train
#   )
# 
# saveRDS(mod, "tidyPasture.rds")
# saveRDS(mod, "fullSoilTidyPasture.rds")
# 
# mod %>%
#   predict(test) %>%
#   bind_cols(test) %>%
#   ggplot(aes(x = yield, y = .pred)) +
#   geom_point()
# 
# test <- mod %>%
#   predict(test) %>%
#   bind_cols(test)
# 
# mod
# rmse <- mean(mod$fit$mse)
# RMSE1 <- sqrt(mean((test$.pred - test$yield)^2)) #0.17
# 
# mod <- readRDS("tidyPasture.rds")
# # check tidyPasture
# mod1 <- readRDS("models/tidyPasture.rds")
# mod$fit$importance
# 
# # compare the new and old models
# modOld <- readRDS("models/tidyPasture.rds")
# modNew <- readRDS("fullSoilTidyPasture.rds")
# modOld
# modNew
