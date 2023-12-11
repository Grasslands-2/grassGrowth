# this script is creating partial dependence plots for all of the variables used in the WI grass mod

library(tidyverse)
library(randomForest)
library(tidymodels)

# upload soil data from all of WI
soil <- read.table("data/WI_Grass_Soil.txt", sep = ",", header = TRUE)

grass <- soil %>%
  filter(cec < 40) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth)) %>%
  filter(yield < 10) %>%
  drop_na()


hist(grass$yield)

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

mod <- readRDS(file = "models/tidyPastureALLWI.rds")
mod
rf_grid <- grid_regular(
  mtry(range = c(5,6)),
  min_n(range = c(3, 4)),
  trees(range = c(70,75)),
  levels = 1
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

best_rmse <- select_best(regular_res, "rmse")

best_rmse

final_res <- tune_wf %>%
  finalize_workflow(best_rmse) %>%
  last_fit(split)

collect_metrics(final_res)

final_fitted <- final_res$.workflow[[1]]
predict(final_fitted, train)

library(DALEXtra)



grass_explainer <- explain_tidymodels(
  final_fitted,
  data = dplyr::select(train, -yield),
  y = as.integer(train$yield),
  verbose = FALSE
)

grass_explainer <- explain_tidymodels(
  modOld,
  data = dplyr::select(train, -yield),
  y = as.integer(train$yield),
  verbose = FALSE
)

grass_explainer

pdp_cec <- model_profile(
  grass_explainer,
  variables = "cec",
  N = NULL
)

pdp_cec

as_tibble(pdp_cec$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "CEC",
    y = "Predicted grass yield",
    color = NULL,
    title = "Partial dependence plot for CEC",
    subtitle = "Predictions from a decision tree model"
  )

pdp_depth <- model_profile(
  grass_explainer,
  variables = "total.depth",
  N = NULL
)

as_tibble(pdp_depth$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "Soil depth",
    y = "Predicted grass yield",
    color = NULL,
    title = "Partial dependence plot for depth",
    subtitle = "Predictions from a decision tree model"
  )


pdp_ksat <- model_profile(
  grass_explainer,
  variables = "ksat",
  N = NULL,
  groups = "cropname"
)

as_tibble(pdp_ksat$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "Ksat",
    y = "Predicted grass yield",
    color = NULL,
    title = "Partial dependence plot for ksat",
    subtitle = "Predictions from a decision tree model"
  )



