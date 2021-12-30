library(usmap)
library(tidyverse)

# upload soil data from all of WI
soil <- read.table("data/WI_Grass_Soil.txt", sep = ",", header = TRUE)
countyFIPS <- read_csv("data/countyFips.csv")

wiGrass <- soil %>%
  filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(county) %>%
  tally() 

winn <- soil %>%
  filter(cec < 100) %>%
  filter(county == "Winnebago") %>%
  droplevels()

# clean wiGrass counties so you can join to countyFIPS by county
levels(as.factor(wiGrass$county)) #"CalumetManitowoc"   "KenoshaRacine" "MilwaukeeWaukesha"
levels(as.factor(countyFIPS$County))

wiGrass <- wiGrass %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond Du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))
doubles <- wiGrass %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

wiGrass <- anti_join(wiGrass, doubles) %>%
  droplevels()

levels(wiGrass$county)

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

wiGrass2 <- bind_rows(wiGrass, doubles1, doubles2)

wiGrass2 %>%
  group_by(county) %>%
  tally() %>%
  arrange(desc(n))

wiGrass3 <- left_join(wiGrass2, countyFIPS, by = c("county" = "County")) %>%
  mutate(n_discrete = cut(n, c(1,25,100,500,1200)))


# observations by county --------------------------------------------------


png("figures/observationsPerCounty.png")
plot_usmap("counties", 
           include = "WI",
           data = wiGrass3,
           values = "n") + 
  scale_fill_gradient(low = "#e9fae3", high = "#025414", name = "No. obs.") 
dev.off()  



# variable importance -----------------------------------------------------

mod <- readRDS("models/tidyPastureALLWI.rds")
mod$fit$importance
#mean decrease accuracy = % inc mse
#shows how much the model accuracy decreases if we leave out that variable
library(forcats)
names <- rownames(mod$fit$importance)

png("figures/IncMSE.png")
mod$fit$importance %>%
  data.frame() %>%
  rownames_to_column() %>%
  select(c(variable = rowname, IncMSE = X.IncMSE)) %>%
  mutate(variable = recode(variable,
                           "total.depth" = "Depth",
                           "elev" = "Elevation",
                           "om" = "OM",
                           "ph" = "pH",
                           "cec" = "CEC", 
                           "slope" = "Slope",
                           "cropname" = "Grass species",
                           "clay" = "Clay",
                           "silt" = "Silt", 
                           "sand" = "Sand",
                           "awc" = "AWC",
                           "ksat" = "Ksat")) %>%
  mutate(var_order = fct_reorder(variable, IncMSE)) %>%
  ggplot() +
  geom_col(aes(y = var_order, x = IncMSE)) +
  ylab(" ") +
  xlab("% Increase in OOB MSE")
dev.off()


# predicted yield vs ssurgo yield -----------------------------------------

soil <- read.table("data/WI_Grass_Soil.txt", sep = ",", header = TRUE)
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

pred_df <- mod %>%
  predict(test) %>%
  bind_cols(test) %>%
  filter(yield < 10)

rmse <- round(sqrt(mean((pred_df$.pred - pred_df$yield)^2)),2)

png("figures/predVssurgo.png")
ggplot(data = pred_df, aes(x = yield, y = .pred)) +
  geom_jitter() +
  xlim(0.75,6) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ylab("Predicted yield") +
  xlab("SSURGO recorded yield") +
  annotate("text", x = 1.4, y = 4.5, label = paste("RMSE = ", rmse))
dev.off()


# error across state OOB ------------------------------------------------------

soil <- read.table("data/WI_Grass_Soil.txt", sep = ",", header = TRUE)
grass <- soil %>%
  filter(cec < 100) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(cropname, yield = nonirryield.r, slope, elev, sand, silt,
                  clay, om, ksat, cec, ph, awc, total.depth, county)) %>%
  drop_na()

set.seed(123)
split <- initial_split(grass, strata = yield)
train <- training(split)
test <- testing(split)

pred_df <- mod %>%
  predict(test) %>%
  bind_cols(test) %>%
  filter(yield < 10)

# join to county fips
levels(as.factor(pred_df$county))

pred_df <- pred_df %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond Du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))
doubles <- pred_df %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

pred_df <- anti_join(pred_df, doubles) %>%
  droplevels()

levels(pred_df$county)

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

pred_df2 <- bind_rows(pred_df, doubles1, doubles2)

levels(as.factor(pred_df2$county))

countyFIPS <- read_csv("data/countyFips.csv")

pred_df3 <- left_join(pred_df2, countyFIPS, by = c("county" = "County"))

error <- pred_df3 %>%
  mutate(err = yield - .pred) %>%
  group_by(county, fips) %>%
  summarise(mse = sum(err^2)/n())

png("figures/OOB_mse.png")
plot_usmap("counties", 
           include = "WI",
           data = error,
           values = "mse") + 
  scale_fill_gradient(low = "#e9fae3", high = "#025414", name = "MSE") 
dev.off()


# error across state all data ---------------------------------------------

pred_df <- mod %>%
  predict(grass) %>%
  bind_cols(grass) %>%
  filter(yield < 10)

# join to county fips
levels(as.factor(pred_df$county))

pred_df <- pred_df %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond Du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))
doubles <- pred_df %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

pred_df <- anti_join(pred_df, doubles) %>%
  droplevels()

levels(pred_df$county)

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

pred_df2 <- bind_rows(pred_df, doubles1, doubles2)

levels(as.factor(pred_df2$county))

countyFIPS <- read_csv("data/countyFips.csv")

pred_df3 <- left_join(pred_df2, countyFIPS, by = c("county" = "County"))

error <- pred_df3 %>%
  mutate(err = yield - .pred) %>%
  group_by(county, fips) %>%
  summarise(mse = sum(err^2)/n())

png("figures/allData_mse.png")
plot_usmap("counties", 
           include = "WI",
           data = error,
           values = "mse") + 
  scale_fill_gradient(low = "#e9fae3", high = "#025414", name = "MSE") 
dev.off()


