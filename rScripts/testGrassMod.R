# this script tests different tidy models based on how many counties were used in the model creation
# and if weather should be incorporated into the model

library(randomForest)
library(tidyverse)
library(tidymodels)
library(sf)
library(usmap)


# load models -------------------------------------------------------------

tidyMod <- readRDS("tidyPasture.rds")
weatherMod <- readRDS("tidyPastureWithWeather.rds")
allWIMod <- readRDS("tidyPastureALLWI.rds")
WI_weather_mod <- readRDS("tidyPastureALLWI_weather.rds")

# load data ---------------------------------------------------------------

soil <- read.table("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data/cropdata/WI_Grass_Soil.txt", sep = ",", header = TRUE)
# add weather data to counties
ppt <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_ppt_normals.csv")
tmin <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmin_normals.csv")
tmax <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmax_normals.csv")
tmean <- read_csv("../../../../Shared drives/Grassland 2.0/Workspaces/Objective 4 - Modeling/prizm data/Prism data/WInormals/WI_county_tmean_normals.csv")


# clean data --------------------------------------------------------------


# edit county names
#levels(as.factor(soil$county))
soil_dup <- soil %>%
  filter(county == "MilwaukeeWaukesha"
         | county == "KenoshaRacine"
         | county == "CalumetManitowoc") %>%
  mutate(county = recode(county,
                         "MilwaukeeWaukesha" = "Milwaukee",
                         "KenoshaRacine" = "Kenosha",
                         "CalumetManitowoc" = "Calumet")) %>%
  mutate_if(is.character, as.factor)

#summary(soil_dup)

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

#levels(as.factor(soil$county))


soil_weather <- soil %>%
  left_join(ppt) %>%
  left_join(tmin) %>%
  left_join(tmax)


# run predictions ---------------------------------------------------------


# make grass yield predictions for all of WI
grass_pred_simple <- tidyMod %>%
  predict(soil_weather) %>%
  bind_cols(soil_weather) %>%
  filter(nonirryield.r <10)

# make grass yield predictions for all of WI
grass_pred_weather <- weatherMod %>%
  predict(soil_weather) %>%
  bind_cols(soil_weather) %>%
  filter(nonirryield.r <10)

# make grass yield predictions for all of WI
grass_pred_full <- allWIMod %>%
  predict(soil_weather) %>%
  bind_cols(soil_weather) %>%
  filter(nonirryield.r <10)

# make grass yield predictions for all of WI with Weather
grass_pred_WIweather <- WI_weather_mod %>%
  predict(soil_weather) %>%
  bind_cols(soil_weather) %>%
  filter(nonirryield.r <10)


# model metrics (rmse) ----------------------------------------------------


# calculate rmse for each county
grass_rmse_simple <- grass_pred_simple %>%
  group_by(county) %>%
  summarize(RMSE = sqrt(mean((.pred - nonirryield.r)^2))) 

grass_rmse_weather <- grass_pred_weather %>%
  group_by(county) %>%
  summarize(RMSE = sqrt(mean((.pred - nonirryield.r)^2))) 

grass_rmse_full <- grass_pred_full %>%
  group_by(county) %>%
  summarize(RMSE = sqrt(mean((.pred - nonirryield.r)^2))) 

grass_rmse_WIWeather <- grass_pred_WIweather %>%
  group_by(county) %>%
  summarize(RMSE = sqrt(mean((.pred - nonirryield.r)^2))) 


# clean county names ------------------------------------------------------


# all of WI counties
# original counties used in model creation
counties1 <- c("Brown", "Crawford", "Kewaunee", "Monroe", "Marathon", "Taylor", "Vernon", "Clark", "Grant", 
               "Shawano", "Lafayette", "Dane", "Chippewa", "Dodge", "Fond du Lac", "Manitowoc", "Barron")
# the rest of WI counties
counties2 <- sort(c("Adams", "Ashland", "Bayfield", "Buffalo","Burnett", "Columbia", "Door", "Douglas",
              "Dunn", "Eau Claire", "Florence", 'Forest', "Green", "Green Lake", "Iowa", "Iron",
              "Jackson", "Jefferson", "Juneau", "La Crosse", "Langlade","Lincoln", "Marinette",
              "Marquette", "Menominee", "Oconto", "Oneida", "Outagamie", "Ozaukee", "Pepin", "Pierce",
              "Polk", "Portage", "Price", "Richland", "Rock", "Rusk", "St. Croix", "Sauk", "Sawyer",
              "Sheboygan", "Trempealeau", "Vilas", "Walworth", "Washburn", "Washington", "Waupaca",
              "Waushara", "Winnebago", "Wood", "Calumet", "Kenosha", "Racine", "Milwaukee", "Waukesha"))

county1 <- data.frame(county = counties1, model = "yes") 
county2 <- data.frame(county = counties2, model = "no")
# full county data set
counties <- bind_rows(county1, county2)

# WIfips <- data.frame(code = fips("WI", county = counties$county))
# # counties with FIPS codes
# WIfips <- bind_cols(WIfips, counties)

cut_bins <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7)

# finish cleaning county rmse data set ------------------------------------
#by binding the cleaned counties back into the data set and joining to FIPS

grass_rmse_full_WI <- grass_rmse_full %>%
  left_join(counties) %>%
  mutate(rmse_fct = cut(RMSE, breaks = cut_bins))

grass_rmse_full_simple <- grass_rmse_simple %>%
  left_join(counties) %>%
  mutate(rmse_fct = cut(RMSE, breaks = cut_bins))

grass_rmse_full_weather <- grass_rmse_weather %>%
  left_join(counties) %>%
  mutate(rmse_fct = cut(RMSE, breaks = cut_bins))

grass_rmse_full_WIweather <- grass_rmse_WIWeather %>%
  left_join(counties) %>%
  mutate(rmse_fct = cut(RMSE, breaks = cut_bins))


# load wi shapefile -------------------------------------------------------


countiesST <- st_read("/Volumes/GoogleDrive/My Drive/geospatial capstone/data/WI_Counties2010/WI_Counties2010.shp")

# join shape file to rmse
counties_shape_weather <- left_join(countiesST, grass_rmse_full_weather, by = c("NAME" = "county"))
counties_shape_simple <- left_join(countiesST, grass_rmse_full_simple, by = c("NAME" = "county"))
counties_shape_full <- left_join(countiesST, grass_rmse_full_WI, by = c("NAME" = "county"))
counties_shape_fullWeather <- left_join(countiesST, grass_rmse_full_WIweather, by = c("NAME" = "county"))


# statewide plots ---------------------------------------------------------


# plot rmse by county. full saturation is original counties in model 
png("GrassPredRMSE_weather.png")
p2 <- ggplot() +
  geom_sf(data = counties_shape_weather, aes(fill = rmse_fct, alpha = model)) +
  scale_fill_manual(values = c("#D39200", "#93AA00", "#00BA38", "#00C19F",
                               "#00B9E3", "#619CFF", "#DB72FB", "#FF61C3"))+
  scale_alpha_discrete( 
                       range = c(0.2, 1), guide = FALSE) +
  ggtitle("Original counties plus weather model error") +
  coord_sf()
dev.off()
p2

png("GrassPredRMSE.png")
p1 <- ggplot() +
  geom_sf(data = counties_shape_simple, aes(fill = rmse_fct, alpha = model)) +
  scale_fill_manual(values = c("#D39200", "#93AA00", "#00BA38", "#00C19F",
                               "#00B9E3", "#619CFF", "#DB72FB", "#FF61C3"))+
  scale_alpha_discrete( 
    range = c(0.2, 1), guide = FALSE) +
  ggtitle("Original grass yield error model") +
  coord_sf()
dev.off()
p1

p3 <- ggplot() +
  geom_sf(data = counties_shape_full, aes(fill = rmse_fct, alpha = model)) +
  scale_fill_manual(values = c("#F8766D", "#D39200", "#93AA00", "#00BA38", 
                               "#00C19F", "#00B9E3", "#619CFF"))+
  scale_alpha_discrete( 
   range = c(0.2, 1), guide = FALSE) +
  ggtitle("Full WI model error") +
  coord_sf()
p3

p4 <- ggplot() +
  geom_sf(data = counties_shape_fullWeather, aes(fill = rmse_fct, alpha = model)) +
  scale_fill_manual(values = c("#D39200", "#93AA00", "#00BA38", 
                               "#00C19F", "#00B9E3"))+
  scale_alpha_discrete( 
    range = c(0.2, 1), guide = FALSE) +
  ggtitle("Full WI plus weather model error") +
  coord_sf()

png("GrassErrCompare.png", width = 800, height = 800)
gridExtra::grid.arrange(p1, p2, p3, p4)
dev.off()

png("RMSE_hist_simple.png")
hist_simple <- ggplot() +
  geom_histogram(data = grass_rmse_full_simple, aes(x = RMSE, fill = model), bins = 40) +
  ggtitle("Distribution of prediction error in the model with no weather")
dev.off()

png("RMSE_hist_weather.png")
hist_weather <- ggplot() +
  geom_histogram(data = grass_rmse_full_weather, aes(x = RMSE, fill = model), bins = 40) +
  ggtitle("Distribution of prediction error in the model WITH weather")
dev.off()

hist_WI <- ggplot() +
  geom_histogram(data = grass_rmse_full_WI, aes(x = RMSE, fill = model), bins = 40) +
  ggtitle("Distribution of prediction error full WI model")

hist_WIWeather <- ggplot() +
  geom_histogram(data = grass_rmse_full_WIweather, aes(x = RMSE, fill = model), bins = 40) +
  ggtitle("Distribution of prediction error full WI and Weather model")

gridExtra::grid.arrange(hist_simple, hist_weather, hist_WI, hist_WIWeather)
