# this script creates figures for the manuscript

#library(usmap)
library(tidyverse)
library(tidymodels)
library(DALEXtra)
library(ggplot2)
library(sf)
library(rgdal)
library(colorspace)
library(ggthemes)

# models ------------------------
modOG <- readRDS("models/tidyPastureALLWI.rds")
modCor <- readRDS("models/pastureCorRanger.rds")
#modCor2 <- readRDS("models/pastureCor2Ranger.rds")

# load and clean data from all of WI ------------------------
crops <- read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
  mutate_if(is.character, as.factor)

# clean data--------------
grass <- crops %>%
  filter(cec < 100,
         nonirryield.r < 10) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(-c(yldunits,cokey,comppct.r,compkind,majcompflag,muacres,mukey, musym, compname)) %>%
  rename(yield = nonirryield.r)

countyObs <- grass %>%
  group_by(county) %>%
  tally()

grass <- grass %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))

doubles <- grass %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

grass <- anti_join(grass, doubles) %>%
  droplevels()

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

grass <- bind_rows(grass, doubles1, doubles2)

grass %>%
  group_by(county) %>%
  tally() %>%
  arrange(desc(n))

# histogram ---------------------------------------------------------------

mean(grass$yield)
sd(grass$yield)
summary(grass$yield)

ggplot(data = grass, aes(yield)) +
  geom_histogram(bins = 20)

names(grass)


# observations by county --------------------------------------------------

## using sf/ggplot-------------------

# load counties
counties <- st_read("../../../geospatial capstone/data/WI_Counties2010/WI_Counties2010.shp")
#head(counties)
#str(counties)
counties <- counties %>%
  dplyr::select(c(COUNTY, NAME, geometry))
levels(as.factor(counties$NAME))
levels(as.factor(grass$county))

obs_county <- left_join(counties, countyObs, by = c("NAME" = "county")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(n_discrete = case_when(between(n, 0, 25) ~ "8-25",
                                between(n, 26, 100) ~ "26-100",
                                between(n, 101, 500) ~ "101-500",
                                between(n, 501,1000) ~ "501-1000",
                                between(n, 1001,1200) ~ "1001-1200"))

summary(obs_county)

png("figures/observationsPerCounty_cat_sf.png")
ggplot() +
  geom_sf(data = obs_county, aes(fill = n_discrete), lwd = 0.4, color = "black") +
  coord_sf() +
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB", "#4A7C59"), 
                    name="Number\nof observations",
                    breaks=c("8-25", "26-100", "101-500", "501-1000","1001-1200"),
                    labels=c("8-25", "26-100", "101-500", "501-1000","1001-1200"))
dev.off()  


# model diagnostics -----------------------------------------------------

## clean data for model diags---------------------
grassCor <- grass %>%
  select(c(cropname,slope, elev, sandvc, sandco,sandmed,sandfine, clay,om,
           ksat,cec,ph,awc,frag3,total.depth, yield)) %>%
  drop_na()

set.seed(0731)
tidy_split <- initial_split(grassCor, strata = yield)
train <- training(tidy_split)
test <- testing(tidy_split)

trainx <- train %>%
  dplyr::select(-c(yield))

## modCor explainer---------------------

modCor <- readRDS("models/pastureCorRanger.rds")

cor_explainer <- 
  explain_tidymodels(
    modCor, 
    data = trainx, 
    y = train$yield,
    label = "random forest",
    verbose = FALSE
  )

(eva_rf <- DALEX::model_performance(cor_explainer))
eva_rf
plot(eva_rf, geom = "histogram") 

## modCor2 explainer---------------------
modCor2 <- readRDS("models/pastureCor2Ranger.rds")

cor2_explainer <- 
  explain_tidymodels(
    modCor2, 
    data = trainx, 
    y = train$yield,
    label = "random forest",
    verbose = FALSE
  )

(eva2 <- DALEX::model_performance(cor2_explainer))
eva2
plot(eva2, geom = "histogram") 


# variable importance----------------

## modCor------------------
library(patchwork)
set.seed(1804)
vip_cor <- model_parts(cor_explainer, loss_function = loss_root_mean_square)
plot(vip_cor)

obj <- list(vip_cor)
metric_name <- attr(obj[[1]], "loss_name")
metric_lab <- paste(metric_name, 
                    "after permutations\n(higher indicates more important)")


# full_vip <- bind_rows(obj) %>%
#      filter(variable != "_baseline_")

full_vip <- vip_cor %>%
  filter(variable != "_baseline_") %>%
  dplyr::select(-c(label))

# perm_vals <- full_vip %>% 
#   filter(variable == "_full_model_") %>% 
#   group_by(label) %>%
#   summarise(dropout_loss = mean(dropout_loss))

perm_vals <- full_vip %>% 
  filter(variable == "_full_model_") %>% 
  summarise(dropout_loss = mean(dropout_loss))

p <- full_vip %>%
  filter(variable != "_full_model_") %>% 
  #mutate(variable = fct_reorder(variable, dropout_loss)) %>%
  mutate(variable = str_to_sentence(variable),
         variable = fct_reorder(variable, dropout_loss),
         variable = recode(variable,
                           "Cropname" = "Species",
                           "Awc" = "AWC",
                           "Ph" = "pH",
                           "Om" = "OM",
                           "Elev" = "Elevation",
                           "Total.depth" = "Soil depth",
                           "Sandfine" = "Fine sand",
                           "Sandmed" = "Medium sand",
                           "Sandco" = "Coarse sand",
                           "Sandvc" = "Very coarse sand",
                           "Frag3" = "Rock fragments")) %>%
  ggplot(aes(dropout_loss, variable))  +
  geom_boxplot(fill = "#91CBD765") +
  labs(x = "Root mean square error (RMSE) loss", 
       y = NULL,  fill = NULL,  color = NULL)


p <- p + 
  theme_update(plot.tag = element_text(size = 16),
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)) 

png("figures/varImpCor.png")
p
dev.off()

## modCor2------------------
set.seed(1804)
vip_cor2 <- model_parts(cor2_explainer, loss_function = loss_root_mean_square)
plot(vip_cor2)

obj <- list(vip_cor2)
metric_name <- attr(obj[[1]], "loss_name")
metric_lab <- paste(metric_name, 
                    "after permutations\n(higher indicates more important)")


full_vip2 <- vip_cor2 %>%
  filter(variable != "_baseline_") %>%
  dplyr::select(-c(label))


perm_vals2 <- full_vip2 %>% 
  filter(variable == "_full_model_") %>% 
  summarise(dropout_loss = mean(dropout_loss))

p2 <- full_vip2 %>%
  filter(variable != "_full_model_") %>% 
  #mutate(variable = fct_reorder(variable, dropout_loss)) %>%
  mutate(variable = str_to_sentence(variable),
         variable = fct_reorder(variable, dropout_loss),
         variable = recode(variable,
                           "Cropname" = "Species",
                           "Awc" = "AWC",
                           "Ph" = "pH",
                           "Om" = "OM",
                           "Elev" = "Elevation",
                           "Total.depth" = "Soil depth",
                           "Sandfine" = "Fine sand",
                           "Sandmed" = "Medium sand",
                           "Sandco" = "Course sand",
                           "Sandvc" = "Very course sand",
                           "Frag3" = "Rock fragments")) %>%
  ggplot(aes(dropout_loss, variable))  +
  geom_boxplot(fill = "#91CBD765") +
  labs(x = "Root mean square error (RMSE) loss", 
       y = NULL,  fill = NULL,  color = NULL)


p2 <- p2 + 
  theme_update(plot.tag = element_text(size = 16),
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)) 

p2

# partial dependance plots ------------------

library(viridisLite)

## species--------------------
pdp_species <- model_profile(
  cor_explainer,
  variables = "cropname",
  N = NULL
)

plot(pdp_species)

species_preds <- pdp_species$cp_profiles %>%
  mutate(cropname = fct_reorder(cropname, `_yhat_`))
species_means <- pdp_species$agr_profiles %>%
  rename(cropname = `_x_`) %>%
  mutate(cropname = fct_reorder(cropname, `_yhat_`))

png("figures/pdp_speciesCor.png")
ggplot(species_preds, aes(x = cropname, y = `_yhat_`)) +
  scale_color_brewer()+
  geom_point() +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab(" ") +
  theme(axis.ticks.x = element_blank()) +
  geom_point(data = species_means, aes(x = cropname, y = `_yhat_`), color = "black") +
  geom_line(data = species_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

group_preds <- species_preds %>%
  mutate(cropname = recode(cropname, 
                           "Bluegrass-clover" = "Bluegrass",
                           "Timothy-clover" = "Timothy",
                           "Orchardgrass-clover" = "Orchardgrass"))

group_means <- species_means %>%
  mutate(cropname = recode(cropname, 
                           "Bluegrass-clover" = "Bluegrass",
                           "Timothy-clover" = "Timothy",
                           "Orchardgrass-clover" = "Orchardgrass"))

png("figures/pdp_specieGroupCor.png")
ggplot(group_preds, aes(x = cropname, y = `_yhat_`)) +
  scale_color_brewer()+
  geom_point() +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab(" ") +
  theme(axis.ticks.x = element_blank()) +
  geom_point(data = group_means, aes(x = cropname, y = `_yhat_`), color = "black") +
  geom_line(data = group_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_speciesCor.png")
ggplot(species_preds, aes(x = cropname, y = `_yhat_`)) +
  scale_color_brewer()+
  geom_point() +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab(" ") +
  theme(axis.ticks.x = element_blank()) +
  geom_point(data = species_means, aes(x = cropname, y = `_yhat_`), color = "black") +
  geom_line(data = species_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_species2.png")
ggplot(data = species_means, aes(x = cropname, y =`_yhat_`, fill = cropname)) +
  geom_col() +
  scale_fill_brewer(palette = 2)+
  ylab("Mean predicted yield (tons/acre)") +
  xlab(" ") +
  theme(legend.position = "none")
dev.off()

## awc-------------------
pdp_awc <- model_profile(
  cor_explainer,
  variables = "awc",
  N = NULL
)

plot(pdp_awc)

awc_preds <- pdp_awc$cp_profiles 
awc_means <- pdp_awc$agr_profiles %>%
  rename(awc = `_x_`) 

png("figures/pdp_awcCor.png")
ggplot(awc_preds, aes(x = awc, y = `_yhat_`)) +
  #scale_color_brewer()+
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Available water capacity (cm/cm)") +
  #geom_point(data = awc_means, aes(x = awc, y = `_yhat_`), color = "black") +
  geom_line(data = awc_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = 'dashed')
dev.off()

png("figures/pdp_awc_simple.png")
ggplot(data = awc_means, aes(x = awc, y =`_yhat_`)) +
  geom_line() +
  ylab("Mean predicted yield (tons/acre)") +
  xlab("Available water capacity (cm/cm)")
dev.off()

##ksat----------------------
pdp_ksat <- model_profile(
  cor_explainer,
  variables = "ksat",
  N = NULL
)


plot(pdp_ksat)


ksat_preds <- pdp_ksat$cp_profiles 
ksat_means <- pdp_ksat$agr_profiles %>%
  rename(ksat = `_x_`)

png("figures/pdp_ksat_simple.png")
ggplot(ksat_means, aes(x = ksat, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Ksat (um/s)")
dev.off()

png("figures/pdp_ksatCor.png")
ggplot(ksat_preds, aes(x = ksat, y = `_yhat_`)) +
  #scale_color_brewer()+
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Ksat (um/s)") +
  #geom_point(data = ksat_means, aes(x = ksat, y = `_yhat_`), color = "black") +
  geom_line(data = ksat_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

single_variable(full_explainer, variable = "ksat", type = "pdp")

pdp_ksat

pd_ksat <- model_profile(explainer = full_explainer, variables = "ksat")
plot(pd_ksat, geom = "profiles")

## slope-------------------------
pdp_slope <- model_profile(
  cor_explainer,
  variables = "slope",
  N = NULL
)

plot(pdp_slope)

slope_preds <- pdp_slope$cp_profiles 
slope_means <- pdp_slope$agr_profiles %>%
  rename(slope = `_x_`) 


png("figures/pdp_slopeCor.png")
ggplot(slope_preds, aes(x = slope, y = `_yhat_`)) +
  #scale_color_brewer()+
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Slope (%)") +
  #geom_point(data = sand_means, aes(x = sand, y = `_yhat_`), color = "black") +
  geom_line(data = slope_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_slope_simple.png")
ggplot(slope_means, aes(x = slope, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Slope (%)")
dev.off()

##fine sand----------------------
pdp_sandf <- model_profile(
  cor_explainer,
  variables = "sandfine",
  N = NULL
)

plot(pdp_sandf)

sandf_preds <- pdp_sandf$cp_profiles 
sandf_means <- pdp_sandf$agr_profiles %>%
  rename(sandfine = `_x_`) 


png("figures/pdp_fineSandCor.png")
ggplot(sandf_preds, aes(x = sandfine, y = `_yhat_`)) +
  #scale_color_brewer()+
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Fine sand (%)") +
  #geom_point(data = sand_means, aes(x = sand, y = `_yhat_`), color = "black") +
  geom_line(data = sandf_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_finesand_simple.png")
ggplot(sandf_means, aes(x = sandfine, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Fine sand (%)")
dev.off()

##clay----------------

pdp_clay <- model_profile(
  cor_explainer,
  variables = "clay",
  N = NULL
)

plot(pdp_clay)

clay_preds <- pdp_clay$cp_profiles 
clay_means <- pdp_clay$agr_profiles %>%
  rename(clay = `_x_`) 


png("figures/pdp_clayCor.png")
ggplot(clay_preds, aes(x = clay, y = `_yhat_`)) +
  #scale_color_brewer()+
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Clay (%)") +
  #geom_point(data = sand_means, aes(x = sand, y = `_yhat_`), color = "black") +
  geom_line(data = clay_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_clay_simple.png")
ggplot(clay_means, aes(x = clay, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Clay (%)")
dev.off()

## ph--------------

pdp_ph <- model_profile(
  cor_explainer,
  variables = "ph",
  N = NULL
)

plot(pdp_ph)

ph_preds <- pdp_ph$cp_profiles 
ph_means <- pdp_ph$agr_profiles %>%
  rename(ph = `_x_`) 


png("figures/pdp_ph_simple.png")
ggplot(ph_means, aes(x = ph, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("pH")
dev.off()

## medium sand----------------

pdp_sandmed <- model_profile(
  cor_explainer,
  variables = "sandmed",
  N = NULL
)

plot(pdp_sandmed)

sandmed_preds <- pdp_sandmed$cp_profiles 
sandmed_means <- pdp_sandmed$agr_profiles %>%
  rename(sandmed = `_x_`) 


png("figures/pdp_sandmed_simple.png")
ggplot(sandmed_means, aes(x = sandmed, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Medium sand (%)")
dev.off()

## cec---------------------

pdp_cec <- model_profile(
  cor_explainer,
  variables = "cec",
  N = NULL
)

plot(pdp_cec)

cec_preds <- pdp_cec$cp_profiles 
cec_means <- pdp_cec$agr_profiles %>%
  rename(cec = `_x_`) 


png("figures/pdp_cec_simple.png")
ggplot(cec_means, aes(x = cec, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Cation exchange capacity (meq/100g)")
dev.off()

## coarse sand-----------------

pdp_sandc <- model_profile(
  cor_explainer,
  variables = "sandco",
  N = NULL
)

plot(pdp_sandc)

sandc_preds <- pdp_sandc$cp_profiles 
sandc_means <- pdp_sandc$agr_profiles %>%
  rename(sandco = `_x_`) 


png("figures/pdp_sandco_simple.png")
ggplot(sandc_means, aes(x = sandco, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Coarse sand (%)")
dev.off()

## om------------------------

pdp_om <- model_profile(
  cor_explainer,
  variables = "om",
  N = NULL
)

plot(pdp_om)

om_preds <- pdp_om$cp_profiles 
om_means <- pdp_om$agr_profiles %>%
  rename(om = `_x_`) 


png("figures/pdp_om_simple.png")
ggplot(om_means, aes(x = om, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Organic matter (%)")
dev.off()

## very coarse sand-----------------------

pdp_sandvc <- model_profile(
  cor_explainer,
  variables = "sandvc",
  N = NULL
)

plot(pdp_sandvc)

sandvc_preds <- pdp_sandvc$cp_profiles 
sandvc_means <- pdp_sandvc$agr_profiles %>%
  rename(sandvc = `_x_`) 


png("figures/pdp_sandvc_simple.png")
ggplot(sandvc_means, aes(x = sandvc, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Very coarse sand (%)")
dev.off()

## elevation--------------------

pdp_elev <- model_profile(
  cor_explainer,
  variables = "elev",
  N = NULL
)

plot(pdp_elev)

elev_preds <- pdp_elev$cp_profiles 
elev_means <- pdp_elev$agr_profiles %>%
  rename(elev = `_x_`) 


png("figures/pdp_elev_simple.png")
ggplot(elev_means, aes(x = elev, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Elevation (m)")
dev.off()

##frag3---------------------

pdp_frag <- model_profile(
  cor_explainer,
  variables = "frag3",
  N = NULL
)

plot(pdp_frag)

frag_preds <- pdp_frag$cp_profiles 
frag_means <- pdp_frag$agr_profiles %>%
  rename(frag = `_x_`) 


png("figures/pdp_frag_simple.png")
ggplot(frag_means, aes(x = frag, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Rock fragments (3-10 in) (%)")
dev.off()

## soil depth----------------------

pdp_depth <- model_profile(
  cor_explainer,
  variables = "total.depth",
  N = NULL
)

plot(pdp_depth)

depth_preds <- pdp_depth$cp_profiles 
depth_means <- pdp_depth$agr_profiles %>%
  rename(depth = `_x_`) 


png("figures/pdp_depth_simple.png")
ggplot(depth_means, aes(x = depth, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Soil depth (cm)")
dev.off()

# predicted yield vs ssurgo yield -----------------------------------------

## modCor--------------------
soil <-  read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
  mutate_if(is.character, as.factor)

summary(soil)

grass <- soil %>%
  filter(cec < 100,
         nonirryield.r < 10) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(-c(yldunits,cokey,comppct.r,compkind,majcompflag,muacres,mukey, county, musym, compname)) %>%
  rename(yield = nonirryield.r)

grassCor <- grass %>%
  select(c(cropname,slope, elev, sandvc, sandco,sandmed,sandfine, clay,om,
           ksat,cec,ph,awc,frag3,total.depth, yield)) %>%
  drop_na(elev)

modCor <- readRDS("models/pastureCorRanger.rds")

set.seed(123)
split <- initial_split(grassCor, strata = yield)
train <- training(split)
test <- testing(split)

pred_df <- modCor %>%
  predict(test) %>%
  bind_cols(test) 

modCor
rmse <- sqrt(0.0178)

png("figures/predVssurgoModCor.png")
ggplot(data = pred_df, aes(x = yield, y = .pred)) +
  geom_jitter() +
  xlim(0.75,6) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ylab("Predicted yield") +
  xlab("SSURGO representative yield")
dev.off()

##modCor2-------------
modCor2 <- readRDS("models/pastureCor2Ranger.rds")

pred_df <- modCor2 %>%
  predict(test) %>%
  bind_cols(test) 

modCor2
rmse2 <- sqrt(0.0184)

png("figures/predVssurgoModCor2.png")
ggplot(data = pred_df, aes(x = yield, y = .pred)) +
  geom_jitter() +
  xlim(0.75,6) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ylab("Predicted yield") +
  xlab("SSURGO representative yield")
dev.off()




# error across state OOB ------------------------------------------------------

soil <-  read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
  mutate_if(is.character, as.factor)

summary(soil)

grass <- soil %>%
  filter(cec < 100,
         nonirryield.r < 10) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(-c(yldunits,cokey,comppct.r,compkind,majcompflag,muacres,mukey, musym, compname)) %>%
  rename(yield = nonirryield.r)

grassCor <- grass %>%
  select(c(cropname,slope, elev, sandvc, sandco,sandmed,sandfine, clay,om,
           ksat,cec,ph,awc,frag3,total.depth, yield, county)) %>%
  drop_na(elev)

set.seed(123)
split <- initial_split(grassCor, strata = yield)
train <- training(split)
test <- testing(split)

## modCor--------------------
pred_df <- modCor %>%
  predict(test) %>%
  bind_cols(test) %>%
  filter(yield < 10)

summary(pred_df)

# join to county fips
levels(as.factor(pred_df$county))

## clean prediction data--------------------
pred_df <- pred_df %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))
doubles <- pred_df %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

pred_df <- anti_join(pred_df, doubles) %>%
  droplevels()

#levels(pred_df$county)

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

pred_df2 <- bind_rows(pred_df, doubles1, doubles2)

#levels(as.factor(pred_df2$county))

#countyFIPS <- read_csv("data/countyFips.csv")

#pred_df3 <- left_join(pred_df2, countyFIPS, by = c("county" = "County"))

error <- pred_df2 %>%
  mutate(err = .pred - yield,
         err2 = err^2) %>%
  group_by(county) %>%
  summarise(count = n(),
            sumSqErr = sum(err2),
            mse = sumSqErr/n(),
            rmse = sqrt(mse)
            )

error <- error %>%
  mutate(rmse_cat = case_when(between(rmse, 0.03, 0.0744) ~ "0.03-0.074",
                              between(rmse, 0.0745, 0.0984) ~ "0.075-0.098",
                              between(rmse, 0.0985, 0.1084) ~ "0.099-0.108",
                              between(rmse, 0.1085, 0.25) ~ "0.109-0.25"))

### using sf/ggplot-------------------
# load counties
counties <- st_read("../../../geospatial capstone/data/WI_Counties2010/WI_Counties2010.shp")

err_county <- left_join(counties, error, by = c("NAME" = "county"))



png("figures/errPerCountyModCor_cat_sf.png")
ggplot() +
  geom_sf(data = err_county, aes(fill = rmse_cat), lwd = 0.4, color = "black") +
  coord_sf() +
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB", "#4A7C59"), 
                    name="RMSE",
                    breaks=c("0.03-0.074", "0.075-0.098", "0.099-0.108", "0.109-0.25"),
                    labels=c("0.03-0.074", "0.075-0.098", "0.099-0.108", "0.109-0.25"))
#+
# guides(fill = guide_legend(title.position = "top"))
dev.off()  

## modCor2--------------------
pred_df <- modCor2 %>%
  predict(test) %>%
  bind_cols(test) %>%
  filter(yield < 10)

summary(pred_df)

# join to county fips
levels(as.factor(pred_df$county))

## clean prediction data--------------------
pred_df <- pred_df %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))
doubles <- pred_df %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

pred_df <- anti_join(pred_df, doubles) %>%
  droplevels()

#levels(pred_df$county)

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

pred_df2 <- bind_rows(pred_df, doubles1, doubles2)


error2 <- pred_df2 %>%
  mutate(err = .pred - yield,
         err2 = err^2) %>%
  group_by(county) %>%
  summarise(count = n(),
            sumSqErr = sum(err2),
            mse = sumSqErr/n(),
            rmse = sqrt(mse)
  )

summary(error2$rmse)

error2 <- error2 %>%
  mutate(rmse_cat = case_when(between(rmse, 0.045, 0.0809) ~ "0.05-0.08",
                              between(rmse, 0.081, 0.0974) ~ "0.08-0.10",
                              between(rmse, 0.0975, 0.119) ~ "0.10-0.12",
                              between(rmse, 0.1191, 0.26) ~ "0.12-0.25"))

### using sf/ggplot-------------------

err_county <- left_join(counties, error, by = c("NAME" = "county"))


png("figures/errPerCountyModCor2_cat_sf.png")
ggplot() +
  geom_sf(data = err_county, aes(fill = rmse_cat), lwd = 0.4, color = "black") +
  coord_sf() +
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB", "#4A7C59"), 
                    name="RMSE",
                    breaks=c("0.05-0.08", "0.08-0.10", "0.10-0.12", "0.12-0.25"),
                    labels=c("0.05-0.08", "0.08-0.10", "0.10-0.12", "0.12-0.25"))
#+
# guides(fill = guide_legend(title.position = "top"))
dev.off()  

summary(error$rmse)


write.csv(error, "data/stateError.csv", row.names = FALSE, quote = FALSE)

png("figures/test_rmse.png")
plot_usmap("counties", 
           include = "WI",
           data = error,
           values = "rmse_cat") + 
  scale_fill_discrete(type = c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                               "#68B0AB"),
                      name = "RMSE") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 
dev.off()

# average yield by county--------------------------------
## usmap-------------------
grass <- grass %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))
doubles <- grass %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

grass <- anti_join(grass, doubles) %>%
  droplevels()

#levels(pred_df$county)

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

grass2 <- bind_rows(grass, doubles1, doubles2)



avg_yld <- grass3 %>%
  group_by(county, fips) %>%
  summarize(mnYld = mean(yield))

plot_usmap("counties", 
           include = "WI",
           data = avg_yld,
           values = "mnYld") + 
  scale_fill_gradient(low = "#e9fae3", high = "#025414", name = "No. obs.") 

summary(avg_yld$mnYld)

avg_yld <- avg_yld %>%
  #mutate(n_discrete = cut(n, c(0 ,100, 300, 500, 750, 1200)))
  mutate(yldCat = case_when(between(mnYld, 1.5, 2.5) ~ "1.5-2.5",
                           between(mnYld, 2.5, 2.81) ~ "2.5-2.8",
                           between(mnYld, 2.8101, 3.132) ~ "2.8-3.1",
                           between(mnYld, 3.133,4.4) ~ "3.1-4.4"))

png("figures/yldPerCounty_cat.png")
plot_usmap("counties", 
           include = "WI",
           data = avg_yld,
           values = "yldCat") +
  scale_fill_discrete(type = c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                               "#68B0AB"),
                      name = "Avg. yield (tons/acre)") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))
dev.off()

## ggplot and sf---------------------------------

# load counties
counties <- st_read("../../../geospatial capstone/data/WI_Counties2010/WI_Counties2010.shp")
head(counties)
str(counties)
counties <- counties %>%
  dplyr::select(c(COUNTY, NAME, geometry))

grassCor <- grass %>%
  select(c(cropname,slope, elev, sandvc, sandco,sandmed,sandfine, clay,om,
           ksat,cec,ph,awc,frag3,total.depth, yield, county)) %>%
  drop_na(elev)

summary(grassCor$yield)

wiGrass <- grass %>%
  group_by(county) %>%
  summarize(mnYld = mean(yield)) 

summary(wiGrass$mnYld)

wiGrass <- wiGrass %>%
  mutate(yldCat = case_when(between(mnYld, 1.5, 2.5) ~ "1.5-2.5",
                            between(mnYld, 2.5, 2.81) ~ "2.5-2.8",
                            between(mnYld, 2.8101, 3.132) ~ "2.8-3.1",
                            between(mnYld, 3.133,4.4) ~ "3.1-4.4"))

wiGrass <- wiGrass %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))
doubles <- wiGrass %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

wiGrass <- anti_join(wiGrass, doubles) %>%
  droplevels()

#levels(wiGrass$county)

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

wiGrass2 <- bind_rows(wiGrass, doubles1, doubles2)

head(wiGrass2)
grassYld <- left_join(counties, wiGrass2, by = c("NAME" = "county"))
  

head(grassYld)

png("figures/avgYld_sf.png")
ggplot() +
  geom_sf(data = grassYld, aes(fill = yldCat), lwd = 0.4, color = "black") +
  coord_sf()+
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB", "#4A7C59"), 
                    name="Avg. yield\n(tons/acre)",
                    breaks=c("1.5-2.5", "2.5-2.8", "2.8-3.1", "3.1-4.4"),
                    labels=c("1.5-2.5", "2.5-2.8", "2.8-3.1", "3.1-4.4"))

dev.off()  




## percent rmse of avg yield ----------------------

###modCor------------------------


percentErr <- left_join(grassYld, error, by = c("NAME" = "county")) %>%
  mutate(perErr = round((rmse/mnYld)*100, 2))

summary(percentErr$perErr)

percentErr <- percentErr %>%
  mutate(perCat = case_when(between(perErr, 0, 2.74) ~ "0.9-2.7",
                            between(perErr, 2.7401, 3.5) ~ "2.7-3.5",
                            between(perErr, 3.501, 4.0) ~ "3.5-4.0",
                            between(perErr, 4.01, 7.77) ~ "4.0-7.8"))

#percentErr$perCat <- factor(percentErr$perCat, 
 #                           levels = c("0.74-2.5", "2.5-5.0", "5-7.5", "7.5-9.9", "10-14"))

png("figures/perErr_modCor.png")
ggplot() +
  geom_sf(data = percentErr, aes(fill = perCat), lwd = 0.4, color = "black") +
  coord_sf()+
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB"), 
                    name="% RMSE\nof avg. yld",
                    breaks=c("0.9-2.7", "2.7-3.5", "3.5-4.0", "4.0-7.8"),
                    labels=c("0.9-2.7", "2.7-3.5", "3.5-4.0", "4.0-7.8"))

dev.off()  

###modCor2------------------------


percentErr2 <- left_join(grassYld, error2, by = c("NAME" = "county")) %>%
  mutate(perErr = round((rmse/mnYld)*100, 2))

summary(percentErr2$perErr)

percentErr2 <- percentErr2 %>%
  mutate(perCat = case_when(between(perErr, 0, 3) ~ "1.2-3.0",
                            between(perErr, 3.01, 3.7) ~ "3.0-3.7",
                            between(perErr, 3.701, 4.4) ~ "3.7-4.4",
                            between(perErr, 4.41, 8.2) ~ "4.4-8.2"))

#percentErr$perCat <- factor(percentErr$perCat, 
#                           levels = c("0.74-2.5", "2.5-5.0", "5-7.5", "7.5-9.9", "10-14"))

png("figures/perErr_modCor2.png")
ggplot() +
  geom_sf(data = percentErr2, aes(fill = perCat), lwd = 0.4, color = "black") +
  coord_sf()+
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB"), 
                    name="% RMSE\nof avg. yld",
                    breaks=c("1.2-3.0", "3.0-3.7", "3.7-4.4", "4.4-8.2"),
                    labels=c("1.2-3.0", "3.0-3.7", "3.7-4.4", "4.4-8.2"))

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
  mutate(err = .pred - yield,
         err2 = err^2) %>%
  group_by(county, fips) %>%
  summarise(count = n(),
            sumSqErr = sum(err2),
            mse = sumSqErr/n(),
            rmse = sqrt(mse)
  )

summary(error$rmse)

error <- error %>%
  mutate(rmse_cat = case_when(between(rmse, 0.03, 0.07184) ~ "0.03-0.071",
                              between(rmse, 0.07185, 0.08560) ~ "0.071-0.09",
                              between(rmse, 0.08561, 0.10395) ~ "0.08-0.10",
                              between(rmse, 0.10396, 0.38) ~ "0.10-0.38"))


png("figures/allData_mse.png")
plot_usmap("counties", 
           include = "WI",
           data = error,
           values = "rmse_cat") + 
  scale_fill_discrete(type = c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                               "#68B0AB"),
                      name = "RMSE") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 
   
dev.off()

# app figures---------------------------------

species <- c(
  "Bluegrass-clover",
  "Orchardgrass-clover",
  "Timothy-clover")

noCrop <- grass %>%
  select(-cropname) %>%
  filter(county == "Sauk") %>%
  drop_na() %>%
  droplevels()

soil_df <- noCrop[sample(nrow(noCrop),size = 1),]

#add grass_clover species
pred_df <-
  tibble(cropname = factor(species)) %>%
  group_by(cropname) %>%
  do(soil_df)

# run rf prediction
predictions <- predict(modCor, pred_df)

coef = c(1.2, 1, .95, 0.75, 0.65)
lowYield = round(coef * predictions$.pred[1], 2)
medYield = round(coef * predictions$.pred[3], 2)
highYield = round(coef * predictions$.pred[2], 2)
# combine predictions and rotation length coefficients

prediction_df <-
  tibble(
    Occupancy = rep(c("<1", "1", "3", "7", "Continuous"),3),
    coef = rep(c(1.2, 1, .95, 0.75, 0.65),3)
  ) %>%
  mutate(
    Variety = c(rep("Low",5), rep("Medium", 5), rep("High",5)),
    yield = c(lowYield, medYield, highYield)) 


prediction_df

prediction_df$Variety = factor(prediction_df$Variety, levels = c("Low", "Medium", "High"))

png("figures/websiteGraph.png")
ggplot(prediction_df, aes(x = Occupancy, y = yield, fill = Variety)) +
  geom_col(position = "dodge") +
  scale_fill_manual(breaks = c("Low", "Medium", "High"),
                    labels = c("Low yielding species", "Medium yielding species", "High yielding species"),
                    values = c("#D6EFC7","#96BB7C", "#377338")) +
  ylab("Yield (tons/acre)") +
  geom_abline(slope = 0, intercept = 0, color = "black")+
  theme(legend.title = element_blank(),
        text = element_text(size = 20),
        legend.position = c(0.8, 0.9),
        legend.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "lightgrey"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(vjust = 6))
dev.off()

library(gt)

prediction_df %>% 
  select(-coef) %>%
  pivot_wider(names_from = 'Variety', values_from = 'yield') %>%
  mutate(Occupancy = recode(Occupancy,
                            "<1" = "<1 day",
                            "1" = "1 day",
                            "3" = "3 days", 
                            "7" = "7 days")) %>%
  gt() %>%
  cols_label('Low' = "Low yielding species",
             "Medium" = "Medium yielding species",
             'High' = "High yielding species") %>%
  gtsave("figures/websiteTable.png")



# grazing intensity--------------------------

library(gt)

days = c("<1", "1", "2", "3", "4", "5", "6", "7", "Continuous")
multiplier = c(1.2, 1.2, 1, 1, 0.92, 0.85,0.78, 0.72, 0.65)
df <- data.frame(days = days, multiplier = multiplier)

grazTab <- 
df %>%
  gt() %>%
  tab_footnote(
    footnote = "Gurda 2014",
    locations = cells_body(columns = 'multiplier', rows = 1:2)
  ) %>%
  tab_footnote(
    footnote = "NRCS",
    locations = cells_body(columns = 'multiplier', rows = 3:8)
  ) %>%
  tab_footnote(
    footnote = "Bertlesen et al. 1993",
    locations = cells_body(columns = 'multiplier', rows = 6)
  ) %>%
  tab_footnote(
    footnote = "Oates et al. 2011; Paine et al. 1999; 
    Sollenberger et al. 2012",
    locations = cells_body(columns = 'multiplier', rows = 9)
  ) %>%
  cols_label(days = html("Grazing duration<br>(days)"),
             multiplier = html("Yield<br>adjustment")) %>%
  cols_align(align = "center")

grazTab
gtsave(grazTab, "figures/grazingTable.png")

# yield by species--------------------------

speciesYield <- read_csv("data/speciesYield.csv") 

# separate the ssurgo and university trials
speciesU <- speciesYield %>%
  filter(university == "yes") %>%
  droplevels()
speciesSS <- speciesYield %>%
  filter(university == "no") %>%
  droplevels()

# figure with just university trials
uniYield_sum <- speciesU %>%
  group_by(species) %>%
  summarise(mnYld = mean(tons_acre),
            count = n(),
            sd = sd(tons_acre),
            se = sd/sqrt(count)) %>%
  mutate_if(is.character, as.factor)

ssYield_sum <- speciesSS %>%
  group_by(species) %>%
  summarise(mnYld = mean(tons_acre),
            count = n(),
            sd = sd(tons_acre),
            se = sd/sqrt(count)) %>%
  mutate_if(is.character, as.factor)


ggplot(uniYield_sum, aes(x = fct_reorder(species, mnYld), y = mnYld, color = species)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mnYld - sd, ymax = mnYld + sd)) +
  ylab("Yield (tons/acre) (± sd)") +
  xlab(" ") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(breaks = c("italian ryegrass", "kentucky bluegrass","meadow fescue",            
                               "quackgrass","smooth bromegrass","perennial ryegrass",       
                               "timothy","reed canarygrass","meadow bromegrass",        
                               "festulolium","orchardgrass","tall fescue"),
                     labels = c("Italian ryegrass", "Kentucky bluegrass","meadow fescue",            
                                "quackgrass","smooth bromegrass","perennial ryegrass",       
                                "timothy","reed canarygrass","meadow bromegrass",        
                                "festulolium","orchardgrass","tall fescue"),
                     values = c("#9E0142","#D53E4F", "#F46D43", "#FDAE61",
                                "#66C2A5", "#3288BD", "#2146C7", "#5E4FA2",
                                "#9E0142","#D53E4F", "#F46D43", "#FDAE61"
                                 )) +
  geom_point(data = ssYield_sum, aes(x = species, y = mnYld, size = 2))

yield_sum <- speciesYield %>%
  group_by(species, university) %>%
  summarise(mnYld = mean(tons_acre),
            count = n(),
            sd = sd(tons_acre),
            se = sd/sqrt(count)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(university = recode(university,
                            "yes" = "University trials",
                            "no" = "SSURGO database"))

yield_sum$species <- factor(yield_sum$species, levels = c("italian ryegrass", "kentucky bluegrass","meadow fescue",            
                            "quackgrass","smooth bromegrass","perennial ryegrass",       
                            "timothy","reed canarygrass","meadow bromegrass",        
                            "festulolium","orchardgrass","tall fescue"))

png("figures/yieldXspecies_col1.png")
ggplot(yield_sum, aes(x = species, y = mnYld, color = species, shape = university)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mnYld - sd, ymax = mnYld + sd, lty = university)) +
  ylab("Yield (tons/acre) (± sd)") +
  xlab(" ") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 16)) +
  scale_color_manual(breaks = c("italian ryegrass", "kentucky bluegrass","meadow fescue",            
                                "quackgrass","smooth bromegrass","perennial ryegrass",       
                                "timothy","reed canarygrass","meadow bromegrass",        
                                "festulolium","orchardgrass","tall fescue"),
                     labels = c("Italian ryegrass", "Kentucky bluegrass","meadow fescue",            
                                "quackgrass","smooth bromegrass","perennial ryegrass",       
                                "timothy","reed canarygrass","meadow bromegrass",        
                                "festulolium","orchardgrass","tall fescue"),
                     values = c("#9E0142","#D53E4F", "#F46D43", "#FDAE61",
                                "#66C2A5", "#3288BD", "#2146C7", "#5E4FA2",
                                "#9E0142","#D53E4F", "#F46D43", "#FDAE61"
                     )) 
dev.off()  


# add sample size

yield_sum_u <- speciesYield %>%
  group_by(species, university) %>%
  summarise(mnYld = mean(tons_acre),
            count = n(),
            sd = sd(tons_acre),
            se = sd/sqrt(count)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(university = recode(university,
                             "yes" = "University trials",
                             "no" = "SSURGO database")) %>%
  filter(university == "University trials")

yield_sum_s <- speciesYield %>%
  group_by(species, university) %>%
  summarise(mnYld = mean(tons_acre),
            count = n(),
            sd = sd(tons_acre),
            se = sd/sqrt(count)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(university = recode(university,
                             "yes" = "University trials",
                             "no" = "SSURGO database")) %>%
  filter(university == "SSURGO database")

png("figures/yieldXspecies_col_n.png")
ggplot(yield_sum, aes(x = species, y = mnYld, color = species, shape = university)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mnYld - sd, ymax = mnYld + sd, lty = university)) +
  ylab("Yield (tons/acre) (± sd)") +
  xlab(" ") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 16)) +
  scale_color_manual(breaks = c("italian ryegrass", "kentucky bluegrass","meadow fescue",            
                                "quackgrass","smooth bromegrass","perennial ryegrass",       
                                "timothy","reed canarygrass","meadow bromegrass",        
                                "festulolium","orchardgrass","tall fescue"),
                     labels = c("Italian ryegrass", "Kentucky bluegrass","meadow fescue",            
                                "quackgrass","smooth bromegrass","perennial ryegrass",       
                                "timothy","reed canarygrass","meadow bromegrass",        
                                "festulolium","orchardgrass","tall fescue"),
                     values = c("#9E0142","#D53E4F", "#F46D43", "#FDAE61",
                                "#66C2A5", "#3288BD", "#2146C7", "#5E4FA2",
                                "#9E0142","#D53E4F", "#F46D43", "#FDAE61"
                     )) +
  geom_text(data = yield_sum_u, aes(y = mnYld + sd, label = count), vjust = -.5) +
  geom_text(data = yield_sum_s, aes(y = mnYld - sd, label = count), vjust = 1.2)
dev.off()

# no legend, labels abbreviated below
yield_sum_abbr <- yield_sum %>%
  mutate(species = recode(species,
                          "italian ryegrass" = "IR", 
                          "kentucky bluegrass" = "KB",
                          "meadow fescue" = "MF",            
                          "quackgrass" = "QG",
                          "smooth bromegrass" = "SB",
                          "perennial ryegrass" = "PR",       
                          "timothy" = "TM",
                          "reed canarygrass" = "RC",
                          "meadow bromegrass" = "MB",        
                          "festulolium" = "FE",
                          "orchardgrass" = "OG",
                          "tall fescue" = "TF"))

png("figures/yieldXspecies_abbr.png")
ggplot(yield_sum_abbr, aes(x = species, y = mnYld, shape = university)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mnYld - sd, ymax = mnYld + sd, lty = university)) +
  ylab("Yield (tons/acre) (± sd)") +
  xlab(" ") +
  theme(axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 16))
dev.off()

yieldGroups <-yield_sum_abbr %>%
  #filter(university == "University trials") %>%
  mutate(grouping = case_when(species == "IR" |
                                species == "KB" |
                                species == "MF" |
                                species == "QG" ~ "Low",
                              species == "SB" |
                                species == "PR" |
                                species == "TM" |
                                species == "RC" ~ "Medium",
                              TRUE ~ "High"))

yieldGroups$grouping <- factor(yieldGroups$grouping, levels = c("High", "Medium", "Low"))

png("figures/yieldgroupsspecies.png")
ggplot(data = yieldGroups, aes(x = species, y = mnYld, color = grouping, shape = university)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mnYld - sd, ymax = mnYld + sd)) +
  ylab("Yield (tons/acre) (± sd)") +
  xlab(" ") +
  labs(color = "Yield \n groups",
       shape = " ") +
  theme(axis.ticks.x = element_blank(),
        #legend.title = element_blank(),
        text = element_text(size = 16))
dev.off()

# average of each yield group

yieldGroupsUni <- yieldGroups %>%
  filter(university == "University trials") %>%
  group_by(grouping) %>%
  summarise(meanYield = mean(mnYld),
            minYield = min(mnYld),
            maxYield = max(mnYld))

# soil hist by area -------------------------
crops <- read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
  mutate_if(is.character, as.factor)

sand <- crops %>%
  select(c(sand, muacres, musym, compname, county)) %>%
  drop_na() %>%
  distinct() 

sandAcres <- sand %>%
  select(c(meanSand, muacres))

test <- rep.int(sandAcres$meanSand, times = sandAcres$muacres)

sandA <- data.frame(sand = test)
hist(sandA$sand)




