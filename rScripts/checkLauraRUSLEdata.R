library(tidyverse)

setwd("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data")

library(dplyr)
library(tidyr)

counties <- c("Brown", "Crawford", "Kewaunee", "Monroe", "Marathon", "Taylor", "Vernon", "Clark", "Grant", 
              "Shawano", "Lafayette", "Dane",
              "Chippewa", "Dodge", "FondDuLac", "Manitowoc", "Barron", 
              "Adams", "Ashland", "Bayfield", "Buffalo","Burnett", "Columbia", "Door", "Douglas",
              "Dunn", "EauClaire", "Florence", 'Forest', "Green", "GreenLake", "Iowa", "Iron",
              "Jackson", "Jefferson", "Juneau", "LaCrosse", "Langlade","Lincoln", "Marinette",
              "Marquette", "Menominee", "Oconto", "Oneida", "Outagamie", "Ozaukee", "Pepin", "Pierce",
              "Polk", "Portage", "Price", "Richland", "Rock", "Rusk", "StCroix", "Sauk", "Sawyer",
              "Sheboygan", "Trempealeau", "Vilas", "Walworth", "Washburn", "Washington", "Waupaca",
              "Waushara", "Winnebago", "Wood", "CalumetManitowoc", "KenoshaRacine", "MilwaukeeWaukesha")


#extract component layers
read_component <- function(){
  component <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_component.csv")
    component[[i]] <- read.csv(file = filename, na.strings = c(" ", " ", "NA", "NaN"))
  }
  component <- do.call(rbind.data.frame, component)
}

component <- read_component()

#extract mapunit and attach county name
read_mapunit <- function(){
  mapunit <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_mapunit.csv")
    mapunit[[i]] <- read.csv(file = filename, na.strings = c(" ", "NA", "NaN", ""))
    mapunit[[i]]$county <- counties[i]
  }
  mapunit <- do.call(rbind.data.frame, mapunit)
}

mapunit <- read_mapunit()

#extract component yields
read_cocropyld <- function(){
  cocropyld <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_cocropyld.csv")
    cocropyld[[i]] <- read.csv(file = filename, na.strings = c("", NA, " ", "NaN"))
  }
  cocropyld <- do.call(rbind.data.frame, cocropyld)
}

cocropyld <- read_cocropyld()

#################################################################################################################################
######## CLEAN DATA #################################################################################
############################################################################################


component <- component %>%
  dplyr::select(c(compname, mukey, cokey)) %>%
  droplevels()

mapunit <- mapunit %>%
  dplyr::select(c(musym, mukey, county)) # took out muname

#full_soil <- left_join(component_horizon_depth, mapunit, by = c("mukey"))
full_soil <- left_join(component, mapunit, by = c("mukey")) %>%
  distinct() %>%
  drop_na()

#colnames(cocropyld)
cocropyld <- cocropyld %>%
  dplyr::select(cropname, yldunits, nonirryield.r, cokey) 

soil_crop <- left_join(cocropyld, full_soil, by = "cokey") %>%
  filter(yldunits == "Tons") %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()
summary(soil_crop)

levels(soil_crop$cropname)

crops <- c("Alfalfa hay", "Bluegrass-white clover", "Bromegrass hay", "Bromegrass-alfalfa",
           "Bromegrass-alfalfa hay", "Cool-season grasses", "Grass hay", "Grass-clover",
           "Grass-legume hay", "Grass-legume pasture", "Kentucky bluegrass", "Orchardgrass-alsike",
           "Orchardgrass-red clover", "Pasture", "Red clover hay", "Reed canarygrass", "Smooth bromegrass",
           "Timothy-alsike", "Timothy-red clover hay")

soil_crop_prod <- soil_crop %>%
  filter(cropname %in% crops) %>% 
  droplevels() 

summary(soil_crop_prod)

# label counties by region
countyDF <- read.csv("data/countyRegion.csv")

crop <- left_join(soil_crop_prod, countyDF)

yldSum <- crop %>%
  group_by(region, cropname) %>%
  summarise(avgYld = mean(nonirryield.r),
            count = n()) %>%
  mutate(region = recode(region, 
                           `south`="upper midwest",
                           `north`= "north central"))

write.csv(yldSum, "yieldByRegion.csv", row.names = FALSE, quote = FALSE)
