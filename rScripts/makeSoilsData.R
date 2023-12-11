# this script uses the downloaded ssurgo soils to get the soil data to compare forage suitability to yield


setwd("/Users/elissachasen/Library/CloudStorage/GoogleDrive-emchasen@wisc.edu/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data")

library(tidyverse)

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

#extract chorizon layers
read_horizon <- function(){
  horizon <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_chorizon.csv")
    #print(i)
    #print(filename)
    horizon[[i]] <- read.csv(file = filename, na.strings = c(" ", "", "NA", "NaN"))
  }
  horizon <- do.call(rbind.data.frame, horizon)
}

chorizon <- read_horizon()

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


######## CLEAN DATA 30 cm #################################################################################


# awc to 40 inches
awc40in <- chorizon %>%
  filter(hzdept.r < 101.6) %>%
  droplevels() %>%
  mutate(thick = ifelse(hzdepb.r > 101, 101 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(awc40 = round(weighted.mean(awc.r, thick, na.rm = TRUE),2))

#deepest horizon bottom of each component
depth <- chorizon %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 30 cm
chorizon <- chorizon %>%
  filter(hzdept.r < 31) %>%
  droplevels()

colnames(chorizon)

chorizon <- chorizon %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandvc.r, sandco.r, sandmed.r, sandfine.r,
               claytotal.r, om.l, om.r, om.h, ksat.r, cec7.l,cec7.r, cec7.h, ph1to1h2o.l,
               ph1to1h2o.r, ph1to1h2o.h, awc.r, frag3to10.r)

summary(chorizon)

#weighted means
chorizon <- chorizon %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            omLow = round(weighted.mean(om.l, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            omHigh = round(weighted.mean(om.h, thick, na.rm = TRUE),2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            cecLow = round(weighted.mean(cec7.l, thick, na.rm = TRUE),2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            cecHigh = round(weighted.mean(cec7.h, thick, na.rm = TRUE),2),
            phLow = round(weighted.mean(ph1to1h2o.l, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            phHigh = round(weighted.mean(ph1to1h2o.h, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
chorizon <- left_join(chorizon, depth, by = "cokey") %>%
  left_join(awc40in, by = "cokey")

summary(chorizon)

names(component)

component <- component %>%
  filter(compkind == "Series"#,majcompflag == "Yes"
         ) %>%
  dplyr::select(c(compname, taxorder, minSlope = slope.l, slope.r, drainagecl,
                  maxSlope = slope.h, elevLow = elev.l, elev.r, elevHigh = elev.h, mukey, cokey)) %>%
 droplevels()

component_horizon <- left_join(component, chorizon, by = c("cokey"))

cocropyld <- cocropyld %>%
  dplyr::select(cropname, yldunits, nonirryield.r, cokey)

soil_crop <- left_join(cocropyld, component_horizon, by = "cokey") %>%
  mutate_if(is.character, as.factor)

summary(soil_crop)

# to look at yield by cropname (skip if making data)-----------

soil_crop <- soil_crop %>%
  drop_na(nonirryield.r) %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike")

crop_sum <- soil_crop %>%
  group_by(cropname) %>%
  summarize(mnYld = mean(nonirryield.r),
            sdYld = sd(nonirryield.r),
            count = n(),
            seYld = sd(nonirryield.r)/sqrt(count))

ggplot(crop_sum, aes(x = cropname, y = mnYld)) +
  geom_col() +
  geom_errorbar(aes(ymin = mnYld - seYld, ymax = mnYld + seYld))

ggplot(crop_sum, aes(x = cropname, y = mnYld)) +
  geom_col() +
  geom_errorbar(aes(ymin = mnYld - sdYld, ymax = mnYld + sdYld))

# continue making data here-------------



soil_crop <- soil_crop %>%
  drop_na(nonirryield.r) %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  mutate(cropname = recode(cropname, 
                           `Bluegrass-white clover`="Bluegrass-clover",
                           `Orchardgrass-alsike`= "Orchardgrass-clover",
                           `Orchardgrass-red clover` = "Orchardgrass-clover",
                           `Timothy-alsike` = "Timothy-clover")) %>%
  droplevels() %>%
  mutate_if(is.character, as.factor)


mapunit <- mapunit %>%
  dplyr::select(c(musym, muacres, mukey, county)) # took out muname

full_soil <- left_join(soil_crop, mapunit, by = c("mukey"))  %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

summary(full_soil)

full_soil <- full_soil %>%
  filter(nonirryield.r < 10)

# convert awc40 from cm/cm to in/40 in

full_soil <- full_soil %>%
  mutate(awc40in = awc40*40)
#summary(full_soil$awcIn)


# rename drainageClass into groups----------
levels(full_soil$drainagecl)
poorDrain <- c("Very poorly drained", "Poorly drained", "Somewhat poorly drained")

full_soil <- full_soil %>%
  mutate(drainageClGroup =  if_else(drainagecl %in% poorDrain, "poorDrain", "wellDrain"))

# rename AWC into groups-----------------
full_soil <- full_soil %>%
  mutate(awcGroup = case_when(awc40in < 4 ~ "low",
                              between(awc40in, 4, 6.9) ~ "mod",
                              awc40in > 6.6 ~ "high"))


# forageGroupID--------

full_soil <- full_soil %>%
  mutate(forageGroupID = case_when(taxorder == "Histosols" ~ 10,
                                   awcGroup == "low" & drainageClGroup == "poorDrain" ~ 1,
                                   awcGroup == "low" & drainageClGroup == "wellDrain" & slope.r > 12 ~ 3,
                                   awcGroup == "low" & drainageClGroup == "wellDrain" & slope.r < 12 ~ 2,
                                   awcGroup == "mod" & drainageClGroup == "poorDrain" ~ 4,
                                   awcGroup == "mod" & drainageClGroup == "wellDrain" & slope.r > 12 ~ 6,
                                   awcGroup == "mod" & drainageClGroup == "wellDrain" & slope.r < 12 ~ 5,
                                   awcGroup == "high" & drainageClGroup == "poorDrain" ~ 7,
                                   awcGroup == "high" & drainageClGroup == "wellDrain" & slope.r > 12 ~ 9,
                                   awcGroup == "high" & drainageClGroup == "wellDrain" & slope.r < 12 ~ 8))

names(full_soil)

full_soil <- full_soil %>%
  select(c(cropname, yield = nonirryield.r, compname, county, musym, minSlope, slope.r, maxSlope, elevLow, elev.r, elevHigh,
           sandvc, sandco, sandmed, sandfine, clay, omLow, om, omHigh, phLow, ph, phHigh,
           cecLow, cec, cecHigh, awc, frag3, ksat, total.depth, forageGroupID))

full_soilClean <- drop_na(full_soil)


setwd("~/Library/CloudStorage/GoogleDrive-emchasen@wisc.edu/My Drive/grassland2.0/grazescape/pasture yield module")
write.csv(full_soilClean, "data/yield_forageGroup.csv", row.names = FALSE, quote = FALSE)


