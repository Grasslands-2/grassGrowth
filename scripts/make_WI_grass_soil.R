setwd("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data")

library(FedData)
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
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, sandvc.r, sandco.r, sandmed.r, sandfine.r,
                silttotal.r, siltco.r, siltfine.r, claytotal.r, partdensity, om.r, ksat.r, gypsum.r, 
                sar.r, ec.r, cec7.r, ph1to1h2o.r, awc.r, fraggt10.r, frag3to10.r)

summary(chorizon)
# pH, CEC, organic matter, content of rock fragments, and bulk density,  
# negative: SAR, EC, and gypsum content, 
# available water-holding capacity

#summary(chorizon)

#weighted means
chorizon <- chorizon %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            siltco = round(weighted.mean(siltco.r, thick, na.rm = TRUE), 2),
            siltfine = round(weighted.mean(siltfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            partdensity = round(weighted.mean(partdensity, na.rm = TRUE), 2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            gypsum = round(weighted.mean(gypsum.r, thick, na.rm = TRUE),2),
            sar = round(weighted.mean(sar.r, thick, na.rm = TRUE), 2),
            ec = round(weighted.mean(ec.r, thick, na.rm = TRUE), 2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2),
            frag10 = round(weighted.mean(fraggt10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
chorizon <- left_join(chorizon, depth, by = "cokey")

summary(chorizon)
# gypsum and sar are all 0s and NAs
hist(chorizon$ec) # almost all 0s 
##TODO remove sar, gypsum, ec, siltco and siltfine and partdensity (too many NAs)
hist(chorizon$frag3, breaks = 20)
hist(chorizon$frag10, breaks = 20)

names(component)
component <- component %>%
  dplyr::select(c(comppct.r, compname, compkind, majcompflag, slope = slope.r, elev = elev.r, mukey, cokey)) %>%
  filter(compkind == "Series") %>%
  droplevels()

component_horizon <- left_join(component, chorizon, by = c("cokey"))

mapunit <- mapunit %>%
  dplyr::select(c(musym, muacres, mukey, county)) # took out muname

#full_soil <- left_join(component_horizon_depth, mapunit, by = c("mukey"))
full_soil <- left_join(component_horizon, mapunit, by = c("mukey")) %>%
  distinct() #%>%
  #drop_na()

summary(full_soil)
# full_soil <- full_soil %>%
#   filter(majcompflag == "Yes")

#colnames(cocropyld)
cocropyld <- cocropyld %>%
  dplyr::select(cropname, yldunits, nonirryield.r, cokey)

soil_crop <- left_join(cocropyld, full_soil, by = "cokey")
summary(soil_crop)

soil_crop_prod <- soil_crop %>%
  drop_na() %>%
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

summary(soil_crop_prod)
hist(soil_crop_prod$frag10)
hist(soil_crop_prod$frag3)

# soil_crop_prod <- soil_crop_prod %>%
#   dplyr::select(-c(sar, ec, gypsum, muacres, musym))

write.table(soil_crop_prod, file = "cropdata/WI_Grass_Soil.txt", sep = ",", row.names = FALSE, quote = FALSE)

write.csv(soil_crop_prod, file = "cropdata/WI_Grass_Soil_full.csv", row.names = FALSE, quote = FALSE)


######## CLEAN DATA 5 cm #################################################################################


#deepest horizon bottom of each component
depth <- chorizon %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 5 cm
chorizon <- chorizon %>%
  filter(hzdept.r < 6) %>%
  droplevels()

colnames(chorizon)

chorizon <- chorizon %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, sandvc.r, sandco.r, sandmed.r, sandfine.r,
                silttotal.r, siltco.r, siltfine.r, claytotal.r, partdensity, om.r, ksat.r, gypsum.r, 
                sar.r, ec.r, cec7.r, ph1to1h2o.r, awc.r, fraggt10.r, frag3to10.r)

summary(chorizon)
# pH, CEC, organic matter, content of rock fragments, and bulk density,  
# negative: SAR, EC, and gypsum content, 
# available water-holding capacity

#summary(chorizon)

#weighted means
chorizon <- chorizon %>%
  mutate(thick = ifelse(hzdepb.r > 5, 5 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            siltco = round(weighted.mean(siltco.r, thick, na.rm = TRUE), 2),
            siltfine = round(weighted.mean(siltfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            partdensity = round(weighted.mean(partdensity, na.rm = TRUE), 2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            gypsum = round(weighted.mean(gypsum.r, thick, na.rm = TRUE),2),
            sar = round(weighted.mean(sar.r, thick, na.rm = TRUE), 2),
            ec = round(weighted.mean(ec.r, thick, na.rm = TRUE), 2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2),
            frag10 = round(weighted.mean(fraggt10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
chorizon <- left_join(chorizon, depth, by = "cokey")

summary(chorizon)

# check out data from site---------------------

adamsHorizonSite <- read.table("ssurgoSiteDownload/WI001/tabular/chorizon.txt", sep = "|")
adamsHorizonFed <- read.csv("EXTRACTIONS/Adams/SSURGO/Adams_SSURGO_chorizon.csv")

names(adamsHorizonSite) <- names(adamsHorizonFed)

#deepest horizon bottom of each component
adams.depth.site <- adamsHorizonSite %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 30 cm
adamsHorizonSite <- adamsHorizonSite %>%
  filter(hzdept.r < 31) %>%
  droplevels()


adamsHorizonSite <- adamsHorizonSite %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, sandvc.r, sandco.r, sandmed.r, sandfine.r,
                silttotal.r, siltco.r, siltfine.r, claytotal.r, partdensity, om.r, ksat.r, gypsum.r, 
                sar.r, ec.r, cec7.r, ph1to1h2o.r, awc.r, fraggt10.r, frag3to10.r)

#weighted means
adamsHorizonSite<- adamsHorizonSite %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            siltco = round(weighted.mean(siltco.r, thick, na.rm = TRUE), 2),
            siltfine = round(weighted.mean(siltfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            partdensity = round(weighted.mean(partdensity, na.rm = TRUE), 2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            gypsum = round(weighted.mean(gypsum.r, thick, na.rm = TRUE),2),
            sar = round(weighted.mean(sar.r, thick, na.rm = TRUE), 2),
            ec = round(weighted.mean(ec.r, thick, na.rm = TRUE), 2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2),
            frag10 = round(weighted.mean(fraggt10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
adamsHorizonSite <- left_join(adamsHorizonSite, adams.depth.site, by = "cokey")

summary(adamsHorizonSite)

#deepest horizon bottom of each component
adams.depth.fed <- adamsHorizonFed %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 30 cm
adamsHorizonFed <- adamsHorizonFed %>%
  filter(hzdept.r < 31) %>%
  droplevels()


adamsHorizonFed <- adamsHorizonFed %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, sandvc.r, sandco.r, sandmed.r, sandfine.r,
                silttotal.r, siltco.r, siltfine.r, claytotal.r, partdensity, om.r, ksat.r, gypsum.r, 
                sar.r, ec.r, cec7.r, ph1to1h2o.r, awc.r, fraggt10.r, frag3to10.r)

#weighted means
adamsHorizonFed<- adamsHorizonFed %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            siltco = round(weighted.mean(siltco.r, thick, na.rm = TRUE), 2),
            siltfine = round(weighted.mean(siltfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            partdensity = round(weighted.mean(partdensity, na.rm = TRUE), 2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            gypsum = round(weighted.mean(gypsum.r, thick, na.rm = TRUE),2),
            sar = round(weighted.mean(sar.r, thick, na.rm = TRUE), 2),
            ec = round(weighted.mean(ec.r, thick, na.rm = TRUE), 2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2),
            frag10 = round(weighted.mean(fraggt10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
adamsHorizonFed <- left_join(adamsHorizonFed, adams.depth.fed, by = "cokey")

summary(adamsHorizonFed)

# compare crawford

crawHorizonSite <- read.table("ssurgoSiteDownload/WI023/tabular/chorizon.txt", sep = "|")
crawHorizonFed <- read.csv("EXTRACTIONS/Crawford/SSURGO/Crawford_SSURGO_chorizon.csv")

names(crawHorizonSite) <- names(crawHorizonFed)

#deepest horizon bottom of each component
craw.depth.site <- crawHorizonSite %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 30 cm
crawHorizonSite <- crawHorizonSite %>%
  filter(hzdept.r < 31) %>%
  droplevels()


crawHorizonSite <- crawHorizonSite %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, sandvc.r, sandco.r, sandmed.r, sandfine.r,
                silttotal.r, siltco.r, siltfine.r, claytotal.r, partdensity, om.r, ksat.r, gypsum.r, 
                sar.r, ec.r, cec7.r, ph1to1h2o.r, awc.r, fraggt10.r, frag3to10.r)

#weighted means
crawHorizonSite<- crawHorizonSite %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            siltco = round(weighted.mean(siltco.r, thick, na.rm = TRUE), 2),
            siltfine = round(weighted.mean(siltfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            partdensity = round(weighted.mean(partdensity, na.rm = TRUE), 2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            gypsum = round(weighted.mean(gypsum.r, thick, na.rm = TRUE),2),
            sar = round(weighted.mean(sar.r, thick, na.rm = TRUE), 2),
            ec = round(weighted.mean(ec.r, thick, na.rm = TRUE), 2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2),
            frag10 = round(weighted.mean(fraggt10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
crawHorizonSite <- left_join(crawHorizonSite, craw.depth.site, by = "cokey")

summary(crawHorizonSite)

#deepest horizon bottom of each component
craw.depth.fed <- crawHorizonFed %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 30 cm
crawHorizonFed <- crawHorizonFed %>%
  filter(hzdept.r < 31) %>%
  droplevels()


crawHorizonFed <- crawHorizonFed %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, sandvc.r, sandco.r, sandmed.r, sandfine.r,
                silttotal.r, siltco.r, siltfine.r, claytotal.r, partdensity, om.r, ksat.r, gypsum.r, 
                sar.r, ec.r, cec7.r, ph1to1h2o.r, awc.r, fraggt10.r, frag3to10.r)

#weighted means
crawHorizonFed<- crawHorizonFed %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            siltco = round(weighted.mean(siltco.r, thick, na.rm = TRUE), 2),
            siltfine = round(weighted.mean(siltfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            partdensity = round(weighted.mean(partdensity, na.rm = TRUE), 2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            gypsum = round(weighted.mean(gypsum.r, thick, na.rm = TRUE),2),
            sar = round(weighted.mean(sar.r, thick, na.rm = TRUE), 2),
            ec = round(weighted.mean(ec.r, thick, na.rm = TRUE), 2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2),
            frag10 = round(weighted.mean(fraggt10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
crawHorizonFed <- left_join(crawHorizonFed, craw.depth.fed, by = "cokey")

summary(crawHorizonFed)



