# this script cleans the data used to create the yield by species point plot

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#clean undersander--------------
#load files
under <- read.csv("grass species composition/undersanderVarietyClean.csv", na.strings = c(".", ""))

str(under)
range(under$year)
under$location <- tolower(under$location)
under$species <- tolower(under$species)

#levels(as.factor(under$location))
#levels(under$species)

# clean locations and species
under <- under %>%
  mutate(location = recode(location, 
                           `arl`="arlington",
                           `arlearly`= "arlington",
                           `arlfall` = "arlington",
                           `arllate` = "arlington",
                           `mar` = "marshfield",
                           `spo` = "spooner"),
         species = tolower(species),
         species = recode(species,
                          `festulolium ` = "festulolium",
                          `meadow brome` = "meadow bromegrass",
                          `meadow fescue ` = "meadow fescue",
                          `meadowbrome` = "meadow bromegrass",
                          `meadowfescue` = "meadow fescue",
                          `orchardgrass ` = "orchardgrass",
                          `orchardgrass late` = "orchardgrass",
                          `orchargrass` = "orchardgrass",
                          `orchargrasss` = "orchardgrass",
                          `perennial ryegrass ` = "perennial ryegrass",
                          `perennial ryegrass organic` = "perennial ryegrass",
                          `smooth brome` = "smooth bromegrass",
                          `tall fescue ` = "tall fescue",
                          `tallfescue` = "tall fescue",
                          `tmothy` = "timothy",
                          `italian ryegrass ` = "italian ryegrass",
                          `k bluegrass` = "kentucky bluegrass")) %>%
  dplyr::filter(species != "75%tall fescue/25%festul ",
                species != "75%tall fescue/25%festulolium") %>%
  mutate_if(is.character, as.factor)

summary(under)

# remove first year stands, drop nas, create legume.companion var
under <- under %>%
  filter(tons_acre > 0.1) %>%
  mutate(researcher = "undersander",
         university = "yes") %>%
  drop_na(tons_acre) %>%
  rowwise() %>%
  mutate(stand_age = year - year.est) %>%
  mutate(legume.companion = NA) %>%
  #filter(management == "hay") %>%
  #dplyr::select(-management) %>%
  filter(stand_age > 0) %>%
  droplevels() %>%
  mutate_if(is.character, as.factor) 

summary(under)

under <- under %>%
  select(c(species, management, legume.companion, tons_acre, university))

levels(under$species)

## check yield by management--------------- 
# ggplot(under2, aes(x = management, y = tons_acre, color = species)) +
#   geom_point()
# 
# grazeSpecies <- under2 %>%
#   filter(management == "graze") %>%
#   droplevels()
# 
# grazeSpeciesList <- levels(grazeSpecies$species)
# 
# commonSpecies <- under2 %>%
#   filter(species %in% grazeSpeciesList) %>%
#   droplevels()
# 
# ggplot(commonSpecies, aes(x = management, y = tons_acre, color = species)) +
#   geom_jitter()
# 
# levels(haySpecies$species)

# clean casler data------------------

casler <- read.csv("grass species composition/varietyTrialsCasler.csv", na.strings = c("na", "")) %>%
  mutate_if(is.character, as.factor)
summary(casler)

# convert mg_ha to tons_acre
mgha_to_tonsacre_conv = 1.1/2.47

# remove number of harvests
# remove soil texture
# clean species names
# convert year to numeric
# calculate stand age
# if more than one year of data, remove year data
# remove fertilization

casler <- casler %>%
  mutate(Location = recode(Location,
                           `Arlington ARS` = "arlington",
                           `Arlington, WI` = "arlington",
                           `Marshfield ARS` = "marshfield",
                           `Spooner ARS` = "spooner")) %>%
  mutate(Species = recode(Species,
                          "TO" = "tall oatgrass",
                          "TM" = "timothy",
                          "TF" = "tall fescue",
                          "SB" = "smooth bromegrass",
                          "Smooth Bromegrass" = "smooth bromegrass",
                          "RC" = "reed canarygrass",
                          "QG" = "quackgrass",
                          "FL" = "perennial rye",
                          "PR" = "perennial rye",
                          "OG" = "orchardgrass",
                          "OG early" = "orchardgrass",
                          "OG medium" = "orchardgrass",
                          "OG late" = "orchardgrass",
                          "MF" = "meadow fescue",
                          "MB" = "meadow bromegrass",
                          "meadowbrome" = "meadow bromegrass",
                          "Meadowbrome" = "meadow bromegrass",
                          "KB" = "kentucky bluegrass",
                          "MTB" = "mountain bromegrass",
                          "IR" = "italian ryegrass",
                          "HR" = "hybrid ryegrass",
                          "CF" = "creeping foxtail",
                          "PG" = "prairie grass",
                          "Wheatgrass" = "wheatgrass")) %>%
  mutate(tons_acre = case_when(is.na(tons_acre) == TRUE ~ mg_ha*mgha_to_tonsacre_conv,
                               is.na(tons_acre) == FALSE ~ tons_acre)) %>%
  drop_na(tons_acre) %>%
  filter(tons_acre > 1) %>%
  #mutate(year_digits = nchar(as.character(year)))  %>%
  #mutate(year = case_when(year_digits == 4 ~ year)) %>%
  #dplyr::select(-c(Soil.Texture, N.kg_ha, Number.of.harvests, fertilization, mg_ha, year_digits)) %>%
  dplyr::select(-c(Soil.Texture, N.kg_ha, Number.of.harvests, fertilization, mg_ha)) %>%
  #filter(Management == "hay") %>%
  #filter(Legume.companion == "none") %>%
  rename_with(tolower) %>%
  mutate(university = "yes") %>%
  droplevels()

summary(casler)

casler <- casler %>%
  select(c(species, management, legume.companion, tons_acre, university))

levels(casler$species)

## undersander data from Laura----------------
under2 <- read.csv("grass species composition/undersander from Laura/Grass Variety Trials plot data.csv", na.strings = c(".", "NA"))

summary(under2)

# remove seeding year
# clean species names

under2 <- under2 %>%
  mutate_if(is.character, as.factor) %>%
  #filter(STATUS == "COMM") %>%
  dplyr::select(-c(STATUS, CUT1:CUT6, TOTALof..as.a...CHECK)) %>%
  filter(age.of.stand.in.years > 1) %>%
  mutate(SPECIE = tolower(SPECIE),
         species = recode(SPECIE,
                          "festulolium " = "festulolium",
                          "perennial ryegrass organic" = "perennial ryegrass",
                          "orchardgrass " = "orchardgrass")) %>%
  mutate(location = recode(SITE,
                           "ARL" = "arlington",
                           "ARLPR" = "arlington",
                           "LAN" = "lancaster",
                           "LANPR" = "lancaster",
                           "MAR" = "marshfield",
                           "ARLEARLY" = "arlington",
                           "ARLFALL" = "arlington",
                           "ARLIR" = "arlington",
                           "ARLLATE" = "arlington")) %>%
  mutate(researcher = "undersander",
         university = "yes") %>%
  droplevels()

under2 <- under2 %>%
  filter(species != "mixture",
         species != "75%tall fescue/25%festulolium") %>%
  droplevels()

levels(as.factor(under2$species))
summary(under2)

# management column
# legume.companion column
under2 <- under2 %>%
  mutate(legume.companion = NA,
         management = "hay") %>%
  select(species, management, legume.companion, tons_acre = TOTAL.tons.acre, university)

levels(as.factor(under2$species))

# add ssurgo data----------------------

ssurgo <- read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
  mutate_if(is.character, as.factor)

ssurgo <- ssurgo %>%
  mutate(researcher = "ssurgo",
         university = "no",
         legume.companion = "clover",
         management = "ssurgo") %>%
  mutate(species = recode(cropname,
                          "Bluegrass-clover" = "kentucky bluegrass",
                          "Timothy-clover" = "timothy",
                          "Orchardgrass-clover" = "orchardgrass")) %>%
  drop_na(nonirryield.r) %>%
  select(species, tons_acre = nonirryield.r, management, legume.companion, university)


# join data---------------

df <- bind_rows(casler,under, under2, ssurgo)
df <- df %>%
  mutate_if(is.character, as.factor) %>%
  drop_na(species)

summary(df)

hist(df$tons_acre)

df <- df %>%
  filter(between(tons_acre, 0.5, 10))

hist(df$tons_acre)
summary(df)

sort(levels(as.factor(df$species)))


# keep only species that are of interest to consultants
species_list <- c("kentucky bluegrass", "italian ryegrass", "quackgrass",
                  "meadow fescue", "smooth bromegrass", "perennial rye", "perennial ryegrass",
                  "timothy", "reed canarygrass", "meadow bromegrass",
                  "festulolium", "orchardgrass", "tall fescue")

df <- df %>%
  filter(species %in% species_list) %>%
  mutate(species = recode(species,
                          "perennial rye" = "perennial ryegrass")) %>%
  droplevels()

levels(df$species)

write.csv(df, "data/speciesYield.csv", row.names = FALSE, quote = FALSE)



