# this script attempts to relate yields between species to create individual species models

library(tidyverse)


# upload data -------------------------------------------------------------

var <- read_csv("data/CombVarietyTrial.csv")
ssurgo <- read_csv("data/surgoGrassStats.csv")


# relate ssurgo yields to each other using kentucky bluegrass as base ---------------------------

ssurgo_rel <- ssurgo %>% 
  mutate(rel.meanYld = round(meanYld/meanYld[1],2)) %>%
  arrange(rel.meanYld)


# relate variety trial yields to each other using kentucky bluegra --------

# remove varieties as per conversation with Jim, Laura and Gene

TypVar <- var %>%
  filter(species != "tall oatgrass",
         species != "hybrid ryegrass")

var_sum <- TypVar %>%
  group_by(species) %>%
  summarise(meanYld = mean(tons_acre),
            count = n()#,
            #sd = sd(tons_acre),
            #se = sd/sqrt(count)
            ) %>%
  mutate(rel.meanYld = round(meanYld/meanYld[4],2)) %>%
  arrange(species) %>%
  mutate(df = "varietyTrial")


# shrink relative means from variety trial data to match ssurgo re --------

# create same species names and change column heading from cropname to species
ssurgo_clean <- ssurgo_rel %>%
  mutate(species = recode(cropname,
                          "Bluegrass-clover" = "kentucky bluegrass",
                          "Orchardgrass-clover" = "orchardgrass",
                          "Timothy-clover" = "timothy")) %>%
  dplyr::select(c(species, meanYld, rel.meanYld)) %>%
  mutate(df = "ssurgo")

# join variety data and ssurgo data

joined <- bind_rows(var_sum, ssurgo_clean) 

ggplot(subset(joined, df = "varietyTrial"), aes(x = meanYld, y = rel.meanYld, color = species)) +
  geom_point() 


# create relative yields from variety trials, based on SSurgo -------------

# general formula ----
# abs(spX.vt-kbg.vt)/abs(kbg.vt - tim.vt) = abs(spX.ss - kbg.ss)/abs(kbg.ss - tim.ss)
bg.ss = ssurgo_rel[1,5]
tim.ss = ssurgo_rel[2,5]
og.ss = ssurgo_rel[3,5]
kbg.vt = var_sum[3,3]
tim.vt = var_sum[7,3]
og.vt = var_sum[9,3]

# species group 1 (yields lower than bluegrass) ----
quackGrass.vt = var_sum[2,3]
itRyegrass.vt = var_sum[1,3]

# formula: (all relative yields)
# kbg- [(kbg.ss - tim.ss)*(spX.vt - kbg.vt)/(kbg.vt - tim.vt)]

italianRyegrass.rel.Yld = kbg.vt - 
  ((abs(bg.ss - tim.ss))*abs(itRyegrass.vt - kbg.vt)/abs(kbg.vt - tim.vt))

quackGrass.rel.yld = kbg.vt - 
  ((abs(bg.ss - tim.ss))*abs(quackGrass.vt - kbg.vt)/abs(kbg.vt - tim.vt))

# species group 2 (yields between bluegrass and timothy) ----

meadowFescue.vt = var_sum[4,3]
perRyegrass.vt = var_sum[5,3]
smoothBrome.vt = var_sum[6,3]

# formula: (all relative yields)
# [(kbg.ss - tim.ss)*(spX.vt - kbg.vt)/(kbg.vt - tim.vt)] + kbg

meadowFescue.rel.yld = 
  ((abs(bg.ss - tim.ss) * abs(meadowFescue.vt - kbg.vt)/abs(kbg.vt - tim.vt))) + kbg.vt
perRyegrass.rel.yld = 
  ((abs(bg.ss - tim.ss) * abs(perRyegrass.vt - kbg.vt)/abs(kbg.vt - tim.vt))) + kbg.vt
smoothBrome.rel.yld = 
  ((abs(bg.ss - tim.ss) * abs(smoothBrome.vt - kbg.vt)/abs(kbg.vt - tim.vt))) + kbg.vt

# species group 3 (yields between timothy and orchardgrass) ----

reedCan.vt = var_sum[8,3]

# formula: (all relative yields)
# [(og.ss - tim.ss)*(spX.vt - tim.vt)/(og.vt - tim.vt)] + tim.ss

reedCan.rel.yld = 
  ((abs(og.ss - tim.ss) * abs(reedCan.vt - tim.vt)/(og.vt - tim.vt))) + tim.ss
 

# species group 4 (yields greater than orchardgrass) ----
tallFescue.vt = var_sum[10,3]
fest.vt = var_sum[11,3]
meadowBrome.vt = var_sum[12,3]
hybridBrome.vt = var_sum[13,3]

# formula: (all relative yields)
# [(og.ss - tim.ss)*(spX.vt - og.vt)/(og.vt - tim.vt)] + og.ss

tallFescue.rel.yld = 
  ((abs(og.ss - tim.ss) * abs(tallFescue.vt - og.vt)/(og.vt - tim.vt))) + og.ss
fest.rel.yld = 
  ((abs(og.ss - tim.ss) * abs(fest.vt - og.vt)/(og.vt - tim.vt))) + og.ss
meadowBrome.rel.yld = 
  ((abs(og.ss - tim.ss) * abs(meadowBrome.vt - og.vt)/(og.vt - tim.vt))) + og.ss
hybridBrome.rel.yld = 
  ((abs(og.ss - tim.ss) * abs(hybridBrome.vt - og.vt)/(og.vt - tim.vt))) + og.ss


# visualize variety trial relative means with new calculated relat --------

# create new column in var_sum dataframe ----

new_relYld.df = data.frame(species = levels(as.factor(var_sum$species)),
                           meanYld = var_sum$meanYld,
                        rel.meanYld = c(1.52, 1.69, 0.81, 1, 1.53, 1.04, 1.38, 1.11,
                                       0.98, 1.36, 1.17, 1.45, 1.23),
                        df = "adjusted")

joined <- bind_rows(var_sum, new_relYld.df, ssurgo_clean) 

ggplot(joined, aes(x = reorder(species, rel.meanYld), y = rel.meanYld, color = df)) +
  geom_jitter(width = 0.2, height = 0)+
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(aes(x = 3, y = 1), size = 10, shape = 1, color = "gold4") +
  geom_point(aes(x = 7, y = 1.23), size = 10, shape = 1, color = "gold4") + 
  geom_point(aes(x = 8, y = 1.38), size = 10, shape = 1, color = "gold4") +
  ylab("Relative mean yield") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14))


# pastures for profit species ---------------------------------------------

pp <- read_csv("../pasturesForProfit.csv")

good <- pp %>%
  filter(quality == "good")

ggplot(pp, aes(x = reorder(species, rel.yield), y = rel.yield, color = quality)) +
  geom_point() +
  facet_wrap(facets = ~ region)

ggplot(good, aes(x = reorder(species, rel.yield), y = rel.yield)) +
  geom_point() +
  facet_wrap(facets = ~ region)


ggplot(good, aes(x = reorder(species, yield), y = yield)) +
  geom_point() +
  facet_wrap(facets = ~ region)
