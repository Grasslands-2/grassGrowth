library(tidyverse)
library(forcats)
library(ggplot2)

dat <- read_csv("data/speciesClean.csv")
summary(dat)
levels(as.factor(dat$tons_acre))

ggplot(dat, aes(x = fct_reorder(species,tons_acre), y = tons_acre)) +
  geom_boxplot()

dat_sum <- dat %>%
  group_by(species) %>%
  summarize(meanYield = mean(tons_acre),
            n = n(),
            se = sd(tons_acre)/sqrt(n)) 

ggplot(dat_sum, aes(x = fct_reorder(species,meanYield), y = meanYield)) +
  geom_col()+
  geom_errorbar(aes(ymin = meanYield - se, ymax = meanYield + se))


mod.aov <- aov(tons_acre~species, data = analysisData)
summary(mod.aov)

tukey <- TukeyHSD(mod.aov)
tukey.cld <- multcompLetters3(mod.aov, tukey, data = analysisData)
library(multcompView)
library(multcomp)
library(emmeans)
library(lsmeans)

mod.lm <- lm(tons_acre~species, data = analysisData)
# output <- pairs(emmeans(mod.lm, specs = ~species))
# model_means <- emmeans(mod.lm, "species")
# class(model_means)
# model_means <- cld(model_means, 
#                    adjust = "Tukey",
#                    Letters = letters,
#                    alpha = 0.05)
means_mod <- lsmeans(mod.lm, ~species)
cld(object = means_mod, level = 0.1, Letters = letters)
speciesDF <- cld(object = means_mod, level = 0.05, Letters = letters)

write.csv(speciesDF, "grass species composition/species_sigDif.csv", row.names = FALSE, quote = FALSE)

means_mod2 <- emmeans(mod.lm, ~species)
contrast(means_mod, "pairwise")
cld(means_mod2, Letters = letters)
