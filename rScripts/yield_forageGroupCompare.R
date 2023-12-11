# this script compares yield from ssurgo to the forage suitability groups

library(tidyverse)
library(ggplot2)
library(tidymodels)
library(ranger)

dat <- read.csv("data/yield_forageGroup.csv")
dat <- dat %>%
  select(c(cropname, yield, slope = slope.r, elev = elev.r, sandvc, sandco, sandmed, sandfine, 
           clay, om, ksat, cec, ph, awc, frag3, total.depth, forageGroupID)) %>%
  mutate(forageYield = case_when(forageGroupID == 1 ~ "1-2",
                                 forageGroupID == 2 ~ "1-2",
                                 forageGroupID == 3 ~ "1-2",
                                 forageGroupID == 4 ~ "1.5-2.5",
                                 forageGroupID == 5 ~ "3.5-4.5",
                                 forageGroupID == 6 ~ "3.5-4.5",
                                 forageGroupID == 7 ~ "2-3",
                                 forageGroupID == 8 ~ "4-5",
                                 forageGroupID == 9 ~ "4-5", 
                                 forageGroupID == 10 ~ "1-2"),
         yieldClass = case_when(forageYield == "1-2" ~ "Low",
                                forageYield == "1.5-2.5" ~ "Moderately low",
                                forageYield == "2-3" ~ "Moderate",
                                forageYield == "3.5-4.5" ~ "Moderately high",
                                forageYield == "4-5" ~ "High")) %>%
  mutate_if(is.character, as.factor)

  
mod <- readRDS("models/pastureCorRanger.rds")

dat.pred <- mod %>%
  predict(dat) %>%
  bind_cols(dat)

summary(dat.pred)

dat.pred$yieldClass <- factor(dat.pred$yieldClass, levels = 
                                c( "High", "Moderately high",  "Moderate","Moderately low","Low"))

ggplot(dat.pred, aes(x = yield, y = .pred, color = yieldClass)) +
  geom_jitter() +
  ylab("Yield from soils database (tons/acre)") +
  xlab("Predicted yield from model (tons/acre)") +
  labs(color = "Yield groups")

p <- ggplot(dat.pred, aes(x = yield, y = .pred, color = forageYield)) +
  facet_wrap(~forageYield) +
  geom_jitter(alpha = 0.4) +
  scale_color_manual(values = c("#238A8DFF", "#287D8EFF", 
                    "#33638DFF", "#404788FF", "#481567FF")) +
  xlab("Yield from soils database (tons/acre)") +
  ylab("Predicted yield from model (tons/acre)") +
  labs(color = "Forage yield groups \n(tons/acre)") +
  theme(text = element_text(size = 18))

library(gtable)
library(cowplot)

shift_legend <- function(p){
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  return(gp)
}

library(grid)

grid.draw(shift_legend(p))


