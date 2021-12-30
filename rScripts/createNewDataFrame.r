# create colnames
cols <- c("cropname", "yield", "slope", "elev", "sand", "silt", "clay", "om", "ksat","cec", "ph", "awc","total.depth",
          "WI_CCPI_rating", "NCCPI_rating")
# create data frame
new_dat <- data.frame(as.factor("Bluegrass-clover"), 4.0, 9, 258, 10.5, 67.75, 21.75, 1.12, 6, 17.4, 6.3, 0.21, 200, 0.75, 0.51)
# give colnames to new_dat
colnames(new_dat) <- cols
# give new_dat correct cropname levels
grass_levels <- as.factor(c("Bluegrass-clover", "Orchardgrass-clover","Timothy-clover"))
levels(new_dat$cropname) <- grass_levels

predict(modRF, newdata = new_dat)

