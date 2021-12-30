# load data
grass <- read_csv("grassData_simple.csv")
growth <- readxl::read_excel("seasonalGrowth.xlsx")

# example grass yield data for 3 species from predictions
ktbg <- as.integer(grass[1,2])
og <- as.integer(grass[2,2])
tm <- as.integer(grass[4,2])

#multiply total yield by percent for each species
season_growth <- growth %>%
  mutate(yield = case_when(species == "kentuckyBluegrass" & quality == "good" ~ (percentage/100)*ktbg,
                           species == "kentuckyBluegrass" & quality == "poor" ~ (percentage/100)*(0.65*ktbg),
                           species == "orchardGrass" & quality == "good" ~ (percentage/100)*og,
                           species == "orchardGrass" & quality == "poor" ~ (percentage/100)*(0.65*og),
                           species == "timothy" & quality == "good" ~ (percentage/100)*tm,
                           species == "timothy" & quality == "poor" ~ (percentage/100)*(0.65*tm),
                           species == "grass_clover" & quality == "good" ~ (percentage/100)* ktbg,
                           species == "grass_clover" & quality == "poor" ~ (percentage/100)*(0.65*ktbg)
                           )
  )


season_growth$monthF <- factor(season_growth$month, 
                                labels=c("May", "June", "July", "Aug", "Sept", "Oct"))
ggplot(season_growth, aes(x = month, y = yield, color = quality)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~species)

ggplot(season_growth, aes(x = monthF, y = yield, color = quality, group = 1)) +
  geom_point() +
  geom_line() +
  facet_wrap(~species)
         