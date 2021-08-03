source("script/0-packages-and-functions.R")

wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")

#re-organize data
plotdata <- wf %>%
  as.data.frame() %>% #normally don't have to do this but because wf is a spatial object this makes things easier
  mutate(year = as.numeric(YEAR_)) %>% #for some reason YEAR_ is stored as a character vector but we want it to be numeric
  group_by(start_year) %>%
  
  summarize(
    thousands_of_acres_burned = sum(GIS_ACRES) / 1000,
    num_fires_per_year = n() #counts the number of observations in group
  ) %>%
  
  dplyr::filter(start_year >= 2000 & start_year <= 2020)

#fix the 2015 and 2020 data that had missing GIS_ACRES data
wf_2015 <- dplyr::filter(wf, start_year==2015, !is.na(GIS_ACRES))
sum_2015 = sum(wf_2015$GIS_ACRES) / 1000
plotdata$thousands_of_acres_burned[16] = sum_2015
wf_2020 <- dplyr::filter(wf, start_year==2020, !is.na(GIS_ACRES))
sum_2020 = sum(wf_2020$GIS_ACRES) / 1000
plotdata$thousands_of_acres_burned[21] = sum_2020

#plot
png(filename="timeSeriesGraph_Jen.png", width = 800, height = 400) 

#make 2-column plot
par(mfrow = c(1,2)) 

#plot the first graph
plot(plotdata$start_year, plotdata$num_fires_per_year, axes = F, xlab = "Year", ylab = "Fires", main = "Number of Fires", col = "dodgerblue2", type="l")
abline(lm(num_fires_per_year~as.numeric(start_year), data=plotdata), col="firebrick1", lty=2)

#add axes
axis(1) #add x-axis
axis(2, las = 1) #add y-axis

#plot the second graph
plot(plotdata$start_year, plotdata$thousands_of_acres_burned, axes = F, xlab = "Year", ylab = "Thousands of Acres", main = "Burned area", col = "gold1", type="l")
abline(lm(thousands_of_acres_burned~as.numeric(start_year), data=plotdata), col="firebrick1", lty=2)

#add axes for second graph
axis(1) #add x-axis
axis(2, las = 1) #add y-axis

dev.off()
#ends the png call. everything between png() and dev.off() will be written to an external file

#try na.rm = TRUE

