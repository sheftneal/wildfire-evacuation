source("script/0-packages-and-functions.R")

# load data
wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")

#re-organize data:
plotdata <- wf %>%
  as.data.frame() %>% #normally don't have to do this but because wf is a spatial object this makes things easier
  mutate(year = as.numeric(YEAR_)) %>% #for some reason YEAR_ is stored as a character vector but we want it to be numeric
  dplyr::filter(year >= 2000 & year <= 2020) %>%
  dplyr::filter(CAUSE == 1 | CAUSE == 2 | CAUSE == 4 | CAUSE == 7 | CAUSE == 8 | CAUSE == 11 | CAUSE == 18) %>%
  #notes: lightning = 1; equipment use = 2; campfire = 4; arson = 7; playing with fire = 8; power line = 11; escaped prescribed burn = 18 
  group_by(CAUSE, year) %>%
  summarize(
    burned_area = sum(GIS_ACRES, na.rm = T) / 1000,
    num_fires_per_year_per_cause = n()
  ) 

#plot
png(filename = "figures/causes-plot-jena.png", width = 800, height = 400) 

#initialize 2-column plot
par(mfrow = c(1,2)) 

#plot first column
plot(plotdata$year[c(1:21)], plotdata$num_fires_per_year_per_cause[c(1:21)], axes = F, xlab = "", ylab = "Fires", main = "", col = "#AE0001", type="l")
lines(plotdata$year[c(22:42)], plotdata$num_fires_per_year_per_cause[c(22:42)], xlab = "", ylab = "", main = "", col = "#D3A625", type="l")
lines(plotdata$year[c(43:63)], plotdata$num_fires_per_year_per_cause[c(43:63)], xlab = "", ylab = "", main = "", col = "#740001", type="l")
lines(plotdata$year[c(64:84)], plotdata$num_fires_per_year_per_cause[c(64:84)], xlab = "", ylab = "", main = "", col = "#222F5B", type="l")
lines(plotdata$year[c(85:105)], plotdata$num_fires_per_year_per_cause[c(85:105)], xlab = "", ylab = "", main = "", col = "#FFDB00", type="l")
lines(plotdata$year[c(106:126)], plotdata$num_fires_per_year_per_cause[c(106:126)], xlab = "", ylab = "", main = "", col = "#2A623D", type="l")
lines(plotdata$year[c(127:145)], plotdata$num_fires_per_year_per_cause[c(127:145)], xlab = "", ylab = "", main = "", col = "#1A472A", type="l")

#add labels
#mtext(side = 2, text = "Fires") #y-axis label
mtext(side = 3, text = "Number of fires") #main label
axis(1, tick = F) #add x-axis
axis(2, tick = F, las = 1) #add y-axis


#plot second column
plot(plotdata$year[c(1:21)], plotdata$burned_area[c(1:21)], axes = F, xlab = "", ylab = "Thousands of Acres", main = "", col = "#AE0001", type="l") #red 
lines(plotdata$year[c(22:42)], plotdata$burned_area[c(22:42)], xlab = "", ylab = "", main = "", col = "#D3A625", type="l") #gold
lines(plotdata$year[c(43:63)], plotdata$burned_area[c(43:63)], xlab = "", ylab = "", main = "", col = "#740001", type="l") #burgundy
lines(plotdata$year[c(64:84)], plotdata$burned_area[c(64:84)], xlab = "", ylab = "", main = "", col = "#222F5B", type="l") #navy
lines(plotdata$year[c(85:105)], plotdata$burned_area[c(85:105)], xlab = "", ylab = "", main = "", col = "#FFDB00", type="l") #yellow
lines(plotdata$year[c(106:126)], plotdata$burned_area[c(106:126)], xlab = "", ylab = "", main = "", col = "#2A623D", type="l") #green
lines(plotdata$year[c(127:145)], plotdata$burned_area[c(127:145)], xlab = "", ylab = "", main = "", col = "#1A472A", type="l") #dark green

#add labels
#mtext(side = 2, text = "Fires") #y-axis label
mtext(side = 3, text = "Burned Area") #main label
axis(1, tick = F) #add x-axis
axis(2, tick = F, las = 1) #add y-axis

# <legend goes here>

dev.off() #ends the png call. everything between png() and dev.off() will be written to an external file    

