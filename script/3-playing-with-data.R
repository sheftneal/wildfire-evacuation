source("script/0-packages-and-functions.R")

#load and play with the data 

ca <- read_rds("data/california_county_shapefile.rds")
wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")


# look at the data structure

head(wf)


#plot ca counties  


ca %>% st_geometry() %>% plot(lwd = 0.25, border = 'gray50')
plot(st_geometry(ca), lwd = 0.25, border = 'gray50') #exactly the same as previous line but without the %>%


#plot the fires that started in 2020 and were attributed to lightning

wf %>% 
  dplyr::filter(start_day >= "2020-01-01" & end_day <= "2020-12-31" & CAUSE == 1) %>%  #see Data Description at data source page for variable descriptions https://frap.fire.ca.gov/frap-projects/fire-perimeters/
  st_geometry() %>% 
  plot(col = 'red3', border = NA, add = T)


#add fires from 2019 that were more than 10,000 acres

wf %>% 
  dplyr::filter(start_day >= "2019-01-01" & end_day <= "2019-12-31" & GIS_ACRES > 10000) %>%  #see Data Description at data source page for variable descriptions https://frap.fire.ca.gov/frap-projects/fire-perimeters/
  st_geometry() %>% 
  plot(col = 'blue', border = NA, add = T)


#what's the mean and max fire sizes by year?

wf %>% as.data.frame() %>% 
  group_by(start_year) %>% 
  summarize(
    mean_fire = mean(GIS_ACRES, na.rm = T),
    max_fire = max(GIS_ACRES, na.rm = T)
  ) %>% as.data.frame() 


