source("script/0-packages-and-functions.R");

#load and assign 

ca <- read_rds("data/california_county_shapefile.rds")
wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")

county_evac <- ca %>% dplyr::filter(NAME == "Napa" | NAME == "Sonoma")
county_warn <- ca %>% dplyr::filter(NAME == "Lake" )
glass_Fire <- wf %>% dplyr::filter(INC_NUM == "00015497") 

#Plotting 

 plot(st_geometry(county_evac), lwd = 3, border = 'black', col = 'cadetblue4')
 plot(st_geometry(county_warn), lwd = 3, border = 'black', col = 'coral4', add = T)
 plot(st_geometry(glass_Fire), lwd = 3, border = 'red', add = T)
 
 #Key 
 
 legend( "bottomleft", legend = c("Evacuation Orders", "Evacuation Warnings"), 
         col = c('cadetblue4', 'coral4'), bg= "white", lty=1, cex=0.8,
         box.lty=2, box.lwd=2)

