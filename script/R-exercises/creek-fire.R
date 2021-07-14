source("script/0-packages-and-functions.R")

# load data
ca <- read_rds("data/california_county_shapefile.rds")
wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")

# assign variables
creek_fire <- wf %>% dplyr::filter(INC_NUM == "00001391") # Got INC_NUM for creek fire from spreadsheet
counties_orders <- ca %>% dplyr::filter(NAME == "Fresno" | NAME == "Madera")
counties_warnings <- ca %>% dplyr::filter(NAME == "Mariposa")

# plot counties and creek fire
counties_orders %>% st_geometry() %>% plot(lwd = 0.25, col = 'darkturquoise', border = 'black')
counties_warnings %>% st_geometry() %>% plot(lwd = 0.25, col = 'darkviolet', border = 'black', add = T)
creek_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)

# legend
legend("bottomright", 
       legend = c("Evacuation Orders", "Evacuation Warnings"), 
       col = c('darkturquoise', 'darkviolet'), pch = c(19,19), 
       bty = "n", pt.cex = 0.75, cex = 0.65, text.col = "black", 
       horiz = F, inset = c(0.05, 0.05))

