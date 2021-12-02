#checked 12/30

source("script/0-packages-and-functions.R")

# load data
ca <- read_rds("data/california_county_shapefile.rds")
wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")

# assign variables
counties_orders <- ca %>% dplyr::filter(NAME == "Fresno")
creek_fire <- wf %>% dplyr::filter(INC_NUM == "00001391") # Got INC_NUM for creek fire from spreadsheet
mineral_fire <- wf %>% dplyr::filter(INC_NUM == "00010219") # Got INC_NUM for creek fire from spreadsheet
elm_fire <- wf %>% dplyr::filter(INC_NUM == "00006727") # Got INC_NUM for creek fire from spreadsheet
mineral2_fire <- wf %>% dplyr::filter(INC_NUM == "011358") # Got INC_NUM for creek fire from spreadsheet
curry_fire <- wf %>% dplyr::filter(INC_NUM == "009100") # Got INC_NUM for creek fire from spreadsheet
goose_fire <- wf %>% dplyr::filter(INC_NUM == "010852") # Got INC_NUM for creek fire from spreadsheet
rough_fire <- wf %>% dplyr::filter(INC_NUM == "001746") # Got INC_NUM for creek fire from spreadsheet

# plot counties and creek fire
counties_orders %>% st_geometry() %>% plot(lwd = 0.25, col = '#CAE7C1', border = 'black')
creek_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)
mineral_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)
elm_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)
mineral2_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)
curry_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)
goose_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)
rough_fire %>% st_geometry() %>% plot(lwd = 0.25, border = 'red3', add = T)
