source("script/0-packages-and-functions.R")

# loading & assigning data
ca <- read_rds("data/california_county_shapefile.rds")
wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")

# assigning variables
scu_fire <- wf %>% dplyr::filter(INC_NUM == "00005740")
counties_orders <- ca %>% dplyr::filter(NAME == "San Joaquin" | NAME == "Santa Clara"| NAME == "Stanislaus" | NAME == "Alameda")
counties_warnings <- ca %>% dplyr::filter(NAME == "Merced" | NAME == "Contra Costa")

# plotting counties & fire
counties_orders %>% st_geometry() %>% plot(lwd = 1, col = 'firebrick1', border = 'black')
counties_warnings %>% st_geometry() %>% plot(lwd = 1, col = 'dodgerblue2', border = 'black', add = T)
scu_fire %>% st_geometry() %>% plot(lwd = 1, border = 'gold1', add = T)

# legend
legend(
       "bottomright", 
       legend = c("Evacuation Orders", "Evacuation Warnings"), 
       col = c('firebrick1', 'dodgerblue2'), lty = 1, cex = 0.8, box.lty = 1, box.lwd = 1
)

