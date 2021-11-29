source("script/0-packages-and-functions.R")
library(tidycensus)

census_api_key("f5c6e020a6aae7ba5420a4051ad53eee38c621fa")

fresno <- get_acs(geography = "tract", state = "06", county = "Fresno",
                  geometry = TRUE, variables = "B06011_001")
fresno %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma")