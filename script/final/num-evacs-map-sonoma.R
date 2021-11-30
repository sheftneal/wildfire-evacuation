source("script/0-packages-and-functions.R")
library(tidycensus)
census_api_key("f5c6e020a6aae7ba5420a4051ad53eee38c621fa")

ct <- read.csv("data/clean/Sonoma Census Tract Evacuation Order Mapping.csv") %>%
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>%
  mutate(GEOID = as.character(GEOID))

ct$n_order <- apply(ct[,paste("evacuation",1:29, sep = "")], 1, function(x){sum(!is.na(x))})

#Graph of Number of Evacuation Orders in Sonoma County 
#ct$n_order is the variable, 94th census tract is reassigned a value of 17 Evacuation Orders.
Sonoma <- get_acs(geography = "tract", state = "06", county = "Sonoma", geometry = TRUE, variables = "B01002_001")
ct$n_order[94]<-17
Sonoma %>%
  ggplot(aes(fill = ct$n_order)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma")
  
