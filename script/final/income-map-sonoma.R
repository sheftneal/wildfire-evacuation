#checked 12/30

#Median Income Sonoma County Map 
Sonoma <- get_acs(geography = "tract", state = "06", county = "Sonoma",
                  geometry = TRUE, variables = "B06011_001")
Sonoma %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma")
