library(tidycensus)
library(tidyverse)

census_api_key("")

# view variables
v15 <- load_variables(2015, "acs5", cache = TRUE)
#View(v15)

# distribution of age in Fresno county
fresno <- get_acs(state = "06", county = "019", geography = "tract", 
                  variables = "B01002_001", geometry = TRUE)
fresno %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma")

# distribution of age in Madera county
madera <- get_acs(state = "06", county = "039", geography = "tract", 
                  variables = "B01002_001", geometry = TRUE)
madera %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma")


# Examples from online sources:

# age10 <- get_decennial(geography = "state", 
#                        variables = "P013001", 
#                        year = 2010)
# head(age10)
# 
# age10 %>%
#   ggplot(aes(x = value, y = reorder(NAME, value))) + 
#   geom_point()
# 
# v17 <- load_variables(2017, "acs5", cache = TRUE)
# View(v17)
# 
# vt <- get_acs(geography = "county", 
#               variables = c(medincome = "B19013_001"), 
#               state = "VT", 
#               year = 2018)
# vt # this is an estimate and moe is the margin of error
# 
# vt %>%
#   mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
#   ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
#   geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
#   geom_point(color = "red", size = 3) +
#   labs(title = "Household income by county in Vermont",
#        subtitle = "2014-2018 American Community Survey",
#        y = "",
#        x = "ACS estimate (bars represent margin of error)")
# 
# 
# options(tigris_use_cache = TRUE)
# orange <- get_acs(state = "CA", county = "Orange", geography = "tract", 
#                   variables = "B19013_001", geometry = TRUE)
# head(orange)
# 
# orange %>%
#   ggplot(aes(fill = estimate)) + 
#   geom_sf(color = NA) + 
#   coord_sf(crs = 26911) + 
#   scale_fill_viridis_c(option = "magma") 
# 
# racevars <- c(White = "P005003", 
#               Black = "P005004", 
#               Asian = "P005006", 
#               Hispanic = "P004003")
# harris <- get_decennial(geography = "tract", variables = racevars, 
#                         state = "TX", county = "Harris County", geometry = TRUE,
#                         summary_var = "P001001") 
# head(harris)
# 
# harris %>%
#   mutate(pct = 100 * (value / summary_value)) %>%
#   ggplot(aes(fill = pct)) +
#   facet_wrap(~variable) +
#   geom_sf(color = NA) +
#   coord_sf(crs = 26915) + # Appropriate CRS for Houston, but may not be for your data
#   scale_fill_viridis_c()
# 
# ny <- get_acs(geography = "tract", 
#               variables = "B19013_001", 
#               state = "NY", 
#               county = "New York", 
#               geometry = TRUE)
# ggplot(ny, aes(fill = estimate)) + 
#   geom_sf() + 
#   theme_void() + 
#   scale_fill_viridis_c(labels = scales::dollar)
# 
# 
# library(sf)
# ny2 <- get_acs(geography = "tract", 
#                variables = "B19013_001", 
#                state = "NY", 
#                county = "New York", 
#                geometry = TRUE, 
#                cb = FALSE) 
# 
# 
# library(tigris)
# st_erase <- function(x, y) {
#   st_difference(x, st_union(y))
# }
# ny_water <- area_water("NY", "New York", class = "sf") 
# ny_erase <- st_erase(ny2, ny_water)
# 
# ggplot(ny_erase, aes(fill = estimate)) + 
#   geom_sf() + 
#   theme_void() + 
#   scale_fill_viridis_c(labels = scales::dollar)
# 
# library(sf)
# st_write(orange, "orange.shp")

