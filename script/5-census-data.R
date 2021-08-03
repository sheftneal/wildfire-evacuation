library(tidyverse)
library(tidycensus) #note if you haven't already installed you need to first run install.packages("tidycensus")

census_api_key("YOUR-KEY-HERE", install = T) #please use your census api key. If you run with , install = T only need to do once

#Following along with the code from here: https://walker-data.com/tidycensus/articles/basic-usage.html
# you can just copy and paste their code and run it yourself. For example:

         get_acs(
                        geography = "tract", 
                        variables = c(medincome = "B19013_001"), 
                        year = 2018,
                        state = "CA",
                        county = "Alameda"
                 ) %>% 
          dplyr::select(GEOID, NAME) %>% 
          mutate(evacuation1 = "",
                 evacuation2 = "",
                 evacuation3 = "", 
                 evacuation4 = "",
                 evacuation5 = "") %>% 
          write_csv("Alameda_Census_Tracts.csv")




head(age10)

age10 %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()


#Next do the same with the info here: https://walker-data.com/tidycensus/articles/spatial-data.html
