source("script/0-packages-and-functions.R")
library(tidycensus)

census_api_key("")

# look at data
v18 <- load_variables(2018, "acs5", cache = TRUE)

#read in census data
pop <-  get_acs(
  geography = "tract", 
  variables = c(  only_english = "B06007_002",
                  speak_spanish_and_speak_english_very_well = "B06007_004",
                  speak_other_languages_and_speak_english_very_well = "B06007_007",
                  speak_spanish_and_speak_english_not_well = "B06007_005",
                  speak_other_languages_and_speak_english_not_well = "B06007_008"
                ), 
  year = 2018,
  state = "CA",
  county = "Fresno"
) 

#re-organize
pop <- pop %>% dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(
    proficient  = only_english + speak_spanish_and_speak_english_very_well + speak_other_languages_and_speak_english_very_well, 
    not_proficient = speak_spanish_and_speak_english_not_well + speak_other_languages_and_speak_english_not_well,
    perc_proficient = 100 * proficient / (proficient + not_proficient)
  )

# graph the percentage of each census tract that is proficient on a map of Fresno
fresno %>%
  ggplot(aes(fill = pop$perc_proficient)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis")

############################################################################################################
# Prep data for figures:

############################################################################################################
# Plots:

# side by side: pie chart of what percent of the population is and is not proficient in english
# pie chart of the percentage of the time proficient and not proficient folks are being evacuated
# get a statistic on how many non-proficient people have been evacuated since 2013.

