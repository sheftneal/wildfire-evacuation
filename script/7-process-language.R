source("script/0-packages-and-functions.R")
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)

census_api_key("")

# look at data
v18 <- load_variables(2018, "acs5", cache = TRUE)

#read in census data, C16001_ series of variables, check if im overcounting -- add up to get 3390 people when there are 3399 people in census tract 1
pop <-  get_acs(
  geography = "tract", 
  variables = c(  total = "C16001_001",
    
                  only_english = "C16001_002",
                  spanish_english_well = "C16001_004",
                  frenchhaitiuncajun_english_well = "C16001_007",
                  german_english_well = "C16001_010",
                  russian_polish_slavic_english_well = "C16001_013",
                  otherindoeuropean_english_well = "C16001_016",
                  korean_english_well = "C16001_019",
                  chinese_english_well = "C16001_022",
                  vietnamese_english_well = "C16001_025",
                  tagalog_english_well = "C16001_028",
                  otherasian_english_well = "C16001_031",
                  arabic_english_well = "C16001_034",
                  other_english_well = "C16001_037",
                  
                  spanish_english_not_well = "C16001_005",
                  frenchhaitiuncajun_english_not_well = "C16001_008",
                  german_english_not_well = "C16001_011",
                  russian_polish_slavic_english_not_well = "C16001_014",
                  otherindoeuropean_english_not_well = "C16001_017",
                  korean_english_not_well = "C16001_020",
                  chinese_english_not_well = "C16001_023",
                  vietnamese_english_not_well = "C16001_026",
                  tagalog_english_not_well = "C16001_029",
                  otherasian_english_not_well = "C16001_032",
                  arabic_english_not_well = "C16001_035",
                  other_english_not_well = "C16001_038"
                ), 
  year = 2018,
  state = "CA",
  county = "Fresno"
) 


#re-organize
# binary population of proficient and non proficient
pop1 <- pop %>% dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(
    proficient = only_english + spanish_english_well + frenchhaitiuncajun_english_well +
    german_english_well + russian_polish_slavic_english_well + otherindoeuropean_english_well +
    korean_english_well + chinese_english_well + vietnamese_english_well + tagalog_english_well + 
    otherasian_english_well + arabic_english_well + other_english_well, 
    
    not_proficient = spanish_english_not_well + frenchhaitiuncajun_english_not_well + german_english_not_well + 
    russian_polish_slavic_english_not_well + otherindoeuropean_english_not_well + korean_english_not_well + 
    chinese_english_not_well + vietnamese_english_not_well + tagalog_english_not_well + otherasian_english_not_well + 
    arabic_english_not_well + other_english_not_well,
    
    perc_proficient = 100 * proficient / (proficient + not_proficient)
  )

# population of proficient in english, only proficient in spanish, and only proficient in a language that isn't english or spanish
pop2 <- pop %>% dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(
    proficient_english = only_english + spanish_english_well + frenchhaitiuncajun_english_well +
      german_english_well + russian_polish_slavic_english_well + otherindoeuropean_english_well +
      korean_english_well + chinese_english_well + vietnamese_english_well + tagalog_english_well + 
      otherasian_english_well + arabic_english_well + other_english_well, 
    
    only_proficient_spanish = spanish_english_not_well,
    
    only_proficient_not_english_or_spanish = frenchhaitiuncajun_english_not_well + german_english_not_well + 
      russian_polish_slavic_english_not_well + otherindoeuropean_english_not_well + korean_english_not_well + 
      chinese_english_not_well + vietnamese_english_not_well + tagalog_english_not_well + otherasian_english_not_well + 
      arabic_english_not_well + other_english_not_well,
    
    perc_only_proficient_spanish = 100 * only_proficient_spanish / total
  )

# graph the percentage of each census tract that is proficient on a map of Fresno
fresno %>%
  ggplot(aes(fill = pop1$perc_proficient)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis")

# graph the percentage of each census tract that is only proficient in spanish on a map of Fresno
fresno %>%
  ggplot(aes(fill = pop2$perc_only_proficient_spanish)) + 
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

