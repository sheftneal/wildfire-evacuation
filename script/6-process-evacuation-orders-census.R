source("script/0-packages-and-functions.R")
library(tidycensus)

#read in census data
      

  
      pop <-  get_acs(
                        geography = "tract", 
                        variables = c(  male_all = "B01001_002",
                                        male_under5 = "B01001_003",
                                        male_5_9 = "B01001_004",
                                        male_10_14 = "B01001_005",
                                        male_15_17 = "B01001_006",
                                        male_18_19 = "B01001_007",
                                        male_20 = "B01001_008",
                                        male_21 = "B01001_009",
                                        male_22_24 = "B01001_010",
                                        male_25_29 = "B01001_011",
                                        male_30_34 = "B01001_012",
                                        male_35_39 = "B01001_013",
                                        male_40_44 = "B01001_014",
                                        male_45_49 = "B01001_015",
                                        male_50_54 = "B01001_016",
                                        male_55_59 = "B01001_017",
                                        male_60_61 = "B01001_018",
                                        male_62_64 = "B01001_019",
                                        male_65_66 = "B01001_020",
                                        male_67_69 = "B01001_021",
                                        male_70_74 = "B01001_022",
                                        male_75_79 = "B01001_023",
                                        male_80_84 = "B01001_024",
                                        male_over85 = "B01001_025",
                                        female_all = "B01001_026",
                                        female_under5 = "B01001_027",
                                        female_5_9 = "B01001_028",
                                        female_10_14 = "B01001_029",
                                        female_15_17 = "B01001_030",
                                        female_18_19 = "B01001_031",
                                        female_20 = "B01001_032",
                                        female_21 = "B01001_033",
                                        female_22_24 = "B01001_034",
                                        female_25_29 = "B01001_035",
                                        female_30_34 = "B01001_036",
                                        female_35_39 = "B01001_037",
                                        female_40_44 = "B01001_038",
                                        female_45_49 = "B01001_039",
                                        female_50_54 = "B01001_040",
                                        female_55_59 = "B01001_041",
                                        female_60_61 = "B01001_042",
                                        female_62_64 = "B01001_043",
                                        female_65_66 = "B01001_044",
                                        female_67_69 = "B01001_045",
                                        female_70_74 = "B01001_046",
                                        female_75_79 = "B01001_047",
                                        female_80_84 = "B01001_048",
                                        female_over85 = "B01001_049"
                                      ), 
                        year = 2018,
                        state = "CA",
                        county = "Fresno"
                      ) 
      
      #now re-organize
      
      pop <- pop %>% dplyr::select(-moe) %>% 
                pivot_wider(
                names_from = "variable",
                values_from = "estimate"
                          ) %>% 
                    mutate(
                      pop_under18  = male_under5 + male_5_9 +male_10_14 + male_15_17 + female_under5 + female_5_9 + female_10_14 + female_15_17,
                      pop_18_45  = male_18_19 + male_20 + male_21 + male_22_24 + male_25_29 + male_30_34 + male_35_39 + male_40_44  + female_18_19 + female_20 + female_21 + female_22_24 + female_25_29 + female_30_34 + female_35_39 + female_40_44, 
                      pop_45_70  = male_45_49 + male_50_54 + male_55_59 + male_60_61 + male_62_64 + male_65_66 + male_67_69 + female_45_49 + female_50_54 +female_55_59 + female_60_61 + female_62_64 + female_65_66 + female_67_69,
                      pop_over70  = male_70_74 + male_80_84 + male_over85 + female_70_74 + female_80_84 + female_over85,
                      pop_total = male_all + female_all
                    ) %>% 
                    dplyr::select(GEOID, starts_with("pop"))
      
      all.equal(rowSums(pop[,c("pop_under18","pop_18_45","pop_45_70","pop_over70")]), pop$pop_total)
      #close but there are probably some people with unknown ages that don't get into the age cateogries but are in the total
        
      pop <- pop %>% mutate(pop_total = pop_under18 + pop_18_45 + pop_45_70 + pop_over70)



# income
    income <- 
      get_acs(
        geography = "tract", 
        variables =  c(med_income = "B06011_001"), 
        year = 2018,
        state = "CA",
        county = "Fresno"
      ) 
  income <- income %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate)  

  

  acs <- left_join(income, pop)
  
  

  ################################################################################################################################  
  
  
  
#read in census tract mapping
ct <- read.csv("data/clean/Fresno_Census_FilledIn.csv", skip = 2) %>% 
      dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% 
      mutate(GEOID = as.character(GEOID))


#count # evac orders per census tract
ct$n_order <- apply(ct[,paste("evacuation",1:12, sep = "")], 1, function(x){sum(!is.na(x))})

#reorganize so each row is a census tract - evacuation ID combination
ct <- ct %>% 
      dplyr::filter(n_order > 0) %>%  #filter to census tracts with orders
      pivot_longer(
        cols = evacuation1:evacuation14,
        names_to = "Evacuation.Index",
        values_to = "Evacuation.ID"
                    ) %>% 
      dplyr::select(-Evacuation.Index) %>% 
      dplyr::filter(!is.na(Evacuation.ID))


#bring in and clean up evacuation order database
evacs <- read.csv("data/clean/Wildfire Evacuation Order Database - Fresno & Madera County Fires.csv") %>% 
        dplyr::select(-starts_with("X")) %>% 
        dplyr::select(Evacuation.ID, FIRE_NAME, GIS_ACRES,evacuation.order.type,evac_date_issue)


#join data sets

data <- left_join(ct, evacs)
nrow(data) #47 census tract-evacuation order combinations

#what is date range of evacuations?
data$evac_date_issue <- as.Date(data$evac_date_issue, format = "%m/%d/%y")


  #what is date range of evacuations?
    min(data$evac_date_issue)
    max(data$evac_date_issue)

#create year and month variables
data <- data %>% 
        mutate(
          evac_year = lubridate::year(evac_date_issue),
          evac_month = lubridate::month(evac_date_issue)
        )



#how many census tracts did each fire cause evacuation for?
data %>% group_by(FIRE_NAME) %>% summarise(n_fire = n())
#6 fires causes evacuations with CREEK fire causing by far the most






############################################################################################################

#prep data for figures:

#[1] x-axis year, y-axis #total population evacuated by age group
acs$GEOID <- substr(acs$GEOID, 2, 1000) #drop leading 0
annual_evacs_bypop <- data %>% group_by(evac_year) %>% 
                summarise(evac_pop_under18 = sum(pop_under18, na.rm = T),
                          evac_pop_18_45 = sum(pop_18_45, na.rm = T),
                          evac_pop_45_70 = sum(pop_45_70, na.rm = T),
                          evac_pop_over70 = sum(pop_over70, na.rm = T))


#x-axis year, y-axis total population evacuated by age group (a bit different than drawing)
annual_evac_by_age <- data.frame(evac_year = 2012:2021) %>% left_join(annual_evacs_bypop) %>% 
            mutate(
              evac_pop_under18 = replace(evac_pop_under18, is.na(evac_pop_under18), 0),
              evac_pop_18_45 = replace(evac_pop_18_45, is.na(evac_pop_18_45), 0),
              evac_pop_45_70 = replace(evac_pop_45_70, is.na(evac_pop_45_70), 0),
              evac_pop_over70 = replace(evac_pop_over70, is.na(evac_pop_over70), 0)
              )




#evacuated at any point age distribution
ct_norder <- read.csv("data/clean/Fresno_Census_FilledIn.csv", skip = 2) %>% 
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% 
  mutate(GEOID = as.character(GEOID))


#count # evac orders per census tract
ct_norder$n_order <- apply(ct_norder[,paste("evacuation",1:12, sep = "")], 1, function(x){sum(!is.na(x))})
ct_norder <- ct_norder %>% dplyr::filter(n_order > 0) %>% dplyr::select(GEOID, n_order)

ct_acs <- left_join(acs, ct_norder) %>% mutate(n_order = replace(n_order, is.na(n_order), 0))

    #county population
    county_demo <- ct_acs %>% 
                    summarise(
                      pop_under18 = sum(pop_under18, na.rm = T),
                      pop_18_45 = sum(pop_18_45, na.rm = T),
                      pop_45_70 = sum(pop_45_70, na.rm = T),
                      pop_over70 = sum(pop_over70, na.rm = T),
                      med_inc = mean(med_inc, na.rm = T)
                    ) %>% 
      mutate(pop_total = pop_under18 + pop_18_45+ pop_45_70 + pop_over70,
             share_under18 = pop_under18/pop_total,
             share_18_45 = pop_18_45/pop_total,
             share_45_70 = pop_45_70/pop_total,
             share_over70 = pop_over70/pop_total)


    #population by age and whether they were evacuated at any time during our sample
    evacuated_demo <- ct_acs %>% 
              mutate(evacuated = as.numeric(n_order > 0)) %>% 
              group_by(evacuated) %>% 
              summarise(
                pop_under18 = sum(pop_under18, na.rm = T),
                pop_18_45 = sum(pop_18_45, na.rm = T),
                pop_45_70 = sum(pop_45_70, na.rm = T),
                pop_over70 = sum(pop_over70, na.rm = T),
                med_inc = mean(med_inc, na.rm = T)
              ) %>% 
            mutate(pop_total = pop_under18 + pop_18_45+ pop_45_70 + pop_over70,
                   share_under18 = pop_under18/pop_total,
                   share_18_45 = pop_18_45/pop_total,
                   share_45_70 = pop_45_70/pop_total,
                   share_over70 = pop_over70/pop_total)


    save(county_demo, evacuated_demo, annual_evac_by_age, file = "data/clean/FresnoEvacuationData.RData")







