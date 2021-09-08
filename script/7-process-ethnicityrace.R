source("script/0-packages-and-functions.R")
library(tidycensus)

#census_api_key("")

#read in census data
pop <-  get_acs(
  geography = "tract", 
  variables = c(  
                  race_tot = "B03002_001",
                  race_nh_tot = "B03002_002",
                  race_nh_wh = "B03002_003",
                  race_nh_bk = "B03002_004",
                  race_nh_na = "B03002_005",
                  race_nh_as = "B03002_006",
                  race_nh_hw = "B03002_007",
                  race_nh_ot = "B03002_008",
                  race_nh_2ot1 = "B03002_009",
                  race_nh_2ot2 = "B03002_010",
                  race_nh_3ot = "B03002_011",
                  race_h_tot = "B03002_012",
                  race_h_wh = "B03002_013",
                  race_h_bk = "B03002_014",
                  race_h_na = "B03002_015",
                  race_h_as = "B03002_016",
                  race_h_hw = "B03002_017",
                  race_h_ot = "B03002_018",
                  race_h_2ot1 = "B03002_019",
                  race_h_2ot2 = "B03002_020",
                  race_h_3ot = "B03002_021"
                ), 
                year = 2018,
                state = "CA",
                county = "Alameda"
              ) 




#creates categories for share of population by each race (white, black, asian, hawaiian, native, 2+) by ethnicity (hisp, non-hisp) combo 
#


race <- pop %>% dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(
    share_hisp = race_h_tot/race_tot,
    share_nh_wh = race_nh_wh/race_tot,

    share_h_wh = race_h_wh/race_tot,
    share_h_bk = race_h_bk/race_tot,
    share_h_na = race_h_na/race_tot,
    share_h_as = race_h_as/race_tot,
    share_h_hw = race_h_hw/race_tot,
    share_h_ot = race_h_ot/race_tot,
    share_h_2ot1 = race_h_2ot1/race_tot,
    share_h_2ot2 = race_h_2ot2/race_tot,
    share_h_3ot = race_h_3ot/race_tot,
    
    share_nh_wh = race_nh_wh/race_tot,
    share_nh_bk = race_nh_bk/race_tot,
    share_nh_na = race_nh_na/race_tot,
    share_nh_as = race_nh_as/race_tot,
    share_nh_hw = race_nh_hw/race_tot,
    share_nh_ot = race_nh_ot/race_tot,
    share_nh_2ot1 = race_nh_2ot1/race_tot,
    share_nh_2ot2 = race_nh_2ot2/race_tot,
    share_nh_3ot = race_nh_3ot/race_tot

  ) %>% 
  dplyr::select(GEOID, starts_with("share_"))



# income
income <- 
  get_acs(
    geography = "tract", 
    variables =  c(med_income = "B06011_001"), 
    year = 2018,
    state = "CA",
    county = "Alameda"
  ) 
income <- income %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate)  


acs <- left_join(income, race) %>% dplyr::select(-share_h_hw, -share_nh_hw)


#looks like most hispanices put down white or other in race: 
summary(acs$share_h_wh/acs$share_hisp) #share of hispanics that check white
summary(acs$share_h_ot/acs$share_hisp) #share of hispanics that check other
summary((acs$share_h_wh + acs$share_h_ot)/acs$share_hisp) #on average 85-90% hispanics check white or other as race


#now what if we want to combine with income?

#can create income deciles (maybe want to do this at state-level? otherwise will be only comparing income within county)
income_ca <- 
  get_acs(
    geography = "tract", 
    variables =  c(med_income = "B06011_001"), 
    year = 2018,
    state = "CA"
  ) 

#finds the 10th, 20th, etc. through 90th percentile of median income across all CA census tracts
med_income_deciles <- income_ca %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate) %>% dplyr::select(med_inc) %>% quantile(seq(0.1,0.9,.1), na.rm = T)  

#uses the cutoffs determined above to split the census tracts in this county into whichever deciles they fall for statewide distribution
acs$income_decile <- statar::xtile(acs$med_inc, cutpoints = med_income_deciles) #creates variable with values 1-10 identifying which decile census tract median income falls into


#now decile the race variables


#for now will do non-hispanic white, non-hispanic asian, non-hispanic black, non-hispanic, hispanic 

#determine statewide deciles
pop <-  get_acs(
  geography = "tract", 
  variables = c(  
    race_tot = "B03002_001",
    race_nh_wh = "B03002_003",
    race_nh_bk = "B03002_004",
    race_nh_as = "B03002_006",
    race_h_tot = "B03002_012"
  ), 
  year = 2018,
  state = "CA"
) %>% 
  dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(
    share_nh_wh = race_nh_wh/race_tot,
    share_nh_bk = race_nh_bk/race_tot,
    share_nh_as = race_nh_as/race_tot,
    share_h = race_h_tot/race_tot,
         ) %>% 
  dplyr::select(GEOID, share_nh_wh, share_nh_bk, share_nh_as, share_h)

cutoffs_nh_wh <-  quantile(pop$share_nh_wh, seq(0.1,0.9,.1), na.rm = T)  
cutoffs_nh_bk <-  quantile(pop$share_nh_bk, seq(0.1,0.9,.1), na.rm = T)  
cutoffs_nh_as <-  quantile(pop$share_nh_as, seq(0.1,0.9,.1), na.rm = T)  
cutoffs_h <-  quantile(pop$share_h, seq(0.1,0.9,.1), na.rm = T)  


acs$white_decile <- statar::xtile(acs$share_nh_wh, cutpoints = cutoffs_nh_wh) #creates variable with values 1-10 identifying which decile census tract median income falls into
acs$black_decile <- statar::xtile(acs$share_nh_bk, cutpoints = cutoffs_nh_bk) #creates variable with values 1-10 identifying which decile census tract median income falls into
acs$asian_decile <- statar::xtile(acs$share_nh_as, cutpoints = cutoffs_nh_as) #creates variable with values 1-10 identifying which decile census tract median income falls into
acs$hispanic_decile <- statar::xtile(acs$share_hisp, cutpoints = cutoffs_h) #creates variable with values 1-10 identifying which decile census tract median income falls into

#finally combine. eventually can count # people evacuated but here will count total population
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
  county = "Alameda"
) %>% dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(
    pop1  = male_under5 + male_5_9 + female_5_9 + female_10_14 , 
    pop2 = male_10_14 + male_15_17 + female_under5 + female_15_17 + male_18_19+ female_18_19,
    pop3  =  male_20 + male_21 + male_22_24 + male_25_29   + female_20 + female_21 + female_22_24 + female_25_29  , 
    pop4 =  male_30_34 + male_35_39 + female_30_34 + female_35_39, 
    pop5 =   male_40_44 + male_45_49 + female_45_49 + female_40_44,
    pop6 =   male_50_54 + male_55_59 + female_50_54 +female_55_59,
    pop7  = male_60_61 + male_62_64 + male_65_66 + male_67_69 + female_60_61 + female_62_64 + female_65_66 + female_67_69,
    pop8  = male_70_74 + female_70_74 + male_75_79 + female_75_79,
    pop9 = male_80_84 + male_over85 + female_80_84 + female_over85,
    pop_total = male_all + female_all
  ) %>% 
  dplyr::select(GEOID, pop_total)

#now that we have pop bring in and add up totals

acs <- left_join(acs, pop)


output <- list()
output[[1]] <-output[[2]] <- output[[3]] <-output[[4]] <- matrix(nrow = 10, ncol= 10)

for(i in 1:10){
  
  for (j in 1:10){
    
    output[[1]][i,j]<-sum(acs$pop_total[acs$income_decile == i & acs$white_decile==j], na.rm = T)
    output[[2]][i,j]<-sum(acs$pop_total[acs$income_decile == i & acs$black_decile==j], na.rm = T)
    output[[3]][i,j]<-sum(acs$pop_total[acs$income_decile == i & acs$asian_decile==j], na.rm = T)
    output[[4]][i,j]<-sum(acs$pop_total[acs$income_decile == i & acs$hispanic_decile==j], na.rm = T)
    
  }
}


par(mfrow = c(1,4))
#white
plot(raster::raster(as.matrix(output[[1]])), axes = F)
axis(1, at = seq(0.05, .95,.1), labels = 1:10)
axis(2, at = seq(0.05, .95,.1), labels = 1:10, las = 2, line=-10)
mtext(side = 3, text = "White",adj = 0)

#black
plot(raster::raster(as.matrix(output[[2]])), axes = F)
axis(1, at = seq(0.05, .95,.1), labels = 1:10)
axis(2, at = seq(0.05, .95,.1), labels = 1:10, las = 2, line=-10)
mtext(side = 3, text = "Black",adj = 0)

#asian
plot(raster::raster(as.matrix(output[[3]])), axes = F)
axis(1, at = seq(0.05, .95,.1), labels = 1:10)
axis(2, at = seq(0.05, .95,.1), labels = 1:10, las = 2, line=-10)
mtext(side = 3, text = "Asian",adj = 0)

#hispanic
plot(raster::raster(as.matrix(output[[4]])), axes = F)
axis(1, at = seq(0.05, .95,.1), labels = 1:10)
axis(2, at = seq(0.05, .95,.1), labels = 1:10, las = 2, line=-10)
mtext(side = 3, text = "Hispanic",adj = 0)

write_rds(output, file = "data/race_by_income_example.rds")
