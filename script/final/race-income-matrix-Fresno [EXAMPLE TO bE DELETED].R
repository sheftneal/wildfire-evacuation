source("script/0-packages-and-functions.R")
library(tidycensus)
library(viridis)

census_api_key("f5c6e020a6aae7ba5420a4051ad53eee38c621fa")
COUNTY = "Fresno"

#read in census data
pop <-  get_acs(
  geography = "tract", 
  variables = c(  
    race_tot = "B03002_001",
    race_nh_wh = "B03002_003",
    race_nh_bk = "B03002_004",
    race_nh_na = "B03002_005",
    race_nh_as = "B03002_006",
    race_nh_hw = "B03002_007",
    race_nh_ot = "B03002_008",
    race_nh_2ot1 = "B03002_009", #two or more nh
    race_nh_2ot2 = "B03002_010", #two or more including other
    race_nh_3ot = "B03002_011", #two races excluding other or 3 and more
    race_h = "B03002_012"
  ), 
  year = 2018,
  state = "CA",
  county = COUNTY
)



#creates categories for share of population by each race (white, black, asian, hawaiian, native, 2+) by ethnicity (hisp, non-hisp) combo 
race <- pop %>% dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(race_mult = race_nh_2ot1 +race_nh_2ot2 +race_nh_3ot )



# income
income <- 
  get_acs(
    geography = "tract", 
    variables =  c(med_income = "B06011_001"), 
    year = 2018,
    state = "CA",
    county = COUNTY
  ) 
income <- income %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate)  

acs <- left_join(income, race) 



#can create income deciles (maybe want to do this at state-level? otherwise will be only comparing income within county)
income_ca <- 
  get_acs(
    geography = "tract", 
    variables =  c(med_income = "B06011_001"), 
    year = 2018,
    state = "CA"
  ) 

#finds the 10th, 20th, etc. through 90th percentile of median income across all CA census tracts
#med_income_deciles <- income_ca %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate) %>% dplyr::select(med_inc) %>% quantile(seq(0.1,0.9,.1), na.rm = T)  
med_income_deciles <- income_ca %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate) %>% dplyr::select(med_inc) %>% quantile(seq(0.2,0.8,.2), na.rm = T)  

#uses the cutoffs determined above to split the census tracts in this county into whichever deciles they fall for statewide distribution
acs$income_decile <- statar::xtile(acs$med_inc, cutpoints = med_income_deciles) #creates variable with values 1-10 identifying which decile census tract median income falls into


#now decile the race variables:
#for now will do non-hispanic white, non-hispanic asian, non-hispanic black, non-hispanic, hispanic 

#determine statewide deciles
pop <-  get_acs(
  geography = "tract", 
  variables = c(  
    race_tot = "B03002_001",
    race_nh_wh = "B03002_003",
    race_nh_bk = "B03002_004",
    race_nh_na = "B03002_005",
    race_nh_as = "B03002_006",
    race_nh_hw = "B03002_007",
    race_nh_ot = "B03002_008",
    race_nh_2ot1 = "B03002_009", #two or more nh
    race_nh_2ot2 = "B03002_010", #two or more including other
    race_nh_3ot = "B03002_011", #two races excluding other or 3 and more
    race_h = "B03002_012"
  ), 
  year = 2018,
  state = "CA"
)%>% 
  dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  )%>% 
  dplyr::select(GEOID, starts_with("race"))


# cutoffs_nh_wh <-  quantile(pop$share_nh_wh, seq(0.1,0.9,.1), na.rm = T)  
# cutoffs_nh_bk <-  quantile(pop$share_nh_bk, seq(0.1,0.9,.1), na.rm = T)  
# cutoffs_nh_as <-  quantile(pop$share_nh_as, seq(0.1,0.9,.1), na.rm = T)  
# cutoffs_h <-  quantile(pop$share_h, seq(0.1,0.9,.1), na.rm = T)  


# acs$white_decile <- statar::xtile(acs$share_nh_wh, cutpoints = cutoffs_nh_wh) #creates variable with values 1-10 identifying which decile census tract median income falls into
# acs$black_decile <- statar::xtile(acs$share_nh_bk, cutpoints = cutoffs_nh_bk) #creates variable with values 1-10 identifying which decile census tract median income falls into
# acs$asian_decile <- statar::xtile(acs$share_nh_as, cutpoints = cutoffs_nh_as) #creates variable with values 1-10 identifying which decile census tract median income falls into
# acs$hispanic_decile <- statar::xtile(acs$share_hisp, cutpoints = cutoffs_h) #creates variable with values 1-10 identifying which decile census tract median income falls into

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
  county = COUNTY
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


#acs <- left_join(acs, evac) #bring in evacuation order
acs$evac <- round(runif(n = nrow(acs), min = 0, max = 1)) #for now random number for evacuation assignment
acs_byinc <- acs  %>% group_by(income_decile, evac) %>% summarise_at(vars(race_tot:race_mult), sum, na.rm = T)

# evacuated
output0 <- acs_byinc %>% ungroup() %>% 
  drop_na(income_decile) %>% 
  filter(evac == 0) %>% 
  dplyr::select(race_nh_wh, race_h, race_nh_bk, race_nh_as, race_mult) %>% 
  as.matrix()

# not evacuated
output1 <- acs_byinc %>% ungroup() %>% 
  drop_na(income_decile) %>% 
  filter(evac == 1) %>% 
  dplyr::select(race_nh_wh, race_h, race_nh_bk, race_nh_as, race_mult) %>% 
  as.matrix()




# race/income matrix plots

par(mfrow =c (1,2))
plot(raster::raster(as.matrix(output0)), col = inferno(256)[240:80],axes = F) # col = rainbow(256)[100:256]
axis(1, at = seq(.1, .9,.2), labels = c("White","Hispanic","Black","Asian","Multiple"))
#axis(2, at = seq(0.05, .95,.1), labels = 10:1, las = 2)
axis(2, at = seq(0.1, .9,.2), labels = 5:1, las = 2)
mtext(side = 1, text = "Race/Ethnicity",cex=1.5,line=3)
mtext(side = 2, text = "Income Quintile",cex=1.5,line=3)
mtext(side = 3, text ="Evacuated", adj =0, cex=2)

plot(raster::raster(as.matrix(output1)), col = inferno(256)[240:80],axes = F)
axis(1, at = seq(.1, .9,.2), labels = c("White","Hispanic","Black","Asian","Multiple"))
#axis(2, at = seq(0.05, .95,.1), labels = 10:1, las = 2)
axis(2, at = seq(0.1, .9,.2), labels = 5:1, las = 2)
mtext(side = 1, text = "Race/Ethnicity",cex=1.5,line=3)
mtext(side = 3, text ="Not Evacuated", adj =0, cex=2)



# pie chart of county racial distribution
race_dist <- as.data.frame(output0 + output1) # county pop
race_dist <- as.data.frame(output0) # <-- this would be to get the pie chart for the evacuated pop
white <- sum(race_dist$race_nh_wh)
hispanic <- sum(race_dist$race_h)
black <- sum(race_dist$race_nh_bk)
asian <- sum(race_dist$race_nh_as)
multi <- sum(race_dist$race_mult)
total <- white + hispanic + black + asian + multi
slices <- c(white / total, hispanic / total, black / total, asian / total, multi / total)
lbls = paste(c("White", "Hispanic", "Black", "Asian", "Multiracial"), " (", round(slices*100), "%)", sep="") 
pie(slices, labels = lbls, main="Racial Breakdown of Fresno County", col = c("grey20", "grey40", "grey60", "grey80", "grey100"))

# pie chart of county income distribution
inc_dist <- as.data.frame(output0 + output1) # total county
#inc_dist <- as.data.frame(output0) # just the evacuated population
quint1 <- sum(inc_dist[1,])
quint2 <- sum(inc_dist[2,])
quint3 <- sum(inc_dist[3,])
quint4 <- sum(inc_dist[4,])
quint5 <- sum(inc_dist[5,])
total <- quint1 + quint2 + quint3 + quint4 + quint5
slices <- c(quint1 / total, quint2 / total, quint3 / total, quint4 / total, quint5 / total)
lbls = paste(c("1st quintile", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile"), " (", round(slices*100), "%)", sep="") 
pie(slices, labels = lbls, main="Income Breakdown of Fresno County", col = c("grey20", "grey40", "grey60", "grey80", "grey100"))



