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


################################################################################################################################  

# Read in census tract mapping
ct <- read.csv("data/clean/Fresno_Census_FilledIn.csv", skip = 2) %>% 
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% 
  mutate(GEOID = as.character(GEOID))


# Count num evac orders per census tract
ct$n_order <- apply(ct[,paste("evacuation",1:12, sep = "")], 1, function(x){sum(!is.na(x))})

# Reorganize so each row is a census tract - evacuation ID combination
ct <- ct %>% 
  dplyr::filter(n_order > 0) %>%  #filter to census tracts with orders
  pivot_longer(
    cols = evacuation1:evacuation14,
    names_to = "Evacuation.Index",
    values_to = "Evacuation.ID"
  ) %>% 
  dplyr::select(-Evacuation.Index) %>% 
  dplyr::filter(!is.na(Evacuation.ID))


# Bring in and clean up evacuation order database
evacs <- read.csv("data/clean/Wildfire Evacuation Order Database - Fresno & Madera County Fires.csv") %>% 
  dplyr::select(-starts_with("X")) %>% 
  dplyr::select(Evacuation.ID, FIRE_NAME, GIS_ACRES,evacuation.order.type,evac_date_issue)

# Join data sets
data <- left_join(ct, evacs)
nrow(data) #47 census tract-evacuation order combinations

# What is date range of evacuations?
data$evac_date_issue <- as.Date(data$evac_date_issue, format = "%m/%d/%y")
min(data$evac_date_issue)
max(data$evac_date_issue)

# Create year and month variables
data <- data %>% 
  mutate(
    evac_year = lubridate::year(evac_date_issue),
    evac_month = lubridate::month(evac_date_issue)
  )

# How many census tracts did each fire cause evacuation for?
data %>% group_by(FIRE_NAME) %>% summarise(n_fire = n())
# --> 6 fires causes evacuations with CREEK fire causing by far the most

############################################################################################################

# PREP DATA FOR FIGURES

#[1] x-axis year, y-axis #total population evacuated by age group

#GEOID is the census tract identifier but in the ACS data it has a leading 0 before the id which doesn't match our data set so drop leading 0
#drop leading 0 see ?substring() for details but we're taking 2nd character in GEOID through 1000th character. 
#Obvis not 1000 characters but wanted to make sure didn't drop any characters on accident (eg if 11 characters and you do substr(x, 2,9) then will be missing 10th and 11th character
# if you put number greater than longest string length it just takes everything so i put 1000
acs$GEOID <- substr(acs$GEOID, 2, 1000)


#to get annual info for each census tract by year combo
annual_evacs_bypop <- data %>% 
  group_by(GEOID,evac_year) %>% 
  distinct() %>%  #same census tract may have been evacuated multiple times per year but we'll just count once per year. dinstinc() takes every combo of GEOID, year combo once.
  left_join(acs) %>% #bring in ACS data (will merge on GEOID)
  group_by(evac_year) %>% #now for each year summarise totals
  summarise_at(vars(med_inc:pop_total),#pop1 is the leftmost column we want to sum, pop_total is the rightmost. will also tak all cols between these two. 
               ~sum(.x, na.rm = T)) #sum(.x) is summarise_at() syntax telling it to take all the variables (ie pop1 is initial x value, then pop2, then... all the way to pop_total being the final x)

#now we have data frame where rows are years and cols are different pop group totals

#Assuming all fires in county were checked back to 2012 and 2012-2015 there were no evacuations:

#creates empty data frame with all years in first column and merges with evac data
# then replaces NA with 0. Before our evac data only had observations for years with evacs. 
#Now we have a data frame with obs for every year regardless of whether there was evac
annual_evacs_bypop <- data.frame(evac_year = 2012:2021) %>% left_join(annual_evacs_bypop)
annual_evacs_bypop[is.na(annual_evacs_bypop)] <- 0

#evacuated at any point age distribution

#read in data
ct_norder <- read.csv("data/clean/Fresno_Census_FilledIn.csv", skip = 2) %>% #skip = 2 because first two rows of excel data file are not data we want to read in
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% #keep the variables GEOID, NAME, and anything that begins with "evacuation"
  mutate(GEOID = as.character(GEOID)) #convert GEOID from number to character since other data sets we're using store this var as character. all need to be same format for merges.

#count # evac orders per census tract

#define a new variable called "n_order" that takes the sum across rows for the 14 evacuation variable
ct_norder$n_order <- apply(ct_norder[,paste("evacuation",1:14, sep = "")], 1, function(x){sum(!is.na(x))}) #1:14 because data frame I'm using has 14 evacuation variables
#you could get the same thing as line 217 in many different ways. For example:
number_orders <- ct_norder %>% dplyr::select(starts_with("evacuation")) %>% rowSums(na.rm = T)
ct_norder <- ct_norder %>% mutate(n_order = number_orders)

#only keep census tracts with orders
ct_norder <- ct_norder %>% dplyr::filter(n_order > 0) %>% dplyr::select(GEOID, n_order)

#combine acs data with the count of evac orders by census tract
#but the n_order variable will be missing for every census tract that never had a fire
# so those values are currently missing after the join but we want all of those values to be 0
ct_acs <- left_join(acs, ct_norder) %>% mutate(evac = replace(n_order, is.na(n_order), 0))

acs_byinc <- ct_acs  %>% group_by(income_decile, evac) %>% summarise_at(vars(race_tot:race_mult), sum, na.rm = T)

# not evacuated
output0 <- acs_byinc %>% ungroup() %>% 
  drop_na(income_decile) %>% 
  filter(evac == 0) %>% 
  dplyr::select(race_nh_wh, race_h, race_nh_bk, race_nh_as, race_mult) %>% 
  as.matrix()

# evacuated
output1 <- acs_byinc %>% ungroup() %>% 
  drop_na(income_decile) %>% 
  filter(evac > 0) %>% 
  dplyr::select(race_nh_wh, race_h, race_nh_bk, race_nh_as, race_mult) %>% 
  as.matrix()



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


# race/income matrix plots

par(mfrow =c (1,2))
plot(raster::raster(as.matrix(output0)), col = inferno(256)[240:80],axes = F) # col = rainbow(256)[100:256]
axis(1, at = seq(.1, .9,.2), labels = c("White","Hispanic","Black","Asian","Multiple"))
#axis(2, at = seq(0.05, .95,.1), labels = 10:1, las = 2)
axis(2, at = seq(0.1, .9,.2), labels = 5:1, las = 2)
mtext(side = 1, text = "Race/Ethnicity",cex=1.5,line=3)
mtext(side = 2, text = "Income Quintile",cex=1.5,line=3)
mtext(side = 3, text ="Evacuated", adj =0, cex=2)
