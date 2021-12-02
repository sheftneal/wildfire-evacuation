#checked 12/1

source("script/0-packages-and-functions.R")
library(tidycensus)

census_api_key("f5c6e020a6aae7ba5420a4051ad53eee38c621fa")

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

#re-organize
pop <- pop %>% dplyr::select(-moe) %>% 
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
  dplyr::select(GEOID, starts_with("pop"))

all.equal(rowSums(pop[,paste("pop",1:9, sep = "")]), pop$pop_total)

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
  summarise_at(vars(pop1:pop_total),#pop1 is the leftmost column we want to sum, pop_total is the rightmost. will also tak all cols between these two. 
               ~sum(.x, na.rm = T)) #sum(.x) is summarise_at() syntax telling it to take all the variables (ie pop1 is initial x value, then pop2, then... all the way to pop_total being the final x)

annual_evacs_bypop <- data.frame(evac_year = 2012:2020) %>% left_join(annual_evacs_bypop)
annual_evacs_bypop[is.na(annual_evacs_bypop)] <- 0

#line plot - we discussed total population only:
plot(annual_evacs_bypop$evac_year, annual_evacs_bypop$pop_total, col =NA, axes = F, xlab = "", ylab = "", xlim = c(2012, 2020))
lines(annual_evacs_bypop$evac_year, annual_evacs_bypop$pop_total, col = 'black', lwd = 2)
points(annual_evacs_bypop$evac_year, annual_evacs_bypop$pop_total, pch = 16, cex = 2)
axis(1, tick = T, at = 2012:2020)
axis(2, tick = T, las = 2, at = seq(0, 120000, 20000), labels = c(0, paste(seq(20,120, 20), ",000", sep = "")))
mtext(side = 3, text = "Number of People Living in Evacuated Census Tracts in Fresno County vs Time", adj = 0, cex =2,line =1 )
