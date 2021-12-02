#fixed 12/1

source("script/0-packages-and-functions.R")
library(tidycensus)

census_api_key("f5c6e020a6aae7ba5420a4051ad53eee38c621fa")

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
  county = "Sonoma"
) 


#re-organize:
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
    
    perc_proficient = 100 * proficient / (proficient + not_proficient),
    pop1_total = proficient + not_proficient
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
    
    perc_only_proficient_spanish = 100 * only_proficient_spanish / total,
    pop2_total = proficient_english + only_proficient_spanish + only_proficient_not_english_or_spanish
    
  )


# Income -- need this in order to make the prep data for figures part work.
income <- 
  get_acs(
    geography = "tract", 
    variables =  c(med_income = "B06011_001"), 
    year = 2018,
    state = "CA",
    county = "Sonoma"
  ) 
income <- income %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate)  
acs <- left_join(income, pop1)
#acs <- left_join(income, pop2)


############################################################################################################

# Read in census tract mapping
ct <- read.csv("data/clean/Sonoma Census Tract Evacuation Order Mapping.csv") %>% 
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% 
  mutate(GEOID = as.character(GEOID))


# Count num evac orders per census tract
ct$n_order <- apply(ct[,paste("evacuation",1:29, sep = "")], 1, function(x){sum(!is.na(x))})

# Reorganize so each row is a census tract - evacuation ID combination
ct <- ct %>% 
  dplyr::filter(n_order > 0) %>%  #filter to census tracts with orders
  pivot_longer(
    cols = evacuation1:evacuation29,
    names_to = "Evacuation.Index",
    values_to = "Evacuation.ID"
  ) %>% 
  dplyr::select(-Evacuation.Index) %>% 
  dplyr::filter(!is.na(Evacuation.ID))


# Bring in and clean up evacuation order database
evacs <- read.csv("data/clean/Wildfire Evacuation Order Database - Sonoma County Fires.csv") %>% 
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
# --> 7 fires causes evacuations with CREEK fire causing by far the most

################################################################################################################################  

# Prep data for figures:

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
  summarise_at(vars(proficient:pop1_total),#pop1 is the leftmost column we want to sum, pop_total is the rightmost. will also tak all cols between these two. 
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
ct_norder <- read.csv("data/clean/Sonoma Census Tract Evacuation Order Mapping.csv") %>% #skip = 2 because first two rows of excel data file are not data we want to read in
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% #keep the variables GEOID, NAME, and anything that begins with "evacuation"
  mutate(GEOID = as.character(GEOID)) #convert GEOID from number to character since other data sets we're using store this var as character. all need to be same format for merges.


#count # evac orders per census tract

#define a new variable called "n_order" that takes the sum across rows for the 14 evacuation variable
ct_norder$n_order <- apply(ct_norder[,paste("evacuation",1:29, sep = "")], 1, function(x){sum(!is.na(x))}) #1:14 because data frame I'm using has 14 evacuation variables
#you could get the same thing as line 217 in many different ways. For example:
number_orders <- ct_norder %>% dplyr::select(starts_with("evacuation")) %>% rowSums(na.rm = T)
ct_norder <- ct_norder %>% mutate(n_order = number_orders)

#only keep census tracts with orders
ct_norder <- ct_norder %>% dplyr::filter(n_order > 0) %>% dplyr::select(GEOID, n_order)

#combine acs data with the count of evac orders by census tract
#but the n_order variable will be missing for every census tract that never had a fire
# so those values are currently missing after the join but we want all of those values to be 0
ct_acs <- left_join(acs, ct_norder) %>% mutate(n_order = replace(n_order, is.na(n_order), 0))

#no take this data frame and summarise across all the pop variables.
#pop1:pop_total means take every column including and to the right of pop1 through the column pop_total and sum them 
#then we calculate pop shares
county_demo <- ct_acs %>% 
  summarise_at(vars(proficient:pop1_total), ~sum(.x, na.rm = T)) %>% 
  mutate(
    share_proficient = proficient/pop1_total,
    share_not_proficient = not_proficient/pop1_total
  )

#population by age and whether they were evacuated at any time during our sample
evacuated_demo <- ct_acs %>% 
  mutate(evacuated = as.numeric(n_order > 0)) %>% #create binary dummy var == 1 if there were evac orders in tract, 0 else
  group_by(evacuated) %>% 
  summarise_at(vars(proficient:pop1_total), ~sum(.x, na.rm = T))  %>% #separate pop totals for groups that were and were not evac
  mutate(pop1_total = proficient + not_proficient, #calculations from those pop totals
         share_proficient = proficient/pop1_total,
         share_not_proficient = not_proficient/pop1_total
  )



############################################################################################################
# Plots for English Proficiency Sonoma County Maps:
#Run Line Below!

png(filename = "English.png", width=800, height=400)
# graph the percentage of each census tract that is not proficient in English on a map of Sonoma

pop1 <- pop1 %>% rename(NAME1=NAME)
Sonoma <- get_acs(geography = "tract", state = "06", county = "Sonoma", geometry = TRUE, variables = "B01002_001") %>% 
          left_join(pop1)

Sonoma %>%
#  ggplot(aes(fill = 100 - pop1$perc_proficient)) +  #can't use "pop1" to fill colors and "Sonoma" to plot becuase census tracts in different orders in two data objects
  ggplot(aes(fill = 100 - perc_proficient)) +  #can't use "pop1" to fill colors and "Sonoma" to plot becuase census tracts in different orders in two data objects
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma")
dev.off()




pop2 <- pop2 %>% rename(NAME1=NAME)
Sonoma <- get_acs(geography = "tract", state = "06", county = "Sonoma", geometry = TRUE, variables = "B01002_001") %>% 
  left_join(pop2)

png(filename = "Spanish.png", width=800, height=400)
# graph the percentage of each census tract that is only proficient in Spanish on a map of Sonoma
Sonoma %>%
#  ggplot(aes(fill = pop2$perc_only_proficient_spanish)) + 
  ggplot(aes(fill = perc_only_proficient_spanish)) + #plot from column in Sonoma after joinining
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma")
dev.off()

# percentage of the not proficient population that is only proficient in Spanish
mean(pop2$only_proficient_spanish) / (mean(pop2$only_proficient_spanish) + mean(pop2$only_proficient_not_english_or_spanish))


# GET PIE CHART DATA READY
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie_evacuated <- data.frame(Language = c("Proficient in English","Not Proficient in English"), 
                            share = unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_"))) #takes evacuated
) %>% 
  ggplot( aes(x = "", y = share, fill = Age)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  scale_fill_manual(values=c(rainbow(2))) + #here you can change colors
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = cumsum(share) + share ,label =paste(round(100*share), "%",sep="") ), size = 5)

pieData_evacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_"))))
pieData_NOTevacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 0) %>% dplyr::select(starts_with("share_"))))
pieData_county <- county_demo %>% dplyr::select(starts_with("share_")) #if you want to compare to overall county pop regardless of evacuation status

#These Pie Charts are for the English Proficiency of Sonoma County and the Evacuated Population 
#Chart #1 (English Proficiency of Sonoma County)
#There is a NA value in "pop1$perc_proficient". Run the below line to get rid of it
pop1$perc_proficient[100] <-0
slices <- c(mean(pop1$perc_proficient) / 100, 1 - mean(pop1$perc_proficient) / 100) #proficiency of Sonoma county
lbls = paste(c("Proficient English", "Not Proficient English"), " (",round(pieData_county*100),"%)",sep="")
pie(slices, labels = lbls, main="Pie Chart of English Proficiency for Evacuated Populations", col = c("white", "grey"))
#Chart #2 (English Proficiency of Evacuated Population of Sonoma County)
slices <- c(pieData_evacuated[1], pieData_evacuated[2]) #proficiency of Sonoma county evacuated population
lbls = paste(c("Proficient English", "Not Proficient English"), " (",round(pieData_evacuated*100),"%)",sep="")
pie(slices, labels = lbls, main="Pie Chart of English Proficiency for Evacuated Populations", col = c("white", "grey"))

# get a statistic on how many non-proficient people have been evacuated since 2013. - 120,548 for Sonoma County--> Too high?
num_non_proficient_evacuees_since_2013 = annual_evacs_bypop[1,3] + annual_evacs_bypop[2,3] + annual_evacs_bypop[3,3] + annual_evacs_bypop[4,3] + annual_evacs_bypop[5,3]+ annual_evacs_bypop[6,3] + annual_evacs_bypop[7,3] + annual_evacs_bypop[8,3] + annual_evacs_bypop[9,3] + annual_evacs_bypop[10,3]
