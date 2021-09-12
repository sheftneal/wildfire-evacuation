source("script/0-packages-and-functions.R")
library(tidycensus)
library(tidyverse)
library(ggpubr)
library(readxl)
#census_api_key("")

county_name <- "Sonoma"
census_year <- 2018


#read in census data



pop <-  get_acs(
  geography = "tract", 
  variables = c(   no_school_completed = "B15003_002",
                   nursery_school="B15003_003",
                   kindergarten ="B15003_004",
                   first_grade="B15003_005",
                   second_grade="B15003_006",
                   third_grade="B15003_007",
                   fourth_grade="B15003_008",
                   fifth_grade="B15003_009",
                   sixth_grade="B15003_010",
                   seventh_grade="B15003_011",
                   eight_grade="B15003_012",
                   ninth_grade="B15003_013",
                   tenth_grade="B15003_014",
                   eleventh_grade="B15003_015",
                   twelfth_grade_no_diploma="B15003_016",
                   high_school_diploma="B15003_017",
                   ged="B15003_018",
                   some_college_less_than_1_year = "B15003_019",
                   some_college_more_than_1_year = "B15003_020",
                   associate_degree="B15003_021",
                   bachelor_degree="B15003_022",
                   master_degree="B15003_023",
                   professional_school_degree = "B15003_024",
                   doctorate_degree="B15003_025",
                   total="B15003_001"
  ), 
  year = census_year,
  state = "CA",
  county = county_name
) 

#now re-organize

pop <- pop %>% dplyr::select(-moe) %>% 
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>% 
  mutate(
    pop1 =  no_school_completed +nursery_school + kindergarten + first_grade + second_grade + third_grade + fourth_grade + fifth_grade + sixth_grade + seventh_grade + eight_grade + ninth_grade + tenth_grade + eleventh_grade + twelfth_grade_no_diploma + high_school_diploma + ged, # Highschool/GED or Less 
    pop2 = some_college_less_than_1_year + some_college_more_than_1_year + associate_degree + bachelor_degree, # Bachelor's Degree or some College 
    pop3 = master_degree, #Master's Degree 
    pop4 = professional_school_degree + doctorate_degree, #Professional Degree/Doctorate 
    pop_total =  total #Total 
  ) %>% 
  dplyr::select(GEOID, starts_with("pop"))

all.equal(rowSums(pop[,paste("pop",1:4, sep = "")]), pop$pop_total)




# income
income <- 
  get_acs(
    geography = "tract", 
    variables =  c(med_income = "B06011_001"), 
    year = census_year,
    state = "CA",
    county = county_name
  ) 
income <- income %>% dplyr::select(-variable, -moe) %>% rename(med_inc = estimate)  



acs <- left_join(income, pop)



################################################################################################################################  



#read in census tract mapping
ct <- read.csv("data/clean/Sonoma Census Tract Evacuation Order Mapping.csv") %>% 
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% 
  mutate(GEOID = as.character(GEOID)) 


#count # evac orders per census tract
ct$n_order <- apply(ct[,paste("evacuation",1:29, sep = "")], 1, function(x){sum(!is.na(x))})

#reorganize so each row is a census tract - evacuation ID combination
ct <- ct %>% 
  dplyr::filter(n_order > 0) %>%  #filter to census tracts with orders
  pivot_longer(
    cols = evacuation1:evacuation29,
    names_to = "Evacuation.Index",
    values_to = "Evacuation.ID"
  ) %>% 
  dplyr::select(-Evacuation.Index) %>% 
  dplyr::filter(!is.na(Evacuation.ID))



#bring in and clean up evacuation order database
evacs <- read.csv("data/clean/Wildfire Evacuation Order Database - Sonoma County Fires.csv") %>% 
  dplyr::select(-starts_with("X")) %>% 
  dplyr::select(Evacuation.ID, FIRE_NAME, GIS_ACRES,evacuation.order.type,evac_date_issue) %>% 
  dplyr::filter(!is.na(Evacuation.ID)) #drop rows with no evacuations


#join data sets

data <- left_join(ct, evacs)
nrow(data) #342 census tract-evacuation order combinations

#what is date range of evacuations?
data$evac_date_issue <- as.Date(data$evac_date_issue, format = "%m/%d/%y")


#what is date range of evacuations?
min(data$evac_date_issue, na.rm = T)
max(data$evac_date_issue, na.rm = T)

#create year and month variables
data <- data %>% 
  mutate(
    evac_year = lubridate::year(evac_date_issue),
    evac_month = lubridate::month(evac_date_issue)
  )



#how many census tracts did each fire cause evacuation for?
data %>% group_by(FIRE_NAME) %>% summarise(n_fire = n())
#kincade fire had by far most evacuations




############################################################################################################

#prep data for figures:

#[1] x-axis year, y-axis #total population evacuated by age group
acs$GEOID <- substr(acs$GEOID, 2, 1000) #drop leading 0

annual_evacs_bypop <- data %>% mutate(evac_year = replace(evac_year, FIRE_NAME=="LNU Lightning Complex", 2020)) %>% 
  group_by(GEOID,evac_year) %>% 
  distinct() %>%  #same census tract may have been evacuated multiple times per year but we'll just count once per year
  left_join(acs) %>% 
  group_by(evac_year) %>% 
  summarise_at(vars(pop1:pop_total), ~sum(.x, na.rm = T)) 


#Assuming all fires in county were checked back to 2012 and 2012-2015 there were no evacuations:
annual_evacs_bypop <- data.frame(evac_year = 2012:2021) %>% left_join(annual_evacs_bypop)
annual_evacs_bypop[is.na(annual_evacs_bypop)] <- 0



#evacuated at any point age distribution
ct_norder <- read.csv("data/clean/Sonoma Census Tract Evacuation Order Mapping.csv") %>% 
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% 
  mutate(GEOID = as.character(GEOID))


###############################################
#Trying to make Census Graphs for #Evacuation Orders 
#count # evac orders per census tract
ct_norder$n_order <- apply(ct_norder[,paste("evacuation",1:29, sep = "")], 1, function(x){sum(!is.na(x))})
ct_norder <- ct_norder %>% dplyr::filter(n_order > 0) %>% dplyr::select(GEOID, n_order)

ct_acs <- left_join(acs, ct_norder) %>% mutate(n_order = replace(n_order, is.na(n_order), 0))

ggplot(ct_acs$GEOID, ct_acs$n_order)
#png(filename = "evac.png", width = 800, height = 400) 

sf <- get_acs(geography = "tract", 
              state = "CA", 
              county = "Sonoma", 
              geometry = TRUE)

ggplot(sf, aes(fill = ct_acs$n_order)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::label_number(), option = 'magma') +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#dev.off()
#######################################################################################


#county population
county_demo <- ct_acs %>% 
  summarise_at(vars(pop1:pop_total), ~sum(.x, na.rm = T)) %>% 
  mutate(
    share_pop1 = pop1/pop_total,
    share_pop2 = pop2/pop_total,
    share_pop3 = pop3/pop_total,
    share_pop4 = pop4/pop_total,
    
  )



#population by age and whether they were evacuated at any time during our sample
evacuated_demo <- ct_acs %>% 
  mutate(evacuated = as.numeric(n_order > 0)) %>% 
  group_by(evacuated) %>% 
  summarise_at(vars(pop1:pop_total), ~sum(.x, na.rm = T))  %>% 
  mutate(pop_total = pop1 + pop2 + pop3 + pop4,
         share_pop1 = pop1/pop_total,
         share_pop2 = pop2/pop_total,
         share_pop3 = pop3/pop_total,
         share_pop4 = pop4/pop_total
         
  )


#Pie Chart 
#png(filename = "Pie.png", width = 800, height = 400) 
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie_evacuated <- data.frame(Education = c("Highschool Diploma/GED or less", "Bachelor's Degree or Some College", "Master's Degree", "Professional School or Doctorate Degree"), 
                            share = unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_pop"))) #takes evacuated
) %>% 
  ggplot( aes(x = "", y = share, fill = Education)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  scale_fill_manual(values=c("darkred")) + #here you can change colors
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = cumsum(share) + share ,label =paste(round(100*share), "%",sep="") ), size = 5)


pieData_evacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_pop"))))
pieData_NOTevacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 0) %>% dplyr::select(starts_with("share_pop"))))
pieData_county <- county_demo %>% dplyr::select(starts_with("share_")) #if you want to compare to overall county pop regardless of evacuation status

pie(pieData_evacuated, 
    # col = rainbow(1), 
    col= c("#7BCCC4" ,"#4EB3D3", "#2B8CBE", "#08589E"), 
   # col = "darkred", <-- If you want a solid color 
    labels = paste(c("Highschool Diploma/GED or less", "Bachelor's Degree or Some College", "Master's Degree", "Professional School or Doctorate Degree"), " (",round(pieData_evacuated*100),"%)",sep="")
)
mtext(side = 3, text = "Education Distribution of Evacuated Populations",adj = 0, cex = 3)    
#dev.off()
