source("script/0-packages-and-functions.R")
library(tidycensus)

census_api_key("f5c6e020a6aae7ba5420a4051ad53eee38c621fa")

#read in census data
pop <-  get_acs(
  geography = "tract", 
  variables = c(  a1_9999 = "B06010_004",
                  a10000_14999 = "B06010_005",
                  a15000_24999 = "B06010_006",
                  a25000_34999 = "B06010_007",
                  a35000_49999 = "B06010_008",
                  a50000_64999 = "B06010_009",
                  a65000_74999 = "B06010_010",
                  a75000_ormore = "B06010_011"
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
    pop1  = a1_9999, 
    pop2 = a10000_14999,
    pop3 = a15000_24999, 
    pop4 = a25000_34999, 
    pop5 = a35000_49999,
    pop6 = a50000_64999,
    pop7 = a65000_74999,
    pop8  = a75000_ormore,
    pop_total = a1_9999 + a10000_14999 + a15000_24999 + a25000_34999 + a35000_49999 + a50000_64999 + a65000_74999 + a75000_ormore
  ) %>% 
  dplyr::select(GEOID, starts_with("pop"))

all.equal(rowSums(pop[,paste("pop",1:8, sep = "")]), pop$pop_total)


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
#7 fires causes evacuations with CREEK fire causing by far the most






############################################################################################################

#prep data for figures:

#[1] x-axis year, y-axis #total population evacuated by income group
acs$GEOID <- substr(acs$GEOID, 2, 1000) #drop leading 0

annual_evacs_bypop <- data %>% 
  group_by(GEOID,evac_year) %>% 
  distinct() %>%  #same census tract may have been evacuated multiple times per year but we'll just count once per year
  left_join(acs) %>% 
  group_by(evac_year) %>% 
  summarise_at(vars(pop1:pop_total), ~sum(.x, na.rm = T))


#Assuming all fires in county were checked back to 2013 and 2013-2015 there were no evacuations:

annual_evacs_bypop <- data.frame(evac_year = 2013:2020) %>% left_join(annual_evacs_bypop)
annual_evacs_bypop[is.na(annual_evacs_bypop)] <- 0



#evacuated at any point income distribution
ct_norder <- read.csv("data/clean/Fresno_Census_FilledIn.csv", skip = 2) %>% 
  dplyr::select(GEOID, NAME, starts_with("evacuation")) %>% 
  mutate(GEOID = as.character(GEOID))


#count # evac orders per census tract
ct_norder$n_order <- apply(ct_norder[,paste("evacuation",1:12, sep = "")], 1, function(x){sum(!is.na(x))})
ct_norder <- ct_norder %>% dplyr::filter(n_order > 0) %>% dplyr::select(GEOID, n_order)

ct_acs <- left_join(acs, ct_norder) %>% mutate(n_order = replace(n_order, is.na(n_order), 0))

#county population
county_demo <- ct_acs %>% 
  summarise_at(vars(pop1:pop_total), ~sum(.x, na.rm = T)) %>% 
  mutate(
    share_pop1 = pop1/pop_total,
    share_pop2 = pop2/pop_total,
    share_pop3 = pop3/pop_total,
    share_pop4 = pop4/pop_total,
    share_pop5 = pop5/pop_total,
    share_pop6 = pop6/pop_total,
    share_pop7 = pop7/pop_total,
    share_pop8 = pop8/pop_total
    )


ct_acs[164, 14] = 12
ct_acs[165, 14] = 2
ct_acs[166, 14] = 12
ct_acs[167, 14] = 4
ct_acs[190, 14] = 7

#population by income and whether they were evacuated at any time during our sample
evacuated_demo <- ct_acs %>% 
  mutate(evacuated = as.numeric(n_order > 0)) %>% 
  group_by(evacuated) %>% 
  summarise_at(vars(pop1:pop_total), ~sum(.x, na.rm = T))  %>% 
  mutate(pop_total = pop1 + pop2 + pop3 + pop4 + pop5 + pop6 + pop7 + pop8,
         share_pop1 = pop1/pop_total,
         share_pop2 = pop2/pop_total,
         share_pop3 = pop3/pop_total,
         share_pop4 = pop4/pop_total,
         share_pop5 = pop5/pop_total,
         share_pop6 = pop6/pop_total,
         share_pop7 = pop7/pop_total,
         share_pop8 = pop8/pop_total
         )


#a potentially interesting statistic:
#evacuated_demo %>% mutate(share_pop_over60 = (pop7 + pop8 + pop9)/pop_total) %>% dplyr::select(evacuated, share_pop_over60)
#people over 60 make up 16% of county population but nearly 29% of evacuated population  


#save(county_demo, evacuated_demo, annual_evac_by_age, file = "data/clean/FresnoEvacuationData.RData")


################## plots ###################

# AVERAGE INCOME MAP OF COUNTY SIDE BY SIDE PLOTS
fresno_income <- get_acs(state = "06", county = "019", geography = "tract", 
                         variables = "B06011_001", geometry = TRUE)
# plotting income across fresno county
fresno_income %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis")

#pie charts of income distribution. for this we use the share variables in evacuated_demo
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie_evacuated <- data.frame(Inc = c("<10","10-15","15-25","25-35","35-50","50-65","65-75","75+"), 
                            share = unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_pop"))) #takes evacuated
) %>% 
  ggplot( aes(x = "", y = share, fill = Inc)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  scale_fill_manual(values=c(rainbow(8))) + #here you can change colors
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = cumsum(share) + share ,label =paste(round(100*share), "%",sep="") ), size = 5)


pieData_evacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_pop"))))
pieData_NOTevacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 0) %>% dplyr::select(starts_with("share_pop"))))
pieData_county <- as.numeric(county_demo %>% dplyr::select(starts_with("share_"))) #if you want to compare to overall county pop regardless of evacuation status

#or side by side [NOTE: I think this may be the most clear way to show differences]
#add function to define transparency of colors
#add transparency to any color	
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}	

par(mfrow = c(1,1))
barplot(rbind(pieData_evacuated,pieData_county), beside = T ,axes = F, names = c("<10","10-15","15-25","25-35","35-50","50-65","65-75","75+"), col = add.alpha(rep('black',2), c(1, .25)), ylim = c(0, .2))
mtext(side = 1, text = "Income Range (in thousands of dollars)",line=3,cex=1.5)
axis(2, tick = T, las = 2, at = seq(0,.2, .05), labels = paste(seq(0,20,5), "%", sep=""))
mtext(side = 2, "Share of Group Population", line=3,cex=1.5)

# pie(pieData_evacuated, 
#     col = rainbow(8), 
#     labels = paste(c("<10","10-15","15-25","25-35","35-50","50-65","65-75","75+"), " (",round(input*100),"%)",sep="")
# )
# mtext(side = 3, text = "Inc distribution of evacuated populations",adj = 0, cex = 3)    
# 
# 
# 
# #last option is a doughnut plot  
# source("script/doughnut.R") #user written function
# 
# par(mfrow = c(1,2))
# 
# doughnut(pieData_evacuated, 
#          inner.radius= 0.5,
#          outer.radius = 1, 
#          col= rainbow(8),lty =1, density = NA,
#          labels = c("<10","10-15","15-25","25-35","35-50","50-65","65-75","75+"),cex.label = 1.5
# )
# mtext(side = 3, text = "Fresno County Population Evacuated",cex=1.5, adj = 0)
# 
# 
# 
# doughnut(pieData_NOTevacuated, 
#          inner.radius= 0.5,
#          outer.radius = 1, 
#          col= rainbow(8) ,lty =1, density = NA,
#          labels = c("<10","10-15","15-25","25-35","35-50","50-65","65-75","75+"),cex.label = 1.5
# )
# 
# mtext(side = 3, text = "Fresno County Population Not Evacuated",cex=1.5, adj = 0)
# 
# 
# 
# 
# #or can do it with barplot of the income disribution (we discussed not including but i added here just in case)
# 
# par(mfrow = c(1,2))
# 
# barplot(pieData_evacuated,axes = F, names = c("<10","10-15","15-25","25-35","35-50","50-65","65-75","75+"), col =rainbow(8), ylim = c(0, .25))
# mtext(side = 1, text = "Income Group",line=3,cex=1.5)
# axis(2, tick = T, las = 2)
# mtext(side = 2, "Share of population", line=3,cex=1.5)
# mtext(side =3, text = "Evacuated ",cex=2,adj = 0,line=1)
# 
# 
# 
# barplot(pieData_NOTevacuated,axes = F, names = c("<10", paste(seq(10,70,10), "-",seq(20,80,10), sep = ""), ">80"), ylim = c(0,.25),col =rainbow(10))
# mtext(side = 1, text = "Income Group",line=3,cex=1.5)
# axis(2, tick = T, las = 2, at = seq(0,4000,1000), labels = c(0,"1,000","2,000","3,000","4,000"))
# mtext(side =3, text = "Not evacuated",cex=2,adj = 0,line=1)











