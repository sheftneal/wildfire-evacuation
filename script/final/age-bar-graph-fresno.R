#checked 11/30

source("script/0-packages-and-functions.R")
library(tidycensus)
census_api_key("f5c6e020a6aae7ba5420a4051ad53eee38c621fa")

# [1] Read in + clean population data

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
            
            # Re-organize data
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


      acs <- pop
      acs$GEOID <- substr(acs$GEOID, 2, 1000)
      
      
#[2] Read in + clean census tract mapping

      
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

          
  #[3] Calculations
          
                
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

      
      
      
# [3] PREP DATA FOR FIGURES
      

      
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
      ct_acs <- left_join(acs, ct_norder) %>% mutate(n_order = replace(n_order, is.na(n_order), 0))
      
      #no take this data frame and summarise across all the pop variables.
      #pop1:pop_total means take every column including and to the right of pop1 through the column pop_total and sum them
      #then we calculate pop shares
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
          share_pop8 = pop8/pop_total,
          share_pop9 = pop9/pop_total
        )
      
      #population by age and whether they were evacuated at any time during our sample
      evacuated_demo <- ct_acs %>%
        mutate(evacuated = as.numeric(n_order > 0)) %>% #create binary dummy var == 1 if there were evac orders in tract, 0 else
        group_by(evacuated) %>%
        summarise_at(vars(pop1:pop_total), ~sum(.x, na.rm = T))  %>% #separate pop totals for groups that were and were not evac
        mutate(pop_total = pop1 + pop2 + pop3 + pop4 + pop5 + pop6 + pop7 + pop8 + pop9, #calculations from those pop totals
               share_pop1 = pop1/pop_total,
               share_pop2 = pop2/pop_total,
               share_pop3 = pop3/pop_total,
               share_pop4 = pop4/pop_total,
               share_pop5 = pop5/pop_total,
               share_pop6 = pop6/pop_total,
               share_pop7 = pop7/pop_total,
               share_pop8 = pop8/pop_total,
               share_pop9 = pop9/pop_total
        )
      
      #a potentially interesting statistic:
      evacuated_demo %>% mutate(share_pop_over60 = (pop7 + pop8 + pop9)/pop_total) %>% dplyr::select(evacuated, share_pop_over60)
      #people over 60 make up 16% of county population but nearly 29% of evacuated population
      
      
      #save(county_demo, evacuated_demo, annual_evac_by_age, file = "data/clean/FresnoEvacuationData.RData")
      
      

# [4] PLOTS 
            
            # AVERAGE AGE VS EVACUATION ORDERS MAP OF COUNTY SIDE BY SIDE PLOTS
            fresno <- get_acs(state = "06", county = "019", geography = "tract", 
                              variables = "B01002_001", geometry = TRUE)
            fresno <- fresno %>% add_column(num_evacuation_orders = 0, .after = "NAME")
            fresno[14, 3] <- 12 #64.02
            fresno[57, 3] <- 2 #64.03
            fresno[33, 3] <- 14 #64.04
            fresno[68, 3] <- 4 #64.05
            fresno[81, 3] <- 7 #79.02
            
            
            
            # GET PIE CHART / BAR GRAPH DATA READY
            blank_theme <- theme_minimal()+
              theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
              )
            
            pie_evacuated <- data.frame(Age = c("0-10","10-20","20s","30s","40s","50s","60s","70s","80+"), 
                                        share = unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_pop"))) #takes evacuated
            ) %>% 
              ggplot( aes(x = "", y = share, fill = Age)) + 
              geom_bar(width = 1, stat = "identity") +
              coord_polar("y", start = 0) + 
              scale_fill_manual(values=c(rainbow(10))) + #here you can change colors
              blank_theme +
              theme(axis.text.x=element_blank()) +
              geom_text(aes(y = cumsum(share) + share ,label =paste(round(100*share), "%",sep="") ), size = 5)
            
            pieData_evacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 1) %>% dplyr::select(starts_with("share_pop"))))
            pieData_NOTevacuated <- as.numeric(unlist(evacuated_demo %>% dplyr::filter(evacuated == 0) %>% dplyr::select(starts_with("share_pop"))))
            pieData_county <- as.numeric(county_demo %>% dplyr::select(starts_with("share_"))) #if you want to compare to overall county pop regardless of evacuation status
            
            
            #SIDE BY SIDE BAR PLOT OF AGE DISTRIBUTION
            
            #add function to define transparency of colors - add transparency to any color	
            add.alpha <- function(col, alpha=1){
              if(missing(col))
                stop("Please provide a vector of colours.")
              apply(sapply(col, col2rgb)/255, 2, 
                    function(x) 
                      rgb(x[1], x[2], x[3], alpha=alpha))  
            }	
            
            par(mfrow = c(1,1))
            barplot(rbind(pieData_evacuated,pieData_county), beside = T ,axes = F, names = c("<10", paste(seq(10,70,10), "-",seq(20,80,10), sep = ""), ">80"), col = add.alpha(rep('black',2), c(1, .25)), ylim = c(0, .2))
            mtext(side = 1, text = "Age Range",line=3,cex=1.5)
            axis(2, tick = T, las = 2, at = seq(0,.2, .05), labels = paste(seq(0,20,5), "%", sep=""))
            mtext(side = 2, "Share of Group Population", line=3,cex=1.5)
            legend(x = "topright", legend = c("Evacuated", "Total Population"), fill = c('black', add.alpha('black', .25)),bty = "n",cex=2, title = "Group")
