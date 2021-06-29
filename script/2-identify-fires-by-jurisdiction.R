source("script/0-packages-and-functions.R")

#load data 
    
    ca_cty <- read_rds("data/california_county_shapefile.rds")
    wf <- read_rds("data/fires/wf_data_2000_2020_clean.rds")


#fires by jurisdiction:
    
    #pull out relevant variables that will form database:
    
    db <- wf %>% dplyr::filter(YEAR_>2010) %>% 
          dplyr::select(FIRE_NAME, INC_NUM, AGENCY, UNIT_ID, YEAR = YEAR_, start_day, end_day, GIS_ACRES) %>% 
          arrange(YEAR, -GIS_ACRES) %>%  #6,408 candidate fires. Most probably did not have evacuations.
          as.data.frame() %>% 
          dplyr::select(-Shape) %>% 
          dplyr::filter(GIS_ACRES>1000)
   
    write_csv(db, file ="data/list_of_all_fires.csv")
    
          
