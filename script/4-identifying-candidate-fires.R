source("script/0-packages-and-functions.R")

  #load data directly from web. 
    data <- read_csv(url("https://www.fire.ca.gov/imapdata/mapdataall.csv")) #I got this URL by going here: https://www.fire.ca.gov/incidents/, right clicking on "All Incident Data (.csv format)" and copying the link address  
    #I think it's cool you use url() within the read_csv() call to read in data directly from the web.
  
    #you wouldn't necessarily want to do this all the time (it won't work if you're offline) 
    #but a potential positive is that every time you run this command you will be loading the most recent database from the web so as fires are added to the data they will show up in here

    #one other word of caution you never know when something on the web will be taken down (accidentally or on purpose) so i would save a backup at least once
    if(!file.exists("data/fires/calfire_incidents.rds")){write_rds(data, file = "data/fires/calfire_incidents.rds", compress = "gz")}#only create backup if one doesn't already exist

    
    
    #Subset to Sonoma fires
    son_fires <- data %>% 
                  dplyr::filter(incident_date_extinguished >= 2000) %>%  #let's subset to fires since 2000 (you can pick a more recent year if you have too many fires)
                  dplyr::filter(grepl("Sonoma",incident_county)) %>%  #I figured out how to use grep within filter()
                  mutate(year = lubridate::year(incident_date_extinguished)) %>% 
                  arrange(desc(year), desc(incident_acres_burned)) #descending sort by year and then within each year by size of fire so top of each year group has biggest fires
   
                #if you have lots of fires you may also want to target your search using other variable (incident_type == "Wildfire" for example)

    
