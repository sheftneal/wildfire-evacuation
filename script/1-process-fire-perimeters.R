source("script/0-packages-and-functions.R")


    #download a shapefile of California
      ca <- tigris::counties() %>% dplyr::filter(STATEFP=="06")
      write_rds(ca, file = "data/california_county_shapefile.rds", compress = "gz")
      
    #[1] read in and clean up calfire fire perimeter data
      
      wf <- st_read(dsn = "data/fires/fire20_1.gdb", layer = "firep20_1") %>% #data source: https://frap.fire.ca.gov/frap-projects/fire-perimeters/
            st_transform(crs = st_crs(ca)) %>% #re-project to more standard coordinate system
        
        mutate( 
          #create new variables years and days and fire duration from the date variable
          start_year = year(ALARM_DATE), 
          end_year = year(CONT_DATE),
          start_day = date(ALARM_DATE),
          end_day = date(CONT_DATE),
          duration = (difftime(end_day,start_day, units = "days")),
          
          #some clean-up of the dates that were wrong
          start_day = replace(start_day, start_day == "0208-11-12" & !is.na(start_day), "2008-11-12"),
          start_day =  replace(start_day, start_day == "0219-05-28" & !is.na(start_day), "2019-05-28"),
          start_day = replace(start_day, start_day == "2106-09-25" & !is.na(start_day), "2016-09-25"),
          start_year = replace(start_year, start_year == "2106", "2016"),
          end_day = replace(end_day, end_day == "0219-05-28" & !is.na(end_day), "2019-05-28"),
          end_day = replace(end_day, end_day == "0209-08-20" & !is.na(end_day),"2009-08-20"),
          end_day = replace(end_day, end_day == "1089-08-19" & !is.na(end_day),"1989-08-19")
                ) %>% 
          
          #database goes back far we can start in 2000 even if we won't likely get evac orders back that far
          dplyr::filter(end_year >= 2000 & end_year <2021)
      
       
      #write out clean wf data as R data
      
        write_rds(wf, file = "data/fires/wf_data_2000_2020_clean.rds")
      
      
     
