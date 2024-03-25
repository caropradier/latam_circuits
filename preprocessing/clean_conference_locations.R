library(tidyverse)
library(countrycode)
library(datasets)
library(maps)

latam_conference_loc_1990 <- read_delim("data/latam/latam_conference_loc_1990.csv", 
                                        delim = ";", escape_double = FALSE, col_names = c('Pub_ID', 'conference_id','conference_location_id','conference_location'), 
                                        trim_ws = TRUE)

locations <- latam_conference_loc_1990 %>% 
  select(conference_location_id,conference_location) %>% unique()

country_names <- countrycode::countryname_dict

regexs <- countrycode::codelist$country.name.en.regex

us_states <- c(state.abb,state.name)

cities_table <- maps::world.cities%>% select(name, country.etc,pop) %>% 
  filter(!grepl("'",name))

cities <- cities_table$name

country_vector <- c(unique(country_names$country.name.en),
                    "USA", "UK", "England","Korea", 
                    "Virtual", "Online",
                    regexs)

#countries

locations_clean <- locations %>% 
  mutate(conference_location = str_replace_all(conference_location, c("á" = "a", 
                                                                      "é" = "e",
                                                                      "í" = "i",
                                                                      "ó" = "o",
                                                                      "ú" = "u"))) %>% 
  mutate(country = str_extract(conference_location,paste(country_vector, collapse="|"))) 

#us states

locations_clean2 <-  locations_clean %>% 
  filter(is.na(country)) %>% 
  mutate(state = str_extract(conference_location,paste(us_states, collapse="|"))) %>%  
  mutate(country = case_when(!is.na(state)~ "United States")) %>% 
  select(-state)

#cities

locations_clean3 <-  locations_clean2 %>% 
  filter(is.na(country)) %>% 
  select(-country) %>% 
  mutate(city = str_extract(conference_location,paste(cities, collapse="|"))) %>%  
  left_join(.,cities_table, by = c("city" = "name")) %>% 
  rename("country" = "country.etc") %>% 
  group_by(conference_location_id,conference_location) %>% 
  slice_max(order_by = pop, n = 1) %>% 
  select(-pop, -city)

#unidentified

locations_clean4 <- locations_clean3 %>% 
  filter(is.na(country)) %>% 
  mutate(country = case_when(conference_location == "Buzios"| grepl("Janeiro",conference_location) ~ paste0("Brazil"),
                             TRUE ~ "Not Latam (Ins Inf)"))

cleaned_version <- bind_rows(locations_clean4 %>% filter(!is.na(country)),
          locations_clean3 %>% filter(!is.na(country)),
          locations_clean2 %>% filter(!is.na(country)),
          locations_clean %>% filter(!is.na(country)))

country_to_code <- cleaned_version %>% 
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso2c'))

#saveRDS(country_to_code, "data/latam/conference_proceedings_countries.RDS")

#country_to_code <- readRDS( "data/latam/conference_proceedings_countries.RDS")

latam_conference_loc_1990 <- latam_conference_loc_1990 %>% 
  left_join(.,(country_to_code %>% select(-conference_location))
            )
latam_country_codes <- c('AR','BO','BR','CL','CO','MX','PY','PE','UY','VE')

latam_conference_loc_1990 <- latam_conference_loc_1990 %>% 
  select(Pub_ID, "proc_country"="country_code") %>% 
  mutate(latam_conf = ifelse(proc_country %in% latam_country_codes, "Latin American journal", "Non Latin American journal")) 

saveRDS(latam_conference_loc_1990,"results/aux_conference_country_1990.RDS")

