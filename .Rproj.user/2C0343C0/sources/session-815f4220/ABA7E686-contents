library(tidyverse)
library(readxl)
library(countrycode)

latam_authors <- readRDS("data/latam/latam_authors_1990.RDS")

# latam_authors <- read_delim("data/latam/latam_authors_1990.csv",
#                             delim = ";", escape_double = FALSE,
#                             #col_names = TRUE,
#                             col_names = c('Pub_ID','author_seq',
#                                           'country_code','city','author_id',
#                                           'first_name','last_name','gender'),
#                             trim_ws = TRUE
# )%>% 
#   distinct(Pub_ID, author_id,country_code,gender,.keep_all = TRUE) 

latam_meta <- read_delim("data/latam/latam_meta_1990.csv", 
                         delim = ";", escape_double = FALSE, 
                         #col_names = TRUE, 
                         col_names = c('Pub_ID', 'pub_year','source_title', 'source_type',
                                      'level1', 'level2', 'n_cits'),
                         trim_ws = TRUE
                         
)

latam_ids <- read_delim("data/latam/latam_ids_1990.csv", 
                         delim = ";", escape_double = FALSE, 
                         col_names = TRUE, 
                         trim_ws = TRUE
                         
)

latam_country_codes <- c('AR','BO','BR','CL','CO','MX','PY','PE','UY','VE')

####paper_level_tables#####

paper_level_tables <- list()

paper_gender_dist <- latam_authors %>%
  filter(country_code%in%latam_country_codes) %>% 
  filter(gender %in% c('Men','Women')) %>% 
  group_by(Pub_ID, gender) %>% 
  summarise(n=n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = gender, values_from = p, values_fill = 0)

paper_level_tables$paper_gender_dist <- paper_gender_dist

aux_gender_props <-  latam_authors %>% 
  filter(gender %in% c("Men","Women")) %>% 
  group_by(Pub_ID,gender) %>% 
  summarise(n = n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(n = n/sum(n))  %>% 
  pivot_wider(id_cols = "Pub_ID", names_from = "gender", values_from = "n",values_fill = 0) %>% 
  mutate(comp = factor(case_when(Men > Women & Men != 1  ~ "Women minority",
                                 Women >= Men & Women != 1 ~ "Women majority",
                                 Women == 1 ~ "Only women",
                                 Men == 1 ~ "Only men"),
                       levels=c("Only men","Women minority","Women majority","Only women"))) %>% 
  select(Pub_ID,"gender_comp"="comp")

paper_level_tables$aux_gender_props <- aux_gender_props

aux_number_authors <- latam_authors %>% 
  group_by(Pub_ID) %>% 
  summarise(n_collaborators = n_distinct(author_id)) 

paper_level_tables$aux_number_authors <- aux_number_authors

aux_first_author <- latam_authors %>% 
  group_by(Pub_ID) %>% 
  #keep first author
  filter(author_seq == min(author_seq)) %>% 
  #if more than one author is identified as first author but it's the same person, that's ok
  distinct(Pub_ID,author_id, country_code,gender,.keep_all = TRUE) %>% 
  #otherwise, I remove them
  group_by(Pub_ID) %>%
  filter(n() == 1) %>% 
  select(Pub_ID,"first_author_id" = "author_id","first_author_gender" = "gender","first_author_country_code" = "country_code")

paper_level_tables$aux_first_author <- aux_first_author

saveRDS(paper_level_tables,"results/paper_level_tables_1990.RDS")

###authors_list#####

authors_list <- list()

paper_gender_dist <- latam_authors %>%
  filter(country_code%in%latam_country_codes) %>% 
  filter(gender %in% c('Men','Women')) %>% 
  group_by(Pub_ID, gender) %>% 
  summarise(n=n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = gender, values_from = p, values_fill = 0)

general <- paper_gender_dist %>% 
  ungroup() %>% 
  summarise(Men = mean(Men),
            Women = mean(Women)) %>% 
  mutate(country_code = "World")

paper_gender_country_dist <- latam_authors %>%
  filter(country_code%in%latam_country_codes) %>% 
  filter(gender %in% c('Men','Women')) %>% 
  group_by(country_code,Pub_ID, gender) %>% 
  summarise(n=n_distinct(author_id)) %>% 
  group_by(Pub_ID) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = gender, values_from = p, values_fill = 0)

by_country <- paper_gender_country_dist %>% 
  group_by(country_code) %>% 
  summarise(Men = sum(Men),
            Women = sum(Women)) %>% 
  mutate(tot = Men+Women,
         Men = Men/tot,
         Women = Women/tot) %>% 
  select(-tot)

frac_gender_country <- bind_rows(by_country,general)

authors_list$authors_frac_country <- frac_gender_country

saveRDS(authors_list,"results/authors_analysis_1990.RDS")

####journals####

ulrichs_df <- read_excel("data/latam/ulrichs full db.xlsx")

ulrichs_df <- ulrichs_df %>% 
  select(ISSN,Country) %>% 
  distinct()

df <- latam_ids %>% 
  inner_join(ulrichs_df, by=join_by(issn_print==ISSN))

df <- df %>% 
  select(-doi, -issn_print) %>% 
  unique()

df <- df %>% 
  rename("journal_country" = "Country")

df <- df %>% 
mutate(journal_country = countrycode(journal_country, origin = 'country.name', destination = 'iso2c')) %>% 
  mutate(latam_journal = ifelse(journal_country %in% latam_country_codes, "Latin American journal", "Non Latin American journal")) %>% unique()

saveRDS(df, "results/aux_journal_country_1990.RDS")

####topics#####

