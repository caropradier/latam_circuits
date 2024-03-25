library(readr)
library(tidyverse)

meta_df <- read.csv('data/latam/latam_meta_1990.csv', sep=';', header=FALSE,
                    col.names=c('Pub_ID', 'pub_year', 'source_title', 'source_type',
                                'level1', 'level2', 'n_cits')) %>% 
  select('Pub_ID', 'level1', 'level2') %>% 
  distinct(Pub_ID, .keep_all = TRUE) 

text_df <- read.csv("data/latam/latam_text_1990.csv", sep=";", header=FALSE,
                    col.names=c('Pub_ID', 'title', 'abstract'))

# Find rows with missing or empty abstract or title
no_info <- text_df[(is.na(text_df$abstract) | text_df$abstract == '' | text_df$abstract == 'NULL' | text_df$abstract == 'NA') | 
                     (is.na(text_df$title) | text_df$title == '' | text_df$title == 'NULL' | text_df$title == 'NA'), 'Pub_ID']

# Remove rows with missing or empty abstract or title
text_df <- text_df[!(text_df$Pub_ID %in% no_info), ]

# Merging text_df with selected columns from meta_df based on Pub_ID
text_df <- text_df %>% 
  left_join(.,meta_df, by = "Pub_ID")

# Concatenating 'title' and 'abstract' columns into a new 'text' column
text_df <-  text_df %>% 
  #mutate(text = paste0(title, " ", abstract))
  #Modification: using title three times because it is more informative (Method applied in Koz's 2021 article on semantic spaces)
  mutate(text = paste0(title," ",title," ",title, " ", abstract))

text_df <- text_df %>% select(-abstract,-title)

# Converting 'level2' column to a factor (equivalent to categorical in pandas)
text_df$level2 <- as.factor(text_df$level2)

# Creating a new column 'level2_codes' containing the integer codes for 'level2'
text_df <- text_df %>%
  mutate(level2_codes = as.integer(level2))

# Converting 'level1' column to a factor (equivalent to categorical in pandas)
text_df$level1 <- as.factor(text_df$level1)

# Creating a new column 'level1_codes' containing the integer codes for 'level1'
text_df <- text_df %>%
  mutate(level1_codes = as.integer(level1))

#Na values
text_df$level1_codes[is.na(text_df$level1_codes)] <- 0
text_df$level2_codes[is.na(text_df$level2_codes)] <- 0

#save
# write.table(text_df, file = "code/8_global_topics/latam_text_topics.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(text_df, file = "code/8_global_topics/latam_text_topics_3title.txt", sep = "\t", row.names = FALSE, quote = FALSE)
