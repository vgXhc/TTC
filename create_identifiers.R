# Generate unique, anonymous identifiers

library(tidyverse)
library(readxl)
library(openssl)
library(janitor)

#read in two spreadsheets
post_TTC <- read_xlsx("/Users/user1/Downloads/All TTC data 2021-11-11 for Harald.xlsx",
                      sheet = 2) %>% 
  clean_names()

pre_TTC <- read_xlsx("/Users/user1/Downloads/All TTC data 2021-11-11 for Harald.xlsx",
                     sheet = 1) %>% 
  clean_names()

#use first, last name and email address to create MD5 hash
#then remove privacy-relevant fields
post_TTC2 <- post_TTC %>% 
  mutate(id_col = paste0(first_name, last_name, university_email_address)) %>% 
  mutate(id_col_md5 = md5(id_col)) %>% 
  select(-c(id_col, university_email_address)) %>% 
  select(- starts_with("office"))

pre_TTC2 <- pre_TTC %>% 
  mutate(id_col = paste0(first_name, last_name, university_email_address)) %>% 
  mutate(id_col_md5 = md5(id_col)) %>% 
  select(-c(id_col, university_email_address)) %>% 
  select(- starts_with("office"))

# write to csv
write_csv(post_TTC2, "post_TTC.csv")
write_csv(pre_TTC2, "pre_TTC.csv")
