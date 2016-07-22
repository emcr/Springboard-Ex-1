#Set package location and load libraries
.libPaths("C:/Users/emily_rinaldi/Desktop/R/win-library/3.2")
library(dplyr)
library(tidyr)
library(readr)

file = "titanic_original.csv"
titanic_orig <- read_csv(file) %>% tbl_df

clean_ports <- titanic_orig %>%
  mutate(embarked = if_else(is.na(embarked), "S", embarked))

#Replace missing ages with mean
clean_ages <- clean_ports %>% 
  mutate(age = ifelse(is.na(age), summarise(titanic_orig, mean(age,na.rm = TRUE)), age)) %>%
  mutate(age = as.double(age))

#Fill-in missing lifeboat numbers
clean_boats <- clean_ages %>%
  mutate(boat = if_else(is.na(boat),"None", boat))

#Add dummy variable for availability of cabin number
titanic_clean <- clean_boats %>%
  mutate(has_cabin_number = if_else(is.na(cabin), 0, 1))

#write titanic_clean to .csv file
write_csv(titanic_clean, paste(getwd(), "/titanic_clean.csv", sep = ""))