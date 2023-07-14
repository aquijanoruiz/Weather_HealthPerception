# This script merges the cleaned survey data with the cleaned weather data for the years 2018 and 2012 
# Ensanut waves. The script sources two separate scripts: "wrangle_survey.R" and "wrangle_weather.R". 
# The code takes around three minutes to run.

# Version: July 14, 2018
# Author: Alonso Quijano-Ruiz

# Load packages
if(!require(haven)) install.packages("haven")
if(!require(tidyverse)) install.packages("tidyverse")

# Load and process the survey data
source("code/wrangle_survey.R")

# Load and process the weather data
source("code/wrangle_weather.R")

# Merge cleaned survey data with weather data for the 2018 and 2012 Ensanut waves
merged_survey_weather_clean <- bind_rows(
  left_join(survey_clean_2018, weather_clean_2018, by = "person_id") %>%  mutate(survey_wave = "ENSANUT 2018"),
  left_join(survey_clean_2012, weather_clean_2012, by = "person_id") %>% mutate(survey_wave = "ENSANUT 2012"))

# Save data as RData and dta
save(merged_survey_weather_clean, file = "data/merged_survey_weather_clean.RData")
write_dta(merged_survey_weather_clean, path = "data/merged_survey_weather_clean.dta")
