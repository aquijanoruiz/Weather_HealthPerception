# The script sources two separate scripts: "survey.R" and "weather.R". It merges the 
# cleaned survey data with the cleaned weather data for the 2018 and 2012 Ensanut waves. 
# The code may take several minutes to run.

# Version: July 14, 2023
# Author: Alonso Quijano-Ruiz

# Load packages
if(!require(haven)) install.packages("haven")
if(!require(tidyverse)) install.packages("tidyverse")

# Load and process the survey data
source("code/survey.R")

# Load and process the weather data
source("code/weather.R")

# Merge cleaned survey data with weather data for the 2018 and 2012 Ensanut waves
Weather_HealthPerceiption_data <- bind_rows(
  left_join(survey_clean_2018, weather_clean_2018, by = "person_id") %>% mutate(survey_wave = "ENSANUT 2018"),
  left_join(survey_clean_2012, weather_clean_2012, by = "person_id") %>% mutate(survey_wave = "ENSANUT 2012"))

# Save data as RData and dta
save(Weather_HealthPerceiption_data, file = "data/Weather_HealthPerceiption_data.RData")
write_dta(Weather_HealthPerceiption_data, path = "data/Weather_HealthPerceiption_data.dta")
