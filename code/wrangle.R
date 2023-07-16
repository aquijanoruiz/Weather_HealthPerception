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
Weather_HealthPerceiption_2018data <- left_join(survey_clean_2018, weather_clean_2018, by = "person_id")
Weather_HealthPerceiption_2012data <- left_join(survey_clean_2012, weather_clean_2012, by = "person_id")

# Save data as RData and dta
save(Weather_HealthPerceiption_2018data, file = "data/Weather_HealthPerceiption_2018data.RData")
write_dta(Weather_HealthPerceiption_2018data, path = "data/Weather_HealthPerceiption_2018data.dta")
save(Weather_HealthPerceiption_2012data, file = "data/Weather_HealthPerceiption_2012data.RData")
write_dta(Weather_HealthPerceiption_2012data, path = "data/Weather_HealthPerceiption_2012data.dta")
