# The script sources two separate scripts: "survey.R" and "weather.R". It merges the 
# cleaned survey data with the cleaned weather data for the 2018 and 2012 Ensanut waves. 
# The code may take several minutes to run.

# Version: Aug 7, 2023
# Author: Alonso Quijano-Ruiz

# Install packages if necessary
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(labelled)) install.packages("labelled")

# Load packages
library(tidyverse)
library(labelled)

# Load and process the survey data
source("code/survey.R")

# Load and process the weather data
source("code/weather.R")

# Merge cleaned survey data with weather data for the 2018 and 2012 Ensanut waves
Weather_HealthPerception_2018 <- left_join(survey_clean_2018, weather_clean_2018, by = "person_id")
Weather_HealthPerception_2012 <- left_join(survey_clean_2012, weather_clean_2012, by = "person_id")

# Remove Spanish variable labels
Weather_HealthPerception_2018 <- remove_labels(Weather_HealthPerception_2018)
Weather_HealthPerception_2012 <- remove_labels(Weather_HealthPerception_2012)

# Save data as RData and dta
save(Weather_HealthPerception_2018, file = "data/Weather_HealthPerception_2018.RData")
write_dta(Weather_HealthPerception_2018, path = "data/Weather_HealthPerception_2018.dta")
save(Weather_HealthPerception_2012, file = "data/Weather_HealthPerception_2012.RData")
write_dta(Weather_HealthPerception_2012, path = "data/Weather_HealthPerception_2012.dta")
