# This R script loads shapefiles for Ecuador's parishes, extracts temperature and precipitation data 
# for each parish from NOAA global datasets for specific years, and matches the weather data with survey 
# dates from the 2018 and 2012 Ensanut surveys, creating clean weather data sets for each year.

# Version: Oct 15, 2023
# Author: Alonso Quijano-Ruiz

# Install packages if necessary
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(sf)) install.packages("sf")
if (!require(terra)) install.packages("terra", repos='https://rspatial.r-universe.dev')
if (!require(haven)) install.packages("haven")

# Load packages
library(tidyverse)
library(sf)
library(terra)
library(haven)

# Ensanut survey --------------------
# Source: https://www.ecuadorencifras.gob.ec/salud-salud-reproductiva-y-nutricion/
# Load the Ensanut 2018 and 2012 data
unzip("data/ensanut_zip", exdir = "data")
ensanut_2018 <- read_dta("data/1_BDD_ENS2018_f1_personas.dta")
ensanut_2012 <- read_dta("data/ensanut_f1_personas.dta")

# Select the person and parish IDs, as well as the survey dates for each survey
ensanut_2018 <- ensanut_2018 %>% 
  transmute(person_id = id_per, 
            canton_id = substr(upm, 1, 4),
            parish_id = substr(upm, 1, 6),
            survey_date = as.Date(paste(fecha_anio, fecha_mes, fecha_dia, sep = "-")))

ensanut_2012 <- ensanut_2012 %>%
  transmute(person_id = idpers,
            canton_id = case_when(prov >= 10 ~ substr(ciudad, 1,4), prov < 10 ~ paste(0, substr(ciudad, 1,3), sep = "")),
            parish_id = case_when(prov >= 10 ~ as.character(ciudad), prov < 10 ~ paste(0, ciudad, sep = "")),
            survey_date = as.Date(paste(anio, mes, dia, sep = "-")))

# Ecuador shapefile --------------------
# Source: https://gadm.org/
# Load Ecuador parish shapefiles
unzip("data/Shapefiles.zip", exdir = "data")
parish_shp <- st_read("data/ecuador_parroquias.shp")
parish_shp <- st_simplify(parish_shp, preserveTopology = TRUE, dTolerance = 100)

# The sf CRS is WGS 84
head(parish_shp)

# Select the parish id
parish_shp <- parish_shp %>% select(DPA_PARROQ) %>% rename(parish_id = DPA_PARROQ)

# NOAA weather data --------------------
# https://psl.NOAA.gov/data/gridded/data.cpc.globaltemp.html
# https://psl.NOAA.gov/data/gridded/data.cpc.globalprecip.html

weather_paths <- c("data/tmax.2019.nc", "data/tmax.2018.nc", "data/tmax.2013.nc", "data/tmax.2012.nc",  
                   "data/tmin.2019.nc", "data/tmin.2018.nc", "data/tmin.2013.nc", "data/tmin.2012.nc", 
                   "data/precip.2019.nc", "data/precip.2018.nc", "data/precip.2013.nc", "data/precip.2012.nc")

# Extracts the temperature/precipitation for each parish for each day --------------------
# Create the extract_weather function
extract_weather <- function(x) {
  
  # Load NetCDF files as raster objects
  x <- rast(x)
  
  # Rotate the SpatRaster to standard coordinates between -180 and 180 degrees
  x <- rotate(x)
  
  # Reproject the shapefile to match the raster's CRS
  parish_shp <- st_transform(parish_shp, crs(x))
  
  # Mask the SpatRaster using the shapefile
  x <- crop(x, parish_shp, mask = TRUE)
  
  # Extract the layer names and time
  names <- names(x)
  time <- time(x)
  
  # Extract the weighted mean temperature for each parish
  x <- extract(x, parish_shp, fun = mean, na.rm = TRUE, weights = TRUE, bind = TRUE)
  
  # Transform the SpatVector to a tibble
  x <- as_tibble(x)
  
  # Rename the columns to show the dates
  names(x)[names(x) %in% names] <- as.character(as.Date(time))
  
  return(x)
}

# Apply the extract_weather function to extract the weather values for each parish for each day
weather_files <- sapply(weather_paths, extract_weather, simplify = TRUE)

# Extract the file name from each file path and remove the ".nc" extension
basenames <- gsub(".nc$", "", basename(weather_urls))

# Loop through each object in the list and assign it to a separate object in the environment
for (i in 1:length(weather_files)) {
  assign(basenames[i], weather_files[[i]])
}

# Merge and match the survey dates with the weather data --------------------
# Define the dates before and after the survey for which weather data will also be extracted
prepost_date = c(-7:0)

# Create the match_weather function
# x contains the survey data with the survey dates and parish for each subject
# y contains the weather data for all days of the year for each parish
match_weather <- function(x, y_1, y_2, suffix) {
  
  # Merge the survey data with the weather data, remove the parish_id column, and transform to a matrix
  z <- left_join(select(x,  parish_id), y_1, by = "parish_id") %>% 
    left_join(y_2, by = "parish_id") %>%
    select(-parish_id) %>% as.matrix()
  
  # Match the survey dates with the column names
  matched_dates <- match(as.character(x$survey_date), colnames(z))
  
  # Create an empty matrix to store matched weather data
  matched_weather <- matrix(NA, nrow = length(matched_dates), ncol = length(prepost_date))
  
  # Fill the matched weather matrix 
  for(i in 1:nrow(matched_weather)) {
    for(j in 1:ncol(matched_weather)) {
      matched_weather[i,j] <- z[i, matched_dates[i] + prepost_date[j]]
    }
  }
  
  # Transform the matrix to a data frame and add person_id
  matched_weather <- data.frame(x$person_id, matched_weather)
  
  # Add appropriate column names and handle minus signs
  colnames(matched_weather) <- c("person_id", paste(suffix, prepost_date, sep=""))
  names(matched_weather) <- gsub("-", "_", names(matched_weather))
  
  # Transform the data frame to a tibble
  matched_weather <- as_tibble(matched_weather)
  
  return(matched_weather)
}

# Merge and match the survey dates with the weather data
tmax_person_2018 <- match_weather(ensanut_2018, tmax.2018, tmax.2019, "tmax")
tmin_person_2018 <- match_weather(ensanut_2018, tmin.2018, tmin.2019, "tmin")
precip_person_2018 <- match_weather(ensanut_2018, precip.2018, precip.2019, "precip")
tmax_person_2012 <- match_weather(ensanut_2012, tmax.2012, tmax.2013, "tmax")
tmin_person_2012 <- match_weather(ensanut_2012, tmin.2012, tmin.2013, "tmin")
precip_person_2012 <- match_weather(ensanut_2012, precip.2012, precip.2013, "precip")

# Join the temperature data with the precipitation data
weather_clean_2018 <- tmax_person_2018 %>% 
  left_join(tmin_person_2018, by = "person_id") %>%
  left_join(precip_person_2018, by = "person_id") 

weather_clean_2012 <- tmax_person_2012 %>% 
  left_join(tmin_person_2012, by = "person_id") %>%
  left_join(precip_person_2012, by = "person_id")

# Hot/rainy parish dummies --------------------
# Calculate the average daily maximum temperature for each parish in 2018
extreme_parish <- tibble(
  parish_id = tmax.2018$parish_id,
  avg_tmax = rowMeans(tmax.2018[,2:ncol(tmax.2018)]),
  avg_precip = rowMeans(precip.2018[,2:ncol(precip.2018)])
)

# Calculate the mean and 75th percentile (third quartile) of the mean daily
# maximum temperature for all parishes in 2018
mean_tmax_2018 <- mean(extreme_parish$parish_tmax_mean, na.rm = TRUE)
q3_tmax_2018 <- quantile(extreme_parish$parish_tmax_mean, 0.75, na.rm = TRUE)

# Identify extreme temperature parishes using two criteria:
# - hot_parish: Parishes with average daily maximum temperature above the 75th percentile
extreme_parish <- extreme_parish %>% 
  mutate(hot_parish = ifelse(parish_tmax_mean >= q3_tmax_2018, 1, 0))

# Calculate the mean daily precipitation for each parish in 2018
extreme_parish <- extreme_parish %>%
  mutate(parish_precip_mean = rowMeans(precip.2018[,2:ncol(precip.2018)]))

# Calculate the mean, standard deviation, and 75th percentile (third quartile) of the mean daily
# precipitation for all parishes in 2018
mean_precip_2018 <- mean(extreme_parish$parish_precip_mean, na.rm = TRUE)
sd_precip_2018 <- sd(extreme_parish$parish_precip_mean, na.rm = TRUE)
q3_precip_2018 <- quantile(extreme_parish$parish_precip_mean, 0.75, na.rm = TRUE)

# Identify extreme precipitation parishes using two criteria:
# - rainy_parish_84: Parishes with mean daily precipitation above the mean + 1 standard deviation
# - rainy_parish_q3: Parishes with mean daily precipitation above the 75th percentile
extreme_parish <- extreme_parish %>% mutate(
  rainy_parish_84 = ifelse(parish_precip_mean >= mean_precip_2018 + sd_precip_2018, 1, 0),
  rainy_parish_q3 = ifelse(parish_precip_mean >= q3_precip_2018, 1, 0)
)

# Put everything together --------------------
# Join the matched weather data for 2018 and add the extreme weather dummy variables
weather_clean_2018 <- weather_clean_2018 %>%
  # Add the parish IDs to join the extreme weather dummy variables
  mutate(parish_id = ensanut_2018$parish_id) %>%
  left_join(extreme_parish, by = "parish_id") %>%
  select(-parish_id) %>%
  # Add the canton IDs to join the extreme weather dummy variables
  mutate(canton_id = ensanut_2018$canton_id) %>%
  left_join(canton_tmax_2018, by = "canton_id") %>%
  left_join(canton_precip_2018, by = "canton_id") %>%
  select(-canton_id)

# Join the matched weather data for 2012 and add the extreme weather dummy variables
weather_clean_2012 <- weather_clean_2012 %>%
  # Add the parish IDs to join the extreme weather dummy variables
  mutate(parish_id = ensanut_2012$parish_id) %>%
  left_join(extreme_parish, by = "parish_id") %>%
  select(-parish_id) %>%
  # Add the canton IDs to join the extreme weather dummy variables
  mutate(canton_id = ensanut_2012$canton_id) %>%
  left_join(canton_tmax_2018, by = "canton_id") %>%
  left_join(canton_precip_2018, by = "canton_id") %>%
  select(-canton_id)
