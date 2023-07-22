# This R script loads shapefiles for Ecuador's parishes, extracts temperature and precipitation data 
# for each parish from NOAA global datasets for specific years, and matches the weather data with survey 
# dates from the Ensanut surveys of 2012 and 2018, creating clean weather data sets for each year.

# Version: Jul 21, 2023
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

# Load the data --------------------
## NOAA weather data --------------------
# https://psl.NOAA.gov/data/gridded/data.cpc.globaltemp.html
# https://psl.NOAA.gov/data/gridded/data.cpc.globalprecip.html

# List of URLs for weather data files
weather_urls <- c(
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmax.2019.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmax.2018.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmax.2013.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmax.2012.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmin.2019.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmin.2018.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmin.2013.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_temp/tmin.2012.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_precip/precip.2019.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_precip/precip.2018.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_precip/precip.2013.nc",
  "https://downloads.psl.noaa.gov//Datasets/cpc_global_precip/precip.2012.nc"
)

# Create file paths for downloaded data
weather_paths <- file.path("data", basename(weather_urls))

# Download weather data files and save them with corresponding file names
download.file(weather_urls, destfile = weather_paths, mode = "wb")

## Ecuador shapefiles --------------------
# Source: https://gadm.org/
# Load Ecuador parish shapefiles
unzip("data/Shapefiles.zip", exdir = "data")
ecuador_shp <- st_read("data/gadm41_ECU_3.shp")
ecuador_shp <- st_simplify(ecuador_shp, preserveTopology = TRUE, dTolerance = 100)

# The sf CRS is WGS 84
head(ecuador_shp)

# Select the parish id
ecuador_shp <- ecuador_shp %>% select(CC_3) %>% rename(id = CC_3)

## 2018 and 2012 Ensanut surveys --------------------
# Source: https://www.ecuadorencifras.gob.ec/salud-salud-reproductiva-y-nutricion/
# Load the Ensanut 2018 and 2012 data
unzip("data/ENSANUT.zip", exdir = "data")
ensanut_2018 <- read_dta("data/1_BDD_ENS2018_f1_personas.dta")
ensanut_2012 <- read_dta("data/ensanut_f1_personas.dta")

# Select the person and parish ids, as well as the survey dates for each survey
ensanut_2018 <- ensanut_2018 %>% 
  transmute(person_id = id_per, 
            id = substr(upm, 1, 6),
            survey_date = as.Date(paste(fecha_anio, fecha_mes, fecha_dia, sep = "-")))

ensanut_2012 <- ensanut_2012 %>%
  transmute(person_id = idpers,
            id = case_when(prov >= 10 ~ as.character(ciudad), prov < 10 ~ paste(0, ciudad, sep = "")),
            survey_date = as.Date(paste(anio, mes, dia, sep = "-")))

# Extracts the temperature/precipitation for each parish for each day --------------------
# Create the extract_weather function
extract_weather <- function(x) {
  
  # Load NetCDF files as raster objects
  x <- rast(x)
  
  # Rotate the SpatRaster to standard coordinates between -180 and 180 degrees
  x <- rotate(x)
  
  # Mask the SpatRaster using the shapefile
  x <- crop(x, ecuador_shp, mask = TRUE)
  
  # Extract the layer names and time
  names <- names(x)
  time <- time(x)
  
  # Extract the weighted mean temperature for each parish
  x <- extract(x, ecuador_shp, fun = mean, na.rm = TRUE, weights = TRUE, bind = TRUE)
  
  # Transform the SpatVector to a data frame
  x <- as.data.frame(x)
  
  # Rename the columns to show the dates
  names(x)[names(x) %in% names] <- as.character(as.Date(time))
  
  return(x)
}

# Apply the extract_weather function to extract the weather values for each parish for each day
weather_files <- sapply(weather_paths, extract_weather, simplify = TRUE)

# Loop through each object in the list and assign it to a separate object in the environment
basenames <- gsub(".nc$", "", basename(weather_urls))
for (i in 1:length(weather_files)) {
  assign(basenames[i], weather_files[[i]])
}

# Merge and match the survey dates with the weather data --------------------
# Define the dates before and after the survey for with weather data will also be extracted
prepost_date = c(-3:3)

# Create the match_weather function
match_weather <- function(x, y_1, y_2, date_to_match = ensanut_2018$survey_date, suffix) {
  
  # Merge survey data with weather data, remove the parish_id column and transform to a matrix
  z <- left_join(select(x,  id), y_1, by = "id") %>% left_join(y_2, by = "id") %>%
    select(-id) %>% as.matrix()
  
  # Match survey dates with column names in the merged data
  matched_dates <- match(as.character(date_to_match), colnames(z))
  
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
  
  return(matched_weather)
}

# Merge and match the survey dates with the weather data
tmax_person_2018 <- match_weather(ensanut_2018, tmax_2018, tmax_2019, ensanut_2018$survey_date, "tmax")
tmin_person_2018 <- match_weather(ensanut_2018, tmin_2018, tmin_2019, ensanut_2018$survey_date, "tmin")
precip_person_2018 <- match_weather(ensanut_2018, precip_2018, precip_2019,ensanut_2018$survey_date, "precip")
tmax_person_2012 <- match_weather(ensanut_2012, tmax_2012, tmax_2013, ensanut_2012$survey_date, "tmax")
tmin_person_2012 <- match_weather(ensanut_2012, tmin_2012, tmin_2013, ensanut_2012$survey_date, "tmin")
precip_person_2012 <- match_weather(ensanut_2012, precip_2012, precip_2013, ensanut_2012$survey_date, "precip")

# Put everything together
weather_clean_2018 <- tmax_person_2018 %>% left_join(tmin_person_2018, by = "person_id") %>%
  left_join(precip_person_2018, by = "person_id")
weather_clean_2012 <- tmax_person_2012 %>% left_join(tmin_person_2012, by = "person_id") %>%
  left_join(precip_person_2012, by = "person_id")
