# This R script loads shapefiles for Ecuador's parishes, extracts temperature and precipitation data 
# for each parish from NOAA global datasets for specific years, and matches the weather data with survey 
# dates from the Ensanut surveys of 2012 and 2018, creating clean weather data sets for each year.

# Version: Jul 22, 2023
# Author: Alonso Quijano-Ruiz

# Load packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(sf)) install.packages("sf")
if (!require(terra)) install.packages("terra", repos='https://rspatial.r-universe.dev')
if (!require(haven)) install.packages("haven")

# Ecuador shapefile ----------
# Source: https://gadm.org/
# Load Ecuador parish shapefiles
ecuador_shp <- st_read("data/Shapefiles/gadm41_ECU_3.shp")
ecuador_shp <- st_simplify(ecuador_shp, preserveTopology = TRUE, dTolerance = 100)

# The sf CRS is WGS 84
head(ecuador_shp)

# Remove unnecessary columns and rename
ecuador_shp <- ecuador_shp %>% select(CC_3) %>% rename(parish_id = CC_3)

# Create a function that extracts the temperature/precipitation for each parish ----------
extract_weather <- function(x) {
  
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

# NOAA global temperature data ----------
# Source: https://psl.NOAA.gov/data/gridded/data.cpc.globaltemp.html

## Maximum temperature ----------
# Load NOAA global max temperature data
tmax_2019 <- rast("data/NOAA_temperature/tmax.2019.nc")
tmax_2018 <- rast("data/NOAA_temperature/tmax.2018.nc")
tmax_2013 <- rast("data/NOAA_temperature/tmax.2013.nc")
tmax_2012 <- rast("data/NOAA_temperature/tmax.2012.nc")

# The SpatRaster CRS is WGS 84. Because it is the same as the sf object, projection is not needed
print(tmax_2019)

# Extract the weighted mean temperature for each parish for each day
tmax_parish_2019 <- extract_weather(tmax_2019)
tmax_parish_2018 <- extract_weather(tmax_2018)
tmax_parish_2013 <- extract_weather(tmax_2013)
tmax_parish_2012 <- extract_weather(tmax_2012)

## Minimum temperature ----------
# Load NOAA global min temperature data
tmin_2019 <- rast("data/NOAA_temperature/tmin.2019.nc")
tmin_2018 <- rast("data/NOAA_temperature/tmin.2018.nc")
tmin_2013 <- rast("data/NOAA_temperature/tmin.2013.nc")
tmin_2012 <- rast("data/NOAA_temperature/tmin.2012.nc")

# Extract the weighted mean temperature for each parish for each day
tmin_parish_2019 <- extract_weather(tmin_2019)
tmin_parish_2018 <- extract_weather(tmin_2018)
tmin_parish_2013 <- extract_weather(tmin_2013)
tmin_parish_2012 <- extract_weather(tmin_2012)

# NOAA global precipitation data ----------
# Source: https://psl.NOAA.gov/data/gridded/data.cpc.globalprecip.html

# Load NOAA global min temperature data
precip_2019 <- rast("data/NOAA_precipitation/precip.2019.nc")
precip_2018 <- rast("data/NOAA_precipitation/precip.2018.nc")
precip_2013 <- rast("data/NOAA_precipitation/precip.2013.nc")
precip_2012 <- rast("data/NOAA_precipitation/precip.2012.nc")

# Extract the weighted mean temperature for each parish for each day
precip_parish_2019 <- extract_weather(precip_2019)
precip_parish_2018 <- extract_weather(precip_2018)
precip_parish_2013 <- extract_weather(precip_2013)
precip_parish_2012 <- extract_weather(precip_2012)

# Create a function to merge and match the survey dates with the weather data ----------
match_weather <- function(x, y1, y2, prepost_date = c(-7:7), date_to_match, suffix) {
  
  # Merge survey data with weather data using the parish id
  z <- left_join(select(x,  parish_id), y1, by = "parish_id") %>% left_join(y2, by = "parish_id") %>%
    
    # Remove the parish id column and transform the merged data to a matrix
    select(-parish_id) %>% as.matrix()
  
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
  
  # Transform the matrix to a data frame and add the person id
  matched_weather <- data.frame(x$person_id, matched_weather)
  
  # Add appropriate column names and handle minus signs
  colnames(matched_weather) <- c("person_id", paste(suffix, prepost_date, sep=""))
  names(matched_weather) <- gsub("-", "_", names(matched_weather))
  
  return(matched_weather)
}

# Merge and match Ensanut survey dates with the weather data 

ensanut_2012 <- read_dta("data/ensanut/ensanut_f1_personas.dta")

# Ensanut 2018 ----------
# Load the Ensanut 2018 data
# Source: https://www.ecuadorencifras.gob.ec/salud-salud-reproductiva-y-nutricion/
unzip("data/INEC_ENSANUT/ENSANUT.zip", exdir = "data/INEC_ENSANUT")
ensanut_2018 <- read_dta("data/ensanut/1_BDD_ENS2018_f1_personas.dta")

# Select the person and parish ids, as well as the survey dates
ensanut_2018 <- ensanut_2018 %>% 
  transmute(person_id = id_per, 
            parish_id = substr(upm, 1, 6),
            survey_date = paste(fecha_anio, fecha_mes, fecha_dia, sep = "-"))

# Merge and match the survey dates with the weather data
tmax_person_2018 <- match_weather(ensanut_2018, tmax_parish_2018, tmax_parish_2019, 
                                  date_to_match = ensanut_2018$survey_date, suffix = "tmax")
tmin_person_2018 <- match_weather(ensanut_2018, tmin_parish_2018, tmin_parish_2019, 
                                  date_to_match = ensanut_2018$survey_date, suffix = "tmin")
precip_person_2018 <- match_weather(ensanut_2018, precip_parish_2018, precip_parish_2019,
                                    date_to_match = ensanut_2018$survey_date, suffix = "precip")

# Put everything together
weather_data_clean_2018 <- tmax_person_2018 %>% left_join(tmin_person_2018, by = "person_id") %>%
  left_join(precip_person_2018, by = "person_id")

# Save data as RData
save(weather_data_clean_2018, file = "data/weather_data_clean_2018.RData")

# Ensanut 2012 ----------
# Load the Ensanut 2012 data
ensanut_2012 <- read_dta("data/ensanut/ensanut_f1_personas.dta")

# Select the person and parish ids, as well as the survey dates
ensanut_2012 <- ensanut_2012 %>%
  transmute(person_id = idpers,
            parish_id = case_when(prov >= 10 ~ as.character(ciudad), prov < 10 ~ paste(0, ciudad, sep = "")),
            survey_date = paste(anio, mes, dia, sep = "-"))

# Merge and match the survey dates with the weather data
tmax_person_2012 <- match_weather(ensanut_2012, tmax_parish_2012, tmax_parish_2013, 
                                  date_to_match = ensanut_2012$survey_date, suffix = "tmax")
tmin_person_2012 <- match_weather(ensanut_2012, tmin_parish_2012, tmin_parish_2013, 
                                  date_to_match = ensanut_2012$survey_date, suffix = "tmin")
precip_person_2012 <- match_weather(ensanut_2012, precip_parish_2012, precip_parish_2013,
                                    date_to_match = ensanut_2012$survey_date, suffix = "precip")

# Put everything together
weather_data_clean_2012 <- tmax_person_2012 %>% left_join(tmin_person_2012, by = "person_id") %>%
  left_join(precip_person_2012, by = "person_id")

# Save data as RData
save(weather_data_clean_2012, file = "data/weather_data_clean_2012.RData")
