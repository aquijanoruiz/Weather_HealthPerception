# This R script loads the Ecuador shapefiles and raster files and plots the weather maps.
# Version: Sep 23, 2023
# Author: Alonso Quijano-Ruiz

# Load raster files
tmax_2019 <- rast("data/tmax.2019.nc")
tmax_2018 <- rast("data/tmax.2018.nc")
tmin_2019 <- rast("data/tmin.2019.nc")
tmin_2018 <- rast("data/tmin.2018.nc")
precip_2019 <- rast("data/precip.2019.nc")
precip_2018 <- rast("data/precip.2018.nc")

# Load South America shapefiles
sa_shp <- st_read("data/South_America.shp")

# Load Ecuador region shapefiles
coast_shp <- st_read("data/Costa.shp")
coast_shp <- st_transform(coast_shp, crs(tmax_2018))
coast_shp <- st_union(coast_shp)
coast_shp <- st_simplify(coast_shp, preserveTopology = TRUE, dTolerance = 100)
sierra_shp <- st_read("data/Sierra.shp")
sierra_shp <- st_transform(sierra_shp, crs(tmax_2018))
sierra_shp <- st_union(sierra_shp)
sierra_shp <- st_simplify(sierra_shp, preserveTopology = TRUE, dTolerance = 100)
amazon_shp <- st_read("data/Amazonia.shp")
amazon_shp <- st_transform(amazon_shp, crs(tmax_2018))
amazon_shp <- st_union(amazon_shp)
amazon_shp <- st_simplify(amazon_shp, preserveTopology = TRUE, dTolerance = 100)

# Load Ecuador parish shapefiles
parish_shp <- st_read("data/ecuador_parroquias.shp")
parish_shp <- st_simplify(parish_shp, preserveTopology = TRUE, dTolerance = 100)
parish_shp <- parish_shp %>% rename(prov_id = DPA_PROVIN) %>%
  filter(!prov_id %in% "20")

# Reproject the shapefile to match the raster's CRS
parish_shp <- st_transform(parish_shp, crs(tmax_2018))

# Index 2018-11-10 to 2019-01-12 and from 2019-06-08 to 
load("data/Weather_HealthPerception_2018.RData")
survey_dates <- sort(unique(Weather_HealthPerception_2018$survey_date))
index <- (as.Date("2018-01-01") + 0:(365*2 - 1)) %in% survey_dates

# Average maximum temperature
tmax <- c(tmax_2018, tmax_2019)
tmax <- tmax[[index]]
tmax <- rotate(tmax)
tmax <- crop(tmax, parish_shp, mask = TRUE)
tmax <- app(tmax, mean)

# Average minimum temperature
tmin <- c(tmin_2018, tmin_2019)
tmin <- tmin[[index]]
tmin <- rotate(tmin)
tmin <- crop(tmin, parish_shp, mask = TRUE)
tmin <- app(tmin, mean)

# Plot average precipitation
precip <- c(precip_2018, precip_2019)
precip <- precip[[index]]
precip <- rotate(precip)
precip <- crop(precip, parish_shp, mask = TRUE)
precip <- app(precip, mean)

png(file="plots/maps.png", width=6, height=8, res = 150,units="in")
layout(matrix(c(1,1,2,2,0,3,3,0),nrow = 2, ncol = 4, byrow = TRUE))
plot(tmax, main = "Average daily maximum temperature", font.main = 1)
plot(sa_shp$geometry, add = TRUE)
plot(coast_shp, add = TRUE)
plot(amazon_shp, add = TRUE)
plot(tmin, main = "Average daily minimum temperature", font.main = 1)
plot(sa_shp$geometry, add = TRUE)
plot(coast_shp, add = TRUE)
plot(amazon_shp, add = TRUE)
plot(precip, main = "Average daily precipitation", font.main = 1)
plot(sa_shp$geometry, add = TRUE)
plot(coast_shp, add = TRUE)
plot(amazon_shp, add = TRUE)
dev.off()
