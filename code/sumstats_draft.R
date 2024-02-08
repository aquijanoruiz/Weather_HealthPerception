# This R script loads the clean data and performs the summary statistics tables and plots.
# Version: Sep 24, 2023
# Author: Alonso Quijano-Ruiz

# Load packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(sf)) install.packages("sf")
library(tidyverse)
library(openxlsx)
library(sf)

# Summary statistics ----------
# Load data
load("data/Weather_HealthPerception_2018.RData")

sort(unique(Weather_HealthPerception_2018$survey_date))
# Create dummy variables
education_dummies <- model.matrix(~education-1, data=Weather_HealthPerception_2018)
sex_dummies <- model.matrix(~sex-1, data=Weather_HealthPerception_2018)
ethnicity_dummies <- model.matrix(~ethnicity-1, data=Weather_HealthPerception_2018)
region_dummies <- model.matrix(~region-1, data=Weather_HealthPerception_2018)

# Merge the dummy variables with the original data
Weather_HealthPerception_2018 <- cbind(
  Weather_HealthPerception_2018, education_dummies, sex_dummies, ethnicity_dummies, region_dummies)

# Function that obtains the mean, std dev, and number of observations
summ_stat <- function(x) {
  x_mean <- mean(x)
  x_sd <- sd(x)
  x_n <- length(x)
  
  df <- tibble(mean = x_mean, sd = x_sd, n = x_n)
  return(df)
}

# Create the summary statistics table (total sample)
summ_stat_table_total <- Weather_HealthPerception_2018 %>% 
  select(-c("sex", "education", "ethnicity")) %>%
  select(good_health, better_health,
         tmax0, tmin0, precip0,
         age, sexfemale, starts_with(c("education", "ethnicity")), 
         income, sick, got_care, prev_care, hospitalized) %>%
  map_dfr(.f = summ_stat, .id = "variable")

# Create the summary statistics table (by region)
summ_stat_table_coast <- Weather_HealthPerception_2018 %>% 
  filter(region == "Coast") %>%
  select(-c("sex", "education", "ethnicity")) %>%
  select(good_health, better_health,
         tmax0, tmin0, precip0,
         age, sexfemale, starts_with(c("education", "ethnicity")), 
         income, sick, got_care, prev_care, hospitalized) %>% 
  map_dfr(.f = summ_stat, .id = "variable")

summ_stat_table_sierra <- Weather_HealthPerception_2018 %>% 
  filter(region == "Sierra") %>%
  select(-c("sex", "education", "ethnicity")) %>%
  select(good_health, better_health,
         tmax0, tmin0, precip0,
         age, sexfemale, starts_with(c("education", "ethnicity")), 
         income, sick, got_care, prev_care, hospitalized) %>% 
  map_dfr(.f = summ_stat, .id = "variable")

summ_stat_table_amazon <- Weather_HealthPerception_2018 %>% 
  filter(region == "Amazon") %>%
  select(-c("sex", "education", "ethnicity")) %>%
  select(good_health, better_health,
         tmax0, tmin0, precip0,
         age, sexfemale, starts_with(c("education", "ethnicity")), 
         income, sick, got_care, prev_care, hospitalized) %>% 
  map_dfr(.f = summ_stat, .id = "variable")

summ_stat_table <- cbind(summ_stat_table_total, summ_stat_table_sierra, 
                         summ_stat_table_coast, summ_stat_table_amazon)

# Save to xlsx
write.xlsx(x = summ_stat_table, file = "tables/summ_stat_table.xls")

# Histogram of survey dates ----------
survey_plot <- Weather_HealthPerception_2018 %>% 
  ggplot(aes(x = survey_date)) + 
  geom_histogram() +
  labs(title = "Survey Responses Over Time",
       x = "Survey Date",
       y = "Number of Responses") + 
  theme_minimal()
  
ggsave(filename = "plots/survey_plot.png", plot = survey_plot, width = 6, height = 4)

survey_region_plot <- Weather_HealthPerception_2018 %>% 
  group_by(region) %>% 
  ggplot(aes(x = survey_date)) + 
  geom_histogram() +
  facet_wrap(~region) +
  labs(title = "Survey Responses by Region",
       x = "Survey Date",
       y = "Number of Responses") + theme_minimal()

ggsave(filename = "plots/survey_region_plot.png", plot = survey_plot, width = 6, height = 4)

# Parish areas ----------
parish_shp <- st_read("data/ecuador_parroquias.shp")
# Calculate the area (in km^2) of the parishes included in the data
parish_shp$area <- st_area(parish_shp)/(1000^2)
parish_shp <- parish_shp %>% rename(parish_id = DPA_PARROQ) %>%
  filter(parish_id %in% Weather_HealthPerception_2018$parish_id)

# Get the summary statistics
summary(parish_shp$area)
quantile(parish_shp$area, c(0.1, 0.5, 0.9))

# mean diameter
sqrt(mean(parish_shp$area)/pi)*2

# Regression models ----------
# Variables of interest
varofint <- c("tmax0 + tmin0 + precip0")

# Control variables
Weather_HealthPerception_2018$age2 <- Weather_HealthPerception_2018$age ^ 2
controls <- c("age + age2 + sex + ethnicity + education + income")

model <- lm(paste("good_health ~ ", varofint, controls, sep = "+"),
            data = Weather_HealthPerception_2018)
summary(model)
