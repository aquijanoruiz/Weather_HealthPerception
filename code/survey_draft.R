# This R script contains data processing and cleaning operations for the Ensanut 2018 and Ensanut 2012
# datasets. It loads the datasets and performs data transformations.

# Version: Sep 24, 2023
# Author: Alonso Quijano-Ruiz

# Install packages if necessary
if(!require(haven)) install.packages("haven")
if(!require(labelled)) install.packages("labelled")
if(!require(tidyverse)) install.packages("tidyverse")

# Load packages
library(haven)
library(labelled)
library(tidyverse)

# Ensanut 2018 --------------------
# Load the Ensanut 2018 individual and household data
# Source: https://www.ecuadorencifras.gob.ec/salud-salud-reproductiva-y-nutricion/
unzip("data/ENSANUT.zip", exdir = "data")
ensanut_home_2018 <- read_dta("data/2_BDD_ENS2018_f1_hogar.dta")
ensanut_person_2018 <- read_dta("data/1_BDD_ENS2018_f1_personas.dta")
ensanut_home_2018_labels <- var_label(ensanut_home_2018)
ensanut_person_2018_labels <- var_label(ensanut_person_2018)

# Unique identifier
ensanut_person_2018$person_id <- ensanut_person_2018$id_per
ensanut_person_2018$home_id <- ensanut_person_2018$id_hogar
ensanut_home_2018$home_id <- ensanut_home_2018$id_hogar

# Primary sampling unit, strata, and survey weight
ensanut_person_2018 <- mutate(ensanut_person_2018, psu = upm, strata = estrato, weight = fexp)

# Survey date
ensanut_person_2018 <- ensanut_person_2018 %>% mutate(
  survey_date = as.Date(paste(fecha_anio, fecha_mes, fecha_dia, sep = "-")),
  survey_weekday = factor(weekdays(survey_date, abbreviate = TRUE), 
                          levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
  survey_round = factor(ifelse(survey_date < as.Date("2019-06-08"), "round 1", "round 2"), 
                       levels = c("round 1","round 2")))

# Region
ensanut_person_2018$region <- as_factor(ensanut_person_2018$region)
levels(ensanut_person_2018$region) <- c("Sierra", "Coast", "Amazon", "Galapagos")

# Province, canton, and parish
ensanut_person_2018 <- ensanut_person_2018 %>% mutate(
  province_id = as.numeric(prov), canton_id = substr(upm, 1, 4), parish_id = substr(upm, 1, 6))

## Demographic variables --------------------
# Sex
ensanut_person_2018$sex <- as_factor(ensanut_person_2018$sexo)
levels(ensanut_person_2018$sex) <- c("male", "female")

# Age
ensanut_person_2018$age <- ensanut_person_2018$edadanios

# Ethnicity
ensanut_person_2018$ethnicity <- as_factor(ensanut_person_2018$etnia)
levels(ensanut_person_2018$ethnicity) <- c("indigenous", "black", "non-minority", "non-minority", "non-minority")
ensanut_person_2018$ethnicity <- 
  factor(ensanut_person_2018$ethnicity, levels = c("non-minority", "indigenous", "black"))

# Marital status
ensanut_person_2018$marital_status <- as_factor(ensanut_person_2018$estado_civil)
levels(ensanut_person_2018$marital_status) <- c("partnered", "non-partnered", "non-partnered")
ensanut_person_2018$marital_status <- 
  factor(ensanut_person_2018$marital_status, levels = c("non-partnered", "partnered"))

# Educational attainment
ensanut_person_2018$education <- as_factor(ensanut_person_2018$f1_s2_19_1)
levels(ensanut_person_2018$education) <- 
  c("none", "none", "none", "primary", "primary", "secondary", "secondary", "tertiary", "tertiary", "tertiary")

# Enrolled in school -> 1 if the person is enrolled in school
ensanut_person_2018$enrolled_in_school <- as.integer(ensanut_person_2018$f1_s2_17 == 1)

# Employed -> 1 if the person worked in the previous week or has a job to return to
ensanut_person_2018 <- ensanut_person_2018 %>% mutate(
  employed = case_when(is.na(f1_s3_1) ~ NA_real_, f1_s3_1 == 1 ~ 1, f1_s3_2 != 12 ~ 1, f1_s3_3 == 1 ~ 1, TRUE ~ 0))

# Household income per capita
# Column index of income variables
index_income <- c("f1_s3_15", "f1_s3_16_2", "f1_s3_17", # income from self-employment
                  "f1_s3_18", "f1_s3_19", "f1_s3_20_2", # wage/salary + benefits
                  "f1_s3_21", "f1_s3_22_2", # secondary employment income (self-employment + wage/salary)
                  "f1_s3_23_2", #  rental, investments and other non-employment income
                  "f1_s3_24_2", "f1_s3_25_2", "f1_s3_26_2", "f1_s3_28_1", "f1_s3_30_1") # cash & disability transfers

# Change business expenses to negative values
ensanut_person_2018$f1_s3_17 <- -ensanut_person_2018$f1_s3_17

# Calculate individual income
ensanut_person_2018$indiv_income <- rowSums(ensanut_person_2018[,index_income], na.rm = TRUE)

# Calculate household income per capita
ensanut_person_2018 <- ensanut_person_2018 %>% group_by(home_id) %>% 
  mutate(income_percap = sum(indiv_income, na.rm = TRUE)/n()) %>% 
  mutate(log_income_percap = ifelse(income_percap <= 1, 0, log(income_percap))) %>%
  ungroup()

## Health variables --------------------
# Disability id -> 1 if the person has a disability id
# Sick -> 1 if the person suffered from an illness in the past 30 days
# Got care -> 1 if the person received care to treat the reported illness
# Preventive care -> 1 if the person received preventive care in the past 30 days
# Hospitalized -> 1 if the person had been hospitalized in the past year
ensanut_person_2018 <- ensanut_person_2018 %>% mutate(
  disability_id = as.integer(f1_s2_11 %in% 1),
  sick = as.integer(f1_s4_2 == 1),
  got_care = as.integer(f1_s4_6 %in% c(1,2,4)),
  prev_care = as.integer(f1_s4_41 == 1),
  hospitalized = as.integer(f1_s4_54 == 1))

## Outcome variables --------------------
# Good health -> 1 if the person considers themself in good (3), very good (2), or excellent (1) health
ensanut_person_2018$good_health <- as.integer(between(ensanut_person_2018$f1_s4_58,1,3))

# Better health -> 1 if the person considers themselves in better health compared to the previous year
ensanut_person_2018$better_health <- as.integer(ensanut_person_2018$f1_s4_59 == 1)

## Home infrastructure --------------------
# Ceiling -> concrete, fibre cement, roof tiles, zinc roof, or reed
ensanut_home_2018$ceiling <- as_factor(ensanut_home_2018$f1_s1_3)
levels(ensanut_home_2018$ceiling) <- 
  c("concrete, fibre cement (eternit), roof tiles", "concrete, fibre cement (eternit), roof tiles", 
    "zinc roof", "concrete, fibre cement (eternit), roof tiles", "reed, other", "reed, other")

# Floor -> ceramic tile, marble, treated planks, untreated planks, reed, dirt
ensanut_home_2018$floor <- as_factor(ensanut_home_2018$f1_s1_4)
levels(ensanut_home_2018$floor) <- 
  c("ceramic tile, marble, treated planks", "ceramic tile, marble, treated planks", 
    "ceramic tile, marble, treated planks", "concrete, bricks, untrated planks", 
    "concrete, bricks, untrated planks", "reed, dirt, other", "reed, dirt, other", "reed, dirt, other")

# Walls -> concrete, bricks, asbestos, adobe, rammed earth, wood, reed
ensanut_home_2018$walls <- as_factor(ensanut_home_2018$f1_s1_5)
levels(ensanut_home_2018$walls) <- 
  c("concrete, bricks, asbestos", "concrete, bricks, asbestos", "adobe, rammed earth, wood", 
    "adobe, rammed earth, wood", "reed, other", "reed, other", "reed, other")

## Put everything together --------------------
# Merge the ensanut_person_2018 with the ensanut_home_2018 dataset
survey_clean_2018 <- ensanut_person_2018 %>% 
  left_join(select(ensanut_home_2018, home_id, ceiling, floor, walls), by = "home_id")

survey_clean_2018 <- survey_clean_2018 %>% select(
  # home infrastructure and demographic variables
   home_id, person_id, psu, strata, weight, survey_date, survey_weekday, survey_round, 
   region, province_id, canton_id, parish_id, ceiling, floor, walls, sex, age, ethnicity, 
   marital_status, education, enrolled_in_school, employed, log_income_percap,
   # health variables
   disability_id, sick, got_care, prev_care, hospitalized, good_health, better_health) %>%
  rename(income = log_income_percap)

# Ensanut 2012 --------------------
# Load the Ensanut 2012 individual and household data
ensanut_home_2012 <- read_dta("data/ensanut_f1_vivienda.dta")
ensanut_person_2012 <- read_dta("data/ensanut_f1_personas.dta")
ensanut_home_2012_labels <- var_label(ensanut_home_2012)
ensanut_person_2012_labels <- var_label(ensanut_person_2012)

# Unique identifier
ensanut_person_2012$person_id <- ensanut_person_2012$idpers
ensanut_person_2012$home_id <- as.character(ensanut_person_2012$idhog)
ensanut_home_2012$home_id <- as.character(ensanut_home_2012$idhog)

# Survey weight
ensanut_person_2012$weight <- ensanut_person_2012$pw

# Survey date
ensanut_person_2012 <- ensanut_person_2012 %>% mutate(
  survey_date = as.Date(paste(anio, mes, dia, sep = "-")),
  survey_weekday = factor(weekdays(survey_date, abbreviate = TRUE), 
                          levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
  survey_round = "round 1")

# Region
ensanut_person_2012$region <- as_factor(ensanut_person_2012$subreg)
levels(ensanut_person_2012$region) <- 
  c("Sierra", "Sierra", "Coast", "Coast", "Amazon", "Amazon", "Galapagos", "Coast", "Sierra")

# Province, canton, and parish
ensanut_person_2012 <- ensanut_person_2012 %>%
  mutate(province_id = prov, 
         canton_id = case_when(prov >= 10 ~ substr(ciudad, 1,4), prov < 10 ~ paste(0, substr(ciudad, 1,3), sep = "")),
         parish_id = case_when(prov >= 10 ~ as.character(ciudad), prov < 10 ~ paste(0, ciudad, sep = "")))

## Demographic variables --------------------
# Sex
ensanut_person_2012$sex <- as_factor(ensanut_person_2012$pd02)
levels(ensanut_person_2012$sex) <- c("male", "female")

# Age
ensanut_person_2012$age <- ensanut_person_2012$edadanio

# Ethnicity
ensanut_person_2012$ethnicity <- as_factor(ensanut_person_2012$gr_etn)
levels(ensanut_person_2012$ethnicity) <- c("indigenous", "black", "non-minority", "non-minority", "non-minority")
ensanut_person_2012$ethnicity <- 
  factor(ensanut_person_2012$ethnicity, levels = c("non-minority", "indigenous", "black"))

# Marital status
ensanut_person_2012$marital_status <- as_factor(ensanut_person_2012$estado_civil)
levels(ensanut_person_2012$marital_status) <- c("partnered", "non-partnered", "non-partnered", "non-partnered")
ensanut_person_2012$marital_status <- 
  factor(ensanut_person_2012$marital_status, levels = c("non-partnered", "partnered"))

# Educational attainment
ensanut_person_2012$education <- as_factor(ensanut_person_2012$pd19a)
levels(ensanut_person_2012$education) <- 
  c("none", "none", "none", "primary", "secondary", "primary", "secondary", "secondary", "tertiary", "tertiary")

# Enrolled in school -> 1 if the person is enrolled in school
ensanut_person_2012$enrolled_in_school <- as.integer(ensanut_person_2012$pd16 == 1)

# Employed -> 1 if the person worked in the previous week or has a job to return to
ensanut_person_2012 <- ensanut_person_2012 %>% mutate(
  employed = case_when(is.na(pa01) ~ NA_real_, pa01 == 6 | pa01 == 7 ~ 1, TRUE ~ 0))

# Household income per capita
ensanut_person_2012 <- ensanut_person_2012 %>% group_by(home_id) %>% 
  mutate(income_percap = sum(pa08, na.rm = TRUE)/n()) %>% 
  mutate(log_income_percap = ifelse(income_percap <= 1, 0, log(income_percap))) %>%
  ungroup()

## Health variables --------------------
# Disability id -> 1 if the person has a disability id
# Sick -> 1 if the person suffered from an illness in the past 30 days
# Got care -> 1 if the person received care to treat the reported illness
# Preventive care -> 1 if the person received preventive care in the past 30 days
# Hospitalized -> 1 if the person had been hospitalized in the past year
ensanut_person_2012 <- ensanut_person_2012 %>% mutate(
  sick = as.integer(ps02 == 1),
  got_care = as.integer(ps06 %in% c(1,2,4)),
  prev_care = as.integer(ps40 == 1),
  hospitalized = as.integer(ps55 == 1))

## Outcome variables --------------------
# Good health -> 1 if the person considers themself in good (3), very good (2), or excellent (1) health
ensanut_person_2012$good_health <- as.integer(between(ensanut_person_2012$ps71,1,3))

# Better health -> 1 if the person considers themselves in better health compared to the previous year
ensanut_person_2012$better_health <- as.integer(ensanut_person_2012$ps72 == 1)

## Home infrastructure --------------------
# Ceiling -> concrete, fibre cement, roof tiles, zinc roof, or reed
ensanut_home_2012$ceiling <- as_factor(ensanut_home_2012$vi03)
levels(ensanut_home_2012$ceiling) <- 
  c("concrete, fibre cement (eternit), roof tiles", "concrete, fibre cement (eternit), roof tiles", 
    "zinc roof", "concrete, fibre cement (eternit), roof tiles", "reed, other", "reed, other")

# Floor -> ceramic tile, marble, treated planks, untreated planks, reed, dirt
ensanut_home_2012$floor <- as_factor(ensanut_home_2012$vi05)
levels(ensanut_home_2012$floor) <- 
  c("ceramic tile, marble, treated planks", "concrete, bricks, untrated planks", 
    "ceramic tile, marble, treated planks", "concrete, bricks, untrated planks", 
    "reed, dirt, other", "reed, dirt, other", "reed, dirt, other")

# Walls -> concrete, bricks, asbestos, adobe, rammed earth, wood, reed
ensanut_home_2012$walls <- as_factor(ensanut_home_2012$vi04)
levels(ensanut_home_2012$walls) <- 
  c("concrete, bricks, asbestos", "concrete, bricks, asbestos", "adobe, rammed earth, wood", 
    "adobe, rammed earth, wood", "reed, other", "reed, other", "reed, other")

# Air conditioner -> 1 if the household owns an air conditioner
ensanut_home_2012$ac <- as.integer(ensanut_home_2012$vi2015 == 1 | ensanut_home_2012$vi2017 == 1 |
                                     ensanut_home_2012$vi2018 == 1 | ensanut_home_2012$vi2019 == 1)

# Fan -> 1 if the household owns a fan
ensanut_home_2012$fan <- as.integer(ensanut_home_2012$vi2016 == 1)

# Hot water -> 1 if the household has hot water system
ensanut_home_2012$hot_water <- as.integer(ensanut_home_2012$vi2013 == 1 | ensanut_home_2012$vi2014 == 1)

## Put everything together --------------------
# Merge the ensanut_person_2012 with the ensanut_home_2012 dataset
survey_clean_2012 <- ensanut_person_2012 %>% 
  left_join(select(ensanut_home_2012, home_id, ceiling, floor, walls, ac, fan, hot_water), by = "home_id")

survey_clean_2012 <- survey_clean_2012 %>% select(
  # home infrastructure and demographic variables
  home_id, person_id, weight, survey_date, survey_weekday, survey_round, region, 
  province_id, canton_id, parish_id, ceiling, floor, walls, ac, fan, hot_water, 
  sex, age, ethnicity, marital_status, education, enrolled_in_school, employed, log_income_percap,
  # health variables
  sick, got_care, prev_care, hospitalized, good_health, better_health) %>%
  rename(income = log_income_percap)
