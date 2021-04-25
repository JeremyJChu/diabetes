#### Preamble ####
# Purpose: This script is used to create clean and combine all the raw data collected from the CGM. 
# Data: 24 April 2021
# Contact: jeremychuj@gmail.com
# License: MIT
# Pre-requisites: 
# - None

#### Workspace setup ####
#install.packages("readxl")
#install.packages("anytime")
library(tidyverse)
library(readxl)
library(anytime)
library(lubridate)

#### Loading in Data ####

# December 2019
CGM_December_2019 = read_excel("inputs/data/00_December_2019_raw.xls", sheet = "CGM")
Insulin_Carbs_December_2019 = read_excel("inputs/data/00_December_2019_raw.xls", sheet = "Insulin use and carbs")

# January 2020
CGM_January_2020 = read_excel("inputs/data/01_January_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_January_2020 = read_excel("inputs/data/01_January_2020_raw.xls", sheet = "Insulin use and carbs")

# May 2020
CGM_May_2020 = read_excel("inputs/data/05_May_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_May_2020 = read_excel("inputs/data/05_May_2020_raw.xls", sheet = "Insulin use and carbs")

# June 2020
CGM_June_2020 = read_excel("inputs/data/06_June_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_June_2020 = read_excel("inputs/data/06_June_2020_raw.xls", sheet = "Insulin use and carbs")

# July 2020
CGM_July_2020 = read_excel("inputs/data/07_July_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_July_2020 = read_excel("inputs/data/07_July_2020_raw.xls", sheet = "Insulin use and carbs")

# August 2020
CGM_August_2020 = read_excel("inputs/data/08_August_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_August_2020 = read_excel("inputs/data/08_August_2020_raw.xls", sheet = "Insulin use and carbs")

# September 2020
CGM_September_2020 = read_excel("inputs/data/09_September_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_September_2020 = read_excel("inputs/data/09_September_2020_raw.xls", sheet = "Insulin use and carbs")

# October 2020
CGM_October_2020 = read_excel("inputs/data/10_October_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_October_2020 = read_excel("inputs/data/10_October_2020_raw.xls", sheet = "Insulin use and carbs")

# November 2020
CGM_November_2020 = read_excel("inputs/data/11_November_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_November_2020 = read_excel("inputs/data/11_November_2020_raw.xls", sheet = "Insulin use and carbs")

# December 2020
CGM_December_2020 = read_excel("inputs/data/12_December_2020_raw.xls", sheet = "CGM")
Insulin_Carbs_December_2020 = read_excel("inputs/data/12_December_2020_raw.xls", sheet = "Insulin use and carbs")

# Merge CGM
CGM_full <- rbind(CGM_December_2019, CGM_January_2020, CGM_May_2020, CGM_June_2020, CGM_July_2020, CGM_August_2020, CGM_September_2020,
      CGM_October_2020, CGM_November_2020, CGM_December_2020)

# Merge Insulin_Carbs
Insulin_Carbs_Full <- rbind(Insulin_Carbs_December_2019,
                            Insulin_Carbs_January_2020,
                            Insulin_Carbs_May_2020,
                            Insulin_Carbs_June_2020,
                            Insulin_Carbs_July_2020,
                            Insulin_Carbs_August_2020,
                            Insulin_Carbs_September_2020,
                            Insulin_Carbs_October_2020,
                            Insulin_Carbs_November_2020,
                            Insulin_Carbs_December_2020)

#### Data Transformation For Insulin Carbs Dataset ####

# Getting rid of unwanted columns
drop <- c('Immediate Volume (U)', 'Extended Volume (U)', 'Duration (min)', 'Notes')          
Insulin_Carbs_Cleaned <- Insulin_Carbs_Full[, !(names(Insulin_Carbs_Full) %in% drop)]

# Dealing with date time
Insulin_Carbs_Cleaned$Time <- anytime(Insulin_Carbs_Cleaned$Time)

# Renaming columns for sanity
Insulin_Carbs_Cleaned <- 
  Insulin_Carbs_Cleaned %>%
  rename(
    time = Time,
    basal_amount = `Basal Amount (U/h)`,
    bolus_type = `Bolus Type`,
    bolus_volume = `Bolus Volume (U)`,
    carbs = `Carbs(g)`,
    total_daily_dose = `Total daily dose`,
    total_daily_basal = `Total daily basal`
  )

# Add a column for day, month, and year for easier filtering
Insulin_Carbs_Cleaned <-
  Insulin_Carbs_Cleaned %>%
  mutate(day = day(Insulin_Carbs_Cleaned$time))

Insulin_Carbs_Cleaned <-
  Insulin_Carbs_Cleaned %>%
  mutate(month = months(Insulin_Carbs_Cleaned$time))

Insulin_Carbs_Cleaned <-
  Insulin_Carbs_Cleaned %>%
  mutate(year = year(Insulin_Carbs_Cleaned$time))

Insulin_Carbs_Cleaned <-
  Insulin_Carbs_Cleaned %>%
  mutate(hour = hour(Insulin_Carbs_Cleaned$time))

# Since the data is split by seconds, it's near impossible to plot anything decently.
# I decided to aggregate by year -> month -> day -> hour instead so I get the carb, basal and bolus intake by hour
# It shouldn't affect too much except for weird edge cases where the carb intake was a minute before the hour changed

Insulin_Carbs_Grouped <-
  Insulin_Carbs_Cleaned %>% 
  group_by(year, month, day, hour) %>% summarise(basal_amount = sum(basal_amount, na.rm = TRUE),
                                                 bolus_volume =  sum(bolus_volume, na.rm=TRUE),
                                                 carbs = sum(carbs, na.rm = TRUE))


#### Data Transformation for CGM Dataset ####

# Getting rid of unwanted columns
drop_cgm <- c('...3')
CGM_Cleaned <- CGM_full[, !(names(CGM_full) %in% drop_cgm)]

# Dealing with date time
CGM_Cleaned$Time <- anytime(CGM_Cleaned$Time)

# Renaming columns for sanity
CGM_Cleaned <- 
  CGM_Cleaned %>%
  rename(
    time = Time,
    blood_sugar = `mmol/L`
  )

# Add a column for day, month, and year for easier filtering
CGM_Cleaned <-
  CGM_Cleaned %>%
  mutate(day = day(CGM_Cleaned$time))

CGM_Cleaned <-
  CGM_Cleaned %>%
  mutate(month = months(CGM_Cleaned$time))

CGM_Cleaned <-
  CGM_Cleaned %>%
  mutate(year = year(CGM_Cleaned$time))

CGM_Cleaned <-
  CGM_Cleaned %>%
  mutate(hour = hour(CGM_Cleaned$time))

# Grouping and summarizing similar to Insulin_Carbs dataset.
# This time taking the mean of blood sugar levels per hour

CGM_Grouped <-
  CGM_Cleaned %>% 
  group_by(year, month, day, hour) %>% summarise(blood_sugar = mean(blood_sugar, na.rm = TRUE))

# Merging both dataframes together
diabetes_full <- merge(CGM_Grouped, Insulin_Carbs_Grouped, all = TRUE)

# Adding time of day labels

diabetes_full <-
  diabetes_full %>%
  mutate(time_of_day = case_when(
    hour >= 4 & hour <= 11 ~ 'morning',
    hour >= 12 & hour <= 18 ~ 'afternoon',
    hour >= 19 & hour <= 23  ~ 'evening',
    hour <= 3 ~ 'evening')
    )

diabetes_full <-
  diabetes_full %>%
  mutate(sleep = case_when(
    hour >= 8 & hour <= 23 ~ 'awake',
    hour >= 0 & hour <= 7 ~ 'asleep')
  )


# Adding low, high, in_range labels

diabetes_full <-
  diabetes_full %>%
  mutate(range = case_when(
    blood_sugar < 4 ~ 'low',
    blood_sugar >= 4 & blood_sugar < 11 ~ 'in_range',
    blood_sugar >= 11 ~ 'high')
  )

# Turning NA into 0
# First copying original df since I might want a non 0 version
diabetes_full_zero <- diabetes_full
diabetes_full_zero[is.na(diabetes_full_zero)] <- 0

# One Hot Encoding of time_of_day
diabetes_full <-
  diabetes_full %>%
  mutate(time_of_day_coded = case_when(
    time_of_day == 'morning' ~ 1,
    time_of_day == 'afternoon' ~ 2,
    time_of_day == 'evening' ~ 3)
    )

# One Hot Encoding range

diabetes_full <-
  diabetes_full %>%
  mutate(range_coded = case_when(
    range == 'low' ~ 1,
    range == 'in_range' ~ 2,
    range == 'high' ~ 3)
  )

# Bolus with and without carbs
diabetes_full <-
  diabetes_full %>%
  mutate(insulin_food = case_when(
    carbs == 0 & bolus_volume > 0 ~ 1,
    carbs > 0 & bolus_volume > 0 ~ 2,
    carbs > 0 & bolus_volume == 0 ~ 3  
  ))

# Creating a meals per day subset
meals_per_day <-
  diabetes_full %>%
  group_by(year,month,day) %>% 
  filter(carbs != 0) %>% 
  summarise(meals_day = sum(!is.na(carbs)),
            average_bs = mean(blood_sugar, na.rm = TRUE))

meals_per_day <-
  meals_per_day %>%
  mutate(range = case_when(
    average_bs < 4 ~ 'low',
    average_bs >= 4 & average_bs < 11 ~ 'in_range',
    average_bs >= 11 ~ 'high')
  )

# Eat before bed
diabetes_full <-
  diabetes_full %>%
  mutate(bed_snack = case_when(
    hour >= 0 & hour <= 3 ~ 'awake',
    hour >= 0 & hour <= 7 ~ 'asleep')
  )

# Sleep Data

sleep_data <- diabetes_full %>% filter(diabetes_full$sleep == 'asleep')
sleep_data <- 
  sleep_data %>% mutate(bed_food = case_when(
    carbs > 0 ~ 1,
    carbs == 0 ~ 0,
    is.na(carbs) ~ 0
  ))

# Wake up Range
sleep_data <- 
  sleep_data %>% mutate(wakeup_range = case_when(
    hour == 7 & range == 'in_range' ~ 0,
    hour == 7 & range == 'high' ~ 1,
    hour == 7 & range == 'low' ~ 2
  )) 

# Sleep Level
sleep_data <- sleep_data %>% mutate(sleep_level = case_when(
  hour == 0 ~ blood_sugar
))

# Wakeup Level 
sleep_data <- sleep_data %>% mutate(wakeup_level = case_when(
  hour == 7 ~ blood_sugar
))

# Grouping and Aggregating Sleep Data by Day
sleep_data_cleaned <- sleep_data %>% group_by(year, month, day) %>% summarise(bed_food = sum(bed_food, na.rm = TRUE),
                                                        bed_carbs = sum(carbs, na.rm = TRUE),
                                                        wakeup_range = sum(wakeup_range, na.rm = TRUE),
                                                        night_insulin = sum(bolus_volume, na.rm = TRUE),
                                                        sleep_level = sum(sleep_level, na.rm = TRUE),
                                                        wakeup_level = sum(wakeup_level, na.rm = TRUE))


# Encoding insulin taken at night, 0 = no, 1 = yes
sleep_data_cleaned <- 
  sleep_data_cleaned %>% mutate(any_insulin = case_when(
    night_insulin == 0 ~ 0,
    night_insulin > 0 ~ 1
  ))

# Writing and saving data
write_csv(diabetes_full, "inputs/data/13_diabetes-full.csv")
write_csv(sleep_data_cleaned, "inputs/data/14_sleep-data_cleaned.csv")