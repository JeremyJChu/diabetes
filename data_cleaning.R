#install.packages("readxl")
#install.packages("anytime")
library(tidyverse)
library(readxl)
library(anytime)
library(lubridate)

excel_sheets("inputs/data/05_May_2020_raw.xls")

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
drop <- c('Immediate Volume (U)', 'Extended Volume (U)', 'Duration (min)', 'Notes', 'Serial number')          
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


# Simple Plotting
test <- Insulin_Carbs_Grouped %>%
  filter(year == 2020 & month == 'May') %>%
  ggplot(aes(x = carbs, y = bolus_volume)) +
  geom_point()

show(test)

#### Data Transformation for CGM Dataset ####

# Getting rid of unwanted columns
drop_cgm <- c('...3','Serial number')
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

