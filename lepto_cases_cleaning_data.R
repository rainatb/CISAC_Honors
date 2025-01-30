# # # Leptospirosis Cases Data Cleaning # # #

getwd()
setwd('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/month_municipality_data_2007_2024')

# adding in Brazil shapefiles
library(sf)
municipalities <- read_sf('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/BR_Municipios_2021/BR_Municipios_2021.shp')
municipalities <- st_as_sf(municipalities)
municipalities$geometry <- st_transform(municipalities$geometry, 4326)
library(ggplot2)
municipality_plot <- ggplot() +
  geom_sf(data = municipalities, aes(geometry = geometry)) +
  theme_minimal() 

municipality_plot
# I will begin by cleaning the dataframes for leptospirosis data

# starting with 2007

lepto_cases_municipality_2007 <- read.csv('2007_data_municipality_month.csv')
lepto_cases_municipality_2007 <- lepto_cases_municipality_2007[, -14]

colnames(lepto_cases_municipality_2007) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")
# setting all - values to 0 and pivoting longer
library(tidyverse)
library(dplyr)

lepto_cases_municipality_2007 <- lepto_cases_municipality_2007 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2007_long <- lepto_cases_municipality_2007 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2007_long$Year <- 2007
lepto_cases_municipality_2007_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2007_long$Municipality)

# now I will clean up the dataframe for 2008

lepto_cases_municipality_2008 <- read.csv('2008_data_month_municipality.csv')
lepto_cases_municipality_2008 <- lepto_cases_municipality_2008[, -14]

colnames(lepto_cases_municipality_2008) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2008 <- lepto_cases_municipality_2008 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2008_long <- lepto_cases_municipality_2008 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2008_long$Year <- 2008

lepto_cases_municipality_2008_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2008_long$Municipality)

# trying to get 2007 and 2008 into one dataframe
all_lepto_cases_2007_to_2023 <- rbind(lepto_cases_municipality_2007_long, lepto_cases_municipality_2008_long)
# cleaning up dataframe for 2009

lepto_cases_municipality_2009 <- read.csv('2009_data_month_municipality.csv')
lepto_cases_municipality_2009 <- lepto_cases_municipality_2009[, -14]

colnames(lepto_cases_municipality_2009) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2009 <- lepto_cases_municipality_2009 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2009_long <- lepto_cases_municipality_2009 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2009_long$Year <- 2009

lepto_cases_municipality_2009_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2009_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2009_long)
# cleaning up dataframe for 2010

lepto_cases_municipality_2010 <-read.csv('2010_data_month_municipatlity.csv')
lepto_cases_municipality_2010 <- lepto_cases_municipality_2010[, -14]

colnames(lepto_cases_municipality_2010) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2010 <- lepto_cases_municipality_2010 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2010_long <- lepto_cases_municipality_2010 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2010_long$Year <- 2010

lepto_cases_municipality_2010_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2010_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2010_long)

trial_run <- all_lepto_cases_2007_to_2023[grepl("^\\d{6}$", all_lepto_cases_2007_to_2023$Municipality), ]
# trial_run worked- will use this code at the end

# cleaning up dataframe for 2011
lepto_cases_municipality_2011 <- read.csv('2011_data_month_municipality.csv')

lepto_cases_municipality_2011 <- lepto_cases_municipality_2011[, -14]

colnames(lepto_cases_municipality_2011) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2011 <- lepto_cases_municipality_2011 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2011_long <- lepto_cases_municipality_2011 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2011_long$Year <- 2011

lepto_cases_municipality_2011_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2011_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2011_long)

# 2012 - cleaning up 

lepto_cases_municipality_2012 <- read.csv('2012_data_month_municipality.csv')

lepto_cases_municipality_2012 <- lepto_cases_municipality_2012[, -14]

colnames(lepto_cases_municipality_2012) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2012 <- lepto_cases_municipality_2012 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2012_long <- lepto_cases_municipality_2012 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2012_long$Year <- 2012

lepto_cases_municipality_2012_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2012_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2012_long)

# 2013- cleaning up data

lepto_cases_municipality_2013 <- read.csv('2013_data_month_municipality.csv')

lepto_cases_municipality_2013 <- lepto_cases_municipality_2013[, -14]

colnames(lepto_cases_municipality_2013) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2013 <- lepto_cases_municipality_2013 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2013_long <- lepto_cases_municipality_2013 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2013_long$Year <- 2013

lepto_cases_municipality_2013_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2013_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2013_long)


# 2014 - cleaning up data

lepto_cases_municipality_2014 <- read.csv('2014_data_month_municipality.csv')

lepto_cases_municipality_2014 <- lepto_cases_municipality_2014[, -14]

colnames(lepto_cases_municipality_2014) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2014 <- lepto_cases_municipality_2014 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2014_long <- lepto_cases_municipality_2014 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2014_long$Year <- 2014

lepto_cases_municipality_2014_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2014_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2014_long)

# 2015 - cleaning up

lepto_cases_municipality_2015 <- read.csv('2015_data_month_municipality.csv')

lepto_cases_municipality_2015 <- lepto_cases_municipality_2015[, -14]

colnames(lepto_cases_municipality_2015) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2015 <- lepto_cases_municipality_2015 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2015_long <- lepto_cases_municipality_2015 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2015_long$Year <- 2015

lepto_cases_municipality_2015_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2015_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2015_long)


# 2016 - data cleaning up 

lepto_cases_municipality_2016 <- read.csv('2016_data_month_municipality.csv')

lepto_cases_municipality_2016 <- lepto_cases_municipality_2016[, -14]

colnames(lepto_cases_municipality_2016) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2016 <- lepto_cases_municipality_2016 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2016_long <- lepto_cases_municipality_2016 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2016_long$Year <- 2016

lepto_cases_municipality_2016_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2016_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2016_long)

# 2017 - data cleaning up 

lepto_cases_municipality_2017 <- read.csv('2017_data_month_municipality.csv')

lepto_cases_municipality_2017 <- lepto_cases_municipality_2017[, -14]

colnames(lepto_cases_municipality_2017) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2017 <- lepto_cases_municipality_2017 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2017_long <- lepto_cases_municipality_2017 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2017_long$Year <- 2017

lepto_cases_municipality_2017_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2017_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2017_long)


# 2018 - data cleaning up 
lepto_cases_municipality_2018 <- read.csv('2018_data_month_municipality.csv')

lepto_cases_municipality_2018 <- lepto_cases_municipality_2018[, -14]

colnames(lepto_cases_municipality_2018) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2018 <- lepto_cases_municipality_2018 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2018_long <- lepto_cases_municipality_2018 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2018_long$Year <- 2018

lepto_cases_municipality_2018_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2018_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2018_long)


# cleaning up data from 2019

lepto_cases_municipality_2019 <- read.csv('2019_data_month_municipality.csv')

lepto_cases_municipality_2019 <- lepto_cases_municipality_2019[, -14]

colnames(lepto_cases_municipality_2019) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2019 <- lepto_cases_municipality_2019 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2019_long <- lepto_cases_municipality_2019 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2019_long$Year <- 2019

lepto_cases_municipality_2019_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2019_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2019_long)

# cleaning up data from 2020

lepto_cases_municipality_2020 <- read.csv('2020_data_month_municipality.csv')

lepto_cases_municipality_2020 <- lepto_cases_municipality_2020[, -14]

colnames(lepto_cases_municipality_2020) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2020 <- lepto_cases_municipality_2020 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2020_long <- lepto_cases_municipality_2020 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2020_long$Year <- 2020

lepto_cases_municipality_2020_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2020_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2020_long)

# cleaning up data from 2021

lepto_cases_municipality_2021 <- read.csv('2021_data_month_municipality.csv')

lepto_cases_municipality_2021 <- lepto_cases_municipality_2021[, -14]

colnames(lepto_cases_municipality_2021) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2021 <- lepto_cases_municipality_2021 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2021_long <- lepto_cases_municipality_2021 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2021_long$Year <- 2021

lepto_cases_municipality_2021_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2021_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2021_long)

# cleaning up data from 2022

lepto_cases_municipality_2022 <- read.csv('2022_data_month_municipality.csv')

lepto_cases_municipality_2022 <- lepto_cases_municipality_2022[, -14]

colnames(lepto_cases_municipality_2022) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2022 <- lepto_cases_municipality_2022 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2022_long <- lepto_cases_municipality_2022 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2022_long$Year <- 2022

lepto_cases_municipality_2022_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2022_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2022_long)

# 2023 finally

lepto_cases_municipality_2023 <- read.csv('2023_data_month_municipality.csv')

lepto_cases_municipality_2023 <- lepto_cases_municipality_2023[, -14]

colnames(lepto_cases_municipality_2023) <- c("Municipality", "1", "2", "3", "4", "5", "6", "7", "8",
                                             "9", "10", "11", "12")

lepto_cases_municipality_2023 <- lepto_cases_municipality_2023 %>%
  mutate_all(~replace(., . == "-", 0))

lepto_cases_municipality_2023_long <- lepto_cases_municipality_2023 %>%
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "Cases")

lepto_cases_municipality_2023_long$Year <- 2023

lepto_cases_municipality_2023_long$Municipality <- sub("^(\\d{6}).*", "\\1", lepto_cases_municipality_2023_long$Municipality)

all_lepto_cases_2007_to_2023 <- rbind(all_lepto_cases_2007_to_2023, lepto_cases_municipality_2023_long)


finalized_lepto_dataframe <- all_lepto_cases_2007_to_2023[grepl("^\\d{6}$", all_lepto_cases_2007_to_2023$Municipality), ]

write.csv(finalized_lepto_dataframe, '/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/finalized_lepto_cases_df.csv')
