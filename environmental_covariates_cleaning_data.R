# temperature, precipitation, runoff - 
# time to clean up from 2007 to 2023 for each municipality for each month

library(tidyverse)

################## TEMPERATURE ###################

getwd()
setwd('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/Brazil GEE Data')

################## TEMPERATURE MEAN CLEAN UP ###################

temperature_mean <- read.csv('brazil_temperature_monthly_mean.csv')
temperature_mean <- temperature_mean[, -1]

colnames(temperature_mean) <- gsub("_temperature_2m", "", colnames(temperature_mean))
colnames(temperature_mean) <- gsub("X", "", colnames(temperature_mean))

test_of_things <- temperature_mean %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Mean Temperature')


temperature_mean_long <- temperature_mean %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Mean Temperature')

temperature_mean_long <- temperature_mean_long[, -3]
temperature_mean_long <- temperature_mean_long[, -3]
temperature_mean_long <- temperature_mean_long[, -3]

temperature_mean_long$Month <- substr(temperature_mean_long$Year, 5, 6)
temperature_mean_long$Year <- substr(temperature_mean_long$Year, 1, 4)

temperature_mean_long <- temperature_mean_long[, -1]
################## TEMPERATURE MAX CLEAN UP ###################

temperature_max <- read.csv('brazil_temperature_monthly_max.csv')

temperature_max <- temperature_max[, -1]

colnames(temperature_max) <- gsub("_temperature_2m_max", "", colnames(temperature_max))
colnames(temperature_max) <- gsub("X", "", colnames(temperature_max))

temperature_max_long <- temperature_max %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Maximum Temperature')

temperature_max_long <- temperature_max_long[, -3]
temperature_max_long <- temperature_max_long[, -3]
temperature_max_long <- temperature_max_long[, -3]

temperature_max_long$Month <- substr(temperature_max_long$Year, 5, 6)
temperature_max_long$Year <- substr(temperature_max_long$Year, 1, 4)

temperature_max_long <- temperature_max_long[, -1]
################## TEMPERATURE MIN CLEAN UP ###################

temperature_min <- read.csv('brazil_temperature_monthly_min.csv')

temperature_min <- temperature_min[, -1]

colnames(temperature_min) <- gsub("_temperature_2m_min", "", colnames(temperature_min))
colnames(temperature_min) <- gsub("X", "", colnames(temperature_min))

temperature_min_long <- temperature_min %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Minimum Temperature')

temperature_min_long <- temperature_min_long[, -3]
temperature_min_long <- temperature_min_long[, -3]
temperature_min_long <- temperature_min_long[, -3]

temperature_min_long$Month <- substr(temperature_min_long$Year, 5, 6)
temperature_min_long$Year <- substr(temperature_min_long$Year, 1, 4)

temperature_min_long <- temperature_min_long[, -1]
################## TEMPERATURE ###################
################# Temperature Data Merging ########

temperatures_all_long <- left_join(x = temperature_max_long,
                                   y = temperature_mean_long,
                                   by = c("CD_MUN" = "CD_MUN",
                                          "Year" = "Year",
                                          "Month" = "Month"))

temperatures_all_long <- left_join(x = temperatures_all_long,
                                   y = temperature_min_long,
                                   by = c("CD_MUN" = "CD_MUN",
                                          "Year" = "Year",
                                          "Month" = "Month"))

################## RUNOFF ###################


################## RUNOFF MIN CLEAN UP DATA ###################
runoff_min <- read.csv('brazil_runoff_monthly_min.csv')

runoff_min <- runoff_min[, -1]

colnames(runoff_min) <- gsub("_runoff_min", "", colnames(runoff_min))
colnames(runoff_min) <- gsub("X", "", colnames(runoff_min))

runoff_min_long <- runoff_min %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Minimum Runoff')

runoff_min_long <- runoff_min_long[, -3]
runoff_min_long <- runoff_min_long[, -3]
runoff_min_long <- runoff_min_long[, -3]

runoff_min_long$Month <- substr(runoff_min_long$Year, 5, 6)
runoff_min_long$Year <- substr(runoff_min_long$Year, 1, 4)

runoff_min_long <- runoff_min_long[, -1]

################## RUNOFF MAX CLEAN UP DATA ###################

runoff_max <- read.csv("brazil_runoff_monthly_max.csv")

runoff_max <- runoff_max[, -1]

colnames(runoff_max) <- gsub("_runoff_max", "", colnames(runoff_max))
colnames(runoff_max) <- gsub("X", "", colnames(runoff_max))

runoff_max_long <- runoff_max %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Maximum Runoff')

runoff_max_long <- runoff_max_long[, -3]
runoff_max_long <- runoff_max_long[, -3]
runoff_max_long <- runoff_max_long[, -3]

runoff_max_long$Month <- substr(runoff_max_long$Year, 5, 6)
runoff_max_long$Year <- substr(runoff_max_long$Year, 1, 4)

runoff_max_long <- runoff_max_long[, -1]

################## RUNOFF ###################
################# Runoff Data Merging ########

runoff_all_long <- left_join(x = runoff_max_long,
                             y = runoff_min_long,
                             by = c("CD_MUN" = "CD_MUN",
                                    "Year" = "Year",
                                    "Month" = "Month"))


################## PRECIPITATION ##################


################# MAXIMUM PRECIPITATION DATA CLEAN UP ########

precipitation_maximum <- read.csv('Brazil_precipitation_monthly_max.csv')

precipitation_maximum <- precipitation_maximum[, -1]

colnames(precipitation_maximum) <- gsub("_total_precipitation_max", "", colnames(precipitation_maximum))
colnames(precipitation_maximum) <- gsub("X", "", colnames(precipitation_maximum))

precipitation_maximum_long <- precipitation_maximum %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Maximum Precipitation')

precipitation_maximum_long <- precipitation_maximum_long[, -3]
precipitation_maximum_long <- precipitation_maximum_long[, -3]
precipitation_maximum_long <- precipitation_maximum_long[, -3]

precipitation_maximum_long$Month <- substr(precipitation_maximum_long$Year, 5, 6)
precipitation_maximum_long$Year <- substr(precipitation_maximum_long$Year, 1, 4)

precipitation_maximum_long <- precipitation_maximum_long[, -1]

################# MEAN PRECIPITATION DATA CLEAN UP ########

precipitation_mean <- read.csv('Brazil_precipitation_monthly_mean.csv')

precipitation_mean <- precipitation_mean[, -1]

colnames(precipitation_mean) <- gsub("_total_precipitation_sum", "", colnames(precipitation_mean))
colnames(precipitation_mean) <- gsub("X", "", colnames(precipitation_mean))

precipitation_mean_long <- precipitation_mean %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Mean Precipitation')

precipitation_mean_long <- precipitation_mean_long[, -3]
precipitation_mean_long <- precipitation_mean_long[, -3]
precipitation_mean_long <- precipitation_mean_long[, -3]

precipitation_mean_long$Month <- substr(precipitation_mean_long$Year, 5, 6)
precipitation_mean_long$Year <- substr(precipitation_mean_long$Year, 1, 4)

precipitation_mean_long <- precipitation_mean_long[, -1]

################# MINIMUM PRECIPITATION DATA CLEAN UP ########

precipitation_minimum <- read.csv('Brazil_precipitation_monthly_min.csv')

precipitation_minimum <- precipitation_minimum[, -1]

colnames(precipitation_minimum) <- gsub("_total_precipitation_min", "", colnames(precipitation_minimum))
colnames(precipitation_minimum) <- gsub("X", "", colnames(precipitation_minimum))

precipitation_min_long <- precipitation_minimum %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Minimum Precipitation')

precipitation_min_long <- precipitation_min_long[, -3]
precipitation_min_long <- precipitation_min_long[, -3]
precipitation_min_long <- precipitation_min_long[, -3]

precipitation_min_long$Month <- substr(precipitation_min_long$Year, 5, 6)
precipitation_min_long$Year <- substr(precipitation_min_long$Year, 1, 4)

precipitation_min_long <- precipitation_min_long[, -1]

precipitation_min_long$`Minimum Precipitation` <- (precipitation_min_long$`Minimum Precipitation`)*(-1) 

################## Precipitation ###################
################# Precipitation Data Merging ########

precipitation_all_long <- left_join(x = precipitation_maximum_long,
                                    y = precipitation_mean_long,
                                    by = c("CD_MUN" = "CD_MUN",
                                           "Year" = "Year",
                                           "Month" = "Month"))
precipitation_all_long <- left_join(x = precipitation_all_long,
                                    y = precipitation_min_long,
                                    by = c("CD_MUN" = "CD_MUN",
                                           "Year" = "Year",
                                           "Month" = "Month"))

environmental_covariates_df <- left_join(x = temperatures_all_long,
                                         y = precipitation_all_long,
                                         by = c("CD_MUN" = "CD_MUN",
                                                "Year" = "Year",
                                                "Month" = "Month"))

environmental_covariates_df <- left_join(x = environmental_covariates_df,
                                         y = runoff_all_long,
                                         by = c("CD_MUN" = "CD_MUN",
                                                "Year" = "Year",
                                                "Month" = "Month"))
write.csv(environmental_covariates_df, '/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/combined_environmental_covariates_data.csv')

testing_geocodes <- read.csv('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/combined_environmental_covariates_data.csv')

testing_geocodes <- testing_geocodes[, -1]

remove_seventh_digit <- function(cd_mun) {
  cd_mun_str <- as.character(cd_mun)
  if (nchar(cd_mun_str) >= 7) {
    return(paste0(substr(cd_mun_str, 1, 6), substr(cd_mun_str, 8, nchar(cd_mun_str))))
  } else {
    return(cd_mun_str)  # Return as is if less than 7 characters
  }
}




testing_geocodes$CD_MUN <- sapply(testing_geocodes$CD_MUN, remove_seventh_digit) 

testing_geocodes$CD_MUN <- as.numeric(testing_geocodes$CD_MUN)

write.csv(testing_geocodes, '/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/prepped_environmental_covariates_data.csv')
lepto_cases_test <- read.csv('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/finalized_lepto_cases_df.csv')
lepto_cases_test <- lepto_cases_test[, -1]
trial_six_digit_geocode <- left_join(x = testing_geocodes,
                                     y = lepto_cases_test,
                                     by = c("CD_MUN" = "Municipality",
                                            "Month" = "Month",
                                            "Year" = "Year"))


############### Soil Temperature ####

soil_temperature <- read.csv('brazil_soil_temperature_monthly_level1.csv')

soil_temperature <- soil_temperature[, -1]

colnames(soil_temperature) <- gsub("_soil_temperature_level_1", "", colnames(soil_temperature))
colnames(soil_temperature) <- gsub("X", "", colnames(soil_temperature))

soil_temperature_long <- soil_temperature %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Soil Temperature L1')

soil_temperature_long <- soil_temperature_long[, -3]
soil_temperature_long <- soil_temperature_long[, -3]
soil_temperature_long <- soil_temperature_long[, -3]

soil_temperature_long$Month <- substr(soil_temperature_long$Year, 5, 6)
soil_temperature_long$Year <- substr(soil_temperature_long$Year, 1, 4)

soil_temperature_long <- soil_temperature_long[, -1]


############# Total Sum Precipitation ##########

total_sum_precip <- read.csv('Brazil_municipalities_precipitation_monthly_total_sum.csv')

total_sum_precip <- total_sum_precip[, -1]

colnames(total_sum_precip) <- gsub("_total_precipitation_sum", "", colnames(total_sum_precip))
colnames(total_sum_precip) <- gsub("X", "", colnames(total_sum_precip))

total_sum_precip_long <- total_sum_precip %>%
  pivot_longer(cols = 1:204,
               names_to = 'Year',
               values_to = 'Total Precipitation Sum')

total_sum_precip_long <- total_sum_precip_long[, -3]
total_sum_precip_long <- total_sum_precip_long[, -3]
total_sum_precip_long <- total_sum_precip_long[, -3]

total_sum_precip_long$Month <- substr(total_sum_precip_long$Year, 5, 6)
total_sum_precip_long$Year <- substr(total_sum_precip_long$Year, 1, 4)

total_sum_precip_long <- total_sum_precip_long[, -1]

### combining these two ###

soil_temp_and_total_precip <- left_join(x = soil_temperature_long,
                                        y = total_sum_precip_long,
                                        by = c("CD_MUN" = "CD_MUN",
                                               "Year" = "Year",
                                               "Month" = "Month"))


write.csv(soil_temp_and_total_precip, '/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/new_environmental_covariates.csv')


###### adding in soil pH

soil_pH <- read.csv('Soil_pH_values.csv')

soil_pH <- soil_pH[, -1]
soil_pH <- soil_pH[, -1]

