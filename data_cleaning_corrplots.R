# another round of coherent cleaning for corrplot 
# version 2

testing <- read.csv('new_spatio_temporal_cv_data.csv')
testing <- testing[, -1]
testing <- testing[, -5]
testing <- testing[, -6]
testing <- testing[, -6]
testing <- testing[, -17]

testing_new <- left_join(x = testing,
                         y = soil_pH_unique,
                         by = c("CD_MUN"))

testing_new <- testing_new[, -8]
testing_new <- testing_new[, -8]
testing_new <- testing_new[, -8]
testing_new <- testing_new[, -8]
testing_new <- testing_new[, -8]
testing_new <- testing_new[, -8]
testing_new <- testing_new[, -8]

chirps_precip_long_check <- chirps_precip_long %>%
  distinct(CD_MUN, Year, Month, .keep_all = TRUE)

testing_newer <- left_join(x = testing_new,
                           y = lulc_edge_data_covariates,
                           by = c("CD_MUN" = "Municipality",
                                  "Year" = "Year"))
testing_newer_final <- left_join(x = testing_newer,
                                 y = chirps_precip_long_check,
                                 by = c("Month", "Year", "CD_MUN"))

testing_newer_final <- testing_newer_final[, -22]
write.csv(testing_newer_final, 'final_compiled_data_Brazil_all_vars_2007_2023.csv')

result <- left_join(
  x = testing_newer,
  y = chirps_precip_long_check,
  by = c("CD_MUN", "Date")
)

result %>%
  count(CD_MUN, Date) %>%
  filter(n > 1) 

result_trial <- result %>%
  group_by(CD_MUN, Date) %>%  # Group by CD_MUN and Date
  summarize(across(everything(), mean, na.rm = TRUE), .groups = "drop") 

result <- result[, -22]
result <- result[, -22]
result <- result[, -22]
result[is.na(result)] <- 0


new_brz_matrix <- cor(result)
corrplot(new_brz_matrix, method = "circle")

threshold <- 0.7
new_brz_matrix_filtered <- new_brz_matrix * (abs(new_brz_matrix) >= threshold)  # Set values below threshold to 0

corrplot(new_brz_matrix_filtered, method = "number", tl.cex = 0.8, na.label = " ", addCoef.col = "black")
