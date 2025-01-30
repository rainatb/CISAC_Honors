## running the spatiotemporal crossvalidation with test years of 2007, 2008, 2009, 2021, 2022, and 2023

library(xgboost) 
library(rBayesianOptimization)
library(caret)
library(rsample) #to split stratified data
library(dplyr)
library(pROC)
library(caTools)
library(sf)
library(spatialsample)
library(tidyr)
library(data.table)


data <- read.csv('final_compiled_data_Brazil_all_vars_2007_2023.csv')
data <- data[, -1] # getting rid of row numbers column
data <- data[, -4] # getting rid of month
data <- data[, -6] # getting rid of max_runoff

data <- data %>%
  rename(soil_pH = mean)

data <- data[, -7] # getting rid of Soil Temperature
data <- data[, -13] # getting rid of miscellaneous 
data <- data[, -12] # getting rid of Forest_Urban_Area
data <- data[, -11] # getting rid of pasture urban area

only_splits <- read.csv('10_folds_w_CD_MUN.csv')
only_splits <- only_splits[, -1]

new_data_w_splits <- merge(data, only_splits, by = ("CD_MUN"))
new_data_w_splits <- new_data_w_splits[, -11] # getting rid of forest area

test_year <- c(2007, 2008, 2009, 2021, 2022, 2023)

final_lepto_oob_out <- data.frame(matrix(vector(), 0, 7,
                                         dimnames=list(c(), c("auc", "sens", "spec", "oob", "Type", "spatial_fold", "temporal_fold"))), 
                                  stringsAsFactors=F, row.names=NULL)
final_lepto_oob_in <- data.frame(matrix(vector(), 0, 7,
                                        dimnames=list(c(), c("auc", "sens", "spec", "oob", "Type", "spatial_fold", "temporal_fold"))), 
                                 stringsAsFactors=F, row.names=NULL)

oob_out_cv <- data.frame(Type = character(),
                         oob = numeric(),
                         sens = numeric(),
                         spec = numeric(),
                         auc = numeric(),
                         spatial_fold = integer(),
                         temporal_fold = integer(),
                         stringsAsFactors = FALSE)

oob_in_cv <- oob_out_cv  # Use the same structure
n <- 10

i <- length(test_year)

for(j in 1:i){
  
  if (j > 3) {
    temporal_train <- subset(new_data_w_splits, Year < test_year[j])
  } else {
    temporal_train <- subset(new_data_w_splits, Year > test_year[j])
  }
  temporal_test <- subset(new_data_w_splits, Year == test_year[j])
  
  oob_out_cv <- data.frame(matrix(vector(),0, 7,
                                  dimnames=list(c(), c("auc", "sens", "spec", "oob", "Type", "spatial_fold", "temporal_fold"))),
                           stringsAsFactors=F, row.names=NULL)
  
  oob_in_cv <- data.frame(matrix(vector(),0, 7,
                                 dimnames=list(c(), c("auc", "sens", "spec", "oob", "Type", "spatial_fold", "temporal_fold"))),
                          stringsAsFactors=F, row.names=NULL)
  
  # Loop through spatial folds
  for(i in 1:n){
    
    set.seed(i*99)
    
    datrain = subset(temporal_train, fold != i)
    datest  = subset(temporal_test, fold == i)
    
    label_train_column <- as.numeric(unlist(datrain[, 6]))
    label_test_column <- as.numeric(unlist(datest[, 6]))
    
    # Ensure there are cases in the fold (check if the table has at least 2 levels)
    if(length(table(datrain$occurrence)) > 1 && length(table(datest$occurrence)) > 1 &&
       table(datrain$occurrence)[2] > 0 & table(datest$occurrence)[2] > 0){
      
      # Prepare train and test data
      train_matrix <- xgb.DMatrix(
        data = as.matrix(datrain[, c(2:5, 7:15)]),  # Select columns 2 to 5 and 7 to 15
        label = label_train_column  # Use column 6 for labels
      )
      test_matrix <- xgb.DMatrix(
        data = as.matrix(datest[, c(2:5, 7:15)]),  # Select columns 2 to 5 and 7 to 15
        label = label_test_column  # Use column 6 for labels
      )
      
      # Set parameters for XGBoost model tuning
      param_list <- list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = 0.1,  # learning rate
        max_depth = 6,
        subsample = 0.8,
        colsample_bytree = 0.8,
        min_child_weight = 1
      )
      
      # Cross-validation within the training set to tune hyperparameters
      xgb_cv <- xgb.cv(
        params = param_list,
        data = train_matrix,
        nrounds = 100,  # number of boosting rounds
        nfold = 3,  # 3-fold CV within each training set
        early_stopping_rounds = 10,
        maximize = FALSE,
        verbose = TRUE
      )
      
      best_nrounds <- xgb_cv$best_iteration
      
      # Train the final model with optimal rounds
      xgb_model <- xgb.train(
        params = param_list,
        data = train_matrix,
        nrounds = best_nrounds,
        watchlist = list(train = train_matrix, test = test_matrix),
        print_every_n = 10,
        early_stopping_rounds = 10
      )
      
      # Predictions and model performance
      test_pred <- predict(xgb_model, test_matrix)
      auc_out <- roc(response = label_test_column, predictor = test_pred, levels = c(0, 1), auc = TRUE)
      best_threshold_out <- coords(auc_out, "best", ret = c("threshold", "sensitivity", "specificity"))
      sensitivity_out <- best_threshold_out$sensitivity
      specificity_out <- best_threshold_out$specificity
      
      # Store the out-of-sample performance metrics
      out_error <- data.frame(Type = 'lepto', oob = xgb_model$evaluation_log$train_logloss[best_nrounds],
                              sens = sensitivity_out,
                              spec = specificity_out,
                              auc = auc_out$auc,
                              spatial_fold = i,
                              temporal_fold = j)
      oob_out_cv <- rbind(oob_out_cv, out_error)
      
      # In-sample performance metrics
      train_pred <- predict(xgb_model, train_matrix)
      auc_in <- roc(response = label_train_column, predictor = train_pred, levels = c(0, 1), auc = TRUE)
      best_threshold_in <- coords(auc_in, "best", ret = c("threshold", "sensitivity", "specificity"))
      sensitivity_in <- best_threshold_in$sensitivity
      specificity_in <- best_threshold_in$specificity
      
      in_error <- data.frame(Type = 'lepto', oob = xgb_model$evaluation_log$test_logloss[best_nrounds],
                             sens = sensitivity_in,
                             spec = specificity_in,
                             auc = auc_in$auc,
                             spatial_fold = i,
                             temporal_fold = j)
      oob_in_cv <- rbind(oob_in_cv, in_error)
      
      message <- paste0("iteration lepto-", j, "-", i)
      print(message)
    } else {
      # Handle cases where there are no data for a fold
      out_error <- data.frame(Type = "lepto", oob = NA,
                              sens = NA,
                              spec = NA,
                              auc = NA,
                              spatial_fold = i,
                              temporal_fold = j)
      oob_out_cv <- rbind(oob_out_cv, out_error)
      
      in_error <- data.frame(Type = "lepto", oob = NA,
                             sens = NA,
                             spec = NA,
                             auc = NA,
                             spatial_fold = i,
                             temporal_fold = j)
      oob_in_cv <- rbind(oob_in_cv, in_error)
      
      message <- paste0("iteration lepto-", j, "-", i)
      print(message)
    }
  }
  final_lepto_oob_out <- rbind(final_lepto_oob_out, oob_out_cv)
  final_lepto_oob_in <- rbind(final_lepto_oob_in, oob_in_cv)
}

print(mean(final_lepto_oob_in$auc, na.rm = TRUE))
print(mean(final_lepto_oob_out$auc, na.rm = TRUE))

print(mean(final_lepto_oob_out$sens, na.rm = TRUE))
print(mean(final_lepto_oob_out$spec, na.rm = TRUE))

final_lepto_oob_in_test <- final_lepto_oob_in %>%
  mutate(
    F1_Score = 2 * (sens * spec) / (sens + spec),  # F1 score formula
    TSS = sens + spec - 1                         # TSS formula
  )

final_lepto_oob_out_test <- final_lepto_oob_out %>%
  mutate(
    F1_Score = 2 * (sens * spec) / (sens + spec),  # F1 score formula
    TSS = sens + spec - 1                         # TSS formula
  )

print(mean(final_lepto_oob_out_test$TSS, na.rm = TRUE))
print(mean(final_lepto_oob_out_test$F1_Score, na.rm = TRUE))

install.packages("Hmisc")
library(Hmisc)

calculate_ci <- function(metric) {
  mean_value <- mean(metric, na.rm = TRUE)  # Calculate mean
  ci <- Hmisc::smean.cl.normal(metric, na.rm = TRUE)  # Calculate 95% CI
  list(mean = mean_value, lower_ci = ci[2], upper_ci = ci[3])
}

# Apply to each metric
auc_ci <- calculate_ci(final_lepto_oob_out_test$auc)
sens_ci <- calculate_ci(final_lepto_oob_out_test$sens)
spec_ci <- calculate_ci(final_lepto_oob_out_test$spec)
tss_ci <- calculate_ci(final_lepto_oob_out_test$TSS)
f1_ci <- calculate_ci(final_lepto_oob_out_test$F1_Score)

# Print results
print(auc_ci)
print(sens_ci)
print(spec_ci)
print(tss_ci)
print(f1_ci)

min(final_lepto_oob_out_test$sens)
max(final_lepto_oob_out_test$sens)

min(final_lepto_oob_out_test$spec)
max(final_lepto_oob_out_test$spec)

# calculating standard error

calculate_ci_se <- function(metric) {
  mean_value <- mean(metric)  # Mean
  sd_value <- sd(metric)      # Standard deviation
  x <- length(metric)         # Sample size
  
  # Calculate standard error
  se <- sd_value / sqrt(x)
  
  # Return a list with results
  list(mean = mean_value, se = se)
}

auc_se <- calculate_ci_se(final_lepto_oob_out_test$auc)
sens_se <- calculate_ci_se(final_lepto_oob_out_test$sens)
spec_se <- calculate_ci_se(final_lepto_oob_out_test$spec)
tss_se <- calculate_ci_se(final_lepto_oob_out_test$TSS)
f1_se <- calculate_ci_se(final_lepto_oob_out_test$F1_Score)

print(auc_se)
print(sens_se)
print(spec_se)
print(tss_se)
print(f1_se)

auc_lower_se <- auc_se$mean - auc_se$se
auc_upper_se <-auc_se$mean + auc_se$se

sens_lower_se <- sens_se$mean - sens_se$se
sens_upper_se <- sens_se$mean + sens_se$se

specs_lower_se <- spec_se$mean - spec_se$se
specs_upper_se <- spec_se$mean + spec_se$se

tss_lower_se <- tss_se$mean - tss_se$se
tss_upper_se <- tss_se$mean + tss_se$se

f1_lower_se <- f1_se$mean - f1_se$se
f1_upper_se <- f1_se$mean + f1_se$se

write.csv(final_lepto_oob_in_test, 'lepto_oob_in_final_more_test_years.csv')
write.csv(final_lepto_oob_out_test, 'lepto_oob_out_final_more_test_years.csv')


## install if needed (do this exactly once):
install.packages("usethis")

library(usethis)
use_git_config(user.name = "Raina Talwar Bhatia", user.email = "rainatb@stanford.edu")
