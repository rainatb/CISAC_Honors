# rerunning bootstrapping for iterations 21-30

library(xgboost) 
library(rBayesianOptimization)
library(caret)
library(rsample) #to split stratified data
library(dplyr)
library(pROC)
library(caTools)
library(tidyr)
library(pdp)

auc_per_run <- data.frame(model = character(),
                          auc = numeric(),
                          stringsAsFactors = FALSE)

feature_importance_optimal_model <- data.frame(model = character(),
                                               variable = character(),
                                               importance = numeric(),
                                               stringsAsFactors = FALSE)


pdps_optimal_model <- data.frame(matrix(vector(), 0, 4, 
                                        dimnames=list(c(), c('model','variable', 'value', 'yhat'))),
                                 row.names = NULL, stringsAsFactors=F)


#official_final_data <- read.csv('final_compiled_data_Brazil_all_vars_2007_2023.csv')
official_final_data <- read.csv('data/final_compiled_data_Brazil_all_vars_2007_2023.csv')

official_final_data <- official_final_data[, -1] # getting rid of the row numbers
official_final_data <- official_final_data[, -1] # getting rid of CD_MUN
official_final_data <- official_final_data[, -3] # getting rid of Month

official_final_data <- official_final_data %>%
  rename(soil_pH = mean)

official_final_data <- official_final_data[, -5] # getting rid of maximum runoff
official_final_data <- official_final_data[, -6] # getting rid of soil temperature
official_final_data <- official_final_data[, -12] # getting rid of miscanellous area
official_final_data <- official_final_data[, -10] # getting rid of pasture_urban_area
official_final_data <- official_final_data[, -10] # getting rid of forest_urban_area

n <- ncol(official_final_data) - 1


for (x in 21:30) {
  shuffling_rows <- sample(nrow(official_final_data))
  data_shuffled <- official_final_data[shuffling_rows,]
  set.seed(x)
  shuffled_labels <- data_shuffled[, 5]
  shuffled_data <- data_shuffled[, -5]
  indices_training <- sample(nrow(shuffled_data), 0.8*nrow(shuffled_data), replace = FALSE)
  data_train_shuffled <- shuffled_data[indices_training,]
  label_training <- shuffled_labels[indices_training]
  data_test_shuffled <- shuffled_data[-indices_training,]
  label_testing <- shuffled_labels[-indices_training] 
  scale_weight = round((as.data.frame(table(label_training))[1,2] / as.data.frame(table(label_training))[2,2]), 0)
  data_train_shuffled <- as.matrix(data_train_shuffled)
  label_training <- as.matrix(label_training)
  data_test_shuffled <- as.matrix(data_test_shuffled)
  label_testing <- as.matrix(label_testing)
  new_train <- xgb.DMatrix(data_train_shuffled, label=label_training)
  new_test <- xgb.DMatrix(data_test_shuffled, label = label_testing)
  ntrees.max = 200
  xgb_cv_bayes <- function(eta, max.depth, min.child.weight, subsample, colsample_bytree, gamma) {
    cv <- xgb.cv(params = list(booster = "gbtree",
                               eta = eta,
                               max_depth = max.depth,
                               min_child_weight = min.child.weight,
                               subsample = subsample,
                               colsample_bytree = colsample_bytree,
                               gamma = 0,
                               objective = "binary:logistic",
                               eval_metric = "logloss",
                               seed = 25),
                 data = new_train,
                 nrounds = ntrees.max,
                 nfold = 3, 
                 early_stopping_rounds = 10,
                 scale_pos_weight = 2,
                 verbose = T)
    list(Score = -unlist(cv$evaluation_log[cv$best_iteration, "test_logloss_mean"]), # Ensure score is negative, since optimization maximizes
         Pred = cv$pred,
         cb.print.evaluation(period = 1))
  }
  
  #------------------------------------------------------
  # Acquire optimal parameters with Bayesian optimization (maximization function) via the R package "rBayesianOptimization"
  #------------------------------------------------------
  best_params <- BayesianOptimization(xgb_cv_bayes,
                                      bounds = list(eta = c(0.01, 0.3),
                                                    max.depth = c(2L, 10L),
                                                    min.child.weight = c(1L, 15L),
                                                    subsample = c(0.6, 1),
                                                    colsample_bytree = c(0.6, 1)),
                                      init_grid_dt = NULL,
                                      init_points = 10,
                                      n_iter = 40,
                                      acq = "ucb",
                                      kappa = 3,
                                      eps = 1.5,
                                      verbose = T)
  
  #------------------------------------------------------
  # Using the tuned hyperparameters, run a second cross-validation to acquire nrounds
  #------------------------------------------------------
  xgb_cv <- xgb.cv(params = best_params,
                   data = new_train,
                   nrounds = ntrees.max,
                   nfold = 5,
                   scale_pos_weight = scale_weight,
                   early_stopping_rounds = 10,
                   objective = "binary:logistic",
                   eval_metric = "logloss",
                   verbose = T)
  
  best_params$nrounds <- xgb_cv$best_ntreelimit
  
  #------------------------------------------------------
  # Run the full xgb model with the suite of optimal parameters
  #------------------------------------------------------
  watchlist <- list(train = new_train, test = new_test)
  xgb.fit <- xgboost(data = new_train,
                     eta = best_params$Best_Par[1],
                     max_depth = best_params$Best_Par[2],
                     min_child_weight = best_params$Best_Par[3],
                     subsample = best_params$Best_Par[4],
                     colsample_bytree = best_params$Best_Par[5],
                     gamma = 0,
                     nrounds = best_params$nrounds,
                     scale_pos_weight = scale_weight, #(330/165)
                     objective = "binary:logistic",
                     eval_metric = "logloss")
  
  ###prediction test
  xgbpred <- predict(xgb.fit, new_test)
  xgbpred <- as.numeric(xgbpred)
  labels <- as.numeric(label_testing)
  
  auc <- roc(response=labels, predictor=xgbpred, levels=c(0,1))
  auc_per_run[x, "model"] <- x
  auc_per_run[x, "auc"] <- auc$auc
  feature_importance_data <- xgb.importance(model = xgb.fit)
  
  for (y in 1:n){
    importance_bootstrap_current <- feature_importance_data[y, "Gain"]
    feature_importance_optimal_model <- rbind(
      feature_importance_optimal_model,
      data.frame(model = paste("Model", x),
                 variable = feature_importance_data[y, 1],
                 importance = importance_bootstrap_current)
    )
  }
  for (z in 1:n) {
    output_mans <-  as.data.frame(pdp::partial(xgb.fit, 
                                               pred.var = colnames(data_train_shuffled)[z],
                                               train = data_train_shuffled[,c(1:n)], type = 'regression'))
    output_data <- data.frame(matrix(vector(), nrow(output_mans), 4,
                                     dimnames=list(c(), c('model','variable', 'value', 'yhat'))), stringsAsFactors=F,
                              row.names=NULL)
    output_data$model <- paste("Model", x)
    output_data$value <- output_mans[[1]]
    
    output_data$yhat <- output_mans[[2]]
    output_data$variable <- colnames(data_train_shuffled)[z]
    pdps_optimal_model <- rbind(pdps_optimal_model, output_data)
  }
}


# change iterations
write.csv(auc_per_run, 'data/auc_for_vars_Jan2025_21_30_its')
write.csv(feature_importance_optimal_model, 'data/feature_importance_for_vars_Jan2025its_21_30.csv')
write.csv(pdps_optimal_model, 'data/pdp_for_vars_Jan2025_its_21_30.csv')


