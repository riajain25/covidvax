# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
data_test = read_tsv("data/clean/covid_test.tsv")

sapply(data_test, class)

# load ridge fit object
load("Desktop/stat471/stat-471-fall-2021/finalproject/results/ridge_fit.Rda")

# load lasso fit object
load("Desktop/stat471/stat-471-fall-2021/finalproject/results/lasso_fit.Rda")

#evalute OLS regression
lm_predictions = predict(lm_fit, 
                         newdata = data_test)

lm_RMSE = sqrt(mean((lm_predictions - data_test$Series_Complete_18PlusPop_Pct)^2))

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = data_test, 
                            s = "lambda.1se") %>% as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-data_test$Series_Complete_18PlusPop_Pct)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = data_test, 
                            s = "lambda.1se") %>% as.numeric()

lasso_RMSE = sqrt(mean((lasso_predictions-data_test$Series_Complete_18PlusPop_Pct)^2))

#evaluate reg tree

load("Desktop/stat471/stat-471-fall-2021/finalproject/results/optimal_regtree.Rda")

pred = predict(optimal_regtree, newdata = data_test)

opregtree_RMSE = sqrt(mean((pred-data_test$Series_Complete_18PlusPop_Pct)^2))

#Random Forest

rf_predictions = predict(rf_fit, newdata = data_test)
rf_predictions

rf_RMSE = sqrt(mean((rf_predictions - data_test$Series_Complete_18PlusPop_Pct)^2))

#evaluate boosting model
load("Desktop/stat471/stat-471-fall-2021/finalproject/results/gbm_fit_optimal.Rda")

gbm_predictions = predict(gbm_fit_optimal, 
                          n.trees = optimal_num_trees, 
                          newdata = data_test)

gbm_RMSE = sqrt(mean((gbm_predictions - data_test$Series_Complete_18PlusPop_Pct)^2))

# print nice table
tibble(Method = c("OLS Regression", "Ridge", "Lasso", "Regression Tree", "Random Forest", "Boosting"), `Test RMSE` = c(lm_RMSE, ridge_RMSE, lasso_RMSE, opregtree_RMSE, rf_RMSE, gbm_RMSE))

