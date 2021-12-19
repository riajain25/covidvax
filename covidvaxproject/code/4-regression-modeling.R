# load libraries
library(glmnetUtils)    # to run ridge and lasso
source("Desktop/stat471/stat-471-fall-2021/functions/plot_glmnet.R") # for lasso/ridge trace plots

data_train = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/clean/data_train.csv") # read in the training data
data_test = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/clean/data_test.csv") #read in test data

#Check and adjust data types
sapply(data_test, class)
sapply(data_train, class)
data_train <- data_train %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))
data_test <- data_test %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))


#Check distribution of Vax Rates -- not skewed right, so no log transformation of variable
data_train %>% ggplot() + geom_histogram(aes(x=Series_Complete_18PlusPop_Pct))


#OLS regression
lm_fit = lm(Series_Complete_18PlusPop_Pct ~., data=data_train)
summary(lm_fit)

r_squared = summary(lm_fit)$r.squared

# run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(Series_Complete_18PlusPop_Pct ~ .,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = data_train)

# save the ridge fit object
save(ridge_fit, file = "Desktop/stat471/stat-471-fall-2021/finalproject/results/ridge_fit.Rda")

# create ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "Desktop/stat471/stat-471-fall-2021/finalproject/results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

#lambda for ridge
lambda_ridge = ridge_fit$lambda.1se

#create ridge trace plot
p = plot_glmnet(ridge_fit, data_train, features_to_plot = 5)
ggsave(filename = "Desktop/stat471/stat-471-fall-2021/finalproject/results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)



# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(Series_Complete_18PlusPop_Pct ~ .,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = data_train)

lambda_lasso = lasso_fit$lambda.1se

num_features_lasso = lasso_fit$nzero[lasso_fit$lambda == lasso_fit$lambda.1se]

# save the lasso fit object
save(lasso_fit, file = "Desktop/stat471/stat-471-fall-2021/finalproject/results/lasso_fit.Rda")

  
# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "Desktop/stat471/stat-471-fall-2021/finalproject/results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit, data_train, features_to_plot = 5)
ggsave(filename = "Desktop/stat471/stat-471-fall-2021/finalproject/results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by lasso and their coefficients
lasso_features = extract_std_coefs(lasso_fit, data_train)
lasso_features %>% filter(coefficient != 0) %>% arrange(desc(abs(coefficient))) %>%  write_tsv("Desktop/stat471/stat-471-fall-2021/finalproject/results/lasso-features-table.tsv")

