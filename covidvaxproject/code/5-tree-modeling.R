# [omitted from this template]

library(rpart)
library(rpart.plot)
library(tidyverse)
library(randomForest)

data_train = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/clean/data_train.csv") # read in the training data

#Check and adjust data types
sapply(data_test, class)
sapply(data_train, class)
data_train <- data_train %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))
data_test <- data_test %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))

#regression tree

tree_fit = rpart(Series_Complete_18PlusPop_Pct ~ ., data = data_train)

#Plot reg tree
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "Desktop/stat471/stat-471-fall-2021/finalproject/results/reg-tree-plot.png")
rpart.plot(tree_fit)
dev.off()

tree_fit$variable.importance

#Plot CV plot 
regtree_cptable = printcp(tree_fit) %>% as_tibble()

regtree_cvplot = regtree_cptable %>%
  ggplot(aes(x = nsplit+1, y = xerror, 
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  geom_point() + geom_line() +
  geom_errorbar(width = .2) +
  xlab("Number of terminal nodes") + ylab("CV error") +
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") +
  theme_bw()

ggsave(filename = "Desktop/stat471/stat-471-fall-2021/finalproject/results/regtree-cv-plot.png", 
       plot = regtree_cvplot, 
       device = "png", 
       width = 6, 
       height = 4)

#choosing # terminal nodes based on 1se rule

optimal_regtree_info = regtree_cptable %>%
  filter(xerror - xstd < min(xerror)) %>%
  arrange(nsplit) %>% 
  head(1)

optimal_regtree_info

#get optimal reg tree
optimal_regtree = prune(tree = tree_fit, cp = optimal_regtree_info$CP)

png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "Desktop/stat471/stat-471-fall-2021/finalproject/results/optimal-regtree-plot.png")
rpart.plot(optimal_regtree)
dev.off()

#save optimal tree fit
save(optimal_regtree, file = "Desktop/stat471/stat-471-fall-2021/finalproject/results/optimal_regtree.Rda")

#Random Forest

rf_fit = randomForest(Series_Complete_18PlusPop_Pct ~ ., mtry =7, data = data_train, importance = TRUE)

plot(rf_fit)

#tuning mtry, 16 total variables

rf_3 = randomForest(Series_Complete_18PlusPop_Pct ~ ., mtry = 3, data = data_train)
rf_6 = randomForest(Series_Complete_18PlusPop_Pct ~ ., mtry= 6, data = data_train)
rf_9 = randomForest(Series_Complete_18PlusPop_Pct ~ ., mtry= 9, data = data_train)
rf_12 = randomForest(Series_Complete_18PlusPop_Pct ~ ., mtry= 12, data = data_train)
rf_16 = randomForest(Series_Complete_18PlusPop_Pct ~ ., mtry= 16, data = data_train)

oob_errors_rf = bind_rows(
  tibble(ntree = 1:500, oob_err = rf_3$mse, m =3),
  tibble(ntree = 1:500, oob_err = rf_6$mse, m =6),
  tibble(ntree = 1:500, oob_err = rf_9$mse, m =9),
  tibble(ntree = 1:500, oob_err = rf_12$mse, m =12),
  tibble(ntree = 1:500, oob_err = rf_16$mse, m =16)
)

oob_errors_rf %>% 
  ggplot(aes(x= ntree, y = oob_err, colour=factor(m))) +
  geom_line() + theme_bw()

mvalues = seq(1, 16, by = 2)
oob_errors_rf2 = numeric(length(mvalues))
ntree= 500

for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(Series_Complete_18PlusPop_Pct ~ ., mtry = m, data = data_train)
  oob_errors_rf2[idx] = rf_fit$mse[ntree]
}

tibble(m = mvalues, oob_err = oob_errors_rf2) %>% ggplot(aes(x = m, y = oob_err)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = mvalues)  +
  theme_bw()

#optimal m = 7

#var importance
rf_fit$importance
varImpPlot(rf_fit)


#Boosting
library(gbm)
set.seed(1)

gbm_fit = gbm(Series_Complete_18PlusPop_Pct ~ ., 
              distribution = "gaussian",
              n.trees = 300,
              interaction.depth = 1,
              shrinkage = .1, 
              cv.folds = 5, 
              data = data_train)


opt_num_trees =gbm.perf(gbm_fit)

opt_num_trees

#Optimal number of trees around 200


#tuning the interaction depth

set.seed(1)
gbm_fit_1 = gbm(Series_Complete_18PlusPop_Pct ~ ., 
                distribution = "gaussian",
                n.trees = 250,
                interaction.depth = 1,
                shrinkage = .1, 
                cv.folds = 5, 
                data = data_train)

gbm_fit_2 = gbm(Series_Complete_18PlusPop_Pct ~ ., 
                distribution = "gaussian",
                n.trees = 250,
                interaction.depth = 2,
                shrinkage = .1, 
                cv.folds = 5, 
                data = data_train)

gbm_fit_3 = gbm(Series_Complete_18PlusPop_Pct ~ ., 
                distribution = "gaussian",
                n.trees = 250,
                interaction.depth = 3,
                shrinkage = .1, 
                cv.folds = 5, 
                data = data_train)

gbm_fit_4 = gbm(Series_Complete_18PlusPop_Pct ~ ., 
                distribution = "gaussian",
                n.trees = 250,
                interaction.depth = 4,
                shrinkage = .1, 
                cv.folds = 5, 
                data = data_train)

ntrees= 250
cv_errors_boosting = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth =1),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth =2),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth =3),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_4$cv.error, depth =4)
)

cv_errors_boosting %>%
  ggplot(aes(x=ntree, y=cv_err, colour=factor(depth))) + 
  geom_line() +
  theme_bw()

gbm_fit_optimal = gbm_fit_4
optimal_num_trees = gbm.perf(gbm_fit_4, plot.it = FALSE)
optimal_num_trees

#Optimal num trees = 174

summary(gbm_fit_optimal, n.trees = optimal_num_trees, plotit = FALSE)

#make plots

gbm_fit_plot_pergop = plot(gbm_fit_optimal, i.var = "per_gop", n.trees = optimal_num_trees)
gbm_fit_plot_perBIPOC = plot(gbm_fit_optimal, i.var = "per_BIPOC", n.trees = optimal_num_trees)
gbm_fit_plot_peruninsured = plot(gbm_fit_optimal, i.var = "per_uninsured"   , n.trees = optimal_num_trees)
gbm_fit_plot_perlessthan18 = plot(gbm_fit_optimal, i.var = "per_18andyounger", n.trees = optimal_num_trees)


#save boosting model

save(gbm_fit_optimal, file = "Desktop/stat471/stat-471-fall-2021/finalproject/results/gbm_fit_optimal.Rda")

