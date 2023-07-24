library("mlr3")
library("mlr3learners")
library("mlr3fairness")
library("ggplot2")
library("DALEX")

setwd("path/to/r-scripts")

# load dataset
source("adult.R")

set.seed(4)

# create classification task
adult_task <- TaskClassif$new(id = "adult",
                              backend = adult,
                              target = "income",
                              positive = ">50K")

# set protected attribute and weights
adult_task$set_col_roles("sex", add_to = "pta")
adult_task$set_col_roles("fnlwgt", add_to = "weight")

# remove feature
adult_task$set_col_roles("sex", remove_from = "feature")
adult_task$set_col_roles("race", remove_from = "feature")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

# split data into train and test set
split <- partition(adult_task, ratio = 0.8)

# learner ranger
adult_ranger <- lrn("classif.ranger",
                    predict_type = "prob",
                    importance = "permutation",#"none", "impurity", "impurity_corrected", "permutation"
                    num.threads = 4
)

# train model
adult_ranger_train <- adult_ranger$train(adult_task, split$train)

# adult_ranger_prd <- adult_ranger$predict(adult_task, split$test)
# 
# adult_ranger_prd$score(msr("classif.auc"))# 0.9193265

###
# dalex model analysis

# fetch and remove fnlwgt
adult_test_weights <- adult[split$test,]$fnlwgt
adult <- adult[,c("age", "workclass", "education", "marital_status", "occupation", "relationship", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")]#"fnlwgt", "sex", "race", 

# predict_function = function(m,x) m$predict_newdata(newdata = x)$prob[,1],

model_adult_ranger_test <- DALEX::explain(adult_ranger_train,
                                          predict_function = function(m,x) predict(m, x, predict_type = "prob")[,1],
                                          data = adult[split$test, ncol(adult) * -1],
                                          y = adult[split$test,]$income == ">50K",
                                          weights = adult_test_weights,
                                          type = "classification",
                                          label = "Ranger",
                                          verbose = FALSE)

# model performance
mperf_adult_ranger_test <- DALEX::model_performance(model_adult_ranger_test)
mperf_adult_ranger_test

plot(mperf_adult_ranger_test, geom = "roc")
