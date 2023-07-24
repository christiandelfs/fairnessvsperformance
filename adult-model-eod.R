library("mlr3")
library("mlr3learners")
library("ggplot2")
library("mlr3pipelines")
library("mlr3viz")

setwd("path/to/r-scripts")

set.seed(4)

source("adult.R")

# source("gui.R")
# 
# msout <- list(msr("classif.auc"), msr("fairness.gui"))

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
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

# split data into train and test set
split <- partition(adult_task, ratio = 0.8)#, stratify = TRUE

# learner ranger
adult_ranger_eod <- lrn("classif.ranger",
  predict_type = "response",
  num.threads = 4,
  respect.unordered.factors = "order"
)

#poed = po("EOd")#, privileged = "Male"
poed <- PipeOpEOd$new(id = "eod", param_vals = list(alpha = 1.0, privileged = "Male"))
learner = PipeOpLearnerCV$new(adult_ranger_eod, id = "learner_cv", param_vals = list(resampling.method = "cv", resampling.folds = 5))
graph = learner %>>% poed#po("threshold", param_vals = list(thresholds = 0.5)) %>>%
#graph = po("learner_cv", adult_ranger_eod) %>>% poed#po("threshold", param_vals = list(thresholds = 0.5)) %>>%
glrn = GraphLearner$new(graph)
glrn$train(adult_task, split$train)
adult_ranger_eod_prd <- glrn$predict(adult_task, split$test)
adult_ranger_eod_prd$score(list(msr("classif.acc"), msr("fairness.eod")), adult_task)

# classif.acc fairness.equalized_odds 
# 0.84897734              0.03381717

# library("pROC")
# 
# adult_ranger_eod_prd$truth
# adult_ranger_eod_prd$response
# 
# adult_ranger_eod_prd$confusion
# 
# adult_ranger_eod_prd$score(msr("fairness.gui"), adult_task)
# msr("fairness.gui")$score(adult_ranger_eod_prd, adult_task)
# 
# roc(ifelse(adult_ranger_eod_prd$truth == ">50K", 1, 0), ifelse(adult_ranger_eod_prd$response == ">50K", 1, 0))
# roc(adult_ranger_eod_prd$truth, ifelse(adult_ranger_eod_prd$response == ">50K", 1, 0), levels = c(">50K","<=50K"))#, direction = ">"
# 
# autoplot(adult_ranger_eod_prd)
