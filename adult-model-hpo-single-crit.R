library("mlr3")
library("mlr3learners")
library("mlr3tuning")
library("paradox")

setwd("path/to/r-scripts")

set.seed(4)

# load dataset
source("adult.R")

# load gui fairness measure
source("gui.R")

msout <- list(msr("classif.auc"), msr("fairness.gui"))

# split data into train+test und hpo set
ids = sample(nrow(adult), as.integer(nrow(adult) * 0.8))
adult_tt <- adult[ids,]#train+test
adult_hpo <- adult[-ids,]#hpo

# create classification task for hpo
adult_task_hpo <- TaskClassif$new(id = "adult_hpo",
                                  backend = adult_hpo,
                                  target = "income",
                                  positive = ">50K")

# set protected attribute and weights
adult_task_hpo$set_col_roles("sex", add_to = "pta")
adult_task_hpo$set_col_roles("fnlwgt", add_to = "weight")

# remove feature
adult_task_hpo$set_col_roles("sex", remove_from = "feature")
adult_task_hpo$set_col_roles("race", remove_from = "feature")
adult_task_hpo$set_col_roles("fnlwgt", remove_from = "feature")

# create classification task
adult_task <- TaskClassif$new(id = "adult_tt",
                              backend = adult_tt,
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

###
# single crit hyper parameter optimization

# library(mlr3tuningspaces)
# 
# mlr_tuning_spaces
# lts("classif.ranger.default")
# lts("classif.ranger.rbv2")
# 
# <TuningSpace:classif.ranger.rbv2>: Classification Ranger with RandomBot
# id lower upper                 levels logscale
# 1:                 num.trees   1.0  2000                           FALSE
# 2:                   replace    NA    NA             TRUE,FALSE    FALSE
# 3:           sample.fraction   0.1     1                           FALSE
# 4:                mtry.ratio   0.0     1                           FALSE
# 5: respect.unordered.factors    NA    NA ignore,order,partition    FALSE
# 6:             min.node.size   1.0   100                           FALSE
# 7:                 splitrule    NA    NA        gini,extratrees    FALSE
# 8:         num.random.splits   1.0   100                           FALSE

# create learner for hpo
adult_ranger_hpo <- lrn("classif.ranger",
  predict_type = "prob",
  num.threads = 4,
  respect.unordered.factors = "order",
  #num.trees = to_tune(400, 600)
  #mtry.ratio = to_tune(5e-1, 1)
  #min.node.size = to_tune(1, 100)
  #splitrule = "extratrees",
  #num.random.splits = to_tune(1, 100)
  max.depth = to_tune(1, 31)
  #sample.fraction = to_tune(1e-1, 1)
)

# tuning instance
instance = ti(
  task = adult_task_hpo,
  learner = adult_ranger_hpo,
  resampling = rsmp("cv", folds = 5),
  #resampling = rsmp ("bootstrap", repeats = 20, ratio = 0.8),
  #measure = msr("classif.auc"),
  measure = msr("fairness.gui"),
  #terminator = trm("combo",
  #  list(
  #    trm("perf_reached", level = 8e-01),
  #    trm("stagnation", iters = 5, threshold = 1e-04)
  #  ), any = FALSE
  #)
  terminator = trm("stagnation", iters = 5, threshold = 1e-04)
  #terminator = trm("evals", n_evals = 10),
  #term_evals = 51,
  #batch_size = 1,
  #evaluate_default = TRUE
)

#tuner <- tnr("random_search", batch_size = 5)
tuner <- tnr("grid_search")#, resolution = 1, param_resolutions = 1, batch_size = 1

tuner$optimize(instance)

# learner ranger
adult_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  num.threads = 4,
                  respect.unordered.factors = "order"
)

# assign optimized param set to learner
adult_ranger$param_set$values = instance$result_learner_param_vals

# normal
adult_ranger$train(adult_task, split$train)

adult_ranger_prd <- adult_ranger$predict(adult_task, split$test)
# adult_ranger_prd$set_threshold(0.5)

adult_ranger_prd$score(msout, adult_task)

# gui
# rsmp("cv", folds = 5)
# num.tress = to_tune(400, 600)
# classif.auc fairness.pp_np 
# 0.9136342      0.2349181

# gui
# rsmp ("bootstrap", repeats = 20, ratio = 0.8)
# num.trees = to_tune(400,600)


# gui
# rsmp("cv", folds = 5)
# mtry.ratio = to_tune(5e-1, 1)
# classif.auc fairness.pp_np 
# 0.9026552      0.2454691

# gui
# rsmp("cv", folds = 5)
# min.node.size = to_tune(1, 100)
# classif.auc fairness.pp_np 
# 0.9114550      0.2476855

# gui
# rsmp("cv", folds = 5)
# max.depth = to_tune(1, 31)


# gui
# resampling = rsmp ("bootstrap", repeats = 20, ratio = 0.8)
# max.depth = to_tune(1, 31)
# classif.auc fairness.pp_np 
# 0.9047144      0.2147978

# # resampling
# rr <- resample(adult_task, adult_ranger, rsmp("cv", folds = 5))
# #rr <- resample(adult_task, adult_ranger, rsmp("bootstrap", repeats = 20, ratio = 0.8))
# 
# rr$aggregate(msout)
