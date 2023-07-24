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

###
# single crit optimization with resampling

# learner ranger
adult_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  num.threads = 4,
                  respect.unordered.factors = "order"
                  #splitrule = "extratrees"
)

# create search space for hpo
search_space <- ps(
  #num.threads = p_int(lower = 4, upper = 4),
  #num.trees = p_int(lower = 400, upper = 600)
  max.depth = p_int(lower = 1, upper = 31)
  #mtry = p_int(lower = 1, upper = ncol(adult_test) - 1),
  #mtry.ratio = p_dbl(lower = 5e-1, upper = 1.0)
  #min.node.size = p_int(lower = 1, upper = 100)
  #num.random.splits = p_int(lower = 1, upper = 10)
  #sample.fraction = p_dbl(lower = 1e-1, upper = 1.0)
)

# create auto_tuner object
adult_ranger_tuned <- auto_tuner(#
  tuner = tnr("grid_search"),
  learner    = adult_ranger,
  resampling = rsmp("cv", folds = 5),
  #resampling = rsmp ("bootstrap", repeats = 20, ratio = 0.8),
  #measure    = msr("classif.auc"),
  measure    = msr("fairness.gui"),
  search_space = search_space,
  #terminator = trm("combo",
  #  list(
  #    trm("perf_reached", level = 8e-01),
  #    trm("stagnation", iters = 5, threshold = 1e-04)
  #  ), any = FALSE
  #),
  terminator = trm("stagnation", iters = 5, threshold = 1e-04),
  #terminator = trm("evals", n_evals = 10),
  #terminator = trm("none"),
  #method = "grid_search"
  #term_evals = 4
)

#as.data.table(tuned_ranger$archive$search_space)[, list(id, class, lower, upper, nlevels)]

# resampling
rr <- resample(adult_task, adult_ranger_tuned, rsmp("cv", folds = 5))#, store_models = TRUE

# rr_prd <- rr$prediction()
# # rr_prd$set_threshold(0.5)
# rr_prd$score(msout, adult_task)

rr$aggregate(msout)

# gui
# rsmp("cv", folds = 5)
# max.depth = p_int(lower = 1, upper = 31)
# classif.auc fairness.pp_np 
# 0.89134279     0.09759271

# gui
# rsmp ("bootstrap", repeats = 20, ratio = 0.8)
# max.depth = p_int(lower = 1, upper = 31)


# rr_prd <- rr$prediction()
# 
# # gui
# # rsmp("cv", folds = 5)
# # max.depth = p_int(lower = 1, upper = 31)
# 
# rr_prd$score(msr("classif.tpr"), task = adult_task)
# # 0.2471977
# 
# rr_prd$score(msr("classif.fpr"), task = adult_task)
# # 0.01660611
# 
# rr_prd$score(msr("classif.tnr"), task = adult_task)
# # 0.9833939
# 
# rr_prd$score(msr("classif.fnr"), task = adult_task)
# # 0.7528023
