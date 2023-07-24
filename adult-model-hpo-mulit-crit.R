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

# create classification task for train+test
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

# create learner for hpo
adult_ranger_hpo <- lrn("classif.ranger",
  predict_type = "prob",
  num.threads = 4,
  respect.unordered.factors = "order",#partition
  num.trees = to_tune(400, 600)
  #mtry.ratio = to_tune(5e-1, 1)
  #min.node.size = to_tune(1, 100)
  #splitrule = "extratrees",
  #num.random.splits = to_tune(1, 100)
  #max.depth = to_tune(1, 31)
  #sample.fraction = to_tune(1e-1, 1)
)

# create tuning instance
instance = ti(
  task = adult_task_hpo,
  learner = adult_ranger_hpo,
  resampling = rsmp("cv", folds = 5),
  #resampling = rsmp ("bootstrap", repeats = 20, ratio = 1.0),#
  measures = msrs(c("classif.auc", "fairness.gui")),
  # terminator = trm("combo",
  #                  list(
  #                    trm("perf_reached", level = 1e-01),
  #                    trm("stagnation", iters = 5, threshold = 1e-04)
  #                  ), any = FALSE
  # ),
  terminator = trm("evals", n_evals = 30)
  #term_evals = 51,
  #batch_size = 1,
  #store_models = TRUE
  #evaluate_default = TRUE
)

#tuner <- tnr("random_search", batch_size = 5)
tuner <- tnr("grid_search")#, resolution = 1, param_resolutions = 1, batch_size = 1

tuner$optimize(instance)
#instance$archive$best()

# learner ranger
adult_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  num.threads = 4,
                  respect.unordered.factors = "order"
)

# assign optimized param set to learner
adult_ranger$param_set$values = instance$result_learner_param_vals[[1]]

# normal
adult_ranger$train(adult_task, split$train)

adult_ranger_prd <- adult_ranger$predict(adult_task, split$test)
# adult_ranger_prd$set_threshold(0.5)

adult_ranger_prd$score(msout, adult_task)

# 1
# rsmp("cv", folds = 5)
# max.depth = to_tune(1, 31)


# # resampling
# rr <- resample(adult_task, adult_ranger, rsmp("cv", folds = 5))
# #rr <- resample(adult_task, adult_ranger, rsmp("bootstrap", repeats = 20, ratio = 0.8))
# 
# rr$aggregate(msout)

# rsmp("cv", folds = 5)
# max.depth = to_tune(1, 31)
