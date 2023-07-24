library("mlr3")
library("mlr3learners")
library("mlr3verse")
library("mlr3fselect")
library("mlr3tuning")
library("mlr3filters")
library("ggplot2")
library("DALEX")

setwd("path/to/r-scripts")

# V1  V2        V3      V4        V5            V6              V7          V8            V9    V10 V11           V12           V13             V14             V15
# age workclass fnlwgt  education education-num marital-status  occupation  relationship  race  sex capital-gain  capital-loss  hours-per-week  native-country  income

# load dataset
source("adult.R")

source("gui.R")

set.seed(4)

# create classification task
adult_task <- TaskClassif$new(id = "adult",
                            backend = adult,
                            target = "income",
                            positive = ">50K")

adult_task$set_col_roles("fnlwgt", add_to = "weight")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

split <- partition(adult_task, ratio = 0.8)

# random forrest
# learner ranger
adult_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  importance = "permutation",#"none", "impurity", "impurity_corrected", "permutation"
                  num.threads = 4
)

adult_ranger_train <- adult_ranger$train(adult_task, split$train)

#feature importance
adult_ranger_train$model$variable.importance
# age   capital_gain   capital_loss      education hours_per_week marital_status native_country 
# 0.0175458387   0.0380952216   0.0087456560   0.0162223832   0.0102548418   0.0245890069   0.0007924342 
# occupation           race   relationship            sex      workclass 
# 0.0151174877   0.0012917749   0.0364998961   0.0060176064   0.0042039425

#adult_ranger_prd <- adult_ranger$predict_newdata(adult_test)
#adult_ranger_prd <- adult_ranger$predict(adult_task, split$test)

###
#mlr3
#feature selection

#as.data.table(mlr_learners)[sapply(properties, function(x) "importance" %in% x)]
#as.data.table(mlr_learners)[sapply(properties, function(x) "selected_features" %in% x)]

filter = flt("importance", learner = adult_ranger)
filter = flt("permutation", learner = adult_ranger)
#filter = flt("auc", learner = adult_ranger)
filter$calculate(adult_task)
filter$scores

#importance
# relationship   capital_gain marital_status            age      education     occupation hours_per_week 
# 0.0384616289   0.0374850050   0.0286879323   0.0180585862   0.0174175148   0.0157865170   0.0101998149 
# capital_loss            sex      workclass           race native_country 
# 0.0087085411   0.0067932064   0.0044504743   0.0012476612   0.0007518516

#permutation learner = adult_ranger


# # fselector
# instance = fselect(
#   fselector = fs("sequential", strategy = "sbs"),
#   task =  adult_task,
#   learner = adult_ranger,
#   resampling = rsmp("cv", folds=5),
#   measure = msr("classif.auc")
# )
# 
# instance$archive
# instance$result

#fselectinstance single crit
instance = fsi(
  task = adult_task,
  learner = adult_ranger,
  resampling = rsmp("cv", folds=5),
  measure = msr("classif.auc"),
  #measure = msr("fairness.gui"),
  terminator = trm("evals", n_evals = 10)
)

fselector = fs("sequential", strategy = "sbs")
fselector$optimize(instance)

#instance$archive
instance$result


# #fselectinstance multi crit
# instance = fsi(
#   task = adult_task,
#   learner = adult_ranger,
#   resampling = rsmp("cv", folds=5),
#   measure = msrs(c("classif.auc", "fairness.gui")),
#   terminator = trm("evals", n_evals = 20)
# )
# 
# fselector = fs("sequential", strategy = "sbs")
# fselector$optimize(instance)

# #auto fselector
# at = auto_fselector(
#   fselector = fs("grid_search"),
#   learner = adult_ranger,
#   resampling = rsmp("cv", folds=5),
#   measure = msr("classif.auc"),
#   terminator = trm("evals", n_evals = 10)
# )
# 
# grid = benchmark_grid(
#   task = sgc_task,
#   learner = list(at, adult_ranger),
#   resampling = rsmp("cv", folds = 3)
# )
# 
# bmr = benchmark(grid)
# bmr$aggregate(msrs(c("classif.auc", "fairness.gui")))

###
# dalex model analysis

# remove fnlwgt
adult_test_weights <- adult[split$test,]$fnlwgt
adult <- adult[,c("age", "workclass", "education", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")]#"fnlwgt", 

# predict_function = function(m,x) m$predict_newdata(newdata = x)$prob[,1],

model_adult_ranger_test <- DALEX::explain(adult_ranger_train,
  predict_function = function(m,x) predict(m, x, predict_type = "prob")[,1],
  data = adult[split$test, ncol(adult) * -1],
  y = adult[split$test,]$income == ">50K",
  weights = adult_test_weights,
  type = "classification",
  label = "Ranger",
  verbose = FALSE)

# #model performance
# mperf_adult_ranger_test <- DALEX::model_performance(model_adult_ranger_test)
# mperf_adult_ranger_test
# 
# plot(mperf_adult_ranger_test, geom = "roc")

#model parts, feature importance
mp_adult_ranger_test <- DALEX::model_parts(model_adult_ranger_test)
mp_adult_ranger_test
# variable mean_dropout_loss  label
# 1    _full_model_        0.08280142 Ranger
# 2  native_country        0.08332172 Ranger
# 3            race        0.08396306 Ranger
# 4             sex        0.08500048 Ranger
# 5       workclass        0.08744801 Ranger
# 6    capital_loss        0.09410313 Ranger
# 7  hours_per_week        0.09815892 Ranger
# 8      occupation        0.10370046 Ranger
# 9  marital_status        0.10499883 Ranger
# 10      education        0.10974711 Ranger
# 11            age        0.11798494 Ranger
# 12   relationship        0.12181754 Ranger
# 13   capital_gain        0.13089148 Ranger
# 14     _baseline_        0.49439405 Ranger

plot(mp_adult_ranger_test)

# #Partial Dependence profile, feature effects
# mprof_adult_ranger_test <- DALEX::model_profile(model_adult_ranger_test, "age")
# mprof_adult_ranger_test
# 
# plot(mprof_adult_ranger_test) +
#   theme(legend.position = "top") +
#   ggtitle("Partial Dependence for Ranger Credit model","")

# ###
# # woe binning
# library(scorecard)
# 
# manual_bins <- list()
# 
# bins <- woebin(adult[split$train,], y = "income", method = "chimerge", bin_num_limit = 6, positive = ">50K", breaks_list = manual_bins)
# 
# #woebin_plot(bins, x = "sex")
# 
# # assign woes
# train_woes <- woebin_ply(adult[split$train,], bins, to = "woe")
# test_woes <- woebin_ply(adult[split$test,], bins, to = "woe")
# 
# # compute information value
# iv(train_woes, y = "income", positive = ">50K")
# # variable  info_value
# # 1: marital_status_woe 1.265321208
# # 2:   relationship_woe 1.234404888
# # 3:            age_woe 1.111300592
# # 4:   capital_gain_woe 0.594385581
# # 5: hours_per_week_woe 0.467472135
# # 6:     occupation_woe 0.434597930
# # 7:      education_woe 0.417825503
# # 8:            sex_woe 0.302594784
# # 9:           race_woe 0.037315179
# # 10:      workclass_woe 0.021956690
# # 11: native_country_woe 0.006105797
# # 12:   capital_loss_woe 0.000000000
# 
# # compute correlation
# cor(train_woes[,-1])

