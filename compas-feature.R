library("mlr3")
library("mlr3learners")
library("mlr3verse")
library("mlr3fselect")
library("mlr3tuning")
library("ggplot2")

setwd("path/to/r-scripts")

# load dataset
compas <- read.table("compas-scores-two-years.csv", sep =",", header = TRUE, stringsAsFactors = TRUE)

#str(compas)

compas$length_of_stay <- as.integer(as.Date(compas$c_jail_out) - as.Date(compas$c_jail_in))
#compas$raceb <- as.factor(ifelse(compas$race == "African-American", "black", "not-black"))

# select/remove columns
compas <- compas[,c("sex", "age", "age_cat", "race", "juv_fel_count", "juv_misd_count", "juv_other_count", "priors_count", "c_charge_degree", "is_recid", "decile_score", "score_text", "length_of_stay", "two_year_recid")]#, "raceb"

compas <- compas[which(!is.na(compas$length_of_stay)),]

compas$two_year_recid <- as.factor(compas$two_year_recid)

set.seed(4)

# create classification task
compas_task <- TaskClassif$new(id = "compas",
                            backend = compas,
                            target = "two_year_recid",
                            positive = "1")

# split data into train and test set
split = partition(compas_task, ratio = 0.8)

# random forrest
# learner ranger
compas_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  importance = "permutation",#"none", "impurity", "impurity_corrected", "permutation"
                  num.threads = 4
)

compas_ranger_train <- compas_ranger$train(compas_task, split$train)

#feature importance
compas_ranger_train$model$variable.importance
# age         age_cat c_charge_degree    decile_score        is_recid   juv_fel_count 
# 0.0044895826    0.0025273493    0.0004601364    0.0067578868    0.4058212497    0.0001927835 
# juv_misd_count juv_other_count  length_of_stay    priors_count            race      score_text 
# 0.0004868762    0.0008766713    0.0007570899    0.0062673716    0.0002668157    0.0034415167 
# sex 
# 0.0001091544

#compas_ranger_prd <- compas_ranger$predict(compas_test, split$test)

# # learner log reg
# compas_logreg <- lrn("classif.log_reg",
#                   predict_type = "prob")
# 
# compas_logreg_train <- compas_logreg$train(compas_task, split$train)
# 
# #compas_logreg_prd <- compas_logreg$predict(compas_task, split$test)

###
#mlr3
#feature selection

#as.data.table(mlr_learners)[sapply(properties, function(x) "importance" %in% x)]
#as.data.table(mlr_learners)[sapply(properties, function(x) "selected_features" %in% x)]

filter = flt("importance", learner = compas_ranger)
filter = flt("permutation", learner = compas_ranger)#compas_logreg
filter = flt("auc", learner = compas_logreg)
filter$calculate(compas_task)
filter$scores
as.data.table(filter)

#importance
# is_recid    decile_score    priors_count             age         age_cat      score_text 
# 0.4070368466    0.0069126662    0.0057575111    0.0047264638    0.0028606155    0.0023845240 
# juv_other_count c_charge_degree  length_of_stay  juv_misd_count            race             sex 
# 0.0007175880    0.0005904300    0.0005535033    0.0005087096    0.0004229419    0.0003085750 
# juv_fel_count 
# 0.0002502759

#permutation learner = compas_ranger


#permutation learner = compas_logreg


# # fselector
# instance = fselect(
#   fselector = fs("sequential"),
#   task =  compas_task,
#   learner = compas_ranger,
#   resampling = rsmp("cv", folds = 5),
#   measure = msr("classif.auc")
# )
# 
# instance$archive
# instance$result

# instance = fselect(
#   fselector = fs("sequential", strategy = "sbs"),
#   task =  compas_task,
#   learner = compas_ranger,
#   resampling = rsmp("cv", folds=5),
#   measure = msr("classif.auc")
# )

#fselectinstance single crit
instance = fsi(
  task = compas_task,
  learner = compas_ranger,
  resampling = rsmp("cv", folds=5),
  measure = msr("classif.auc"),
  terminator = trm("evals", n_evals = 20)
)

fselector = fs("sequential", strategy = "sbs")
fselector$optimize(instance)

#instance$archive
instance$result



# #fselectinstance multi crit
# instance = fsi(
#   task = compas_task,
#   learner = compas_ranger,
#   resampling = rsmp("cv", folds=5),
#   measure = msrs(c("classif.tp", "classif.fp")),
#   terminator = trm("evals", n_evals = 20)
# )
# 
# fselector = fs("sequential", strategy = "sbs")
# fselector$optimize(instance)

# #auto fselector
# at = auto_fselector(
#   fselector = fs("random_search"),
#   learner = compas_ranger,
#   resampling = rsmp("cv", folds=5),
#   measure = msr("classif.auc"),
#   terminator = trm("evals", n_evals = 10)
# )
# 
# grid = benchmark_grid(
#   task = compas_task,
#   learner = list(at, lrn("classif.ranger", predict_type = "prob")),
#   resampling = rsmp("cv", folds = 3)
# )
# 
# bmr = benchmark(grid)
# bmr$aggregate(msrs(c("classif.auc", "time_train")))

###
# dalex model analysis

# predict_function = function(m,x) m$predict_newdata(newdata = x)$prob[,1],

model_compas_ranger_test <- DALEX::explain(compas_ranger_train,#compas_logreg_train
                                        predict_function = function(m,x) predict(m, x, predict_type = "prob")[,1],
                                        data = compas[split$test, ncol(compas) * -1],
                                        y = compas[split$test,]$two_year_recid == "1",
                                        type = "classification",
                                        label = "Ranger",
                                        verbose = FALSE)

# #model performance
# mperf_compas_ranger_test <- DALEX::model_performance(model_compas_ranger_test)
# mperf_compas_ranger_test
# 
# plot(mperf_compas_ranger_test, geom = "roc")

#model parts, feature importance
mp_compas_ranger_test <- DALEX::model_parts(model_compas_ranger_test)
mp_compas_ranger_test
# variable mean_dropout_loss  label
# 1     _full_model_        0.02854517 Ranger
# 2   length_of_stay        0.02621151 Ranger
# 3              sex        0.02717908 Ranger
# 4  c_charge_degree        0.02813067 Ranger
# 5          age_cat        0.02839688 Ranger
# 6  juv_other_count        0.02872548 Ranger
# 7              age        0.02887000 Ranger
# 8   juv_misd_count        0.02888109 Ranger
# 9    juv_fel_count        0.02903639 Ranger
# 10            race        0.02928621 Ranger
# 11      score_text        0.02938088 Ranger
# 12    priors_count        0.02945664 Ranger
# 13    decile_score        0.03136340 Ranger
# 14        is_recid        0.41216036 Ranger
# 15      _baseline_        0.50175964 Ranger

plot(mp_compas_ranger_test)

# #Partial Dependence profile, feature effects
# mprof_compas_ranger_test <- DALEX::model_profile(model_compas_ranger_test, "priors_count")
# mprof_compas_ranger_test
# plot(mprof_compas_ranger_test)
# 
# plot(mprof_compas_ranger_test) +
#   theme(legend.position = "top") +
#   ggtitle("Partial Dependence for Ranger Credit model","")

# ###
# # woe binning
# library(scorecard)
# 
# manual_bins <- list()
# 
# bins <- woebin(compas[split$train,], y = "two_year_recid", method = "chimerge", bin_num_limit = 6, positive = "1", breaks_list = manual_bins)
# 
# woebin_plot(bins, x = "length_of_stay")
# 
# # assign woes
# train_woes <- woebin_ply(compas[split$train,], bins, to = "woe")
# train_woes
# test_woes <- woebin_ply(compas[split$test,], bins, to = "woe")
# 
# # compute information value
# iv(train_woes, y = "two_year_recid", positive = "1")
# 
# # compute correlation
# c <- cor(train_woes[,-1])
