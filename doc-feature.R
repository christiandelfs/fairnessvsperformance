library("mlr3")
library("mlr3learners")
library("mlr3verse")
library("mlr3fselect")
library("mlr3tuning")
library("ggplot2")
library("scorecard")

setwd("path/to/r-scripts")

# load dataset
load(file = "doc.Robj")

# remove observations with empty values
doc <- doc[which(!(doc$EDUCATION %in% c(0, 5, 6)) & doc$MARRIAGE != 0),]

# remove columns PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6
# doc <- doc[,c("LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE", "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6", "default.payment.next.month")]
# doc <- doc[,c("LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6", "default.payment.next.month")]

set.seed(4)

# create classification task
doc_task <- TaskClassif$new(id = "doc",
                            backend = doc,
                            target = "default.payment.next.month",
                            positive = "yes")

# split data into train and test set
split = partition(doc_task, ratio = 0.8)

# random forrest
# learner ranger
doc_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  importance = "permutation",#"none", "impurity", "impurity_corrected", "permutation"
                  num.threads = 4
)

doc_ranger_train <- doc_ranger$train(doc_task, split$train)

#feature importance
doc_ranger_train$model$variable.importance
# AGE    BILL_AMT1    BILL_AMT2    BILL_AMT3    BILL_AMT4    BILL_AMT5    BILL_AMT6    EDUCATION 
# 0.0019722131 0.0128892128 0.0206294477 0.0195313037 0.0181221653 0.0127477794 0.0127077970 0.0002094785 
# ID    LIMIT_BAL     MARRIAGE        PAY_0        PAY_2        PAY_3        PAY_4        PAY_5 
# 0.0006854605 0.0109562589 0.0008576867 0.0383551416 0.0170544991 0.0100115903 0.0075267203 0.0076431572 
# PAY_6     PAY_AMT1     PAY_AMT2     PAY_AMT3     PAY_AMT4     PAY_AMT5     PAY_AMT6          SEX 
# 0.0067460284 0.0166030145 0.0112487056 0.0106577081 0.0073479565 0.0087108078 0.0056128135 0.0002355306

#doc_ranger_prd <- doc_ranger$predict(doc_task, split$test)

# # learner log reg
# doc_logreg <- lrn("classif.log_reg",
#                   predict_type = "prob")
# 
# doc_logreg_train <- doc_logreg$train(doc_task, split$train)
# 
# #doc_logreg_prd <- doc_logreg$predict(doc_task, split$test)

###
#mlr3
#feature selection

#as.data.table(mlr_learners)[sapply(properties, function(x) "importance" %in% x)]
#as.data.table(mlr_learners)[sapply(properties, function(x) "selected_features" %in% x)]

filter = flt("importance", learner = doc_ranger)
filter = flt("permutation", learner = doc_ranger)#doc_logreg
filter = flt("auc", learner = doc_ranger)
filter$calculate(doc_task)
filter$scores

#importance
# PAY_0    BILL_AMT2    BILL_AMT3    BILL_AMT4     PAY_AMT1        PAY_2    BILL_AMT5    BILL_AMT6 
# 0.0377262456 0.0197557524 0.0188682287 0.0178170763 0.0172218181 0.0157156072 0.0147919428 0.0129314326 
# BILL_AMT1        PAY_3     PAY_AMT2     PAY_AMT3    LIMIT_BAL     PAY_AMT5        PAY_4     PAY_AMT4 
# 0.0119649796 0.0114749945 0.0114461289 0.0105912597 0.0090711225 0.0087069339 0.0086969658 0.0080729372 
# PAY_5        PAY_6     PAY_AMT6          AGE     MARRIAGE           ID    EDUCATION          SEX 
# 0.0075398578 0.0072110634 0.0056146907 0.0021129492 0.0007042028 0.0006597708 0.0002057124 0.0001498304

#permutation learner = doc_ranger


#permutation learner = doc_logreg


# # fselector
# instance = fselect(
#   fselector = fs("sequential"),
#   task =  doc_task,
#   learner = doc_ranger,
#   resampling = rsmp("cv", folds = 5),
#   measure = msr("classif.auc")
# )
# 
# instance$archive
# instance$result

# instance = fselect(
#   fselector = fs("sequential", strategy = "sbs"),
#   task =  doc_task,
#   learner = doc_ranger,
#   resampling = rsmp("cv", folds=5),
#   measure = msr("classif.auc")
# )

#fselectinstance single crit
instance = fsi(
  task = doc_task,
  learner = doc_ranger,
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
#   task = doc_task,
#   learner = doc_ranger,
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
#   learner = lrn("classif.ranger", predict_type = "prob"),
#   resampling = rsmp("cv", folds=5),
#   measure = msr("classif.auc"),
#   terminator = trm("evals", n_evals = 10)
# )
# 
# grid = benchmark_grid(
#   task = doc_task,
#   learner = list(at, lrn("classif.log_reg", predict_type = "prob")),
#   resampling = rsmp("cv", folds = 3)
# )
# 
# bmr = benchmark(grid)
# bmr$aggregate(msrs(c("classif.auc", "time_train")))

###
# dalex model analysis

# predict_function = function(m,x) m$predict_newdata(newdata = x)$prob[,1],

model_doc_ranger_test <- DALEX::explain(doc_ranger_train,#doc_logreg_train
                                        predict_function = function(m,x) predict(m, x, predict_type = "prob")[,1],
                                        data = doc[split$test, ncol(doc) * -1],
                                        y = doc[split$test,]$default.payment.next.month == "yes",
                                        type = "classification",
                                        label = "Ranger",
                                        verbose = FALSE)

# #model performance
# mperf_doc_ranger_test <- DALEX::model_performance(model_doc_ranger_test)
# mperf_doc_ranger_test
# 
# plot(mperf_doc_ranger_test, geom = "roc")

#model parts, feature importance
mp_doc_ranger_test <- DALEX::model_parts(model_doc_ranger_test)
mp_doc_ranger_test
# variable mean_dropout_loss  label
# 1  _full_model_         0.2266496 Ranger
# 2     BILL_AMT5         0.2244979 Ranger
# 3     EDUCATION         0.2259421 Ranger
# 4      MARRIAGE         0.2261505 Ranger
# 5      PAY_AMT5         0.2264171 Ranger
# 6     BILL_AMT6         0.2264862 Ranger
# 7           SEX         0.2265830 Ranger
# 8     BILL_AMT4         0.2267429 Ranger
# 9           AGE         0.2268034 Ranger
# 10     PAY_AMT4         0.2268542 Ranger
# 11     PAY_AMT6         0.2277908 Ranger
# 12     PAY_AMT3         0.2281347 Ranger
# 13    BILL_AMT3         0.2288477 Ranger
# 14           ID         0.2292112 Ranger
# 15    BILL_AMT1         0.2294097 Ranger
# 16        PAY_5         0.2294247 Ranger
# 17    BILL_AMT2         0.2314193 Ranger
# 18     PAY_AMT2         0.2319230 Ranger
# 19        PAY_4         0.2321516 Ranger
# 20        PAY_6         0.2325018 Ranger
# 21     PAY_AMT1         0.2356855 Ranger
# 22        PAY_3         0.2358119 Ranger
# 23        PAY_2         0.2386904 Ranger
# 24    LIMIT_BAL         0.2542221 Ranger
# 25        PAY_0         0.3048171 Ranger
# 26   _baseline_         0.5049540 Ranger

plot(mp_doc_ranger_test)

# #Partial Dependence profile, feature effects
# mprof_doc_ranger_test <- DALEX::model_profile(model_doc_ranger_test, "SEX")
# mprof_doc_ranger_test
# plot(mprof_doc_ranger_test)
# 
# plot(mprof_doc_ranger_test) +
#   theme(legend.position = "top") +
#   ggtitle("Partial Dependence for Ranger Credit model","")

# ###
# # woe binning
# manual_bins <- list()
# 
# bins <- woebin(doc[split$train,], y = "default.payment.next.month", method = "chimerge", bin_num_limit = 6, positive = "yes", breaks_list = manual_bins)
# 
# woebin_plot(bins, x = "PAY_0")
# 
# # assign woes
# train_woes <- woebin_ply(doc[split$train,], bins, to = "woe")
# train_woes
# test_woes <- woebin_ply(doc[split$test,], bins, to = "woe")
# 
# # compute information value
# iv(train_woes, y = "default.payment.next.month", positive = "yes")
# 
# # compute correlation
# cor(train_woes[,-1])
