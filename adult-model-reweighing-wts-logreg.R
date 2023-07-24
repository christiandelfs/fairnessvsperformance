library("mlr3")
library("mlr3learners")
library("mlr3pipelines")

setwd("path/to/r-scripts")

set.seed(4)

# load dataset
source("adult.R")

# create gui fairness measure
source("gui.R")

msout <- list(msr("classif.auc"), msr("fairness.gui"))

# create classification task
adult_task <- TaskClassif$new(id = "adult",
                              backend = adult,
                              target = "income",
                              positive = ">50K")

# set protected attribute and weights
adult_task$set_col_roles("sex", add_to = "pta")
#adult_task$set_col_roles("fnlwgt", add_to = "weight")

# remove weights from feature
#adult_task$set_col_roles("sex", remove_from = "feature")
#adult_task$set_col_roles("race", remove_from = "feature")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

# split data into train and test set
split <- partition(adult_task, ratio = 0.8)#, stratify = TRUE

###
# learner ranger
adult_log_reg <- lrn("classif.log_reg",
                    predict_type = "prob"
)



###
# reweighing wts
po_rw_wts <- PipeOpReweighingWeights$new(id = "reweighing_wts", param_vals = list(alpha = 1.0))
task_po_rw_wts <- po_rw_wts$train(list(adult_task))
#task_po_rw_wts[[1]]

# remove feature sex+race
task_po_rw_wts$output$set_col_roles("sex", remove_from = "feature")
task_po_rw_wts$output$set_col_roles("race", remove_from = "feature")

# normal
# train model
adult_log_reg$train(task_po_rw_wts$output, split$train)

# prediction
adult_log_reg_prd <- adult_log_reg$predict(task_po_rw_wts$output, split$test)
#adult_log_reg_prd$set_threshold(0.5)

# scoring
adult_log_reg_prd$score(msout, task_po_rw_wts$output)

# classif.auc fairness.pp_np 
# 0.9065155      0.2175245

# ###
# # resampling
# rr_rw_wts <- resample(task_po_rw_wts$output, adult_log_reg, rsmp("cv", folds = 5))
# #rr_rw_wts <- resample(task_po_rw_wts$output, adult_log_reg, rsmp("bootstrap", repeats = 20, ratio = 0.8))
# 
# # rr_rw_wts_prd <- rr_rw_wts$prediction()
# # # rr_rw_wts_prd$set_threshold(0.5)
# # rr_rw_wts_prd$score(msout, adult_task)
# 
# rr_rw_wts$aggregate(msout)

# rsmp("cv", folds = 5)
# classif.auc fairness.pp_np 
# 0.9132833      0.1045893

# rsmp("bootstrap", repeats = 20, ratio = 0.8)
# classif.auc fairness.pp_np 
# 0.9107037      0.1153809

# rr_rw_wts_prd <- rr_rw_wts$prediction()
# 
# # rsmp("cv", folds = 5)
# 
# rr_rw_wts_prd$score(msr("classif.tpr"), task = adult_task)
# # 0.6090528
# 
# rr_rw_wts_prd$score(msr("classif.fpr"), task = adult_task)
# # 0.05966895
# 
# rr_rw_wts_prd$score(msr("classif.tnr"), task = adult_task)
# # 0.940331
# 
# rr_rw_wts_prd$score(msr("classif.fnr"), task = adult_task)
# # 0.3909472