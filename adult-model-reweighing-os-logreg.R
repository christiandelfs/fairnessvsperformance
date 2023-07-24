library("mlr3")
library("mlr3learners")
library("ggplot2")
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
#adult_task$col_roles$pta <- "sex"
adult_task$set_col_roles("sex", add_to = "pta")
#adult_task$set_col_roles("fnlwgt", add_to = "weight")

#adult_task$set_col_roles("sex", remove_from = "feature")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

###
# learner ranger
adult_log_reg <- lrn("classif.log_reg",
                    predict_type = "prob"
)



###
# reweighing os
po_rw_os <- PipeOpReweighingOversampling$new(id = "reweighing_os")#, param_vals = list(alpha = 1.0)
task_po_rw_os <- po_rw_os$train(list(adult_task))
#task_po_rw_os[[1]]

# set proteced attribute and weights
task_po_rw_os$output$set_col_roles("sex", add_to = "pta")
#task_po_rw_os$output$set_col_roles("fnlwgt", add_to = "weight")

# remove feature
task_po_rw_os$output$set_col_roles("sex", remove_from = "feature")
task_po_rw_os$output$set_col_roles("race", remove_from = "feature")
task_po_rw_os$output$set_col_roles("fnlwgt", remove_from = "feature")
task_po_rw_os$output$set_col_roles("native_country", remove_from = "feature")

# split data into train and test set
split <- partition(task_po_rw_os$output, ratio = 0.8)

###
# normal
adult_log_reg$train(task_po_rw_os$output, split$train)

adult_log_reg_prd <- adult_log_reg$predict(task_po_rw_os$output, split$test)
# adult_log_reg_prd$set_threshold(0.5)

adult_log_reg_prd$score(msout, task_po_rw_os$output)

# classif.auc fairness.pp_np 
# 0.903193862    0.002315639

# ###
# # resampling
# #rr_rw_os <- resample(task_po_rw_os$output, adult_log_reg, rsmp("cv", folds = 5))
# rr_rw_os <- resample(task_po_rw_os$output, adult_log_reg, rsmp("bootstrap", repeats = 20, ratio = 0.8))
# 
# # rr_rw_os_prd <- rr_rw_os$prediction()
# # # rr_rw_os_prd$set_threshold(0.5)
# # rr_rw_os_prd$score(msout, task_po_rw_os)
# 
# rr_rw_os$aggregate(msout)

# rsmp("cv", folds = 5)
# classif.auc fairness.pp_np 
# 0.900889569    0.003699172

# rsmp("bootstrap", repeats = 20, ratio = 0.8)
# classif.auc fairness.pp_np 
# 0.900628737    0.003715205

# rr_rw_os_prd <- rr_rw_os$prediction()
# 
# # rsmp("cv", folds = 5)
# 
# rr_rw_os_prd$score(msr("classif.tpr"), task = adult_task)
# # 0.5810776 
# 
# rr_rw_os_prd$score(msr("classif.fpr"), task = adult_task)
# # 0.06337402
# 
# rr_rw_os_prd$score(msr("classif.tnr"), task = adult_task)
# # 0.936626
# 
# rr_rw_os_prd$score(msr("classif.fnr"), task = adult_task)
# # 0.4189224