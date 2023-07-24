library("mlr3")
library("mlr3learners")
library("mlr3pipelines")

setwd("path/to/r-scripts")

# load dataset
source("sgc.R")

set.seed(4)

#sgc <- sgc[,c("status", "duration", "credit_history", "purpose", "savings", "sex", "credit_risk")]

# create gui fairness measure
source("gui.R")

msout <- list(msr("classif.auc"), msr("fairness.gui"))

# create classification task
sgc_task <- TaskClassif$new(id = "sgc",
                            backend = sgc,
                            target = "credit_risk",
                            positive = "bad")

# set protected attribute sex
#sgc_task$col_roles$pta <- "sex"
sgc_task$set_col_roles("sex", add_to = "pta")

# remove feature sex
sgc_task$set_col_roles("sex", remove_from = "feature")

# split data into train and test set
split <- partition(sgc_task, ratio = 0.8)#, stratify = TRUE

###
# learner ranger
sgc_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  num.threads = 4,
                  respect.unordered.factors = "order"#ignore,order,partition
)



# resampling
#rr <- resample(sgc_task, sgc_ranger, rsmp("cv", folds = 5))
rr <- resample(sgc_task, sgc_ranger, rsmp("bootstrap", repeats = 20, ratio = 0.8))

# rr_prd <- rr$prediction()
# # rr_prd$set_threshold(0.3)
# 
# rr_prd$score(msout, sgc_task)

rr$aggregate(msout)

# rsmp("cv", folds = 5)
# classif.auc fairness.pp_np 
# 0.7939035      0.4972108

# repeats = 20, ratio = 0.8)
# classif.auc fairness.pp_np 
# 0.7715352      0.4215399



# ###
# rr$score(msout)[,c("iteration", "classif.auc", "fairness.pp_np")]
# 
# # rsmp("cv", folds = 5)
# # iteration classif.auc fairness.pp_np
# # 1:         1   0.7956113      1.0473970
# # 2:         2   0.7967914      0.3806450
# # 3:         3   0.8642006      0.2934229
# # 4:         4   0.7868852      0.4410625
# # 5:         5   0.7260290      0.3235264

# # rsmp("bootstrap", repeats = 20, ratio = 0.8)
# 
# # auc
# # min
# min(rr$score(msr("classif.auc"))[,c("classif.auc")])
# # 0.7371913
# 
# # max
# max(rr$score(msr("classif.auc"))[,c("classif.auc")])
# # 0.8168588
# 
# # gui
# # min
# min(rr$score(msr("fairness.gui"))[,c("fairness.pp_np")])
# # 0.1978015
# 
# # max
# max(rr$score(msr("fairness.gui"))[,c("fairness.pp_np")])
# # 0.6968491