library("mlr3")
library("mlr3learners")

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
adult_task$set_col_roles("fnlwgt", add_to = "weight")

# remove feature
adult_task$set_col_roles("sex", remove_from = "feature")
adult_task$set_col_roles("race", remove_from = "feature")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

###
# learner ranger
adult_ranger <- lrn("classif.ranger",
                    predict_type = "prob",
                    num.threads = 4,
                    respect.unordered.factors = "order"
)



# resampling
rr <- resample(adult_task, adult_ranger, rsmp("cv", folds = 5))
#rr <- resample(adult_task, adult_ranger, rsmp("bootstrap", repeats = 20, ratio = 0.8))

# rr_prd <- rr$prediction()
# # rr_prd$set_threshold(0.5)
# rr_prd$score(msout, adult_task)
# 
# rr$aggregate(msout)

# rsmp("cv", folds = 5)
# classif.auc fairness.pp_np 
# 0.9168951      0.2457661

# rsmp("bootstrap", repeats = 20, ratio = 0.8)
# classif.auc fairness.pp_np 
# 0.9135194      0.2399244



# ###
# rr_prd <- rr$prediction()
# 
# # rsmp("cv", folds = 5)
# 
# rr_prd$score(msr("classif.tpr"), task = adult_task)
# # 0.6418242
# 
# rr_prd$score(msr("classif.fpr"), task = adult_task)
# # 0.06529404
# 
# rr_prd$score(msr("classif.tnr"), task = adult_task)
# # 0.934706
# 
# rr_prd$score(msr("classif.fnr"), task = adult_task)
# # 0.3581758



# ###
# rr$score(msout)[,c("iteration", "classif.auc", "fairness.pp_np")]
# 
# # rsmp("cv", folds = 5)
# iteration classif.auc fairness.pp_np
# 1:         1   0.9179065      0.2525822
# 2:         2   0.9152429      0.2103250
# 3:         3   0.9203729      0.2790084
# 4:         4   0.9137553      0.2635042
# 5:         5   0.9171977      0.2234108

# # rsmp("bootstrap", repeats = 20, ratio = 0.8)
# 
# # auc
# # min
# min(rr$score(msr("classif.auc"))[,c("classif.auc")])
# # 0.9116822
# 
# # max
# max(rr$score(msr("classif.auc"))[,c("classif.auc")])
# # 0.9167516
# 
# # gui
# # min
# min(rr$score(msr("fairness.gui"))[,c("fairness.pp_np")])
# # 0.2106933
# 
# # max
# max(rr$score(msr("fairness.gui"))[,c("fairness.pp_np")])
# # 0.2781304