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

# set protected attribute sex
#adult_task$col_roles$pta <- "sex"
adult_task$set_col_roles("sex", add_to = "pta")
#adult_task$set_col_roles("fnlwgt", add_to = "weight")

adult_task$set_col_roles("sex", remove_from = "feature")
adult_task$set_col_roles("race", remove_from = "feature")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")
adult_task$set_col_roles("native_country", remove_from = "feature")

###
# learner log_reg
adult_log_reg <- lrn("classif.log_reg",
                    predict_type = "prob"
)



# resampling
#rr <- resample(adult_task, adult_log_reg, rsmp("cv", folds = 5))
rr <- resample(adult_task, adult_log_reg, rsmp("bootstrap", repeats = 20, ratio = 0.8))

# rr_prd <- rr$prediction()
# # rr_prd$set_threshold(0.5)
# rr_prd$score(msout, adult_task)

rr$aggregate(msout)

# rsmp("cv", folds = 5)
# Faktor 'native_country' hat neue Stufen Holand-Netherlands
# classif.auc fairness.pp_np 
# 0.9049747      0.2421142

# rsmp("bootstrap", repeats = 20, ratio = 0.8)
# classif.auc fairness.pp_np 
# 0.9048555      0.2355729

# rr_prd <- rr$prediction()
# 
# # rsmp("cv", folds = 5)
# 
# rr_prd$score(msr("classif.tpr"), task = adult_task)
# # 0.5940789
# 
# rr_prd$score(msr("classif.fpr"), task = adult_task)
# # 0.06779707
# 
# rr_prd$score(msr("classif.tnr"), task = adult_task)
# # 0.9322029
# 
# rr_prd$score(msr("classif.fnr"), task = adult_task)
# # 0.4059211



###
rr$score(msout)[,c("iteration", "classif.auc", "fairness.pp_np")]

# rsmp("cv", folds = 5)
# iteration classif.auc fairness.pp_np
# 1:         1   0.9066261      0.2451025
# 2:         2   0.9032105      0.2089770
# 3:         3   0.9074202      0.2486036
# 4:         4   0.9043201      0.2724963
# 5:         5   0.9032965      0.2353915

# # rsmp("bootstrap", repeats = 20, ratio = 0.8)
# 
# # auc
# # min
# min(rr$score(msr("classif.auc"))[,c("classif.auc")])
# # 0.9012883
# 
# # max
# max(rr$score(msr("classif.auc"))[,c("classif.auc")])
# # 0.9083212
# 
# # gui
# # min
# min(rr$score(msr("fairness.gui"))[,c("fairness.pp_np")])
# # 0.2089542
# 
# # max
# max(rr$score(msr("fairness.gui"))[,c("fairness.pp_np")])
# # 0.2794767