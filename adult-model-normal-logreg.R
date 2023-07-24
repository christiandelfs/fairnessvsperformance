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

split <- partition(adult_task, ratio = 0.8)#, stratify = TRUE

###
# learner log_reg
adult_log_reg <- lrn("classif.log_reg",
                    predict_type = "prob"
)



# normal
adult_log_reg$train(adult_task, split$train)

adult_log_reg_prd <- adult_log_reg$predict(adult_task, split$test)
#adult_log_reg_prd$set_threshold(0.5)

adult_log_reg_prd$score(msout, adult_task)

# classif.auc fairness.pp_np 
# 0.9065155      0.2175245

# ohne native_country
# classif.auc fairness.pp_np 
# 0.9064463      0.2211267