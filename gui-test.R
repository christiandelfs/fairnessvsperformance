library("mlr3")
library("mlr3learners")
library("mlr3fairness")

setwd("path/to/r-scripts")

# load dataset
source("sgc.R")

set.seed(4)

#sgc <- sgc[,c("status", "duration", "credit_history", "purpose", "savings", "sex", "credit_risk")]

# load gui
source("gui.R")

# create classification task
sgc_task <- TaskClassif$new(id = "sgc",
                            backend = sgc,
                            target = "credit_risk",
                            positive = "bad")

# set protected attribute sex
#sgc_task$col_roles$pta <- "sex"
sgc_task$set_col_roles("sex", add_to = "pta")
#sgc_task$set_col_roles("sex", add_to = "stratum")

# remove feature
sgc_task$set_col_roles("sex", remove_from = "feature")

# split data into train and test set
split = partition(sgc_task, ratio = 0.8)

# learner ranger
sgc_ranger <- lrn("classif.ranger",
                  predict_type = "prob",
                  num.threads = 4,
                  respect.unordered.factors = "order"
)

#sgc_ranger_train <- sgc_ranger$train(sgc_task)
sgc_ranger_train <- sgc_ranger$train(sgc_task, split$train)

# predict on sgc_task
sgc_ranger_prd = sgc_ranger$predict(sgc_task, split$test)
#sgc_ranger_prd$set_threshold(0.3)

sgc_ranger_prd_score <- sgc_ranger_prd$score(msr("fairness.gui"), task = sgc_task)
sgc_ranger_prd_score

# fairness.pp_np 
# 0.7705463 

# predict_newdata on sgc_task_test
sgc_task_test <- TaskClassif$new(id = "sgc_test",
                            backend = sgc[split$test,],
                            target = "credit_risk",
                            positive = "bad")

sgc_task_test$set_col_roles("sex", add_to = "pta")
sgc_task_test$set_col_roles("sex", remove_from = "feature")

sgc_ranger_prd_test = sgc_ranger$predict_newdata(sgc[split$test,])
#sgc_ranger_prd_test$set_threshold(0.3)

sgc_ranger_prd_test_score <- sgc_ranger_prd_test$score(msr("fairness.gui"), task = sgc_task_test)
sgc_ranger_prd_test_score

# fairness.pp_np 
# 0.7705463

unfairness <- function(yhat = pds > 0.3, protected = test$gender){
  tab <- prop.table(table(yhat, protected), 2)
  sum((tab[,1] - tab[,2]) * log(tab[,1]/tab[,2]))
}

unfairness(yhat = sgc_ranger_prd$response, protected = sgc[split$test,]$sex)

# ifelse(sgc_ranger_prd$prob[,"good"] > 0.5, "good", "bad")
# sgc_ranger_prd$response

# [1] 0.7705463
