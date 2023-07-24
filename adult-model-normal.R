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
#adult_task$col_roles$pta <- "sex"
adult_task$set_col_roles("sex", add_to = "pta")
#adult_task$set_col_roles("fnlwgt", add_to = "weight")

# remove feature
adult_task$set_col_roles("sex", remove_from = "feature")
adult_task$set_col_roles("race", remove_from = "feature")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

# split data into train and test set
split <- partition(adult_task, ratio = 0.8)#, stratify = TRUE

###
# learner ranger
adult_ranger <- lrn("classif.ranger",
                    predict_type = "prob",
                    num.threads = 4,
                    respect.unordered.factors = "order"
)



# normal
# train model
adult_ranger$train(adult_task, split$train)

# prediction on test data
adult_ranger_prd <- adult_ranger$predict(adult_task, split$test)
#adult_ranger_prd$set_threshold(0.5)

# scoring AUC+GUI
adult_ranger_prd$score(msout, adult_task)

# classif.auc fairness.pp_np 
# 0.9223984      0.2272191

# adult_ranger_prd$score(msr("classif.tpr"), task = adult_task)
# # 0.6332905
# 
# adult_ranger_prd$score(msr("classif.fpr"), task = adult_task)
# # 0.05894227
# 
# adult_ranger_prd$score(msr("classif.tnr"), task = adult_task)
# # 0.9410577
# 
# adult_ranger_prd$score(msr("classif.fnr"), task = adult_task)
# # 0.3667095