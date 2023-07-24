library("mlr3")
library("mlr3learners")
library("mlr3fairness")

setwd("path/to/r-scripts")

set.seed(4)

# load dataset
source("adult.R")

# create gui fairness measure
source("gui.R")

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

# split data into train and test set
split <- partition(adult_task, ratio = 0.8)

###
# learner ranger
adult_ranger <- lrn("classif.ranger",
                    predict_type = "prob",
                    num.threads = 4,
                    respect.unordered.factors = "order"
)


###
# normal
adult_ranger$train(adult_task, split$train)

adult_ranger_prd <- adult_ranger$predict(adult_task, split$test)
#adult_ranger_prd$set_threshold(0.5)

# select test subset
adult_split_test <- adult[split$test,]
# add column with respones and probs
adult_split_test$response <- adult_ranger_prd$response
adult_split_test$prob <- adult_ranger_prd$prob[,c(">50K")]

###
# Demographic parity/statistical parity/acceptance rate parity/benchmarking
# manually
nrow(adult_split_test[which(adult_split_test$sex == "Male" & adult_split_test$response == ">50K"),]) / nrow(adult_split_test[which(adult_split_test$sex == "Male"),])
# 0.2612462
nrow(adult_split_test[which(adult_split_test$sex == "Female" & adult_split_test$response == ">50K"),]) / nrow(adult_split_test[which(adult_split_test$sex == "Female"),])
# 0.08500627

# with created classif.pp measure
msr_pp <- groupwise_metrics(msr("classif.pp"), adult_task)
adult_ranger_prd$score(msr_pp, adult_task)
# subgroup.pp_Male subgroup.pp_Female 
# 0.26124620         0.08500627

###
# Conditional statistical parity
nrow(adult_split_test[which(adult_split_test$sex == "Male" & adult_split_test$education == "Masters" & adult_split_test$response == ">50K"),]) / nrow(adult_split_test[which(adult_split_test$sex == "Male" & adult_split_test$education == "Masters"),])
# 0.7124352
nrow(adult_split_test[which(adult_split_test$sex == "Female" & adult_split_test$education == "Masters" & adult_split_test$response == ">50K"),]) / nrow(adult_split_test[which(adult_split_test$sex == "Female" & adult_split_test$education == "Masters"),])
# 0.2941176

###
# Predictive parity/outcome test
# with classif.ppv measure
msr_c_ppv <- groupwise_metrics(msr("classif.ppv"), adult_task)
adult_ranger_prd$score(msr_c_ppv, adult_task)
# subgroup.ppv_Male subgroup.ppv_Female 
# 0.7661431           0.7564576

# msr_c_fdr <- groupwise_metrics(msr("classif.fdr"), adult_task)
# adult_ranger_prd$score(msr_c_fdr, adult_task)
# # subgroup.fdr_Male subgroup.fdr_Female 
# # 0.2338569           0.2435424

# manually
nrow(adult_split_test[which(adult_split_test$sex == "Male" & adult_split_test$income == ">50K" & adult_split_test$response == ">50K"),]) / nrow(adult_split_test[which(adult_split_test$sex == "Male" & adult_split_test$response == ">50K"),])
# 0.7661431
nrow(adult_split_test[which(adult_split_test$sex == "Female" & adult_split_test$income == ">50K" & adult_split_test$response == ">50K"),]) / nrow(adult_split_test[which(adult_split_test$sex == "Female" & adult_split_test$response == ">50K"),])
# 0.7564576

###
# balance for positive class
mean(adult_split_test[which(adult_split_test$sex == "Male" & adult_split_test$income == ">50K"),]$prob)
# 0.6331672
mean(adult_split_test[which(adult_split_test$sex == "Female" & adult_split_test$income == ">50K"),]$prob)
# 0.5681477
