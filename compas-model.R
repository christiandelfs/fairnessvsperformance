#library(DBI)
#library(fairml)

library("mlr3")
library("mlr3learners")
library("mlr3fairness")

setwd("path/to/r-scripts")

# mydb <- dbConnect(RSQLite::SQLite(), "compas.db")
# 
# dbListTables(mydb)
# #"casearrest", "charge", "compas", "jailhistory", "people", "prisonhistory", "summary"
# dbGetQuery(mydb, 'SELECT * FROM prisonhistory LIMIT 5')
# 
# #dbDisconnect(mydb)

#data(compas)

# load dataset
compas <- read.table("compas-scores-two-years.csv", sep =",", header = TRUE, stringsAsFactors = TRUE)

#str(compas)

compas$length_of_stay <- as.integer(as.Date(compas$c_jail_out) - as.Date(compas$c_jail_in))
compas <- compas[which(!is.na(compas$length_of_stay)),]

compas$raceb <- as.factor(ifelse(compas$race == "African-American", "black", "not-black"))

# select/remove columns
compas <- compas[,c("sex", "age", "age_cat", "raceb", "race", "juv_fel_count", "juv_misd_count", "juv_other_count", "priors_count", "c_charge_degree", "is_recid", "decile_score", "score_text", "length_of_stay", "two_year_recid")]

compas$two_year_recid <- as.factor(compas$two_year_recid)

# create classification task
compas_task <- TaskClassif$new(id = "compas",
                               backend = compas,
                               target = "two_year_recid",
                               positive = "1")

compas_task$set_col_roles("raceb", add_to = "pta")
compas_task$set_col_roles("raceb", remove_from = "feature")
compas_task$set_col_roles("sex", remove_from = "feature")
compas_task$set_col_roles("race", remove_from = "feature")

# split data into train and test set
split = partition(compas_task, ratio = 0.8)

# learner ranger
compas_ranger <- lrn("classif.ranger",
                     predict_type = "prob",
                     num.threads = 4
)

compas_ranger$train(compas_task, split$train)

compas_ranger_prd <- compas_ranger$predict(compas_task, split$test)
# compas_ranger_prd$set_threshold(0.5)

compas_ranger_prd$score(list(msr("fairness.fpr"), msr("fairness.fnr")), task = compas_task)

m_fpr <- groupwise_metrics(msr("classif.fpr"), compas_task)
compas_ranger_prd$score(m_fpr, compas_task)

m_fnr <- groupwise_metrics(msr("classif.fnr"), compas_task)
compas_ranger_prd$score(m_fnr, compas_task)
