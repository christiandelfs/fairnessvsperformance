# library(fairmodels)
# library(gbm)
# library(DALEX)

#https://cran.r-project.org/web/packages/fairmodels/vignettes/Advanced_tutorial.html

adult <- fairmodels::adult

# prop.table(table(adult$salary, adult$race))
# # Amer-Indian-Eskimo Asian-Pac-Islander        Black        Other        White
# # <=50K       0.0084456866       0.0234329412 0.0840576149 0.0075550505 0.6356991493
# # >50K        0.0011056171       0.0084763981 0.0118853844 0.0007677897 0.2185743681

adult$salary   <- as.numeric(adult$salary) -1 # 0=bad, 1=good
protected     <- adult$race
adult <- adult[colnames(adult) != "race"] # remove race

# making model
set.seed(1)
gbm_model <- gbm::gbm(salary ~. , data = adult, distribution = "bernoulli")

# making explainer
gbm_explainer <- DALEX::explain(gbm_model,
                                data = adult[,-1],
                                y = adult$salary,
                                colorize = FALSE)

fobject <- fairmodels::fairness_check(gbm_explainer, 
                                      protected  = protected, 
                                      privileged = "White", 
                                      colorize = FALSE)

plot(fobject)
