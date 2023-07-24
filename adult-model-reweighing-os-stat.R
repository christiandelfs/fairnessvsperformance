library("mlr3")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3fairness")
library("ggplot2")
library("ggmosaic")
library("gridExtra")

setwd("path/to/r-scripts")

set.seed(4)

# load dataset
source("adult.R")

# create classification task
adult_task <- TaskClassif$new(id = "adult",
                              backend = adult,
                              target = "income",
                              positive = ">50K")

# set protected attribute and weights
#adult_task$col_roles$pta <- "sex"
adult_task$set_col_roles("sex", add_to = "pta")
adult_task$set_col_roles("fnlwgt", add_to = "weight")

#adult_task$set_col_roles("sex", remove_from = "feature")
#adult_task$set_col_roles("fnlwgt", remove_from = "feature")

###
# reweighing os
po_rw_os <- PipeOpReweighingOversampling$new(id = "reweighing_os")#, param_vals = list(alpha = 1.0)
task_po_rw_os <- po_rw_os$train(list(adult_task))
#task_po_rw_os[[1]]

# fetch data from task
adult_os <- task_po_rw_os$output$data()

###
# number of observations
# before
# total
nrow(adult)# 48842
# male
nrow(adult[which(adult$sex == "Male"),])# 32650
# female
nrow(adult[which(adult$sex == "Female"),])# 16192

# after
# total
nrow(adult_os)# 48750
# male
nrow(adult_os[which(adult_os$sex == "Male"),])# 32641
# female
nrow(adult_os[which(adult_os$sex == "Female"),])# 16109

# # diff before-after
# # total
# 48842 - 48750# 92
# # male
# 32650 - 32641# 9
# # female
# 16192 - 16109# 83

###
# V1 age
hist(adult_os$age)#, plot = FALSE
#wtd.hist(adult_os$age, weight=adult_os$fnlwgt)

p1 <- ggplot(adult_os, aes(x=income, y=age)) + 
  geom_boxplot()

p2 <- ggplot(adult_os, aes(age, fill = income)) + #, weight = fnlwgt
  stat_bin(binwidth = 5, center = 2.5)

grid.arrange(p1, p2, ncol=2)

###
# V2 workclass Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked

prop.table(table(adult_os$workclass))
# ?      Federal-gov        Local-gov     Never-worked          Private     Self-emp-inc 
# 0.0578871795     0.0291282051     0.0653743590     0.0001846154     0.6917128205     0.0334153846 
# Self-emp-not-inc        State-gov      Without-pay 
# 0.0815794872     0.0402666667     0.0004512821

ggplot(adult_os, aes(workclass, fill = income)) + #, weight = fnlwgt
  geom_bar()

ggplot(data = adult_os) +
  geom_mosaic(aes(x=product(workclass), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V4 education Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool

prop.table(table(adult_os$education))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th          9th   Assoc-acdm 
# 0.028697436  0.037005128  0.013723077  0.005148718  0.010625641  0.020041026  0.015569231  0.033928205 
# Assoc-voc    Bachelors    Doctorate      HS-grad      Masters    Preschool  Prof-school Some-college 
# 0.042112821  0.163220513  0.012287179  0.324553846  0.055364103  0.001764103  0.016184615  0.219774359

prop.table(table(adult_os$income, adult_os$education))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th          9th
# >50K  1.641026e-03 1.825641e-03 8.820513e-04 1.435897e-04 4.923077e-04 1.087179e-03 8.205128e-04
# <=50K 2.705641e-02 3.517949e-02 1.284103e-02 5.005128e-03 1.013333e-02 1.895385e-02 1.474872e-02
# 
# Assoc-acdm    Assoc-voc    Bachelors    Doctorate      HS-grad      Masters    Preschool
# >50K  9.435897e-03 1.091282e-02 6.754872e-02 8.943590e-03 5.058462e-02 3.128205e-02 2.051282e-05
# <=50K 2.449231e-02 3.120000e-02 9.567179e-02 3.343590e-03 2.739692e-01 2.408205e-02 1.743590e-03
# 
# Prof-school Some-college
# >50K  1.169231e-02 4.139487e-02
# <=50K 4.492308e-03 1.783795e-01

# prop.table(table(adult_os[which(adult_os$education == "Bachelors"),]$income, adult_os[which(adult_os$education == "Bachelors"),]$sex))
# prop.table(table(adult_os[which(adult_os$education == "Masters"),]$income, adult_os[which(adult_os$education == "Masters"),]$sex))
# prop.table(table(adult_os[which(adult_os$education == "Doctorate"),]$income, adult_os[which(adult_os$education == "Doctorate"),]$sex))

p1 <- ggplot(adult_os, aes(education, fill = income)) + #, weight = fnlwgt
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(education), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V6 marital-status Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse

prop.table(table(adult_os$marital_status))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.1340307692          0.0009230769          0.4638974359          0.0127589744 
# Never-married             Separated               Widowed 
# 0.3292512821          0.0295179487          0.0296205128

prop.table(table(adult_os$income, adult_os$marital_status))
# Divorced Married-AF-spouse Married-civ-spouse Married-spouse-absent Never-married    Separated
# >50K  0.0184410256      0.0004717949       0.1927384615          0.0015179487  0.0189538462 0.0023794872
# <=50K 0.1155897436      0.0004512821       0.2711589744          0.0112410256  0.3102974359 0.0271384615
# 
# Widowed
# >50K  0.0042051282
# <=50K 0.0254153846

prop.table(table(adult_os[which(adult_os$marital_status == "Married-civ-spouse"),]$income))
# >50K     <=50K 
# 0.4154765 0.5845235

prop.table(table(adult_os[which(adult_os$income == ">50K"),]$marital_status))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.077253588           0.001976454           0.807424594           0.006359027 
# Never-married             Separated               Widowed 
# 0.079401908           0.009968205           0.017616224 

p1 <- ggplot(adult_os, aes(marital_status, fill = income)) + #, weight = fnlwgt
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(marital_status), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V7 occupation Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces

prop.table(table(adult_os$occupation))
# ?      Adm-clerical      Armed-Forces      Craft-repair   Exec-managerial 
# 0.0580717949      0.1127794872      0.0003076923      0.1285948718      0.1233230769 
# Farming-fishing Handlers-cleaners Machine-op-inspct     Other-service   Priv-house-serv 
# 0.0320000000      0.0444512821      0.0623179487      0.0983384615      0.0043487179 
# Prof-specialty   Protective-serv             Sales      Tech-support  Transport-moving 
# 0.1286564103      0.0200820513      0.1086358974      0.0290051282      0.0490871795 

prop.table(table(adult_os$income, adult_os$occupation))
# ? Adm-clerical Armed-Forces Craft-repair Exec-managerial Farming-fishing
# >50K  6.420513e-03 2.090256e-02 4.102564e-05 2.379487e-02    5.889231e-02    2.769231e-03
# <=50K 5.165128e-02 9.187692e-02 2.666667e-04 1.048000e-01    6.443077e-02    2.923077e-02
# 
# Handlers-cleaners Machine-op-inspct Other-service Priv-house-serv Prof-specialty Protective-serv
# >50K       2.461538e-03      6.871795e-03  5.497436e-03    1.230769e-04   6.094359e-02    5.600000e-03
# <=50K      4.198974e-02      5.544615e-02  9.284103e-02    4.225641e-03   6.771282e-02    1.448205e-02
# 
# Sales Tech-support Transport-moving
# >50K  2.756923e-02 8.779487e-03     8.041026e-03
# <=50K 8.106667e-02 2.022564e-02     4.104615e-02

# prop.table(table(adult_os[which(adult_os$occupation == "Exec-managerial"),]$income, adult_os[which(adult_os$occupation == "Exec-managerial"),]$sex))
# prop.table(table(adult_os[which(adult_os$occupation == "Prof-specialty"),]$income, adult_os[which(adult_os$occupation == "Prof-specialty"),]$sex))

p1 <- ggplot(adult_os, aes(occupation, fill = income)) + #, weight = fnlwgt
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(occupation), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V8 relationship Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried

prop.table(table(adult_os$relationship))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.38598974     0.25987692     0.03064615     0.15435897     0.09823590     0.07089231 

prop.table(table(adult_os$income, adult_os$relationship))
# Husband Not-in-family Other-relative   Own-child   Unmarried        Wife
# >50K  0.142010256   0.033538462    0.001538462 0.002933333 0.009415385 0.049271795
# <=50K 0.243979487   0.226338462    0.029107692 0.151425641 0.088820513 0.021620513

prop.table(table(adult_os[which(adult_os$relationship == "Husband"),]$income))
# >50K    <=50K 
# 0.367912 0.632088 

prop.table(table(adult_os[which(adult_os$relationship == "Wife"),]$income))
# >50K     <=50K 
# 0.6950231 0.3049769

prop.table(table(adult_os[which(adult_os$income == ">50K"),]$relationship))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.59491278     0.14050013     0.00644496     0.01228839     0.03944316     0.20641059

p1 <- ggplot(adult_os, aes(relationship, fill = income)) + #, weight = fnlwgt
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income")

p2 <- ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(relationship), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V9 race White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black

prop.table(table(adult_os$race))
# Amer-Indian-Eskimo Asian-Pac-Islander              Black              Other              White 
# 0.009723077        0.030974359        0.093764103        0.008328205        0.857210256 

ggplot(adult_os, aes(race, fill = income)) + #, weight = fnlwgt
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(race), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V10 sex Female, Male

prop.table(table(adult_os$sex))
# Female     Male 
# 0.330441 0.669559 

ggplot(adult_os, aes(sex, fill = income)) + #, weight = fnlwgt
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(sex), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V11 capital-gain
hist(adult_os$capital_gain)

p1 <- ggplot(adult_os, aes(x=income, y=capital_gain)) + 
  geom_boxplot()

p2 <- ggplot(adult_os, aes(capital_gain, fill = income)) + #, weight = fnlwgt
  stat_bin(binwidth = 5000, center = 2500)

grid.arrange(p1, p2, ncol=2)

###
# V12 capital-loss
hist(adult_os$capital_loss)

ggplot(adult_os, aes(x=income, y=capital_loss)) + 
  geom_boxplot()

ggplot(adult_os, aes(capital_loss, fill = income)) + #, weight = fnlwgt
  stat_bin(binwidth = 200, center = 100)

###
# V13 hours-per-week
hist(adult_os$hours_per_week)

quantile(adult_os[which(adult_os$income == ">50K"),]$hours_per_week)

p1 <- ggplot(adult_os, aes(x=income, y=hours_per_week)) + 
  geom_boxplot()

p2 <- ggplot(adult_os, aes(hours_per_week, fill = income)) + #, weight = fnlwgt
  stat_bin(binwidth = 5, center = 2.5)

grid.arrange(p1, p2, ncol=2)

###
# V14 native-country United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands

prop.table(table(adult_os$native_country))

ggplot(adult_os, aes(native_country, fill = income)) + #, weight = fnlwgt
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(native_country), fill = income)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V15 income
prop.table(table(adult_os$income))
# >50K     <=50K 
# 0.2387077 0.7612923

###
# stat income/sex
# >50K favorable
# <=50K unfavorable
# male privileged
# female unprivileged

# prop.table(wtd.table(adult_os$income, y = adult_os$sex, weights = adult_os$fnlwgt))
# # Female       Male
# # >50K  0.07719804 0.15896986
# # <=50K 0.24495097 0.51888112

prop.table(table(adult_os$income, adult_os$sex))
# Female       Male
# >50K  0.07932308 0.15938462
# <=50K 0.25111795 0.51017436

# prop.table(wtd.table(adult_os[which(adult_os$sex == "Male"),]$income, weights = adult_os[which(adult_os$sex == "Male"),]$fnlwgt))
# # >50K     <=50K 
# # 0.2345204 0.7654796

prop.table(table(adult_os[which(adult_os$sex == "Male"),]$income))
# >50K     <=50K 
# 0.2380442 0.7619558

# prop.table(wtd.table(adult_os[which(adult_os$sex == "Female"),]$income, weights = adult_os[which(adult_os$sex == "Female"),]$fnlwgt))
# # >50K     <=50K 
# # 0.2396346 0.7603654

prop.table(table(adult_os[which(adult_os$sex == "Female"),]$income))
# >50K     <=50K 
# 0.2400521 0.7599479

# prop.table(wtd.table(adult_os[which(adult_os$income == ">50K"),]$sex, weights = adult_os[which(adult_os$income == ">50K"),]$fnlwgt))
# # Female      Male 
# # 0.3268778 0.6731222

prop.table(table(adult_os[which(adult_os$income == ">50K"),]$sex))
# Female      Male 
# 0.3323021 0.6676979

p1 <- ggplot(adult_os, aes(income, fill = sex)) + #, weight = fnlwgt
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="sex")

p2 <- ggplot(data = adult_os) + 
  geom_mosaic(aes(x=product(income), fill = sex)) + #, weight = fnlwgt
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

grid.arrange(p1, p2, ncol=2)
