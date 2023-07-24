library("mlr3")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3fairness")
library("ggplot2")
library("ggmosaic")
library("gridExtra")
library("weights")#wtd.hist
library("questionr")#wtd.table two-way

setwd("path/to/r-scripts")

set.seed(4)

# load dataset
source("adult.R")

# V1  V2        V3      V4        V5            V6              V7          V8            V9    V10 V11           V12           V13             V14             V15
# age workclass fnlwgt  education education-num marital-status  occupation  relationship  race  sex capital-gain  capital-loss  hours-per-week  native-country  income

# create classification task
adult_task <- TaskClassif$new(id = "adult",
                              backend = adult,
                              target = "income",
                              positive = ">50K")

# set protected attribute and weights
adult_task$set_col_roles("sex", add_to = "pta")
adult_task$set_col_roles("fnlwgt", add_to = "weight")

# remove feature fnlwgt
#adult_task$set_col_roles("sex", remove_from = "feature")
adult_task$set_col_roles("fnlwgt", remove_from = "feature")

# reweighing wts
po_rw_wts <- PipeOpReweighingWeights$new(id = "reweighing_wts", param_vals = list(alpha = 1.0))
task_po_rw_wts <- po_rw_wts$train(list(adult_task))
#task_po_rw_wts[[1]]

# task_po_rw_wts$output$weights
# # with original weights
# #     1:      1  84803.22
# # adult_task$weights
# #     1:      1  77516
# # 84803.22 / 77516#1.094009
# # without original weights
# #    1:      1 1.0940093

# fetch data from task
adult_wts <- task_po_rw_wts$output$data()
# add weight column to data
adult_wts$fnlwgt <- task_po_rw_wts$output$weights$weight

###
# V1 age
#hist(adult_wts$age)#, plot = FALSE
wtd.hist(adult_wts$age, weight=adult_wts$fnlwgt)

p1 <- ggplot(adult_wts, aes(x=income, y=age)) + 
  geom_boxplot()

p2 <- ggplot(adult_wts, aes(age, fill = income, weight = fnlwgt)) + 
  stat_bin(binwidth = 5, center = 2.5)

grid.arrange(p1, p2, ncol=2)

###
# V2 workclass Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked

prop.table(wtd.table(adult_wts$workclass, weights = adult_wts$fnlwgt))
# ?      Federal-gov        Local-gov     Never-worked          Private     Self-emp-inc 
# 0.0565006391     0.0279567117     0.0659268133     0.0002415401     0.7039148895     0.0312133207 
# Self-emp-not-inc        State-gov      Without-pay 
# 0.0747145235     0.0391271514     0.0004044108

ggplot(adult_wts, aes(workclass, fill = income, weight = fnlwgt)) +
  geom_bar()

ggplot(data = adult_wts) +
  geom_mosaic(aes(x=product(workclass), fill = income, weight = fnlwgt)) +
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V4 education Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool

prop.table(wtd.table(adult_wts$education, weights = adult_wts$fnlwgt))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th          9th   Assoc-acdm 
# 0.029521314  0.038187948  0.014123337  0.006469571  0.013045720  0.019646643  0.016517193  0.033851876 
# Assoc-voc    Bachelors    Doctorate      HS-grad      Masters    Preschool  Prof-school Some-college 
# 0.039922454  0.162535774  0.011876846  0.322313426  0.053695442  0.002195831  0.015979574  0.220117052

prop.table(wtd.table(adult_wts$income, y = adult_wts$education, weights = adult_wts$fnlwgt))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th          9th
# >50K  1.581371e-03 1.573070e-03 9.792445e-04 1.276603e-04 4.593011e-04 9.405792e-04 7.355714e-04
# <=50K 2.793994e-02 3.661488e-02 1.314409e-02 6.341911e-03 1.258642e-02 1.870606e-02 1.578162e-02
# Assoc-acdm    Assoc-voc    Bachelors    Doctorate      HS-grad      Masters    Preschool
# >50K  9.366450e-03 1.056573e-02 6.788459e-02 8.711514e-03 4.850611e-02 3.123447e-02 1.655798e-05
# <=50K 2.448543e-02 2.935673e-02 9.465119e-02 3.165332e-03 2.738073e-01 2.246098e-02 2.179273e-03
# Prof-school Some-college
# >50K  1.146418e-02 4.233118e-02
# <=50K 4.515397e-03 1.777859e-01

# prop.table(wtd.table(adult_wts[which(adult_wts$education == "Bachelors"),]$income, adult_wts[which(adult_wts$education == "Bachelors"),]$sex, weights = adult_wts[which(adult_wts$education == "Bachelors"),]$fnlwgt))
# prop.table(wtd.table(adult_wts[which(adult_wts$education == "Masters"),]$income, adult_wts[which(adult_wts$education == "Masters"),]$sex, weights = adult_wts[which(adult_wts$education == "Masters"),]$fnlwgt))
# prop.table(wtd.table(adult_wts[which(adult_wts$education == "Doctorate"),]$income, adult_wts[which(adult_wts$education == "Doctorate"),]$sex, weights = adult_wts[which(adult_wts$education == "Doctorate"),]$fnlwgt))

p1 <- ggplot(adult_wts, aes(education, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(education), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V6 marital-status Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse

prop.table(wtd.table(adult_wts$marital_status, weights = adult_wts$fnlwgt))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.1304014166          0.0009529248          0.4555537913          0.0133859015 
# Never-married             Separated               Widowed 
# 0.3402067761          0.0323701699          0.0271290198

prop.table(wtd.table(adult_wts$income, y = adult_wts$marital_status, weights = adult_wts$fnlwgt))
# Divorced Married-AF-spouse Married-civ-spouse Married-spouse-absent Never-married    Separated
# >50K  0.0180551264      0.0004983324       0.1909219492          0.0012586596  0.0193384061 0.0028317844
# <=50K 0.1123462902      0.0004545923       0.2646318421          0.0121272419  0.3208683701 0.0295383855
# Widowed
# >50K  0.0035733030
# <=50K 0.0235557168

prop.table(table(adult_wts$income, adult_wts$marital_status))
# Divorced Married-AF-spouse Married-civ-spouse Married-spouse-absent Never-married    Separated
# >50K  0.0137381762      0.0002866385       0.2044142337          0.0011875026  0.0150075754 0.0020269440
# <=50K 0.1220670734      0.0004709062       0.2537774866          0.0116702838  0.3149748168 0.0292985545
# 
# Widowed
# >50K  0.0026206953
# <=50K 0.0284591131

prop.table(wtd.table(adult_wts[which(adult_wts$marital_status == "Married-civ-spouse"),]$income, weights = adult_wts[which(adult_wts$marital_status == "Married-civ-spouse"),]$fnlwgt))
# >50K     <=50K 
# 0.4190986 0.5809014

prop.table(wtd.table(adult_wts[which(adult_wts$income == ">50K"),]$marital_status, weights = adult_wts[which(adult_wts$income == ">50K"),]$fnlwgt))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.076350273           0.002107314           0.807357570           0.005322533 
# Never-married             Separated               Widowed 
# 0.081776918           0.011974855           0.015110537

p1 <- ggplot(adult_wts, aes(marital_status, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(marital_status), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V7 occupation Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces

prop.table(wtd.table(adult_wts$occupation, weights = adult_wts$fnlwgt))
# ?      Adm-clerical      Armed-Forces      Craft-repair   Exec-managerial   Farming-fishing 
# 0.0567421792      0.1140320896      0.0003428778      0.1294074531      0.1209403513      0.0292043736 
# Handlers-cleaners Machine-op-inspct     Other-service   Priv-house-serv    Prof-specialty   Protective-serv 
# 0.0474457753      0.0636192359      0.0979008820      0.0044530487      0.1265351763      0.0210961627 
# Sales      Tech-support  Transport-moving 
# 0.1089114129      0.0294342322      0.0499347496

prop.table(wtd.table(adult_wts$income, y = adult_wts$occupation, weights = adult_wts$fnlwgt))
# ? Adm-clerical Armed-Forces Craft-repair Exec-managerial Farming-fishing Handlers-cleaners
# >50K  6.231729e-03 2.099682e-02 1.054961e-04 2.347645e-02    5.770347e-02    2.144587e-03      2.522882e-03
# <=50K 5.051045e-02 9.303527e-02 2.373817e-04 1.059310e-01    6.323689e-02    2.705979e-02      4.492289e-02
# Machine-op-inspct Other-service Priv-house-serv Prof-specialty Protective-serv        Sales
# >50K       6.680690e-03  5.384044e-03    8.434796e-05   6.090689e-02    5.671397e-03 2.736515e-02
# <=50K      5.693855e-02  9.251684e-02    4.368701e-03   6.562828e-02    1.542477e-02 8.154626e-02
# Tech-support Transport-moving
# >50K  9.001006e-03     8.202614e-03
# <=50K 2.043323e-02     4.173214e-02

# prop.table(wtd.table(adult_wts[which(adult_wts$occupation == "Exec-managerial"),]$income, adult_wts[which(adult_wts$occupation == "Exec-managerial"),]$sex, weights = adult_wts[which(adult_wts$occupation == "Exec-managerial"),]$fnlwgt))
# prop.table(wtd.table(adult_wts[which(adult_wts$occupation == "Prof-specialty"),]$income, adult_wts[which(adult_wts$occupation == "Prof-specialty"),]$sex, weights = adult_wts[which(adult_wts$occupation == "Prof-specialty"),]$fnlwgt))

p1 <- ggplot(adult_wts, aes(occupation, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(occupation), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V8 relationship Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried

prop.table(wtd.table(adult_wts$relationship, weights = adult_wts$fnlwgt))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.38015394     0.26129170     0.03321287     0.15771034     0.09985936     0.06777179

prop.table(wtd.table(adult_wts$income, y = adult_wts$relationship, weights = adult_wts$fnlwgt))
# Husband Not-in-family Other-relative   Own-child   Unmarried        Wife
# >50K  0.142017943   0.033058069    0.001438081 0.002925849 0.009567346 0.047470274
# <=50K 0.238136002   0.228233634    0.031774785 0.154784488 0.090292012 0.020301518

prop.table(wtd.table(adult_wts[which(adult_wts$relationship == "Husband"),]$income, weights = adult_wts[which(adult_wts$relationship == "Husband"),]$fnlwgt))
# >50K     <=50K 
# 0.3735801 0.6264199

prop.table(wtd.table(adult_wts[which(adult_wts$relationship == "Wife"),]$income, weights = adult_wts[which(adult_wts$relationship == "Wife"),]$fnlwgt))
# >50K    <=50K 
# 0.700443 0.299557

prop.table(wtd.table(adult_wts[which(adult_wts$income == ">50K"),]$relationship, weights = adult_wts[which(adult_wts$income == ">50K"),]$fnlwgt))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.600555681    0.139793680    0.006081259    0.012372628    0.040457733    0.200739019

p1 <- ggplot(adult_wts, aes(relationship, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income")

p2 <- ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(relationship), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V9 race White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black

prop.table(wtd.table(adult_wts$race, weights = adult_wts$fnlwgt))
# Amer-Indian-Eskimo Asian-Pac-Islander              Black              Other              White 
# 0.006237713        0.026251567        0.115087484        0.008677373        0.843745863

ggplot(adult_wts, aes(race, fill = income, weight = fnlwgt)) + 
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(race), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V10 sex Female, Male

prop.table(wtd.table(adult_wts$sex, weights = adult_wts$fnlwgt))
# Female      Male 
# 0.3237261 0.6762739

ggplot(adult_wts, aes(sex, fill = income, weight = fnlwgt)) + 
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(sex), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V11 capital-gain
wtd.hist(adult_wts$capital_gain, weight=adult_wts$fnlwgt)

p1 <- ggplot(adult_wts, aes(x=income, y=capital_gain)) + 
  geom_boxplot()

p2 <- ggplot(adult_wts, aes(capital_gain, fill = income, weight = fnlwgt)) + 
  stat_bin(binwidth = 5000, center = 2500)

grid.arrange(p1, p2, ncol=2)

###
# V12 capital-loss
wtd.hist(adult_wts$capital_loss, weight=adult_wts$fnlwgt)

ggplot(adult_wts, aes(x=income, y=capital_loss)) + 
  geom_boxplot()

ggplot(adult_wts, aes(capital_loss, fill = income, weight = fnlwgt)) + 
  stat_bin(binwidth = 200, center = 100)

###
# V13 hours-per-week
wtd.hist(adult_wts$hours_per_week, weight=adult_wts$fnlwgt)

DescTools::Quantile(adult_wts[which(adult_wts$income == ">50K"),]$hours_per_week, weights=adult_wts[which(adult_wts$income == ">50K"),]$fnlwgt / max(adult_wts[which(adult_wts$income == ">50K"),]$fnlwgt))

p1 <- ggplot(adult_wts, aes(x=income, y=hours_per_week)) + 
  geom_boxplot()

p2 <- ggplot(adult_wts, aes(hours_per_week, fill = income, weight = fnlwgt)) + 
  stat_bin(binwidth = 5, center = 2.5)

grid.arrange(p1, p2, ncol=2)

###
# V14 native-country United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands

prop.table(wtd.table(adult_wts$native_country, weights = adult_wts$fnlwgt))

ggplot(adult_wts, aes(native_country, fill = income, weight = fnlwgt)) + 
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(native_country), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V15 income
prop.table(wtd.table(adult_wts$income, weights = adult_wts$fnlwgt))
# >50K     <=50K 
# 0.2364776 0.7635224

###
# stat income/sex
# >50K favorable
# <=50K unfavorable
# male privileged
# female unprivileged

prop.table(wtd.table(adult_wts$income, y = adult_wts$sex, weights = adult_wts$fnlwgt))
# Female       Male
# >50K  0.07692085 0.15955671
# <=50K 0.24680525 0.51671719

prop.table(wtd.table(adult_wts[which(adult_wts$sex == "Male"),]$income, weights = adult_wts[which(adult_wts$sex == "Male"),]$fnlwgt))
# >50K    <=50K 
# 0.235935 0.764065

prop.table(wtd.table(adult_wts[which(adult_wts$sex == "Female"),]$income, weights = adult_wts[which(adult_wts$sex == "Female"),]$fnlwgt))
# >50K     <=50K 
# 0.2376109 0.7623891

prop.table(wtd.table(adult_wts[which(adult_wts$income == ">50K"),]$sex, weights = adult_wts[which(adult_wts$income == ">50K"),]$fnlwgt))
# Female      Male 
# 0.3252776 0.6747224

p1 <- ggplot(adult_wts, aes(income, fill = sex, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="sex")

p2 <- ggplot(data = adult_wts) + 
  geom_mosaic(aes(x=product(income), fill = sex, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

grid.arrange(p1, p2, ncol=2)
