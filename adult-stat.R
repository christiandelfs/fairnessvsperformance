library("ggplot2")
library("ggmosaic")
library("gridExtra")
library("tableone")
library("weights")#wtd.hist
library("questionr")#wtd.table two-way
#library("DescTools")#weighted summary/quantile

#setwd("C:\\Users\\User\\Documents\\HOST\\7-Semester\\Datasets\\Adult\\adult")
setwd("C:\\Users\\User\\Documents\\HOST\\7-Semester\\Thesis\\r-scripts")

# V1  V2        V3      V4        V5            V6              V7          V8            V9    V10 V11           V12           V13             V14             V15
# age workclass fnlwgt  education education-num marital-status  occupation  relationship  race  sex capital-gain  capital-loss  hours-per-week  native-country  
#adult <- read.table("adult.data", sep =",", strip.white = TRUE, header = FALSE, stringsAsFactors = TRUE)
load(file = "adult.Robj")

adult <- rbind(adult_train, adult_test)

#str(adult)

# table1 <- CreateTableOne(vars = colnames(adult)[1:14],
#                          data = adult,
#                          strata = "income")
# 
# table1

###
# V1 age
#hist(adult$age)#, plot = FALSE
wtd.hist(adult$age, weight=adult$fnlwgt)
# boxplot(adult$age)
# boxplot(adult$age~adult$income)
# quantile(adult$age)
#Quantile(adult$age, weights=adult$fnlwgt)
#Quantile(adult$age, weights=adult$fnlwgtnrm)

# summary(adult$age)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 17.00   28.00   37.00   38.64   48.00   90.00 

# summary(adult[which(adult$income == ">50K"),]$age)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 19.00   36.00   43.00   44.28   51.00   90.00
# 
# weighted.mean(adult[which(adult$income == ">50K"),]$age, adult[which(adult$income == ">50K"),]$fnlwgt)
# 43.91461
# 
# DescTools::Quantile(adult[which(adult$income == ">50K"),]$age, weights=adult[which(adult$income == ">50K"),]$fnlwgt / max(adult[which(adult$income == ">50K"),]$fnlwgt))
# #0%  25%  50%  75% 100% 
# #21   36   43   51   90
# 
# quantile(adult[which(adult$income == ">50K"),]$age)
# # 0%  25%  50%  75% 100% 
# # 19   36   43   51   90
# 
# summary(adult[which(adult$income == "<=50K"),]$age)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 17.00   25.00   34.00   36.87   46.00   90.00
# 
# weighted.mean(adult[which(adult$income == "<=50K"),]$age, adult[which(adult$income == "<=50K"),]$fnlwgt)
# 36.23185
# 
# DescTools::Quantile(adult[which(adult$income == "<=50K"),]$age, weights=adult[which(adult$income == "<=50K"),]$fnlwgt / max(adult[which(adult$income == "<=50K"),]$fnlwgt))
# # 0%  25%  50%  75% 100% 
# # 17   25   33   45   90
# 
# quantile(adult[which(adult$income == "<=50K"),]$age)
# # 0%  25%  50%  75% 100% 
# # 17   25   34   46   90

p1 <- ggplot(adult, aes(x=income, y=age)) + 
geom_boxplot()

p2 <- ggplot(adult, aes(age, fill = income, weight = fnlwgt)) + 
stat_bin(binwidth = 5, center = 2.5)

grid.arrange(p1, p2, ncol=2)

###
# V2 workclass Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked
# table(adult$workclass)
# ? 1836

prop.table(wtd.table(adult$workclass, weights = adult$fnlwgt))
# ?      Federal-gov        Local-gov     Never-worked          Private 
# 0.0565792444     0.0283800695     0.0643752845     0.0002321278     0.7051966279 
# Self-emp-inc Self-emp-not-inc        State-gov      Without-pay 
# 0.0327506787     0.0731991775     0.0389061640     0.0003806258

prop.table(table(adult$workclass))
# ?      Federal-gov        Local-gov     Never-worked          Private 
# 0.0573072356     0.0293190287     0.0642070349     0.0002047418     0.6941976168 
# Self-emp-inc Self-emp-not-inc        State-gov      Without-pay 
# 0.0347037386     0.0790712911     0.0405593547     0.0004299578
barplot(table(adult$workclass))

ggplot(adult, aes(workclass, fill = income, weight = fnlwgt)) +
  geom_bar()

ggplot(data = adult) +
  geom_mosaic(aes(x=product(workclass), fill = income, weight = fnlwgt)) +
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

# ###
# # V3 fnlwgt

###
# V4 education Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool
# table(adult$education)

prop.table(wtd.table(adult$education, weights = adult$fnlwgt))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th          9th 
# 0.029468505  0.038162944  0.014009988  0.006266486  0.012629910  0.019338862  0.016240940 
# Assoc-acdm    Assoc-voc    Bachelors    Doctorate      HS-grad      Masters    Preschool 
# 0.033476687  0.039918375  0.163176884  0.011804270  0.321396344  0.052052531  0.002140880 
# Prof-school Some-college 
# 0.016798332  0.223118061

prop.table(table(adult$education))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th          9th 
# 0.028438639  0.037099218  0.013451538  0.005057123  0.010421359  0.019552844  0.015478482 
# Assoc-acdm    Assoc-voc    Bachelors    Doctorate      HS-grad      Masters    Preschool 
# 0.032779165  0.042197289  0.164305311  0.012161664  0.323164490  0.054399902  0.001699357 
# Prof-school Some-college 
# 0.017075468  0.222718152
barplot(table(adult$education))

prop.table(wtd.table(adult$income, y = adult$education, weights = adult$fnlwgt))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th
# <=50K 2.761209e-02 3.643081e-02 1.292200e-02 6.104355e-03 1.213945e-02 1.816358e-02
# >50K  1.856413e-03 1.732134e-03 1.087985e-03 1.621316e-04 4.904577e-04 1.175281e-03
# 9th   Assoc-acdm    Assoc-voc    Bachelors    Doctorate      HS-grad
# <=50K 1.548668e-02 2.471324e-02 2.965909e-02 9.507102e-02 3.119129e-03 2.716117e-01
# >50K  7.542565e-04 8.763444e-03 1.025929e-02 6.810586e-02 8.685141e-03 4.978461e-02
# Masters    Preschool  Prof-school Some-college
# <=50K 2.277970e-02 2.119851e-03 4.406655e-03 1.798846e-01
# >50K  2.927283e-02 2.102903e-05 1.239168e-02 4.323342e-02

prop.table(table(adult$income, adult$education))
# 10th         11th         12th      1st-4th      5th-6th      7th-8th
# <=50K 2.665739e-02 3.521559e-02 1.246878e-02 4.893330e-03 9.868556e-03 1.828344e-02
# >50K  1.781254e-03 1.883625e-03 9.827607e-04 1.637935e-04 5.528029e-04 1.269399e-03
# 
# 9th   Assoc-acdm    Assoc-voc    Bachelors    Doctorate      HS-grad
# <=50K 1.463904e-02 2.432333e-02 3.150977e-02 9.647435e-02 3.337292e-03 2.719176e-01
# >50K  8.394415e-04 8.455837e-03 1.068752e-02 6.783097e-02 8.824372e-03 5.124688e-02
# 
# Masters    Preschool  Prof-school Some-college
# <=50K 2.452807e-02 1.678883e-03 4.442898e-03 1.804799e-01
# >50K  2.987183e-02 2.047418e-05 1.263257e-02 4.223824e-02

# prop.table(wtd.table(adult[which(adult$education == "Bachelors"),]$income, adult[which(adult$education == "Bachelors"),]$sex, weights = adult[which(adult$education == "Bachelors"),]$fnlwgt))
# prop.table(wtd.table(adult[which(adult$education == "Masters"),]$income, adult[which(adult$education == "Masters"),]$sex, weights = adult[which(adult$education == "Masters"),]$fnlwgt))
# prop.table(wtd.table(adult[which(adult$education == "Doctorate"),]$income, adult[which(adult$education == "Doctorate"),]$sex, weights = adult[which(adult$education == "Doctorate"),]$fnlwgt))

p1 <- ggplot(adult, aes(education, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult) + 
  geom_mosaic(aes(x=product(education), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

# ###
# # V5 education-num

###
# V6 marital-status Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse
table(adult$marital_status)

prop.table(wtd.table(adult$marital_status, weights = adult$fnlwgt))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.1322863616          0.0007354513          0.4512497764          0.0133905683 
# Never-married             Separated               Widowed 
# 0.3400503559          0.0335238143          0.0287636721

prop.table(table(adult$marital_status))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.1358052496          0.0007575447          0.4581917202          0.0128577863 
# Never-married             Separated               Widowed 
# 0.3299823922          0.0313254985          0.0310798084
barplot(table(adult$marital_status))

prop.table(wtd.table(adult$income, y = adult$marital_status, weights = adult$fnlwgt))
# Divorced Married-AF-spouse Married-civ-spouse Married-spouse-absent Never-married
# <=50K 0.1188738427      0.0004803873       0.2480863243          0.0123539531  0.3247771371
# >50K  0.0134125189      0.0002550640       0.2031634521          0.0010366153  0.0152732188
# Separated      Widowed
# <=50K 0.0312254376 0.0264269569
# >50K  0.0022983767 0.0023367152

prop.table(table(adult$income, adult$marital_status))
# Divorced Married-AF-spouse Married-civ-spouse Married-spouse-absent Never-married
# <=50K 0.1220670734      0.0004709062       0.2537774866          0.0116702838  0.3149748168
# >50K  0.0137381762      0.0002866385       0.2044142337          0.0011875026  0.0150075754
# 
# Separated      Widowed
# <=50K 0.0292985545 0.0284591131
# >50K  0.0020269440 0.0026206953

prop.table(wtd.table(adult[which(adult$marital_status == "Married-civ-spouse"),]$income, weights = adult[which(adult$marital_status == "Married-civ-spouse"),]$fnlwgt))
# <=50K      >50K 
# 0.5497761 0.4502239

prop.table(table(adult[which(adult$marital_status == "Married-civ-spouse"),]$income))
# <=50K      >50K 
# 0.5538675 0.4461325

prop.table(wtd.table(adult[which(adult$income == ">50K"),]$marital_status, weights = adult[which(adult$income == ">50K"),]$fnlwgt))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.056408221           0.001072707           0.854432262           0.004359630 
# Never-married             Separated               Widowed 
# 0.064233654           0.009666144           0.009827382

prop.table(table(adult[which(adult$income == ">50K"),]$marital_status))
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent 
# 0.057414221           0.001197912           0.854282536           0.004962779 
# Never-married             Separated               Widowed 
# 0.062719261           0.008470951           0.010952340

p1 <- ggplot(adult, aes(marital_status, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult) + 
  geom_mosaic(aes(x=product(marital_status), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V7 occupation Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces
table(adult$occupation)
# ? 1843

prop.table(wtd.table(adult$occupation, weights = adult$fnlwgt))
# ?      Adm-clerical      Armed-Forces      Craft-repair   Exec-managerial 
# 0.0568113721      0.1162086184      0.0003513318      0.1268502528      0.1222807590 
# Farming-fishing Handlers-cleaners Machine-op-inspct     Other-service   Priv-house-serv 
# 0.0277714807      0.0451934420      0.0630301114      0.0998719273      0.0050806375 
# Prof-specialty   Protective-serv             Sales      Tech-support  Transport-moving 
# 0.1235629379      0.0213852900      0.1131765237      0.0297290413      0.0486962741

prop.table(table(adult$occupation))
# ?      Adm-clerical      Armed-Forces      Craft-repair   Exec-managerial 
# 0.0575119774      0.1148806355      0.0003071127      0.1251382007      0.1246058720 
# Farming-fishing Handlers-cleaners Machine-op-inspct     Other-service   Priv-house-serv 
# 0.0305065313      0.0424225052      0.0618729782      0.1007943983      0.0049547521 
# Prof-specialty   Protective-serv             Sales      Tech-support  Transport-moving 
# 0.1263666517      0.0201261210      0.1126898980      0.0296056673      0.0482166987
barplot(table(adult$occupation))

prop.table(wtd.table(adult$income, y = adult$occupation, weights = adult$fnlwgt))
# ? Adm-clerical Armed-Forces Craft-repair Exec-managerial Farming-fishing
# <=50K 5.151181e-02 1.005963e-01 2.173493e-04 9.827839e-02    6.367297e-02    2.517468e-02
# >50K  5.299563e-03 1.561229e-02 1.339825e-04 2.857187e-02    5.860779e-02    2.596802e-03
# Handlers-cleaners Machine-op-inspct Other-service Priv-house-serv Prof-specialty
# <=50K      4.228885e-02      5.554975e-02  9.579327e-02    5.042110e-03   6.700209e-02
# >50K       2.904596e-03      7.480360e-03  4.078653e-03    3.852767e-05   5.656085e-02
# Protective-serv        Sales Tech-support Transport-moving
# <=50K    1.465987e-02 8.277183e-02 2.092596e-02     3.873880e-02
# >50K     6.725416e-03 3.040469e-02 8.803085e-03     9.957478e-03

prop.table(table(adult$income, adult$occupation))
# ? Adm-clerical Armed-Forces Craft-repair Exec-managerial Farming-fishing
# <=50K 5.208632e-02 9.915646e-02 2.047418e-04 9.682241e-02    6.506695e-02    2.696450e-02
# >50K  5.425658e-03 1.572417e-02 1.023709e-04 2.831579e-02    5.953892e-02    3.542033e-03
# 
# Handlers-cleaners Machine-op-inspct Other-service Priv-house-serv Prof-specialty
# <=50K      3.959707e-02      5.425658e-02  9.661767e-02    4.893330e-03   6.936653e-02
# >50K       2.825437e-03      7.616396e-03  4.176733e-03    6.142255e-05   5.700012e-02
# 
# Protective-serv        Sales Tech-support Transport-moving
# <=50K    1.382007e-02 8.249048e-02 2.100651e-02     3.836862e-02
# >50K     6.306048e-03 3.019942e-02 8.599156e-03     9.848082e-03

# prop.table(table(adult[which(adult$occupation == "Exec-managerial"),]$income, adult[which(adult$occupation == "Exec-managerial"),]$sex))
# prop.table(table(adult[which(adult$occupation == "Prof-specialty"),]$income, adult[which(adult$occupation == "Prof-specialty"),]$sex))

p1 <- ggplot(adult, aes(occupation, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income") + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

p2 <- ggplot(data = adult) + 
  geom_mosaic(aes(x=product(occupation), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V8 relationship Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried
table(adult$relationship)

prop.table(wtd.table(adult$relationship, weights = adult$fnlwgt))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.39841026     0.25856131     0.03308745     0.15858494     0.10587410     0.04548194

prop.table(table(adult$relationship))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.40366897     0.25762663     0.03083412     0.15521477     0.10493018     0.04772532
barplot(table(adult$relationship))

prop.table(wtd.table(adult$income, y = adult$relationship, weights = adult$fnlwgt))
# Husband Not-in-family Other-relative    Own-child    Unmarried         Wife
# <=50K 0.2180441313  0.2323725113   0.0321080417 0.1563496442 0.0995750009 0.0237747096
# >50K  0.1803661276  0.0261888031   0.0009794033 0.0022352983 0.0062990963 0.0217072323

prop.table(table(adult$income, adult$relationship))
# Husband Not-in-family Other-relative   Own-child   Unmarried        Wife
# <=50K 0.222554359   0.231501577    0.029769461 0.152942140 0.098603661 0.025347037
# >50K  0.181114614   0.026125056    0.001064657 0.002272634 0.006326522 0.022378281

prop.table(wtd.table(adult[which(adult$relationship == "Husband"),]$income, weights = adult[which(adult$relationship == "Husband"),]$fnlwgt))
# <=50K      >50K 
# 0.5472854 0.4527146

prop.table(table(adult[which(adult$relationship == "Husband"),]$income))
# <=50K      >50K 
# 0.5513289 0.4486711

prop.table(wtd.table(adult[which(adult$relationship == "Wife"),]$income, weights = adult[which(adult$relationship == "Wife"),]$fnlwgt))
# <=50K      >50K 
# 0.5227286 0.4772714

prop.table(table(adult[which(adult$relationship == "Wife"),]$income))
# <=50K      >50K 
# 0.5311025 0.4688975

prop.table(wtd.table(adult[which(adult$income == ">50K"),]$relationship, weights = adult[which(adult$income == ">50K"),]$fnlwgt))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.758554931    0.110140668    0.004119018    0.009400859    0.026491729    0.091292796

prop.table(table(adult[which(adult$income == ">50K"),]$relationship))
# Husband  Not-in-family Other-relative      Own-child      Unmarried           Wife 
# 0.756909386    0.109181141    0.004449388    0.009497733    0.026439634    0.093522718

p1 <- ggplot(adult, aes(relationship, fill = income, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="income")

p2 <- ggplot(data = adult) + 
  geom_mosaic(aes(x=product(relationship), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3")) + 
  theme(axis.text.x = element_text(size = 8, angle = 45))

grid.arrange(p1, p2, ncol=2)

###
# V9 race White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black
table(adult$race)

prop.table(wtd.table(adult$race, weights = adult$fnlwgt))
# Amer-Indian-Eskimo Asian-Pac-Islander              Black              Other              White 
# 0.006093993        0.026202794        0.116566496        0.008577743        0.842558975

prop.table(table(adult$race))
# Amer-Indian-Eskimo Asian-Pac-Islander              Black              Other              White 
# 0.009622866        0.031100283        0.095921543        0.008312518        0.855042791
barplot(table(adult$race))

ggplot(adult, aes(race, fill = income, weight = fnlwgt)) + 
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult) + 
  geom_mosaic(aes(x=product(race), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V10 sex Female, Male
table(adult$sex)

prop.table(wtd.table(adult$sex, weights = adult$fnlwgt))
# Female      Male 
# 0.3242472 0.6757528

prop.table(table(adult$sex))
# Female     Male 
# 0.331518 0.668482

barplot(table(adult$sex))

ggplot(adult, aes(sex, fill = income, weight = fnlwgt)) + 
geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult) + 
  geom_mosaic(aes(x=product(sex), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V11 capital-gain
hist(adult$capital_gain)#, plot = FALSE
wtd.hist(adult$capital_gain, weight=adult$fnlwgt)
boxplot(adult$capital_gain)
boxplot(adult$capital_gain~adult$income)
quantile(adult$capital_gain)
summary(adult$capital_gain)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0       0       0    1079       0   99999

p1 <- ggplot(adult, aes(x=income, y=capital_gain)) + 
  geom_boxplot()

p2 <- ggplot(adult, aes(capital_gain, fill = income, weight = fnlwgt)) + 
  stat_bin(binwidth = 5000, center = 2500)

grid.arrange(p1, p2, ncol=2)

###
# V12 capital-loss
hist(adult$capital_loss)#, plot = FALSE
wtd.hist(adult$capital_loss, weight=adult$fnlwgt)
boxplot(adult$capital_loss)
boxplot(adult$capital_loss~adult$income)
quantile(adult$capital_loss)
summary(adult$capital_loss)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0    87.5     0.0  4356.0

ggplot(adult, aes(x=income, y=capital_loss)) + 
  geom_boxplot()

ggplot(adult, aes(capital_loss, fill = income, weight = fnlwgt)) + 
  stat_bin(binwidth = 200, center = 100)

###
# V13 hours-per-week
hist(adult$hours_per_week)#, plot = FALSE
wtd.hist(adult$hours_per_week, weight=adult$fnlwgt)
boxplot(adult$hours_per_week)
boxplot(adult$hours_per_week~adult$income)
quantile(adult$hours_per_week)
summary(adult$hours_per_week)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   40.00   40.00   40.42   45.00   99.00

# summary(adult[which(adult$income == ">50K"),]$hours_per_week)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 1.00   40.00   40.00   45.45   50.00   99.00
# 
# weighted.mean(adult[which(adult$income == ">50K"),]$hours_per_week, adult[which(adult$income == ">50K"),]$fnlwgt)
# # 45.44772
# 
# DescTools::Quantile(adult[which(adult$income == ">50K"),]$hours_per_week, weights=adult[which(adult$income == ">50K"),]$fnlwgt / max(adult[which(adult$income == ">50K"),]$fnlwgt))
# # 0%  25%  50%  75% 100% 
# # 2   40   40   50   99
# 
# quantile(adult[which(adult$income == ">50K"),]$hours_per_week)
# # 0%  25%  50%  75% 100% 
# # 1   40   40   50   99
# 
# DescTools::Quantile(adult[which(adult$income == "<=50K"),]$hours_per_week, weights=adult[which(adult$income == "<=50K"),]$fnlwgt / max(adult[which(adult$income == "<=50K"),]$fnlwgt))
# # 0%  25%  50%  75% 100% 
# # 1   35   40   40   99
# 
# weighted.mean(adult[which(adult$income == "<=50K"),]$hours_per_week, adult[which(adult$income == "<=50K"),]$fnlwgt)
# # 38.73236
# 
# summary(adult[which(adult$income == "<=50K"),]$hours_per_week)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 1.00   35.00   40.00   38.84   40.00   99.00
# 
# quantile(adult[which(adult$income == "<=50K"),]$hours_per_week)

p1 <- ggplot(adult, aes(x=income, y=hours_per_week)) + 
  geom_boxplot()

p2 <- ggplot(adult, aes(hours_per_week, fill = income, weight = fnlwgt)) + 
  stat_bin(binwidth = 5, center = 2.5)

grid.arrange(p1, p2, ncol=2)

###
# V14 native-country United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands
table(adult$native_country)
# ? 583

prop.table(wtd.table(adult$native_country, weights = adult$fnlwgt))
prop.table(table(adult$native_country))
barplot(table(adult$native_country))

ggplot(adult, aes(native_country, fill = income, weight = fnlwgt)) + 
  geom_bar()# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

ggplot(data = adult) + 
  geom_mosaic(aes(x=product(native_country), fill = income, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# V15 income
table(adult$income)
# <=50K  >50K 
# 34014 11208
prop.table(wtd.table(adult$income, weights = adult$fnlwgt))
# <=50K     >50K 
# 0.762224 0.237776

prop.table(table(adult$income))
# <=50K     >50K 
# 0.752156 0.247844
barplot(table(adult$income))

# prop.table(table(adult_train$income))
# # <=50K      >50K 
# # 0.7591904 0.2408096
# 
# prop.table(table(adult_test$income))
# # <=50K      >50K 
# # 0.7637737 0.2362263

###
# stat income/sex
# >50K favorable
# <=50K unfavorable
# male privileged
# female unprivileged

prop.table(wtd.table(adult$income, y = adult$sex, weights = adult$fnlwgt))
# Female       Male
# <=50K 0.28911201 0.47311203
# >50K  0.03513519 0.20264077

prop.table(table(adult$income, adult$sex))
# Female       Male
# <=50K 0.29529913 0.46541911
# >50K  0.03621883 0.20306294

# prop.table(table(adult[which(adult$sex == "Male"),]$income))
# # <=50K      >50K 
# # 0.6875225 0.3124775
# 
# prop.table(table(adult[which(adult$sex == "Female"),]$income))
# # <=50K     >50K 
# # 0.886424 0.113576

prop.table(wtd.table(adult[which(adult$income == ">50K"),]$sex, weights = adult[which(adult$income == ">50K"),]$fnlwgt))
# Female      Male 
# 0.1477659 0.8522341

prop.table(table(adult[which(adult$income == ">50K"),]$sex))
# Female      Male 
# 0.1513648 0.8486352

p1 <- ggplot(adult, aes(income, fill = sex, weight = fnlwgt)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="sex")

p2 <- ggplot(data = adult) + 
  geom_mosaic(aes(x=product(income), fill = sex, weight = fnlwgt)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

grid.arrange(p1, p2, ncol=2)

###
# nrow(adult[which(adult$workclass == "?" | adult$occupation == "?" | adult$native_country == "?"),])#3620, train=2399, test=1221
# 
# 16281+32561#48842
# 48842-2399-1221#45222
# 48842-45222#3620

# nrow(adult[which(adult$workclass == " ?" & adult$occupation == " ?" & adult$native_country == " ?"),])#27
# adult <- adult[which(adult$workclass != "?" & adult$occupation != "?" & adult$native_country != "?"),]

