library("ggplot2")
library("ggmosaic")

setwd("path/to/r-scripts")

#doc <- read.table("default-of-credit-card-clients.csv", sep =",", header = TRUE, stringsAsFactors = TRUE)
load(file = "doc.Robj")

#ID,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,PAY_0,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,BILL_AMT6,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6,default payment next month

###
# LIMIT_BAL
hist(doc$LIMIT_BAL)#, plot = FALSE, xlim = c(0,1000000), ylim = c(0,8000), breaks = c(10000, 50000, 100000, 500000, 1000000)
boxplot(doc$LIMIT_BAL)
boxplot(doc$LIMIT_BAL~doc$default.payment.next.month)
quantile(doc$LIMIT_BAL)
summary(doc$LIMIT_BAL)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10000   50000  140000  167484  240000 1000000
summary(doc[which(doc$default.payment.next.month == "yes"),]$LIMIT_BAL)
quantile(doc[which(doc$default.payment.next.month == "yes"),]$LIMIT_BAL)

ggplot(doc, aes(x=default.payment.next.month, y=LIMIT_BAL)) + 
  geom_boxplot()

ggplot(doc, aes(LIMIT_BAL, fill = default.payment.next.month)) + 
stat_bin(binwidth = 50000, center = 25000)# + 
#geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "black")

###
# SEX 1=male, 2=female
table(doc$SEX)
prop.table(table(doc$SEX))
# female      male 
# 0.6037333 0.3962667
barplot(table(doc$SEX))

ggplot(doc, aes(SEX, fill = default.payment.next.month)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="default.payment.next.month")

ggplot(data = doc) + 
  geom_mosaic(aes(x=product(SEX), fill = default.payment.next.month)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

#doc$SEX <- as.factor(ifelse(doc$SEX == 1, "male", "female"))

###
# EDUCATION 1 = graduate school; 2 = university; 3 = high school; 4 = others
# range 0-6
table(doc$EDUCATION)
prop.table(table(doc$EDUCATION))
# 0            1            2            3            4            5            6 
# 0.0004666667 0.3528333333 0.4676666667 0.1639000000 0.0041000000 0.0093333333 0.0017000000
barplot(table(doc$EDUCATION))

ggplot(doc, aes(EDUCATION, fill = default.payment.next.month)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="default.payment.next.month")

ggplot(data = doc) + 
  geom_mosaic(aes(x=product(EDUCATION), fill = default.payment.next.month)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

nrow(doc[which(doc$EDUCATION %in% c(0, 5, 6)),])#345

hist(doc[which(doc$EDUCATION == 0),]$LIMIT_BAL)
boxplot(doc[which(doc$EDUCATION == 0),]$LIMIT_BAL)
summary(doc[which(doc$EDUCATION == 0),]$LIMIT_BAL)

table(doc[which(doc$EDUCATION == 0),]$SEX)
prop.table(table(doc[which(doc$EDUCATION == 0),]$SEX))
barplot(table(doc[which(doc$EDUCATION == 0),]$SEX))

table(doc[which(doc$EDUCATION == 0),]$MARRIAGE)
prop.table(table(doc[which(doc$EDUCATION == 0),]$MARRIAGE))
barplot(table(doc[which(doc$EDUCATION == 0),]$MARRIAGE))

hist(doc[which(doc$EDUCATION == 0),]$AGE)
boxplot(doc[which(doc$EDUCATION == 0),]$AGE)
summary(doc[which(doc$EDUCATION == 0),]$AGE)

###
# MARRIAGE 1 = married; 2 = single; 3 = others
# range 0-3
table(doc$MARRIAGE)
prop.table(table(doc$MARRIAGE))
# 0          1          2          3 
# 0.00180000 0.45530000 0.53213333 0.01076667
barplot(table(doc$MARRIAGE))

ggplot(doc, aes(MARRIAGE, fill = default.payment.next.month)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="default.payment.next.month")

ggplot(data = doc) + 
  geom_mosaic(aes(x=product(MARRIAGE), fill = default.payment.next.month)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

hist(doc[which(doc$MARRIAGE == 0),]$LIMIT_BAL)
boxplot(doc[which(doc$MARRIAGE == 0),]$LIMIT_BAL)
quantile(doc[which(doc$MARRIAGE == 0),]$LIMIT_BAL)
summary(doc[which(doc$MARRIAGE == 0),]$LIMIT_BAL)

table(doc[which(doc$MARRIAGE == 0),]$SEX)
prop.table(table(doc[which(doc$MARRIAGE == 0),]$SEX))
barplot(table(doc[which(doc$MARRIAGE == 0),]$SEX))

table(doc[which(doc$MARRIAGE == 0),]$EDUCATION)
prop.table(table(doc[which(doc$MARRIAGE == 0),]$EDUCATION))
barplot(table(doc[which(doc$MARRIAGE == 0),]$EDUCATION))

hist(doc[which(doc$MARRIAGE == 0),]$AGE)
boxplot(doc[which(doc$MARRIAGE == 0),]$AGE)
quantile(doc[which(doc$MARRIAGE == 0),]$AGE)
summary(doc[which(doc$MARRIAGE == 0),]$AGE)

###
#AGE
hist(doc$AGE)#, plot = FALSE
boxplot(doc$AGE)
boxplot(doc$AGE~doc$default.payment.next.month)
quantile(doc$AGE)
summary(doc$AGE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.00   28.00   34.00   35.49   41.00   79.00

ggplot(doc, aes(x=default.payment.next.month, y=AGE)) + 
  geom_boxplot()

ggplot(doc, aes(AGE, fill = default.payment.next.month)) + 
stat_bin(binwidth = 5, center = 2.5)

###
# PAY_0,2-6 -1 = pay duly, 1 = payment delay for one month, 2 = payment delay for two months, ..., 8 = payment delay for eight months, 9 = payment delay for nine months and above
# PAY_5-6 ohne 1
# range -2-8

#doc$PAY_0_new <- doc$PAY_0 + 1

table(doc$PAY_0)
prop.table(table(doc$PAY_0))
# -2           -1            0            1            2            3            4 
# 0.0919666667 0.1895333333 0.4912333333 0.1229333333 0.0889000000 0.0107333333 0.0025333333 
# 5            6            7            8 
# 0.0008666667 0.0003666667 0.0003000000 0.0006333333
barplot(table(doc$PAY_0))

ggplot(doc, aes(PAY_0, fill = default.payment.next.month)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="default.payment.next.month")

ggplot(data = doc) + 
  geom_mosaic(aes(x=product(PAY_0), fill = default.payment.next.month)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

###
# BILL_ATM1-6
hist(doc$BILL_AMT1)
boxplot(doc$BILL_AMT1)
boxplot(doc$BILL_AMT1~doc$default.payment.next.month)
quantile(doc$BILL_AMT1)
summary(doc$BILL_AMT1)

ggplot(doc, aes(x=default.payment.next.month, y=BILL_AMT1)) + 
  geom_boxplot()

ggplot(doc, aes(BILL_AMT1, fill = default.payment.next.month)) + 
stat_bin(binwidth = 100000, center = 50000)

###
# PAY_ATM1-6
hist(doc$PAY_AMT1)
boxplot(doc$PAY_AMT1)
boxplot(doc$PAY_AMT1~doc$default.payment.next.month)
quantile(doc$PAY_AMT1)
summary(doc$PAY_AMT1)

ggplot(doc, aes(x=default.payment.next.month, y=PAY_AMT1)) + 
  geom_boxplot()

ggplot(doc, aes(PAY_AMT1, fill = default.payment.next.month)) + 
stat_bin(binwidth = 50000, center = 25000)

###
# default.payment.next.month Yes = 1, No = 0
table(doc$default.payment.next.month)
prop.table(table(doc$default.payment.next.month))
# no    yes 
# 0.7788 0.2212

###
# stat default.payment.next.month/sex
# default.payment.next.month 1=yes, 0=no
# sex 1=male, 2=female
# yes favorable
# no unfavorable
# female privileged
# male unprivileged

prop.table(table(doc$default.payment.next.month, doc$SEX))
# female       male
# no  0.47830000 0.30050000
# yes 0.12543333 0.09576667

prop.table(table(doc[which(doc$SEX == "male"),]$default.payment.next.month))
# no       yes 
# 0.7583277 0.2416723

prop.table(table(doc[which(doc$SEX == "female"),]$default.payment.next.month))
# no       yes 
# 0.7922372 0.2077628

prop.table(table(doc[which(doc$default.payment.next.month == "yes"),]$SEX))
# female      male 
# 0.5670585 0.4329415

p1 <- ggplot(doc, aes(default.payment.next.month, fill = SEX)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="sex")

p2 <- ggplot(data = doc) + 
  geom_mosaic(aes(x=product(default.payment.next.month), fill = SEX)) + 
  DALEX::theme_ema() +
  scale_fill_manual("", values = c("grey", "red3"))

grid.arrange(p1, p2, ncol=2)

# # remove columns PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6
# doc <- doc[which(!(doc$EDUCATION %in% c(0, 5, 6)) & doc$MARRIAGE != 0),]
# doc <- doc[,c("LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE", "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6", "default.payment.next.month")]
