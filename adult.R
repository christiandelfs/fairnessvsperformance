setwd("path/to/r-scripts")

load(file = "adult.Robj")

# V1  V2        V3      V4        V5            V6              V7          V8            V9    V10 V11           V12           V13             V14             V15
# age workclass fnlwgt  education education-num marital-status  occupation  relationship  race  sex capital-gain  capital-loss  hours-per-week  native-country  income

# merge train and test data
adult <- rbind(adult_train, adult_test)#, stringsAsFactors = TRUE

# remove empty values
# adult <- adult[which(adult$workclass != "?" & adult$occupation != "?" & adult$native_country != "?"),]

# remove feature education_num
adult <- adult[,c("age", "workclass", "fnlwgt", "education", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")]#, "education_num"
