setwd("path/to/r-scripts")

load(file = "sgc.Robj")

#laufkont	laufzeit`	moral		        verw		hoehe	  sparkont	beszeit			        rate						  famges			        buerge		    wohnzeit			    verm	    alter	weitkred				        wohn		bishkred	      beruf	pers			    telef			gastarb		      kredit
#status	  duration	credit_history	purpose	amount	savings	  employment_duration	installment_rate	personal_status_sex	other_debtors present_residence	property	age	  other_installment_plans	housing	number_credits	job		people_liable	telephone	foreign_worker	credit_risk

set.seed(4)

# simulate feature sex
sgc$sex <- ifelse(sgc$status %in% c("no checking account", "... < 0 DM"), 1, 0)
sgc$sex <- ifelse(runif(nrow(sgc)) > 0.15, sgc$sex, 1-sgc$sex)
sgc$sex <- factor(ifelse(sgc$sex == 1, "male", "female"))

# remove/exclude feature personal_status_sex
sgc <- sgc[,c("status", "duration", "credit_history", "purpose", "amount", "savings", "employment_duration", "installment_rate", "sex", "other_debtors", "present_residence", "property", "age", "other_installment_plans", "housing", "number_credits", "job", "people_liable", "telephone", "foreign_worker", "credit_risk")]