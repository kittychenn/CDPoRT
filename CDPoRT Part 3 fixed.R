#install.packages('data.table')
#install.packages('questionr')
#install.packages('rcompanion')
#install.packages('purrr')
library(data.table)
library(plyr)
library(questionr)
library(rcompanion)
library(purrr)
library(srvyr)
library(survey)

#MERGE CDPoRT linear predictor variables into the CCHS dataset

#CDPORT_lp is a combines female_lp and male_lp datasets
CDPORT_lp <- female_lp
CDPORT_lp<-arrange(CDPORT_lp, CASEID)
View(CDPORT_lp)

CDPORT_lp$age_rcsf <-
  ifelse(male_lp$female==0, male_lp$age_rcsf, CDPORT_lp$age_rcsf)

CDPORT_lp$age_c1 <-
  ifelse(male_lp$female==0, male_lp$age_c1,CDPORT_lp$age_c1)

CDPORT_lp$age_c2 <-
  ifelse(male_lp$female==0, male_lp$age_c2,CDPORT_lp$age_c2)

CDPORT_lp$age_c3 <-
  ifelse(male_lp$female==0, male_lp$age_c3, NA)

CDPORT_lp$full_lp <-
  ifelse(male_lp$female==0, male_lp$full_lp,CDPORT_lp$full_lp)

CDPORT_lp$full_pred_surv <-
  ifelse(male_lp$female==0, male_lp$full_pred_surv,CDPORT_lp$full_pred_surv)

CDPORT_lp$full_pred_risk <-
  ifelse(male_lp$female==0, male_lp$full_pred_risk,CDPORT_lp$full_pred_risk)

CDPORT_lp$full_aft_lp <-
  ifelse(male_lp$female==0, male_lp$full_aft_lp,CDPORT_lp$full_aft_lp)

CDPORT_lp$full_aft_surv <-
  ifelse(male_lp$female==0, male_lp$full_aft_surv,CDPORT_lp$full_aft_surv)

CDPORT_lp$full_aft_risk <-
  ifelse(male_lp$female==0, male_lp$full_aft_risk,CDPORT_lp$full_aft_risk)


#keep linear predictor variables from CDPORT_lp and add to CDPORT dataset
#create new variables in CDPORT that are equivalent to CDPORT_lp variables
CDPORT$age_rcsf <- CDPORT_lp$age_rcsf
CDPORT$age_c1 <- CDPORT_lp$age_c1
CDPORT$age_c2 <- CDPORT_lp$age_c2
CDPORT$age_c3 <- CDPORT_lp$age_c3
CDPORT$full_lp <- CDPORT_lp$full_lp
CDPORT$full_pred_surv <- CDPORT_lp$full_pred_surv
CDPORT$full_pred_risk <- CDPORT_lp$full_pred_risk
CDPORT$full_aft_lp <- CDPORT_lp$full_aft_lp
CDPORT$full_aft_surv <- CDPORT_lp$full_aft_surv
CDPORT$full_aft_risk <- CDPORT_lp$full_aft_risk

#Create a variable for the number of cases
CDPORT$ncd_full = CDPORT$full_pred_risk*CDPORT$WTS_M

#ESTIMATE CDPORT RISK
#*using calculated linear predictor from the full model

#distribution of full_pred_risk
ggplot(CDPORT, aes(x=full_pred_risk)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

#distribution of ncd_full
ggplot(CDPORT, aes(x=ncd_full)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

###############################################################################

#*By Age

#average 10-year predicted risk of chronic disease by age group
#WeightedSum - weighted frequencies (i.e. corrected population) for Ontario
#ncd_full - sum of new chronic disease cases for Ontario

risk_agecat <- 
  svy_design %>%
  group_by(age_cat) %>%
  summarise(Frequency = n(),
            Mean = mean(full_pred_risk),
            StdErrorMean = sd(full_pred_risk)/sqrt(n()),
            Prevalence = survey_mean()*100,
            ncd_full = sum(ncd_full),
            WeightedSum = survey_total()) 

#distribution of predicted risk for each age category 
ggplot(CDPORT, aes(x=age_cat, y=full_pred_risk)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)


#distribution of new chronic disease cases for each age category 
ggplot(CDPORT, aes(x=age_cat, y=ncd_full)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

################################################################################

#*By Sex

#average 10-year predicted risk of chronic disease by sex
#sum of new chronic disease cases for each sex
risk_sex <- 
  svy_design %>%
  group_by(female) %>%
  summarise(Frequency = n(),
            Mean = mean(full_pred_risk),
            StdErrorMean = sd(full_pred_risk)/sqrt(n()),
            Prevalence = survey_mean()*100,
            ncd_full = sum(ncd_full),
            WeightedSum = survey_total()) 

#distribution of predicted risk for each sex 
ggplot(CDPORT, aes(x=factor(female), y=full_pred_risk)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

#distribution of new chronic disease cases for each sex 
ggplot(CDPORT, aes(x=factor(female), y=ncd_full)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

################################################################################

#By Household Income

#average 10-year predicted risk of chronic disease by income
#WeightedSum - weighted frequencies (i.e. corrected population) for Ontario
#ncd_full - sum of new chronic disease cases for Ontario
risk_inc <- 
  svy_design %>%
  group_by(income_ca_5) %>%
  summarise(Frequency = n(),
            Mean = mean(full_pred_risk),
            StdErrorMean = sd(full_pred_risk)/sqrt(n()),
            Prevalence = survey_mean()*100,
            ncd_full = sum(ncd_full),
            WeightedSum = survey_total()) 

#distribution of predicted risk by income 
ggplot(CDPORT, aes(x=income_ca_5, y=full_pred_risk)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

#distribution of new chronic disease cases by income
ggplot(CDPORT, aes(x=income_ca_5, y=ncd_full)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

################################################################################

#By BMI

#average 10-year predicted risk of chronic disease for each bmi category
#WeightedSum - weighted frequencies (i.e. corrected population) for Ontario
#ncd_full - sum of new chronic disease cases for Ontario
risk_bmi <- 
  svy_design %>%
  group_by(bmi_a) %>%
  summarise(Frequency = n(),
            Mean = mean(full_pred_risk),
            StdErrorMean = sd(full_pred_risk)/sqrt(n()),
            Prevalence = survey_mean()*100,
            ncd_full = sum(ncd_full),
            WeightedSum = survey_total())

#distribution of predicted risk for each bmi category
ggplot(CDPORT, aes(x=bmi_a, y=full_pred_risk)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

#distribution of new chronic disease cases for each bmi category
ggplot(CDPORT, aes(x=bmi_a, y=ncd_full)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

################################################################################

#By High Blood Pressure

#average 10-year predicted risk of chronic disease based on blood pressure
#WeightedSum - weighted frequencies (i.e. corrected population) for Ontario
#ncd_full - sum of new chronic disease cases for Ontario
risk_hbp <- 
  svy_design %>%
  group_by(hbp_flag) %>%
  summarise(Frequency = n(),
            Mean = mean(full_pred_risk),
            StdErrorMean = sd(full_pred_risk)/sqrt(n()),
            Prevalence = survey_mean()*100,
            ncd_full = sum(ncd_full),
            WeightedSum = survey_total())

#distribution of predicted risk based on blood pressure
ggplot(CDPORT, aes(x=hbp_flag, y=full_pred_risk)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

#distribution of new chronic disease cases based on blood pressure
ggplot(CDPORT, aes(x=hbp_flag, y=ncd_full)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

################################################################################

#By Smoking

#average 10-year predicted risk of chronic disease based on smoking
#WeightedSum - weighted frequencies (i.e. corrected population) for Ontario
#ncd_full - sum of new chronic disease cases for Ontario
risk_smk <- 
  svy_design %>%
  group_by(smoking) %>%
  summarise(Frequency = n(),
            Mean = mean(full_pred_risk),
            StdErrorMean = sd(full_pred_risk)/sqrt(n()),
            Prevalence = survey_mean()*100,
            ncd_full = sum(ncd_full),
            WeightedSum = survey_total())

#distribution of predicted risk based on smoking
ggplot(CDPORT, aes(x=smoking, y=full_pred_risk)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

#distribution of new chronic disease cases based on smoking
ggplot(CDPORT, aes(x=smoking, y=ncd_full)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

################################################################################

#By Alcohol Consumption

#average 10-year predicted risk of chronic disease based on alcohol consumption
#WeightedSum - weighted frequencies (i.e. corrected population) for Ontario
#ncd_full - sum of new chronic disease cases for Ontario
risk_alc <- 
  svy_design %>%
  group_by(alc_a) %>%
  summarise(Frequency = n(),
            Mean = mean(full_pred_risk),
            StdErrorMean = sd(full_pred_risk)/sqrt(n()),
            Prevalence = survey_mean()*100,
            ncd_full = sum(ncd_full),
            WeightedSum = survey_total())

#distribution of predicted risk based on alcohol consumption
ggplot(CDPORT, aes(x=alc_a, y=full_pred_risk)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

#distribution of new chronic disease cases based on alcohol consumption
ggplot(CDPORT, aes(x=alc_a, y=ncd_full)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=23, size=4)

################################################################################
