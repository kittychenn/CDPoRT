#Prepare Data
#install.packages(tidyverse)
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("matrixStats")
#install.packages('survey')
#install.packages('base')
library(tidyverse)
library(readxl)
library(dplyr)
library(matrixStats)
library(survey)
library(base)

CCHS201516 <- read.csv(("~/Undergrad/Honours/cchs-82M0013-E-2015-2016-Annual-component_F1.csv"))
#View(CCHS201516)

# Creating smaller data frame for CCHS data, contains only variables used in Part 1
attach(CCHS201516)
CCHSdata <- data.frame(GEO_PRV, WTS_M, DHHGAGE, DHH_SEX, ALC_010, 
                       ALC_015, ALC_020, ALWDVWKY, SMKDVSTY,
                       FVCDVFRU, FVCDVORA, FVCDVGRN, FVCDVJUI, FVCDVVEG,
                       SDCDGCGT, INCDVRCA, INCDVRPR, EHG2DVR3, DHHGMS,
                       CCC_015, CCC_065, GENDVHDI, GEN_020, CCC_130, CCC_030, CCC_085, 
                       CCC_090, CCC_095, HWTDGWTK, HWTDGHTM, WTS_M)
detach(CCHS201516)
CDPORT = CCHSdata

#*create continuous age grouping
CDPORT$cchs_age_grp <-
  ifelse(CDPORT$DHHGAGE==1, "12-14",
  ifelse(CDPORT$DHHGAGE==2, "15-17",
  ifelse(CDPORT$DHHGAGE==3, "18-19",
  ifelse(CDPORT$DHHGAGE==4, "20-24",
  ifelse(CDPORT$DHHGAGE==5, "25-29",
  ifelse(CDPORT$DHHGAGE==6, "30-34",
  ifelse(CDPORT$DHHGAGE==7, "35-39",
  ifelse(CDPORT$DHHGAGE==8, "40-44",
  ifelse(CDPORT$DHHGAGE==9, "45-49",
  ifelse(CDPORT$DHHGAGE==10, "50-54",
  ifelse(CDPORT$DHHGAGE==11, "55-59",
  ifelse(CDPORT$DHHGAGE==12, "60-64",
  ifelse(CDPORT$DHHGAGE==13, "65-69",
  ifelse(CDPORT$DHHGAGE==14, "70-74",
  ifelse(CDPORT$DHHGAGE==15, "75-79",
  ifelse(CDPORT$DHHGAGE==16, "80-100", 'Unknown'))))))))))))))))

# Notes:
#    - this is GENerated from the cchs_age_grp with a discrete uniform distribution for each group
#    - need to add plus one to the final value (b) of UNIFORM distribution because UNIFORM will only do [a,b);

CDPORT$cchs_age_cts <-
  ifelse(CDPORT$cchs_age_grp=='12-14',floor(runif(1,min=12,max=14+1)), 
  ifelse(CDPORT$cchs_age_grp=='15-17',floor(runif(1,min=15,max=17+1)),
  ifelse(CDPORT$cchs_age_grp=='18-19',floor(runif(1,min=18,max=19+1)),
  ifelse(CDPORT$cchs_age_grp=='20-24',floor(runif(1,min=20,max=24+1)),
  ifelse(CDPORT$cchs_age_grp=='25-29',floor(runif(1,min=25,max=29+1)),
  ifelse(CDPORT$cchs_age_grp=='30-34',floor(runif(1,min=30,max=34+1)),
  ifelse(CDPORT$cchs_age_grp=='35-39',floor(runif(1,min=35,max=39+1)),
  ifelse(CDPORT$cchs_age_grp=='40-44',floor(runif(1,min=40,max=44+1)),
  ifelse(CDPORT$cchs_age_grp=='45-49',floor(runif(1,min=45,max=49+1)),
  ifelse(CDPORT$cchs_age_grp=='50-54',floor(runif(1,min=50,max=54+1)),
  ifelse(CDPORT$cchs_age_grp=='55-59',floor(runif(1,min=55,max=59+1)),
  ifelse(CDPORT$cchs_age_grp=='60-64',floor(runif(1,min=60,max=64+1)),
  ifelse(CDPORT$cchs_age_grp=='65-69',floor(runif(1,min=65,max=69+1)),
  ifelse(CDPORT$cchs_age_grp=='70-74',floor(runif(1,min=70,max=74+1)),
  ifelse(CDPORT$cchs_age_grp=='75-79',floor(runif(1,min=75,max=79+1)),
  ifelse(CDPORT$cchs_age_grp=='80-100',floor(runif(1,min=80,max=100+1)), NA)))))))))))))))) 

#*Age centred at 45
CDPORT$age_c <- CDPORT$cchs_age_cts - 45
  #** Age groups
CDPORT$age_cat <-
  ifelse(  CDPORT$cchs_age_cts < 20,'Lt 20 yrs',
  ifelse(20<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 34,'20 to 34 yrs',
  ifelse(35<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 44,'35 to 40 yrs',
  ifelse(45<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 54,'45 to 54 yrs',
  ifelse(55<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 64,'55 to 64 yrs',
  ifelse(65<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 74,'65 to 74 yrs',
  ifelse(75<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 84,'75 to 84 yrs',
  ifelse(CDPORT$cchs_age_cts >= 85,'85+ yrs','Unknown'))))))))
  

#*Sex
CDPORT$female <- 
  ifelse(CDPORT$DHH_SEX==1,0,
  ifelse(CDPORT$DHH_SEX==2,1, NA))
CDPORT$male <- 
  ifelse(CDPORT$DHH_SEX==1,1,
  ifelse(CDPORT$DHH_SEX==2,0, NA))

#*ALCOHOL CONSUMPTION
#original code (used do function from SAS)
#CDPORT$alc_a <-
#  ifelse(CDPORT$ALC_010==2 | CDPORT$ALC_015 %in% c(1, 2, 3),'Never Drinker',
#         ifelse(CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7),do(
#           if((CDPORT$DHH_SEX==1 & 0<=CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 3)|(CDPORT$DHH_SEX==2 & 0<=CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 2)){
#             'LightDrinker'
#           } else if((CDPORT$DHH_SEX==1 & 4<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 21)|(CDPORT$DHH_SEX==2 & 3<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 14)){
#             'ModerateDrinker'
#           } else if((CDPORT$DHH_SEX==1 & 21<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<996)|(CDPORT$DHH_SEX==2 & 14<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<996)|(5<=CDPORT$ALC_020 & CDPORT$ALC_020<=6)){
#             'HeavyDrinker'
#           })), 'Unknown')

#only alc_a that works is below, 'Never Drinker' and total is accurate, need to fix other categories 
#thinking there is an issue with the brackets that are used to separate the different parts of the conditions

############### EP EDITS ################################################
CDPORT$alc_a <-
  ifelse(CDPORT$ALC_010==2 | CDPORT$ALC_015 %in% c(1, 2, 3),'Never drinker',
               ifelse((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) &
                     ((CDPORT$DHH_SEX==1 & 21<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <996)|
                     (CDPORT$DHH_SEX==2 & 14<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <996)|
                     (5<=CDPORT$ALC_020 & CDPORT$ALC_020 <=6)), 'Heavy drinker',
               ifelse((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) &
                      ((CDPORT$DHH_SEX==1 & 4<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 21)|
                      (CDPORT$DHH_SEX==2 & 3<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 14)), 'Moderate drinker',
                ifelse((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) &
                      ((CDPORT$DHH_SEX==1 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 3)|
                      (CDPORT$DHH_SEX==2 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <= 2)), 'Light drinker',
                                     'Unknown'))))
#####################################################################################


#ifelse((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) &
#        ((CDPORT$DHH_SEX==1 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 3)|
#          (CDPORT$DHH_SEX==2 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <= 2)), 'LightDrinker',
#           ifelse((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) &
#                   ((CDPORT$DHH_SEX==1 & 4<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 21)|
#                     (CDPORT$DHH_SEX==2 & 3<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 14)), 'ModerateDrinker',

#testing a function
#alc_afx <-function(row){
#  ALC_010 <-row[['ALC_010']]
#  ALC_015 <-row[['ALC_015']]
#  ALC_020 <-row[['ALC_020']]
#  DHH_SEX <-row[['DHH_SEX']]
#  ALWDVWKY <-row[['ALWDVWKY']]
  
#  if(CDPORT$ALC_010==2 | CDPORT$ALC_015 %in% c(1, 2, 3)){
#    alc_a <-'Never Drinker'
#  }
#  else if((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) & ((CDPORT$DHH_SEX==1 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 3)|(CDPORT$DHH_SEX==2 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <= 2))){
#    alc_a <-'LightDrinker'
#  }
#  else if((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) & ((CDPORT$DHH_SEX==1 & 4<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 21)|(CDPORT$DHH_SEX==2 & 3<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 14))){
#    alc_a <-'ModerateDrinker'
#  }
#  else if((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) & (CDPORT$DHH_SEX==1 & 21<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <996)|(CDPORT$DHH_SEX==2 & 14<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <996)|(5<=CDPORT$ALC_020 & CDPORT$ALC_020 <=6)){
#    alc_a <-'HeavyDrinker'
#    }
#  return(alc_a)
#}
#CDPORT$alc_a <-apply(CDPORT, 1, alc_afx)

#testing another modification
#CDPORT$alc_a <-'Unknown'
#  if(CDPORT$ALC_010==2 | CDPORT$ALC_015 %in% c(1, 2, 3)){'Never Drinker'
#    }else if((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) & ((CDPORT$DHH_SEX==1 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 3)|(CDPORT$DHH_SEX==2 & 0<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <= 2))){'LightDrinker'
#      }else if((CDPORT$ALC_010==1 & CDPORT$ALC_015 %in% c(4, 5, 6, 7)) & ((CDPORT$DHH_SEX==1 & 4<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 21)|(CDPORT$DHH_SEX==2 & 3<= CDPORT$ALWDVWKY & CDPORT$ALWDVWKY<= 14))){'ModerateDrinker'
#        }else if((CDPORT$DHH_SEX==1 & 21<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <996)|(CDPORT$DHH_SEX==2 & 14<CDPORT$ALWDVWKY & CDPORT$ALWDVWKY <996)|(5<=CDPORT$ALC_020 & CDPORT$ALC_020 <=6)){'HeavyDrinker'}

#*SMOKING
CDPORT$smoking <- 
  ifelse(CDPORT$SMKDVSTY==1,'Daily',
  ifelse(CDPORT$SMKDVSTY==2,'Occasional smoker',
  ifelse(CDPORT$SMKDVSTY==3, 'Always occassional',
  ifelse(CDPORT$SMKDVSTY==4,'Former daily',
  ifelse(CDPORT$SMKDVSTY==5,'Former occassional',
  ifelse(CDPORT$SMKDVSTY==6,'Never','Unknown'))))))

#*DAILY FRUIT AND VEG CONSUMPTION
#**Fruit: based on the CCHS derived variable FVCDFRU
#**Carrot: based on the CCHS derived variable FVCDCAR
#**Potato: based on the CCHS derived variable FVCDPOT
#**Juice: based on the CCHS derived variable FVCDJUI
#**Salad: based on the CCHS derived variable FVCDSAL
#**Other_Veg: based on the CCHS derived variable FVCDVEG 
#**If the values for fruit, carrot, potato, juice, salad and other_veg are missing, just ignore
CDPORT$fruit = ifelse(CDPORT$FVCDVFRU==999.9,NA, CDPORT$FVCDVFRU) 
CDPORT$orange = CDPORT$FVCDVORA
CDPORT$orange = ifelse(CDPORT$FVCDVORA==999.9,NA, CDPORT$FVCDVORA)
CDPORT$juice = CDPORT$FVCDVJUI
CDPORT$juice = ifelse(CDPORT$FVCDVJUI==999.9,NA, CDPORT$FVCDVJUI)
CDPORT$juice2 <- 
  ifelse(is.na(CDPORT$juice), NA,
  ifelse(CDPORT$FVCDVJUI<1,CDPORT$juice,1))#new variable created to be able to add juice to fruit_veg
CDPORT$green = CDPORT$FVCDVGRN
CDPORT$green = ifelse(CDPORT$FVCDVGRN==999.9,NA, CDPORT$FVCDVGRN)
CDPORT$other_veg = CDPORT$FVCDVVEG
CDPORT$other_veg = ifelse(CDPORT$FVCDVVEG==999.9,NA, CDPORT$FVCDVVEG)
CDPORT$juice <-NULL
CDPORT$fruit_veg = 
  ifelse((is.na(CDPORT$fruit) | is.na(CDPORT$orange) | is.na(CDPORT$juice2) |
           is.na(CDPORT$green) | is.na(CDPORT$other_veg)), NA,
         rowSums(CDPORT[ , c('fruit', 'orange', 'juice2', 'green', 'other_veg')]))

#**If missing value for Fruit_Veg, then delete.
#all feeder variables above have accurate frequencies (when compared to SAS) but fruit_veg_a categories show different frequencies?
CDPORT$fruit_veg_a <- 
  ifelse(0<=CDPORT$fruit_veg & CDPORT$fruit_veg<3,0,
  ifelse(3<=CDPORT$fruit_veg & CDPORT$fruit_veg<6,1,
  ifelse(CDPORT$fruit_veg>=6,2,
         NA)))

#*VISIBLE MINORITY
CDPORT$ethnicity_a <-
  ifelse(CDPORT$SDCDGCGT==1,'White',
  ifelse(CDPORT$SDCDGCGT==2,'Non-white', 'Unknown'))

#*HOUSEHOLD INCOME (provincial-level)
CDPORT$income_pr_5 <- 
  ifelse(CDPORT$INCDVRPR %in% c(1, 2),'Q1',
  ifelse(CDPORT$INCDVRPR %in% c(3, 4),'Q2',
  ifelse(CDPORT$INCDVRPR %in% c(5, 6),'Q3',
  ifelse(CDPORT$INCDVRPR %in% c(7, 8),'Q4',
  ifelse(CDPORT$INCDVRPR %in% c(9, 10),'Q5', 'Unknown')))))
CDPORT$income_ca_5 <-
  ifelse(CDPORT$INCDVRCA %in% c(1, 2),'Q1',
  ifelse(CDPORT$INCDVRCA %in% c(3, 4),'Q2',
  ifelse(CDPORT$INCDVRCA %in% c(5, 6),'Q3',
  ifelse(CDPORT$INCDVRCA %in% c(7, 8),'Q4',
  ifelse(CDPORT$INCDVRCA %in% c(9, 10),'Q5', 'Unknown')))))

#*EDUCATION
CDPORT$education_cat <- 
  ifelse(CDPORT$EHG2DVR3==1,'Less Than Secondary',
  ifelse(CDPORT$EHG2DVR3==2,'Secondary Graduate',
  ifelse(CDPORT$EHG2DVR3==3,'More Than Secondary', 'Unknown')))

#**Education - 5 cat
CDPORT$education_d <- 
  ifelse(CDPORT$EHG2DVR3==1,'Less Than Secondary',
  ifelse(CDPORT$EHG2DVR3 %in% c(2, 3),'Secondary Graduate', 'Unknown'))

#*Marital Status
CDPORT$marital_a <- 
  ifelse(CDPORT$DHHGMS %in% c(1, 2),'Married or CommonLaw',
  ifelse(CDPORT$DHHGMS %in% c(3),'Widowed, Separated, or Divorced',
  ifelse(CDPORT$DHHGMS %in% c(4),'Single', 'Unknown')))

#*Asthma
CDPORT$asthma_flag <- 
  ifelse(CDPORT$CCC_015==1,'Yes',
  ifelse(CDPORT$CCC_015==2,'No', 'Unknown'))

#*Corrected BMI
######################## EP EDITS ########################################################
#(moved NA from the end of the argument to the middle and changed < to >=)
CDPORT$bmi <- 
  ifelse(CDPORT$HWTDGWTK>=999.96 & CDPORT$HWTDGHTM>=9.996, NA, (CDPORT$HWTDGWTK / (CDPORT$HWTDGHTM^2)))
###########################################################################################
CDPORT$bmi_corr_cat= NA
CDPORT$bmi_corr <-
  ifelse(CDPORT$female==0,(-0.29227 + 1.03239*CDPORT$bmi),
  ifelse(CDPORT$female==1,(0.10927 + 1.02584*CDPORT$bmi), CDPORT$bmi))

#**BMI categories
#bmi categories 1, 5, and 9 have discrepancies in their frequencies when compared to SAS but the rest are accurate
CDPORT$bmi_a <-
  ifelse(is.na(CDPORT$bmi_corr), 9,
  ifelse(0<=CDPORT$bmi_corr & CDPORT$bmi_corr<18.5,1, 
  ifelse(18.5<=CDPORT$bmi_corr & CDPORT$bmi_corr<25,0,
  ifelse(25 <=CDPORT$bmi_corr & CDPORT$bmi_corr<30,2,
  ifelse(30 <=CDPORT$bmi_corr & CDPORT$bmi_corr<35,3,
  ifelse(35 <=CDPORT$bmi_corr & CDPORT$bmi_corr<40,4,
  ifelse(40 <=CDPORT$bmi_corr & CDPORT$bmi_corr<999,5,9)))))))
    
#*High blood pressure
CDPORT$hbp_flag <-
  ifelse(CDPORT$CCC_065==1,'Yes',
  ifelse(CDPORT$CCC_065==2,'No', 'Unknown'))

#*Self-Rated GENeral health
CDPORT$health_a <-
  ifelse(CDPORT$GENDVHDI %in% c(0,1),'Poor or fair', 
  ifelse(CDPORT$GENDVHDI==2,'Good',
  ifelse(CDPORT$GENDVHDI %in% c(3,4),'Excellent or very good', 'Unknown')))

#*Life stress
CDPORT$stress_a <-
  ifelse(CDPORT$GEN_020 %in% c(1),'Not at all stressful',
  ifelse(CDPORT$GEN_020 %in% c(2),'Not very stressful',
  ifelse(CDPORT$GEN_020 %in% c(3),'A bit stressful',
  ifelse(CDPORT$GEN_020 %in% c(4,5),'Quite a bit or extremely stressful', 'Unknown'))))

#*Cancer
CDPORT$cancer <- 
  ifelse(CDPORT$CCC_130==1,'Yes',
  ifelse(CDPORT$CCC_130==2,'No', 'Unknown'))

#*COPD
CDPORT$copd <-
  ifelse(CDPORT$CCC_030==1,'Yes',
  ifelse(CDPORT$CCC_030==2,'No', 'Unknown'))

#*Heart Disease
CDPORT$hrd <-
  ifelse(CDPORT$CCC_085==1,'Yes',
  ifelse(CDPORT$CCC_085==2,'No', 'Unknown'))
      
#*Stroke
CDPORT$stroke <-
  ifelse(CDPORT$CCC_090==1,'Yes',
  ifelse(CDPORT$CCC_090==2,'No', 'Unknown'))
      
#*Diabetes
CDPORT$diab <-
  ifelse(CDPORT$CCC_095==1,'Yes',
  ifelse(CDPORT$CCC_095==2,'No', 'Unknown'))

#*Food security
CDPORT$foodsec <-
  ifelse(CDPORT$FSCDVHF2==0, 'Food insecure',
  ifelse(CDPORT$FSCDVHF2==1, 'Moderately food insecure',
  ifelse(CDPORT$FSCDVHF2==2, 'Severely food insecure', 'Unknown')))

#*Occupation groups
CDPORT$occupation <-
  ifelse(CDPORT$LBFDGOCG==1, 'Management, Natural and Applied Sciences, Health, Social Sciences, Education, Religion, Art, Culture, Recreation',
  ifelse(CDPORT$LBFDGOCG==2, 'Business, Finance, Administration',
  ifelse(CDPORT$LBFDGOCG==3, 'Sales and Service',
  ifelse(CDPORT$LBFDGOCG==4, 'Trades, Transport and Equipment Operator',
  ifelse(CDPORT$LBFDGOCG==5, 'Primary Industry,
Processing, Manufacturing and Utilities','Unknown')))))

#*Household size
CDPORT$hld_size <-
  ifelse(CDPORT$DHHDGHSZ==1, 'One persons',
  ifelse(CDPORT$DHHDGHSZ==2, 'Two persons',
  ifelse(CDPORT$DHHDGHSZ==3, 'Three persons',
  ifelse(CDPORT$DHHDGHSZ==4, 'Four persons', 
  ifelse(CDPORT$DHHDGHSZ==5, 'Five or more persons','Unknown')))))

#*Immigrant
CDPORT$imm <-
  ifelse(CDPORT$SDCDVIMM==1, 'Immigrant',
  ifelse(CDPORT$SDCDVIMM==2, 'Non-immigrant', 'Unknown'))

#*Flag for missing on any predictor variable (except BMI)
CDPORT$missing_female <-
  ifelse(CDPORT$alc_a=='Unknown'|CDPORT$smoking=='Unknown'|
           CDPORT$fruit_veg_a=='Unknown'| CDPORT$ethnicity_a == "Unknown" |
           CDPORT$education_d == "Unknown"| CDPORT$marital_a == "Unknown" | 
           CDPORT$asthma_flag == "Unknown" | CDPORT$hbp_flag == "Unknown" | 
           CDPORT$health_a == "Unknown" | CDPORT$stress_a == "Unknown", 1, 0)
CDPORT$missing_male <-
  ifelse(CDPORT$alc_a == "Unknown" | CDPORT$smoking  == "Unknown" |CDPORT$fruit_veg_a=='Unknown'|
           CDPORT$ethnicity_a  == "Unknown" | CDPORT$income_ca_5 == "Unknown" |
           CDPORT$asthma_flag == "Unknown" | CDPORT$hbp_flag == "Unknown" |
           CDPORT$health_a == "Unknown" | CDPORT$stress_a == "Unknown", 1, 0)

CDPORT$missing_f <-
  ifelse(CDPORT$female == 1 & CDPORT$missing_female == 1, 1, 0)
CDPORT$missing_m <-
  ifelse(CDPORT$female == 0 & CDPORT$missing_male == 1, 1, 0)

#check that all variables are in the CDPORT data frame (from the CCHS dataset)
#names(CDPORT)

#restricting sample to province of Ontario
CDPORT <- filter(CDPORT, CDPORT$GEO_PRV == 35) 

#Apply Exclusions
ind <- with(CDPORT, (CDPORT$cchs_age_cts<20) | (CDPORT$cancer=='Yes')|
                                           (CDPORT$copd=='Yes')|
                                           (CDPORT$hrd=='Yes')|
                                           (CDPORT$stroke=='Yes')|
                                           (CDPORT$diab=='Yes')|
                                           (CDPORT$missing_f==1)|
                                           (CDPORT$missing_m==1))
CDPORT <- CDPORT[!ind, ]
#*Checking variable frequencies
summary(CDPORT)
table(CDPORT$alc_a)
table(CDPORT$age_cat)
table(CDPORT$female)
table(CDPORT$smoking)
table(CDPORT$fruit_veg)
table(CDPORT$fruit_veg_a)
table(CDPORT$fruit)
table(CDPORT$orange)
table(CDPORT$juice2)
table(CDPORT$green)
table(CDPORT$other_veg)
table(CDPORT$ethnicity_a)
table(CDPORT$income_pr_5)
table(CDPORT$income_ca_5)
table(CDPORT$education_cat)
table(CDPORT$education_d)
table(CDPORT$marital_a)
table(CDPORT$asthma_flag)
table(CDPORT$bmi_a)
table(CDPORT$hbp_flag)
table(CDPORT$health_a)
table(CDPORT$stress_a)
table(CDPORT$male)

## Equity 
table(CDPORT$imm)
table(CDPORT$occupation)
table(CDPORT$foodsec)
table(CDPORT$hld_size)
table(CDPORT$race)

# BMI
CDPORT$bmi_corrs = summary(CDPORT$bmi_corr)
class(CDPORT$bmi_corr)

#*proc univariate portion; checks distribution of continuous age with weights
svydesign(ids=0, variables=CDPORT$cchs_age_c, weights=CDPORT$WTS_M)
weighted.mean(CDPORT$cchs_age_cts, CDPORT$sample_wt)
complete.cases(CDPORT)

# remove NA rows in R
CDPORT_NA <- filter(CDPORT, rowSums(is.na(CDPORT)) !=ncol(CDPORT))
