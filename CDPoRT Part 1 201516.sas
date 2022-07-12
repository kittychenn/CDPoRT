/*CDPoRT 
  PART 1:  Data Preparation

  Created on: Dec 2 2020
  Last updated: Dec 2 2020

  Source:  Ng et al. Development and Validation of the Chronic Disease Population Risk Tool (CDPoRT) to predict incidence of adult chronic disease; JAMA Open, 2020.
           https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2766780

  Contact: pophealthanalytics.dlsph@utoronto.ca

  Purpose: CDPoRT Part 1 is used to prepare the predictor variables for the CDPoRT
           algorithm and to apply the exclusions. This program uses the CCHS 2013/14 PUMF.

*******************************************************************************************************************************************************************
*******************************************************************************************************************************************************************;


/*******************************************************************
Prepare data
*******************************************************************/

*1) import data set;
*importing a CSV file into SAS;

proc import out = CCHS201516
datafile = "C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 1/CCHS201516.csv"
dbms = csv replace;
getnames = yes;
run;

data CDPoRT;
set CCHS201516;
run;


/*******************************************************************
Recode CCHS variables for use in CDPoRT
*******************************************************************/

data CDPORT;
keep CASEID geo_prv GEODGHR4 wts_m sample_wt cchs_age_grp dhhgage cchs_age_cts Age_c Age_Cat Female Male DHH_SEX alc_a alc_010 alc_015 alc_020 alwdvwky smkdvsty smoking FVCDVFRU
fruit FVCDVORA FVCDVJUI FVCDVGRN FVCDVVEG Fruit Carrot Juice Salad Other_Veg Fruit_Veg Fruit_Veg_a ethnicity_a SDCDGCGT Income_pr_5 Income_ca_5 INCDVRPR
INCDVRCA Education_Cat EHG2DVR3 Education_d Marital_a DHHGMS Asthma_flag ccc_015 BMI HWTDGWTK HWTDGHTM BMI_corr BMI_corr_cat BMI_a HBP_flag ccc_065 Health_a
GENDVHDI  GEN_020 stress_a ccc_130 cancer  ccc_030 copd  ccc_085 hrd  ccc_090 stroke  ccc_095 diab  missing_female missing_f missing_male missing_m;
length stress_a $ 25 health_a $ 25 Education_d $20 alc_a $15 smoking $ 20 ethnicity_a $ 15 Education_Cat $ 20 ;
	set CDPORT; 
	if geo_prv = 35; *restricting sample to province of Ontario;

sample_wt = wts_m;

** create continuous age grouping;

cchs_age_grp="Unknown";
 if dhhgage in (1) then cchs_age_grp= "12-14";
 if dhhgage in (2) then cchs_age_grp= "15-17";
 if dhhgage in (3) then cchs_age_grp= "18-19";
 if dhhgage in (4) then cchs_age_grp= "20-24";
 if dhhgage in (5) then cchs_age_grp= "25-29";
 if dhhgage in (6) then cchs_age_grp= "30-34";
 if dhhgage in (7) then cchs_age_grp= "35-39";
 if dhhgage in (8) then cchs_age_grp= "40-44";
 if dhhgage in (9) then cchs_age_grp= "45-49";
 if dhhgage in (10) then cchs_age_grp= "50-54";
 if dhhgage in (11) then cchs_age_grp= "55-59";
 if dhhgage in (12) then cchs_age_grp= "60-64";
 if dhhgage in (13) then cchs_age_grp= "65-69";
 if dhhgage in (14) then cchs_age_grp= "70-74";
 if dhhgage in (15) then cchs_age_grp= "75-79";
 if dhhgage in (16) then cchs_age_grp= "80-100";

/* age */
* Notes:
	- this is generated from the cchs_age_grp with a discrete uniform distribution for each group
	- need to add plus one to the final value (b) of UNIFORM distribution because UNIFORM will only do [a,b);

select (cchs_age_grp);

	when ('12-14') cchs_age_cts = floor(rand('UNIFORM',12,14+1));
	when ('15-17') cchs_age_cts = floor(rand('UNIFORM',15,17+1));
	when ('18-19') cchs_age_cts = floor(rand('UNIFORM',18,19+1));
	when ('20-24') cchs_age_cts = floor(rand('UNIFORM',20,24+1));
	when ('25-29') cchs_age_cts = floor(rand('UNIFORM',25,29+1));
	when ('30-34') cchs_age_cts = floor(rand('UNIFORM',30,34+1));
	when ('35-39') cchs_age_cts = floor(rand('UNIFORM',35,39+1));
	when ('40-44') cchs_age_cts = floor(rand('UNIFORM',40,44+1));
	when ('45-49') cchs_age_cts = floor(rand('UNIFORM',45,49+1));
	when ('50-54') cchs_age_cts = floor(rand('UNIFORM',50,54+1));
	when ('55-59') cchs_age_cts = floor(rand('UNIFORM',55,59+1));
	when ('60-64') cchs_age_cts = floor(rand('UNIFORM',60,64+1));
	when ('65-69') cchs_age_cts = floor(rand('UNIFORM',65,69+1));
	when ('70-74') cchs_age_cts = floor(rand('UNIFORM',70,74+1));
	when ('75-79') cchs_age_cts = floor(rand('UNIFORM',75,79+1));
	when ('80-100') cchs_age_cts = floor(rand('UNIFORM',80,100+1));
	otherwise;
end;

** Age centered at 45 ;
       Age_c= cchs_age_cts - 45;

	** Age groups;
              If .   < cchs_age_cts< 20    then Age_Cat="Lt 20 yrs   ";  
         else if 20 <= cchs_age_cts<= 34   then Age_Cat="20 to 34 yrs";  
         else if 35 <= cchs_age_cts<= 44   then Age_Cat="35 to 44 yrs";  
         else if 45 <= cchs_age_cts<= 54   then Age_Cat="45 to 54 yrs";  
         else if 55 <= cchs_age_cts<= 64   then Age_Cat="55 to 64 yrs";  
         else if 65 <= cchs_age_cts<= 74   then Age_Cat="65 to 74 yrs";  
         else if 75 <= cchs_age_cts<= 84   then Age_Cat="75 to 84 yrs";
         else if       cchs_age_cts>=85    then Age_Cat="85+ yrs";  

** Sex; 
         if DHH_SEX =1   then Female=0;  
	     if DHH_SEX =2   then Female=1; 

		 if DHH_SEX =1   then Male=1;  
	     if DHH_SEX =2   then Male=0; 


**ALCOHOL CONSUMPTION;

     alc_a= "Unknown";
    if  alc_010=2  or alc_015 in (1, 2, 3)                                                       then alc_a= "NeverDrinker";
    if  alc_010=1  and alc_015 in (4, 5, 6, 7)   then do;
        if  (dhh_Sex=1 and 0<=alwdvwky<=3) or   (dhh_Sex=2 and 0<=alwdvwky<=2)                 then alc_a= "LightDrinker";
        if  (dhh_Sex=1 and 4<=alwdvwky<=21) or  (dhh_Sex=2 and 3<=alwdvwky<=14)                then alc_a= "ModerateDrinker";
        if  (dhh_Sex=1 and 21<alwdvwky< 996) or (dhh_Sex=2 and 14<alwdvwky<996) or 5<=alc_020<=6 then alc_a= "HeavyDrinker";
     end;

**SMOKING;
smoking = "Unknown";
if smkdvsty =1    then smoking = "Daily";
if smkdvsty =2    then smoking = "Occasional smoker";
if smkdvsty =3    then smoking = "Always occassional";
if smkdvsty =4    then smoking = "Former daily";
if smkdvsty =5    then smoking = "Former occassional";
if smkdvsty =6    then smoking = "Never";


** DAILY FRUIT AND VEG CONSUMPTION;
    *Fruit: based on the CCHS derived variable FVCDFRU
     Carrot: based on the CCHS derived variable FVCDCAR
     Potato: based on the CCHS derived variable FVCDPOT
     Juice: based on the CCHS derived variable FVCDJUI
     Salad: based on the CCHS derived variable FVCDSAL
     Other_Veg: based on the CCHS derived variable FVCDVEG 
     If the values for fruit, carrot, potato, juice, salad and other_veg are missing, just ignore;

Fruit = FVCDVFRU;
if FVCDVFRU =999.9 then Fruit = "Unknown";

Carrot = FVCDVORA;
if FVCDVORA = 999.9 then Carrot = "Unknown";

Juice = FVCDVJUI;
if FVCDVJUI = 999.9 then Juice = "Unknown";

Salad = FVCDVGRN;
if FVCDVGRN = 999.9 then Salad = "Unknown";

Other_Veg = FVCDVVEG; 
if FVCDVVEG = 999.9 then Other_Veg = "Unknown";

Fruit_Veg = sum(Fruit, Carrot, min(Juice,1), Salad, Other_Veg);
if Fruit = "Unknown" or
   Carrot = "Unknown" or
   Juice = "Unknown" or
   Salad = "Unknown" or
   Other_Veg = "Unknown" 
   then Fruit_Veg = "Unknown";
  
  *If missing value for Fruit_Veg, then delete.;  
if Fruit_Veg = "Unknown" then Fruit_Veg = "Unknown";
else Fruit_Veg = round(Fruit_Veg, 0.1);

  if 0 <= Fruit_Veg < 3 then Fruit_Veg_a = 0;
  else if 3 <= Fruit_Veg < 6 then Fruit_Veg_a = 1;
  else if Fruit_Veg >=6 then Fruit_Veg_a = 2;



  **VISIBLE MINORITY;
ethnicity_a= "Unknown"; 
if SDCDGCGT= 1                                          then ethnicity_a="White";  
	   else if SDCDGCGT = 2                             then ethnicity_a="Non-White"; 	

** HOUSEHOLD INCOME (provincial-level); 
	        Income_pr_5="Unknown"; 
			Income_ca_5="Unknown";
         If INCDVRPR in (1, 2) then Income_pr_5= "Q1";
         If INCDVRPR in (3, 4) then Income_pr_5= "Q2"; 
         If INCDVRPR in (5, 6) then Income_pr_5= "Q3";
         If INCDVRPR in (7, 8) then Income_pr_5= "Q4"; 	
         If INCDVRPR in (9, 10) then Income_pr_5= "Q5"; 
		 If INCDVRCA in (1, 2) then Income_ca_5= "Q1";
         If INCDVRCA in (3, 4) then Income_ca_5= "Q2"; 
         If INCDVRCA in (5, 6) then Income_ca_5= "Q3";
         If INCDVRCA in (7, 8) then Income_ca_5= "Q4"; 	
         If INCDVRCA in (9, 10) then Income_ca_5= "Q5"; 



** EDUCATION; 
               Education_Cat= "Unknown";
               If EHG2DVR3= 1        then Education_Cat= "LessThanSecondary";
          else If EHG2DVR3= 2        then Education_Cat= "SecondaryGraduate"; 
          else if EHG2DVR3 in (3, 4) then Education_Cat= "MoreThanSecondary"; 

	/* Education - 5 cat*/
		  Education_d = "Unknown";
          If EHG2DVR3 = 1 then Education_d =  "LessThanSecondary";
          else If EHG2DVR3 in (2, 3, 4)then Education_d = "SecondaryGraduate";


** Marital status; 
		         Marital_a="Unknown";
              If DHHGMS in (1, 2)    then Marital_a= "Married or CommonLaw";   
	     else if DHHGMS in (3) then Marital_a= "Widowed, Separated or Divorced";  
	     else if DHHGMS in (4)       then Marital_a= "Single";  

** Asthma; 
             Asthma_flag= "Unknown"; 
           if  ccc_015= 1 then Asthma_flag= "Yes";
	     if  ccc_015= 2 then Asthma_flag= "No";


** Corrected BMI;
         if HWTDGWTK >= 999.96 and HWTDGHTM >= 9.996 then BMI = .; else BMI=(HWTDGWTK/HWTDGHTM**2);
         BMI_corr = .;
         BMI_corr_cat = .;
         if Female = 0 then BMI_corr = -0.29227 + 1.03239*BMI;
		 if Female = 1 then BMI_corr = 0.10927 + 1.02584*BMI;
           
     ** BMI categories;
              if     .< BMI_corr< 18.5 then BMI_a= 1;
         else if 18.5<= BMI_corr< 25   then BMI_a= 0;
         else if 25  <= BMI_corr< 30   then BMI_a= 2;
         else if 30  <= BMI_corr< 35   then BMI_a= 3; 
         else if 35  <= BMI_corr< 40   then BMI_a= 4; 
         else if 40<=BMI_corr<999      then BMI_a= 5;
         else                               BMI_a= 9;

** High blood pressure; 
             HBP_flag= "Unknown";
         if  ccc_065=1 then HBP_flag= "Yes";
         if  ccc_065=2 then HBP_flag= "No";


** Self-Rated general health; 
                 Health_a   = "Unknown";
              if GENDVHDI in (0, 1)    then Health_a= "Poor or Fair";
         else if GENDVHDI=2            then Health_a= "Good";       
         else if GENDVHDI in (3, 4)    then Health_a= "Excellent or Very Good";


** Life stress; 
                 stress_a= "Unknown";
              if GEN_020 in (1)       then stress_a= "Not at all stressful";  
         else if GEN_020 in (2)       then stress_a= "Not very stressful";  
         else if GEN_020 in (3)       then stress_a= "A bit stressful";  
		 else if GEN_020 in (4, 5)    then stress_a= "Quite a bit or extremely stressful";

** Cancer; 
             cancer= "Unknown";
         if  ccc_130=1 then cancer= "Yes";
         if  ccc_130=2 then cancer= "No";

** COPD; 
             copd= "Unknown";
         if  ccc_030=1 then copd= "Yes";
         if  ccc_030=2 then copd= "No";

** Heart Disease; 
             hrd= "Unknown";
         if  ccc_085=1 then hrd= "Yes";
         if  ccc_085=2 then hrd= "No";

** Stroke; 
             stroke= "Unknown";
         if  ccc_090=1 then stroke= "Yes";
         if  ccc_090=2 then stroke= "No";

** Diabetes; 
             diab= "Unknown";
         if  ccc_095=1 then diab= "Yes";
         if  ccc_095=2 then diab= "No";

** Flag for missing on any predictor variable (except BMI) ;
missing_female = 0; missing_male = 0; missing_f = 0; missing_m = 0;
	if alc_a = "Unknown" | smoking = "Unknown" | Fruit_Veg_a = "Unknown" | ethnicity_a = "Unknown" | Education_d = "Unknown" | Marital_a = "Unknown" | Asthma_flag = "Unknown" | HBP_flag = "Unknown" | Health_a = "Unknown" | stress_a = "Unknown" then missing_female = 1; 
	if alc_a = "Unknown" | smoking  = "Unknown" | Fruit_Veg_a  = "Unknown" | ethnicity_a  = "Unknown" | Income_ca_5 = "Unknown" | Asthma_flag = "Unknown" | HBP_flag = "Unknown" | Health_a = "Unknown" | stress_a = "Unknown" then missing_male = 1;
	if Female = 1 & missing_female  = 1 then missing_f = 1; 
	if Female = 0 & missing_male = 1 then missing_m = 1;

run;

*******************************************************************************************************************************************************************
APPLY EXCLUSIONS
*******************************************************************************************************************************************************************;

data CDPORT; set CDPORT;
if cchs_age_cts  < 20 	then delete; *included those 20 and above only;
if cancer = "Yes" | copd = "Yes" | hrd = "Yes" | stroke = "Yes" | diab = "Yes" then delete; *drop anyone with cancer, copd, heart disease, stroke or diabetes;
if missing_f = 1 then delete; *drop any female with a missing predictor variable;
if missing_m = 1 then delete; *drop any male with a missing predictor variable;

	
run;

**Checking variable frequencies;

proc surveyfreq data = CDPORT; table Age_c; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Age_cat; weight sample_wt; run;
proc surveyfreq data = CDPORT; table female; weight sample_wt; run;
proc surveyfreq data = CDPORT; table male; weight sample_wt; run;
proc surveyfreq data = CDPORT; table alc_a; weight sample_wt; run;
proc surveyfreq data = CDPORT; table smoking; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Fruit; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Carrot; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Juice; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Salad; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Other_Veg; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Fruit_Veg; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Fruit_Veg_a; weight sample_wt; run;
proc surveyfreq data = CDPORT; table ethnicity_a; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Income_ca_5; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Education_Cat; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Education_d; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Marital_a; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Asthma_flag; weight sample_wt; run;
proc surveyfreq data = CDPORT; table BMI_a; weight sample_wt; run;
proc surveyfreq data = CDPORT; table HBP_flag; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Health_a; weight sample_wt; run;
proc surveyfreq data = CDPORT; table stress_a; weight sample_wt; run;

proc surveyfreq data = CDPORT; table bmi_corr; weight sample_wt; run;
proc univariate data = CDPORT; var cchs_age_cts; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Fruit ; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Carrot; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Juice; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Salad; weight sample_wt; run;
proc surveyfreq data = CDPORT; table Other_Veg; weight sample_wt; run;
Fruit = "Unknown" or
   Carrot = "Unknown" or
   Juice = "Unknown" or
   Salad = "Unknown" or
   Other_Veg 
