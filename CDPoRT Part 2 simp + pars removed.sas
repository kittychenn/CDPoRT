/*CDPoRT 
  PART 2:  Linear Predictor Macro

  Created on: Dec 2 2020
  Last updated: Dec 2 2020

  Source:  Ng et al. Development and Validation of the Chronic Disease Population Risk Tool (CDPoRT) to predict incidence of adult chronic disease; JAMA Open, 2020.
           https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2766780

  Contact: pophealthanalytics.dlsph@utoronto.ca

  Purpose: CDPoRT Part 2 is used to create a macro that calculates the linear predictor

  
* Notes:			%RCSPLINE from harrell
http://biostat.mc.dvanderbilt.edu/wiki/Main/SasMacros

FULL_LP / PARS_LP / SIMPLE_LP:
	-Calculates the linear predictor for the full, parsimonious and simple models
FULL_PRED_SURV / PARS_PRED_SURV / SIMPLE_PRED_SURV:
	-Predicted survival probabiliy for full, parsimonious and simple models
FULL_PRED_RISK / PARS_PRED_RISK / SIMPLE_PRED_RISK:
	-Predicted probability of chornic disease for the the full, parsimonious and simple models

FU_TIME macro variable sets the follow-up time in years (default is ten years)

The AFT variables also calculate the linear predictor, survival and risk using
	a slightly different formula (e.g. full_aft_lp,	full_aft_surv, full_aft_risk)


*******************************************************************************************************************************************************************
*******************************************************************************************************************************************************************;



/*******************************************************************
Harrell Spline code
*******************************************************************/

%MACRO RCSPLINE(x,knot1,knot2,knot3,knot4,knot5,knot6,knot7,
                  knot8,knot9,knot10, norm=2);
%LOCAL j v7 k tk tk1 t k1 k2;
%LET v7=&x; %IF %LENGTH(&v7)=8 %THEN %LET v7=%SUBSTR(&v7,1,7);
  %*Get no. knots, last knot, next to last knot;
    %DO k=1 %TO 10;
    %IF %QUOTE(&&knot&k)=  %THEN %GOTO nomorek;
    %END;
%LET k=11;
%nomorek: %LET k=%EVAL(&k-1); %LET k1=%EVAL(&k-1); %LET k2=%EVAL(&k-2);
%IF &k<3 %THEN %PUT ERROR: <3 KNOTS GIVEN.  NO SPLINE VARIABLES CREATED.;
%ELSE %DO;
 %LET tk=&&knot&k;
 %LET tk1=&&knot&k1;
 DROP _kd_; _kd_=
 %IF &norm=0 %THEN 1;
 %ELSE %IF &norm=1 %THEN &tk - &tk1;
 %ELSE (&tk - &knot1)**.666666666666; ;
    %DO j=1 %TO &k2;
    %LET t=&&knot&j;
    &v7&j=max((&x-&t)/_kd_,0)**3+((&tk1-&t)*max((&x-&tk)/_kd_,0)**3
        -(&tk-&t)*max((&x-&tk1)/_kd_,0)**3)/(&tk-&tk1)%STR(;);
    %END;
 %END;
%MEND;



/*************************************************************************
Calculate the linear predictors for each person
*************************************************************************/

%macro lp(in = CDPoRT,
		  out = lp,
		  fu_time = 10);

  data female_&out.
	   male_&out.;
	set &in.;
 	if female = 1 then output female_&out;
	else output male_&out.;
  run;

  proc sort data = female_&out.;
    by caseid;
  run;

  proc sort data = male_&out.;
    by caseid;
  run;

  /* ------ */
  /* Female */
  /* ------ */
  data female_&out. ;
    set female_&out. ;
	%rcspline(age_c,-22,-8,8,32);
	keep caseid alc_a smoking fruit_veg_a age_c: ethnicity_a education_d marital_a
		asthma_flag bmi_a hbp_flag health_a stress_a female
		sample_wt ;

		
  run;

  /* Full model */
  data female_&out. ;
    set female_&out. ;

	lp_alcohol = 0;
	lp_smoking = 0;
	lp_fvc = 0;
	lp_age = 0;
	lp_age1 = 0;
	lp_age2 = 0;
	lp_ethnicity = 0;
	lp_education = 0;
	lp_marital = 0;
	lp_asthma = 0;
	lp_bmi = 0;
	lp_hbp = 0;
	lp_health = 0;
	lp_stress = 0;

	
    if alc_a = "HeavyDrinker" then lp_alcohol = 0.2395; *Heavy drinker;
	else if alc_a = "ModerateDrinker" then lp_alcohol = 0.1035; *Moderate drinker;
	else if alc_a = "NeverDrinker" then lp_alcohol = 0.3163; *Non-drinker;

	
    if smoking = "Always occassional" then lp_smoking = 0.2722; *Always occasional smoker;
	else if smoking = "Occasional smoker" then lp_smoking = 0.6087; *Current occasional smoker, former daily smoker;
	else if smoking = "Daily" then lp_smoking = 1.050; *Daily smoker;
	else if smoking = "Former occassional" then lp_smoking = -0.1565; *Former occasional smoker;
	else if smoking = "Former daily" then lp_smoking = 0.2008; *Former daily smoker;


	if fruit_veg_a = 1 then lp_fvc = -0.0614; *3 to 6 times;
	else if fruit_veg_a = 2 then lp_fvc = -0.1168; *6+ times;


	lp_age = 0.1285*age_c; *Age continuous (spline term 1); 
	lp_age1 = -0.2481*age_c1; *Age spline term 2;
	lp_age2 = 0.5088*age_c2; *Age spline term 3;

	
      if ethnicity_a = "Non-White" then lp_ethnicity = 0.3402; *Visible minority;

	
      if education_d in ("SecondaryGraduate") then lp_education = -0.1046; *Post-secondary education;

	
    if marital_a = "Single" then lp_marital = 0.0726; *Single;
	else if marital_a = "Widowed, Separated or Divorced" then lp_marital = 0.0849; *Widowed;


	
	if asthma_flag = "Yes" then lp_asthma = 0.3730; *Asthma;

	if bmi_a = 1 then lp_bmi = -0.1823; *Underweight;
	else if bmi_a = 2 then lp_bmi = 0.3696; *Overweight;
	else if bmi_a = 3 then lp_bmi = 0.6141; *Obese class 1;
	else if bmi_a = 4 then lp_bmi = 1.0320; *Obese class 2;
	else if bmi_a = 5 then lp_bmi = 1.1634; *Obese class 3;
	else if bmi_a = 9 then lp_bmi = 0.4308; *Unknown BMI;


	if hbp_flag = "Yes" then lp_hbp = 0.3258; *HBP;

	
    if health_a = "Poor or Fair" then lp_health = 0.1939; *Poor or fair health;
	else if health_a = "Excellent or Very Good" then lp_health = -0.1850; *Very good or excellent health;


	
    if stress_a = "A bit stressful" then lp_stress = -0.0008; *A bit stressful;
	else if stress_a = "Not very stressful" then lp_stress = -0.1031; *Not very stressful;
	else if stress_a in ("Quite a bit or extremely stressful") then lp_stress = 0.0152; *Quite or extremely stressful;

	full_lp = sum(of lp_:, -4.3449);
	full_pred_surv = exp(-1*exp(full_lp)*((&fu_time.)**1.1275));
	full_pred_risk = 1-full_pred_surv;

	full_aft_lp = -1*full_lp/1.1275;
	full_aft_surv = 1*exp(-1*exp((log(&fu_time.)-full_aft_lp)/0.887));
	full_aft_risk = 1-full_aft_surv;

	drop lp_: ;
  run;

  
  /* ---- */
  /* Male */
  /* ---- */
  data male_&out. ;
    set male_&out. ;
	%rcspline(age_c,-23,-11,-2,9,26);
	keep caseid alc_a smoking fruit_veg_a age_c: ethnicity_a income_ca_5
		asthma_flag bmi_a hbp_flag health_a stress_a female
		sample_wt;
  run;

  /* Full model */
  data male_&out. ;
    set male_&out. ;

	lp_alcohol = 0;
	lp_smoking = 0;
	lp_fvc = 0;
	lp_age = 0;
	lp_age1 = 0;
	lp_age2 = 0;
	lp_age3 = 0;
	lp_ethnicity = 0;
	lp_income = 0;
	lp_asthma = 0;
	lp_bmi = 0;
	lp_hbp = 0;
	lp_health = 0;
	lp_stress = 0;

	
	if alc_a = "HeavyDrinker" then lp_alcohol = -0.1188; *Heavy drinker;
	else if alc_a = "ModerateDrinker" then lp_alcohol = -0.0091; *Moderate drinker;
	else if alc_a = "NeverDrinker" then lp_alcohol = 0.1806; *Non-drinker;


	
	if smoking = "Always occassional" then lp_smoking = 0.0501; *Always occasional smoker;
	else if smoking = "Occasional smoker" then lp_smoking = 0.3514; *Current occasional smoker, former daily smoker;
	else if smoking = "Daily" then lp_smoking = 0.8324; *Daily smoker;
	else if smoking = "Former occassional" then lp_smoking = 0.0121; *Former occasional smoker;
	else if smoking = "Former daily" then lp_smoking = 0.1619; *Former daily smoker;

	if fruit_veg_a = 1 then lp_fvc = -0.0760; *3 to 6 times;
	else if fruit_veg_a = 2 then lp_fvc = -0.1626; *6+ times;

	lp_age = 0.1900*age_c; *Age continuous (spline term 1); 
	lp_age1 = -0.4187*age_c1; *Age spline term 2;
	lp_age2 = 0.9329*age_c2; *Age spline term 3;
	lp_age3 = -0.4848*age_c3; *Age spline term 4;
	

	if ethnicity_a = "Non-White" then lp_ethnicity = 0.2690; *Visible minority;



	if income_ca_5 in ("Q1") then lp_income = 0.1139; *Low income;
	else if income_ca_5 in ("Unknown") then lp_income = 0.1345; * Unknown income;

	if asthma_flag = "Yes" then lp_asthma = 0.2749; *Asthma;

	if bmi_a = 1 then lp_bmi = 0.4222; *Underweight;
	else if bmi_a = 2 then lp_bmi = 0.1428; *Overweight;
	else if bmi_a = 3 then lp_bmi = 0.5687; *Obese class 1;
	else if bmi_a = 4 then lp_bmi = 0.9830; *Obese class 2;
	else if bmi_a = 5 then lp_bmi = 1.1879; *Obese class 3;
	else if bmi_a = 9 then lp_bmi = 0.3607; *Unknown BMI;


	if hbp_flag = "Yes" then lp_hbp = 0.3599; *HBP;


    if health_a = "Poor or Fair" then lp_health = 0.1138; *Poor or fair health;
	else if health_a = "Excellent or Very Good" then lp_health = -0.2896; *Very good or excellent health;



    if stress_a = "A bit stressful" then lp_stress = -0.0318; *A bit stressful;
	else if stress_a = "Not very stressful" then lp_stress = -0.0821; *Not very stressful;
	else if stress_a in ("Quite a bit or extremely stressful") then lp_stress = -0.1284; *Quite or extremely stressful;



	full_lp = sum(of lp_:, -3.1830);
	full_pred_surv = exp(-1*exp(full_lp)*((&fu_time.)**1.1770));
	full_pred_risk = 1-full_pred_surv;

	full_aft_lp = -1*full_lp/1.1770;
	full_aft_surv = 1*exp(-1*exp((log(&fu_time.)-full_aft_lp)/0.8496));
	full_aft_risk = 1-full_aft_surv;

	drop lp_: ;
  run;

%mend;


/*************************************************************************
Run the macro
*************************************************************************/

%lp;


/*******************************************************************
Check Female
*******************************************************************/
*/
***Check RCS;
proc print data = female_lp (obs = 30);
  var age_c age_c1 age_c2;
run;

***Check predicted probabilities - FEMALES;
proc print data = female_lp (obs = 20);
  var full_lp full_pred_surv full_pred_risk full_aft_lp full_aft_surv full_aft_risk;
run;

*/

/*******************************************************************
Check Male
*******************************************************************/
/*
***Check RCS;
proc print data = male_lp (obs = 20);
  var caseid age_c age_c1 age_c2 age_c3;
run;

***Check predicted probabilities - MALES;
proc print data = male_lp (obs = 20);
  var full_lp full_pred_surv full_pred_risk full_aft_lp full_aft_surv full_aft_risk;
run;

*/

