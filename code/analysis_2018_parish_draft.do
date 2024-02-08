/*
Version: Aug 31, 2023
Author: Alonso Quijano-Ruiz
*/

// Set working directory
clear
cd /Users/elocarinista/Research/Weather_HealthPerception
set more off

// Create a log file
log using "logs/analysis.log", replace

// Load the 2018 data
use "data/Weather_HealthPerception_2018.dta"

// ******************************************************************************************
// Table 2: Daily weather and self-rated health, with controls and fixed effects
// ******************************************************************************************

global varofint "tmin0 tmax0 precip0"
global demo_controls "age c.age#c.age i.sex i.ethnicity i.education income"
global health_controls "sick got_care prev_care hospitalized"

* Naive OLS without fixed effects

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls $health_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_1.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

* Fixed effects model with parish fixed effects

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_2.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

* Fixed effects model with parish and day-by-day fixed effects

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_3.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Table 3: Heterogenous daily weather effects by gender
// ******************************************************************************************

// Heterogeneous effects

gen female = 0
replace female = 1 if sex == 2

gen tmax0_female = tmax0 * female
gen tmin0_female = tmin0 * female 
gen precip0_female = precip0 * female

global interfem "tmax0 tmax0_female tmin0 tmin0_female precip0 precip0_female"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $interfem $demo_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_female tmin0_female precip0_female)
esttab model* using tables/model_4.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_female tmin0_female precip0_female) stats(r2 N) title(Panel A: Heterogeneous effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $interfem $demo_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_female tmin0_female precip0_female)
esttab model* using tables/model_5.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_female tmin0_female precip0_female) stats(r2 N) title(Panel A: Heterogeneous effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $interfem $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_female tmin0_female precip0_female)
esttab model* using tables/model_6.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_female tmin0_female precip0_female) stats(r2 N) title(Panel A: Heterogeneous effects) addnote("Robust standard errors clustered at parish level")

// Female only

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls if female == 1, vce(cluster parish_id)
	local m `++m'
}
esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using tables/model_7.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel B: Female only) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if female == 1, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}
esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using tables/model_8.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel B: Female only) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if female == 1, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}
esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using tables/model_9.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel B: Female only) addnote("Robust standard errors clustered at parish level")

// Male only

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls if female == 0, vce(cluster parish_id)
	local m `++m'
}
esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using tables/model_10.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel C: Male only) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if female == 0, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}
esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using tables/model_11.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel C: Male only) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if female == 0, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}
esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using tables/model_12.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel C: Male only) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Table 4: Heterogeneous daily weather effects by region
// ******************************************************************************************

// Heterogeneous effects

gen sierra = 0
replace sierra = 1 if region == 1

gen coast = 0
replace coast = 1 if region == 2

gen amazon = 0
replace amazon = 1 if region == 3

gen tmax0_coast = tmax0 * coast
gen tmin0_coast = tmin0 * coast
gen precip0_coast = precip0 * coast

gen tmax0_amazon = tmax0 * amazon
gen tmin0_amazon = tmin0 * amazon
gen precip0_amazon = precip0 * amazon

global interregion "tmax0 tmin0 precip0 coast amazon tmax0_coast tmin0_coast precip0_coast tmax0_amazon tmin0_amazon precip0_amazon"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $interregion $demo_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_coast tmin0_coast precip0_coast tmax0_amazon tmin0_amazon precip0_amazon) stats(r2 N)
esttab model* using tables/model_13.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_coast tmin0_coast precip0_coast tmax0_amazon tmin0_amazon precip0_amazon) stats(r2 N) title(Table 3: Heterogenous effects by region) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $interregion $demo_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_coast tmin0_coast precip0_coast tmax0_amazon tmin0_amazon precip0_amazon) stats(r2 N)
esttab model* using tables/model_14.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_coast tmin0_coast precip0_coast tmax0_amazon tmin0_amazon precip0_amazon) stats(r2 N) title(Table 3: Heterogenous effects by region) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $interregion $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_coast tmin0_coast precip0_coast tmax0_amazon tmin0_amazon precip0_amazon) stats(r2 N)
esttab model* using tables/model_15.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_coast tmin0_coast precip0_coast tmax0_amazon tmin0_amazon precip0_amazon) stats(r2 N) title(Table 3: Heterogenous effects by region) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Appendix A: Daily weather and self-rated health split by region
// ******************************************************************************************

// Sierra only

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls if region == 1, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_16.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel A: Sierra) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if region == 1, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_17.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel A: Sierra) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if region == 1, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_18.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel A: Sierra) addnote("Robust standard errors clustered at parish level")

// Coast only

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls if region == 2, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_19.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel B: Coast) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if region == 2, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_20.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel B: Coast) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if region == 2, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_21.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel B: Coast) addnote("Robust standard errors clustered at parish level")

// Amazon only

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls if region == 3, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_22.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel C: Amazon) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if region == 3, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_23.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel C: Amazon) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls if region == 3, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_24.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Panel C: Amazon) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Table 5: Effects of short-term weather shocks on self-rated health
// ******************************************************************************************

gen tmax0_1 = tmax0 - tmax_1
gen tmax0_3 = tmax0 - tmax_3
gen tmax0_7 = tmax0 - tmax_7

gen tmin0_1 = tmin0 - tmin_1
gen tmin0_3 = tmin0 - tmin_3
gen tmin0_7 = tmin0 - tmin_7

gen precip0_1 = precip0 - precip_1
gen precip0_3 = precip0 - precip_3
gen precip0_7 = precip0 - precip_7

global varofint2 "tmax0_1 tmax0_3 tmax0_7 tmin0_1 tmin0_3 tmin0_7 precip0_1 precip0_3 precip0_7"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint2 $demo_controls $health_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_1 tmax0_3 tmax0_7 tmin0_1 tmin0_3 tmin0_7 precip0_1 precip0_3 precip0_1 precip0_3 precip0_7) stats(r2 N)
esttab model* using tables/model_25.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_1 tmax0_3 tmax0_7 tmin0_1 tmin0_3 tmin0_7 precip0_1 precip0_3 precip0_1 precip0_3 precip0_7) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint2 $demo_controls $health_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_1 tmax0_3 tmax0_7 tmin0_1 tmin0_3 tmin0_7 precip0_1 precip0_3 precip0_1 precip0_3 precip0_7) stats(r2 N)
esttab model* using tables/model_26.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_1 tmax0_3 tmax0_7 tmin0_1 tmin0_3 tmin0_7 precip0_1 precip0_3 precip0_1 precip0_3 precip0_7) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint2 $demo_controls $health_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_1 tmax0_3 tmax0_7 tmin0_1 tmin0_3 tmin0_7 precip0_1 precip0_3 precip0_1 precip0_3 precip0_7) stats(r2 N)
esttab model* using tables/model_27.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_1 tmax0_3 tmax0_7 tmin0_1 tmin0_3 tmin0_7 precip0_1 precip0_3 precip0_1 precip0_3 precip0_7) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Appendix 2: Daily weather and self-rated health, using average temperature
// ******************************************************************************************

global varofint3 "tavg0 precip0"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint3 $demo_controls $health_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tavg0 precip0) stats(r2 N)
esttab model* using tables/model_28.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tavg0 precip0) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint3 $demo_controls $health_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tavg0 precip0) stats(r2 N)
esttab model* using tables/model_29.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tavg0 precip0) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint3 $demo_controls $health_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tavg0 precip0) stats(r2 N)
esttab model* using tables/model_30.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tavg0 precip0) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Appendix 3: Fixed effects model with weather interacted with hot and rainy parish
// ******************************************************************************************

// Hot parish

rename hot_parish_q3 hot_parish

gen tmax0_hot_parish = tmax0 * hot_parish
gen tmin0_hot_parish = tmin0 * hot_parish
gen precip0_hot_parish = precip0 * hot_parish

global varofint31 "tmax0 tmin0 precip0 hot_parish tmax0_hot_parish tmin0_hot_parish precip0_hot_parish"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint31 $demo_controls $health_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_hot_parish tmin0_hot_parish precip0_hot_parish) stats(r2 N)
esttab model* using tables/model_31.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_hot_parish tmin0_hot_parish precip0_hot_parish) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint31 $demo_controls $health_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_hot_parish tmin0_hot_parish precip0_hot_parish) stats(r2 N)
esttab model* using tables/model_32.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_hot_parish tmin0_hot_parish precip0_hot_parish) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint31 $demo_controls $health_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_hot_parish tmin0_hot_parish precip0_hot_parish) stats(r2 N)
esttab model* using tables/model_33.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_hot_parish tmin0_hot_parish precip0_hot_parish) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// Rainy parish

rename rainy_parish_q3 rainy_parish

gen tmax0_rainy_parish = tmax0 * rainy_parish
gen tmin0_rainy_parish = tmin0 * rainy_parish
gen precip0_rainy_parish = precip0 * rainy_parish

global varofint32 "tmax0 tmin0 precip0 rainy_parish tmax0_rainy_parish tmin0_rainy_parish precip0_rainy_parish"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint32 $demo_controls $health_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_rainy_parish tmin0_rainy_parish precip0_rainy_parish) stats(r2 N)
esttab model* using tables/model_34.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_rainy_parish tmin0_rainy_parish precip0_rainy_parish) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint32 $demo_controls $health_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_rainy_parish tmin0_rainy_parish precip0_rainy_parish) stats(r2 N)
esttab model* using tables/model_35.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_rainy_parish tmin0_rainy_parish precip0_rainy_parish) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint32 $demo_controls $health_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_rainy_parish tmin0_rainy_parish precip0_rainy_parish) stats(r2 N)
esttab model* using tables/model_36.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_rainy_parish tmin0_rainy_parish precip0_rainy_parish) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Appendix 4: Daily weather and self-rated health, using survey weights
// ******************************************************************************************

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls $health_controls [pweight=weight], vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_37.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls [pweight=weight], absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_38.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls [pweight=weight], absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_39.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Appendix 5: Daily weather and self-rated health, split by survey round
// ******************************************************************************************

// Round 1 only

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls $health_controls if survey_round == 1, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_40.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls if survey_round == 1, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_41.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls if survey_round == 1, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_42.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// Round 2 only

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls $health_controls if survey_round == 2, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_43.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls if survey_round == 2, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_44.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls if survey_round == 2, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_45.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Appendix 6: Daily weather and self-rated health, using Ensanut 2012
// ******************************************************************************************

// Load the 2012 data
use "data/Weather_HealthPerception_2012.dta", clear

global varofint "tmin0 tmax0 precip0"
global demo_controls "age c.age#c.age i.sex i.ethnicity i.education income"
global health_controls "sick got_care hospitalized"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reg `indepvar' $varofint $demo_controls $health_controls, vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_46.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls, absorb(parish_id) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_47.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	eststo model_`m'_`varofint': reghdfe `indepvar' $varofint $demo_controls $health_controls, absorb(parish_id survey_date) vce(cluster parish_id)
	local m `++m'
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N)
esttab model* using tables/model_48.csv, replace se star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0) stats(r2 N) title(Two-way fixed effects model with parish and day-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

log close
