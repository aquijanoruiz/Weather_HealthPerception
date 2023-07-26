/*
This script ...

Version: July 25, 2023
Author: Alonso Quijano-Ruiz
*/

// Set the working directory and load the data
clear
cd "/Users/elocarinista/Documents/R_Projects/Weather_HealthPerception/data"
use "Weather_HealthPerception_2018data.dta"

// Generate new variables
gen female = 0
replace female = 1 if sex == 2

gen age_group = "Children & Yound Adolescents" if age < 15
replace age_group = "Working Population" if age >= 15 & age < 65
replace age_group = "Elderly Population" if age >= 65

// display td(08jun2019) shows 21708
gen survey_round2 = "Round 1"
replace survey_round2 = "Round 2" if survey_date >= 21708

// Filter people aged 5 or above
keep if age >= 5

// Remove Galapagos Islands
drop if region == 4

// ******************************************************************************************
// Model 1: Naive OLS model with robust standard errors clustered at parish level
// ******************************************************************************************

global demo_controls "age c.age#c.age female i.ethnicity i.education income_percap"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reg `indepvar' `varofint' $demo_controls, vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Naive OLS model with robust standard errors clustered at parish level) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Model 2: Fixed effects model with parish fixed effects
// ******************************************************************************************

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reg `indepvar' `varofint' $demo_controls, absorb(parish_id) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Fixed effects model with parish fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Model 3: Fixed effects model with parish and year-by-day fixed effects
// ******************************************************************************************

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Fixed effects model with parish and year-by-day fixed effects) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Model 3: Weather effects by region
// ******************************************************************************************

** Coast

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if region == 1, absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Table 2 Panel A: Coast) addnote("Robust standard errors clustered at parish level")

** Sierra

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if region == 2, absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Table 2 Panel B: Sierra) addnote("Robust standard errors clustered at parish level")

** Amazon

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if region == 3, absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Table 2 Panel C: Amazon) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Model 3:
// Weather effects by age group
// ******************************************************************************************

** Children & Yound Adolescents

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if age_group == "Children & Yound Adolescents", absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Table 3 Panel A: Children & Yound Adolescents) addnote("Robust standard errors clustered at parish level")

** Working population

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if age_group == "Working Population", absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Table 3 Panel B: Working Population) addnote("Robust standard errors clustered at parish level")

** Elderly population

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if age_group == "Elderly Population", absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Table 3 Panel B: Elderly Population) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Model 4: Heterogeneous effects: weather interacted with female
// ******************************************************************************************

gen tmax0_female = tmax0 * female
gen tmin0_female = tmin0 * female 
gen precip0_female = precip0 * female

local tmax "tmax0 tmax0_female"
local tmin "tmin0 tmin0_female"
local precip "precip0 precip0_female"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
    foreach varofint in tmax tmin precip {
        eststo model_`m'_`varofint': reghdfe `indepvar' ``varofint'' $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id)
        local m `++m'
    }
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_female tmin0_female precip0_female)
esttab model* using reg_a1.csv, replace se keep(tmax0_female tmin0_female precip0_female) star(* 0.10 ** 0.05 *** 0.01) title(Impact of varofint fluctuations three days before and after the survey date) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Model 5: Heterogeneous effects: weather interacted with elerly population
// ******************************************************************************************

gen elderly = 0
replace elderly = 1 if age >= 65

gen tmax0_elderly = tmax0 * elderly
gen tmin0_elderly = tmin0 * elderly
gen precip0_elderly = precip0 * elderly

local tmax "tmax0 elderly tmax0_elderly"
local tmin "tmin0 elderly tmin0_elderly"
local precip "precip0 elderly precip0_elderly"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
    foreach varofint in tmax tmin precip {
        eststo model_`m'_`varofint': reghdfe `indepvar' ``varofint'' $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id)
        local m `++m'
    }
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0_elderly tmin0_elderly precip0_elderly)
esttab model* using reg_a1.csv, replace se keep(tmax0_elderly tmin0_elderly precip0_elderly) star(* 0.10 ** 0.05 *** 0.01) title(Heterogeneous effects: weather interacted with elerly population) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Robustness checks:
// Dynamic impacts using weather values three days before and after the survey date
// ******************************************************************************************

local tmax "tmax_3 tmax_2 tmax_1 tmax0 tmax1 tmax2 tmax3"
local tmin "tmin_3 tmin_2 tmin_1 tmin0 tmin1 tmin2 tmin3"
local precip "precip_3 precip_2 precip_1 precip0 precip1 precip2 precip3"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
    foreach weather in tmax tmin precip {
        eststo model_`m'_`weather': reghdfe `indepvar' ``weather'' $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id)
        local m `++m'
    }
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax_3 tmax_2 tmax_1 tmax0 tmax1 tmax2 tmax3 tmin_3 tmin_2 tmin_1 tmin0 tmin1 tmin2 tmin3 precip_3 precip_2 precip_1 precip0 precip1 precip2 precip3)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Impact of weather fluctuations three days before and after the survey date) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Robustness checks:
// Fixed effects model with health controls
// ******************************************************************************************

global health_controls "sick got_care prev_care hospitalized"

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls $health_controls, absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Fixed effects model with health controls) addnote("Robust standard errors clustered at parish level")

// ******************************************************************************************
// Robustness checks:
// Fixed effects model with two-way clustering by parish and date fixed effects
// ******************************************************************************************

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id survey_date)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Fixed effects model with two-way clustering by parish and date fixed effects) addnote("Robust standard errors clustered at parish and year-by-day levels")

// ******************************************************************************************
// Robustness checks:
// Fixed effects model with robust standard errors clustered at parish and household level
// ******************************************************************************************

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls, absorb(parish_id survey_date) vce(cluster parish_id home_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Fixed effects model with robust standard errors clustered at parish and household level) addnote("Robust standard errors clustered at parish and household levels")

// ******************************************************************************************
// Robustness checks:
// Weather effects by survey round
// ******************************************************************************************

** Round 1 2018-11-10 to 2019-01-12

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if survey_round == 1, absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Panel A: Round 1) addnote("Robust standard errors clustered at parish level")


** Round 2 2019-06-08 to 2019-07-17

est clear
local m=1
foreach indepvar of varlist good_health better_health {
	foreach varofint of varlist tmax0 tmin0 precip0 {
		eststo model_`m': reghdfe `indepvar' `varofint' $demo_controls if survey_round == 2, absorb(parish_id survey_date) vce(cluster parish_id)
		local m `++m'
	}
}

esttab model*, se nobaselevels star(* 0.10 ** 0.05 *** 0.01) keep(tmax0 tmin0 precip0)
esttab model* using reg_a1.csv, replace se keep(tmax0 tmin0 precip0) star(* 0.10 ** 0.05 *** 0.01) title(Panel B: Round 2) addnote("Robust standard errors clustered at parish level")

