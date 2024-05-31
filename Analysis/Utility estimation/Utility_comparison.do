set more off
clear all

*******************************
*******************************
** 1. Descriptive statistics **
*******************************
*******************************

** Import data
cd "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis"
import delimited "new_data_utility.csv", clear

* DG *
drop if game_type != "DG"

/*preserve
drop if a!=1
gen coop = scenarios/endowment
set scheme lean1
hist coop, percent xtitle("% Endowment")
graph export hist_coop.pdf, replace
tab treatment_id, sum(coop)
restore

preserve
gen coop = scenarios/endowment
set scheme lean1
collapse (mean) mean_app, by(coop)
twoway line mean_app coop , xtitle("% Endowment") ytitle("Average Appropriateness") yline(0)
graph export hist_mean_app.pdf, replace
restore
*/

preserve
tempfile temp
gen coop = scenarios/endowment
set scheme lean1
collapse (mean) mean_app sd_app, by(coop)
gen db = 2
save `temp' 
restore

* generate db variable for graph *
gen db = 1
* generate cooperation variable *
gen coop = scenarios/endowment

append using `temp'
twoway (hist coop if a==1 & db==1, percent xtitle("% Endowment") yaxis(2) yscale(range(0) axis(1))) /// 
(line mean_app coop if db==2, xtitle("% Endowment") ytitle("Mean Appropriateness") yline(0) yaxis(1) yscale(range(0) axis(2))), legend( pos(12) label (1 "Choices") label (2 "Mean Appropriateness") rows(1))
graph export "Utility estimation\Output\Figures\hist_coop_mean_app.pdf", replace

twoway (line mean_app coop if db==2, xtitle("% Endowment") ytitle("Mean Appropriateness") yline(0) yaxis(1) yscale(range(0) axis(1))) ///
(line sd_app coop if db==2, yaxis(2) ytitle("Norm Uncertainty", axis(2))), ///
legend( pos(12) label (1 "Mean Appropriateness") label (2 "Norm Uncertainty") rows(1))
graph export "Utility estimation\Output\Figures\norm_uncertainty.pdf", replace

* Table 1
tab 
***************************
***************************
** 2. Utility estimation **
***************************
***************************

clear all
cd "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis"
import delimited "new_data_utility.csv", clear

* DG *
drop if game_type != "DG"

* set constraint for social preferences models *
constraint 1 payoff = 1
constraint 2 rho = 0
constraint 3 sigma = rho
constraint 4 sigma = 0

levelsof treatment_id, local(levels)

* Charness Rabin 2002 *
gen r = payoff > endowment/2
gen s = payoff < endowment/2
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

*******************************
*******************************
** 2.1 Paper-wise estimation **
*******************************
*******************************

log using "Utility estimation\Output\Logs\stata_MODELS_DG.log", replace

foreach l of local levels {
	di "treatment -> `l' "
	
	/* S - Selfish */
	clogit a payoff if treatment_id == "`l'", group(id) iter(50) vce(rob)
	est store S`l'
	local deltaS`l' = _b[payoff]
	local se_deltaS`l' = _se[payoff]
	
	/* N - Social expectation - Norm */
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(rob)
	est store N`l'
	local deltaN`l' = _b[payoff]
	local gammaN`l' = _b[mean_app]
	local se_deltaN`l' = _se[payoff]
	local se_gammaN`l' = _se[mean_app]
	
*	/* BA - Behindness averse */
*	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraints(1 2)
*	est store BA`l'
*	local rhoBA`l' = _b[rho]
*	local sigmaBA`l' = _b[sigma]
*	local se_rhoBA`l' = _se[rho]
*	local se_sigmaBA`l' = _se[sigma]
	
*	/* CP - Charity prone */
*	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraints(1 4)
*	est store CP`l'
*	local rhoCP`l' = _b[rho]
*	local sigmaCP`l' = _b[sigma]
*	local se_rhoCP`l' = _se[rho]
*	local se_sigmaCP`l' = _se[sigma]
	
	/* DA - Difference averse */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraint(1)
	est store DA`l'
	local rhoDA`l' = _b[rho]
	local sigmaDA`l' = _b[sigma]
	local se_rhoDA`l' = _se[rho]
	local se_sigmaDA`l' = _se[sigma]
	
	/* FU - Full model (social norms and social preferences)*/
	clogit a payoff rho sigma mean_app if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraint(1)
	est store FU`l'
	local rhoFU`l' = _b[rho]
	local sigmaFU`l' = _b[sigma]
	local gammaFU`l' = _b[mean_app]
	local se_rhoFU`l' = _se[rho]
	local se_sigmaFU`l' = _se[sigma]
	local se_gammaFU`l' = _se[mean_app]

	qui tab id if e(sample)
	local nobs_FU`l' = r(r)
	
}

foreach l of local levels {
estimates stats N`l' DA`l' S`l' FU`l'
matrix temp = r(S)
local N_AIC`l' = temp[1,5]
local DA_AIC`l' = temp[2,5]
local S_AIC`l' = temp[3,5]
local FU_AIC`l' = temp[4,5]
}

log close



/* STAMPA TABELLA COEFFICIENTS IN FORMATO LOG */

log using "Utility estimation\Output\Logs\stata_COEFF_ONG.log", replace

foreach l of local levels {
  di "`l' `deltaS`l'' `deltaN`l'' `gammaN`l'' `rhoDA`l'' `sigmaDA`l'' `rhoFU`l'' `sigmaFU`l'' `gammaFU`l''" 
 }

log close
 
 
  /* STAMPA TABELLA STD DEV IN FORMATO LOG */
log using "Utility estimation\Output\Logs\stata_SE_ONG.log", replace

foreach l of local levels {
  di "`l' `se_deltaS`l'' `se_deltaN`l'' `se_gammaN`l'' `se_rhoDA`l'' `se_sigmaDA`l'' `se_rhoFU`l'' `se_sigmaFU`l'' `se_gammaFU`l''"
 } 

log close


/* STAMPA TABELLA AIC IN FORMATO LOG */ 

log using "Utility estimation\Output\Logs\stata_AIC_ONG.log", replace
 
foreach l of local levels {
  di "`l' `S_AIC`l'' `N_AIC`l'' `DA_AIC`l'' `FU_AIC`l''"
 }

 
log close

log using "Utility estimation\Output\Logs\stata_Nobs.log", replace
 
foreach l of local levels {
  di "`l' `nobs_FU`l''"
 }

log close

** Norm uncertainty models **

foreach l of local levels {
	di "treatment -> `l' "

	/* NU - social norms and uncertainty */
	clogit a payoff mean_app sd_app if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear
	est store NU`l'
	local basedeltaNU`l' = _b[payoff]
	local basegammaNU`l' = _b[mean_app]
	local baseetaNU`l' = _b[sd_app]
	local se_basegammaNU`l' = _se[mean_app]
	local se_baseetaNU`l' = _se[sd_app]
	
	/* NU - social norms and uncertainty w/ interaction */
	clogit a payoff c.mean_app##c.sd_app if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear
	est store NU`l'
	local deltaNU`l' = _b[payoff]
	local gammaNU`l' = _b[mean_app]
	local etaNU`l' = _b[sd_app]
	local se_gammaNU`l' = _se[mean_app]
	local se_etaNU`l' = _se[sd_app]
	local sucaNU`l' = _b[c.mean_app#c.sd_app]
	local se_sucaNU`l' = _se[c.mean_app#c.sd_app]
	
	qui tab id if e(sample)
	local nobs_NU`l' = r(r)
}

foreach l of local levels {
	estimates stats NU`l'
	matrix temp = r(S)
	local NU_AIC`l' = temp[1,5]
}

/* STAMPA TABELLA COEFFICIENTS IN FORMATO LOG */

log using "Utility estimation\Output\Logs\uncertain_stata_COEFF_NU.log", replace

foreach l of local levels {
  di "`l' `basegammaNU`l'' `baseetaNU`l'' `gammaNU`l'' `etaNU`l'' `sucaNU`l''"
 }

log close
 
 
  /* STAMPA TABELLA STD DEV IN FORMATO LOG */
log using "Utility estimation\Output\Logs\uncertain_stata_SE_NU.log", replace

foreach l of local levels {
  di "`l' `se_basegammaNU`l'' `se_baseetaNU`l'' `se_gammaNU`l'' `se_etaNU`l'' `se_sucaNU`l''"
 } 

log close


/* STAMPA TABELLA AIC IN FORMATO LOG */ 

log using "Utility estimation\Output\Logs\uncertain_stata_AIC_NU.log", replace
 
foreach l of local levels {
  di "`l'" "`NU_AIC`l''"
 }

log close

 
*****************************************
*****************************************
** 2.2 Models for representative agent **
*****************************************
*****************************************
* overall model *
eststo clear
eststo :clogit a payoff, group(id) vce(rob)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(rob) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(rob)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(rob) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\overall_models.tex", label replace aic bic se

eststo model2 :clogit a payoff rho sigma, group(id) iter(50) collinear constraint(1) 
eststo model3 :clogit a payoff mean_app, group(id) iter(50) collinear
eststo model4 :clogit a payoff rho sigma mean_app, group(id) iter(50) collinear constraint(1)
suest model2 model3 model4 
test [model2_a]rho= [model4_a]rho
test [model3_a]mean_app= [model4_a]mean_app

/*
eststo clear
eststo :clogit a payoff, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(cluster paper_id) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(cluster paper_id)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster paper_id) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\overall_models_cluster.tex", label replace aic bic se 
*/

* models with norm uncertainty *
eststo clear
eststo :clogit a payoff mean_app sd_app, group(id) iter(50) vce(rob) collinear
eststo :clogit a payoff rho sigma mean_app sd_app, group(id) iter(50) vce(rob) collinear constraint(1)
eststo :clogit a payoff c.mean_app##c.sd_app, group(id) iter(50) vce(rob) collinear
eststo :clogit a payoff rho sigma c.mean_app##c.sd_app, group(id) iter(50) vce(rob) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\uncertainty_models.tex", label replace aic bic se 

eststo clear
eststo :clogit a payoff mean_app sd_app, group(id) iter(50) vce(cluster treatment_id) collinear
eststo :clogit a payoff rho sigma mean_app sd_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
eststo :clogit a payoff c.mean_app##c.sd_app, group(id) iter(50) vce(cluster treatment_id) collinear
eststo :clogit a payoff rho sigma c.mean_app##c.sd_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\uncertainty_models_cluster.tex", label replace aic bic se 

preserve
collapse (mean) mean_app (mean) sd_app, by(treatment_id scenarios)
pwcorr mean_app sd_app
restore

*eststo clear
*eststo :clogit a payoff mean_app, group(id) vce(bootstrap, strata(treatment_id))
*eststo :clogit a payoff mean_app sd_app, group(id) iter(50) vce(cluster treatment_id) collinear
*eststo :clogit a payoff rho sigma mean_app sd_app, group(id) iter(50) vce(bootstrap, strata(treatment_id)) collinear constraint(1)
*esttab using uncertainty_models.tex, label replace aic bic se 

*eststo :clogit a payoff mean_app, group(id) iter(50) collinear constraint(1)
*eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) collinear constraint(1)
*suest est5 est6, vce(cluster treatment_id)
*test [est6_a]mean_app= [est5_a]mean_app

*restore

quietly{
********* NOT SURE TO INCLUDE THIS **************
preserve
* ToG game *
drop if game_type != "ToG"

replace endowment = 5 if treatment_id == "2013Kru001_1b"
replace scenarios = scenarios - 5 if treatment_id == "2013Kru001_1b" | treatment_id == "2019Cha026_3"
replace choice = choice - 5 if treatment_id == "2013Kru001_1b" | treatment_id == "2019Cha026_3"
replace payoff = payoff + 5 if treatment_id == "2019Cha026_3"

* set constraint for social preferences models *
constraint 1 payoff = 1
constraint 2 rho = 0
constraint 3 sigma = rho
constraint 4 sigma = 0

levelsof treatment_id, local(levels)

* Charness Rabin 2002 *
gen r = payoff > endowment
gen s = payoff < endowment
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

* overall model *
eststo clear
eststo :clogit a payoff, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
esttab using take_or_give_overall_models.tex, label replace aic bic se 

restore

preserve
* Donation game *
drop if game_type != "Donation Game"

* set constraint for social preferences models *
constraint 1 payoff = 1
constraint 2 rho = 0
constraint 3 sigma = rho
constraint 4 sigma = 0

levelsof treatment_id, local(levels)

* Charness Rabin 2002 *
gen r = payoff > endowment/2
gen s = payoff < endowment/2
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

* overall model *
eststo clear
eststo :clogit a payoff, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
esttab using take_or_give_overall_models.tex, label replace aic bic se 
}
restore
