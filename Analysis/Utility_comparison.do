set more off
* RUN ANDREA *
clear all
cd "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis"
import delimited "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\data_utility.csv", clear
*log using "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\stata.log", replace

/*
* RUN STEFANO *
cd "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis"
import delimited "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis/data_utility.csv", clear
*/

* DG *
*preserve 
drop if game_type != "DG"
*drop if game_type != "ToG"
* set constraint for IA *
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

log using stata_MODELS_DG.log, replace

*local i = 1
foreach l of local levels {
	di "treatment -> `l' "
	
	/* S - Selfish */
	clogit a payoff if treatment_id == "`l'", group(id) iter(50) vce(rob)
	est store S`l'
	local deltaS`l' = _b[payoff]
	local se_deltaS`l' = _se[payoff]
	
	/* Altruism
	clogit a payoff alpha if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraint(1)
	est store A`l'
	local alphaA`l' = _b[alpha]
	local se_alphaA`l' = _se[alpha]*/

	/* N - Social expectation - Norm */
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(rob)
	est store N`l'
	local deltaN`l' = _b[payoff]
	local gammaN`l' = _b[mean_app]
	local se_deltaN`l' = _se[payoff]
	local se_gammaN`l' = _se[mean_app]
	
	/* BA - Behindness averse */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraints(1 2)
	est store BA`l'
	local rhoBA`l' = _b[rho]
	local sigmaBA`l' = _b[sigma]
	local se_rhoBA`l' = _se[rho]
	local se_sigmaBA`l' = _se[sigma]
	
	/* CP - Charity prone */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraints(1 4)
	est store CP`l'
	local rhoCP`l' = _b[rho]
	local sigmaCP`l' = _b[sigma]
	local se_rhoCP`l' = _se[rho]
	local se_sigmaCP`l' = _se[sigma]
	
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
estimates stats N`l' BA`l' CP`l' DA`l' S`l' FU`l'
matrix temp = r(S)
local N_AIC`l' = temp[1,5]
local BA_AIC`l' = temp[2,5]
local CP_AIC`l' = temp[3,5]
local DA_AIC`l' = temp[4,5]
local S_AIC`l' = temp[5,5]
local FU_AIC`l' = temp[6,5]
}

log close


/*
/* STAMPA TABELLA COEFFICIENTS IN FORMATO LOG */

log using stata_COEFF_ONG.log, replace

foreach l of local levels {
  di "`l' `deltaS`l'' `deltaN`l'' `gammaN`l'' `sigmaBA`l'' `rhoCP`l'' `rhoDA`l'' `sigmaDA`l'' `rhoFU`l'' `sigmaFU`l'' `gammaFU`l''" 
 }

log close
 
 
  /* STAMPA TABELLA STD DEV IN FORMATO LOG */
log using stata_SE_ONG.log, replace

foreach l of local levels {
  di "`l' `se_deltaS`l'' `se_deltaN`l'' `se_gammaN`l'' `se_sigmaBA`l'' `se_rhoCP`l'' `se_rhoDA`l'' `se_sigmaDA`l'' `se_rhoFU`l'' `se_sigmaFU`l'' `se_gammaFU`l''"
 } 

log close


/* STAMPA TABELLA AIC IN FORMATO LOG */ 

log using stata_AIC_ONG.log, replace
 
foreach l of local levels {
  di "`l' `S_AIC`l'' `N_AIC`l'' `BA_AIC`l'' `CP_AIC`l'' `DA_AIC`l'' `FU_AIC`l''"
 }

 
log close

log using stata_Nobs.log, replace
 
foreach l of local levels {
  di "`l' `nobs_FU`l''"
 }

log close
*/ 
* overall model *
eststo clear
eststo :clogit a payoff, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
esttab using overall_models.tex, label replace aic bic se 

*eststo :clogit a payoff mean_app, group(id) iter(50) collinear constraint(1)
*eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) collinear constraint(1)
*suest est5 est6, vce(cluster treatment_id)
*test [est6_a]mean_app= [est5_a]mean_app
