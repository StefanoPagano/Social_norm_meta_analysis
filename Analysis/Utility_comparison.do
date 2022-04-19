set more off
* RUN ANDREA *
/*
cd "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis"
import delimited "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\data_utility.csv", clear
log using "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\stata.log", replace
*/

* RUN STEFANO *
cd "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis"
import delimited "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis/data_utility.csv", clear

* DG *
preserve 
drop if game_type != "DG"

* Fehr and schmidt model *
* set constraint for IA *
constraint 1 payoff = 1
constraint 2 rho = 0
constraint 3 sigma <= rho <= 0
constraint 4 sigma = 0
constraint 5 sigma < 0 < rho < 1
gen sqr_other_payoff_ahead = other_payoff_ahead^2
* run loop *
levelsof treatment_id, local(levels)
/*
foreach l of local levels {
	clogit a payoff other_payoff_ahead other_payoff_behind if treatment_id == "`l'", group(id) vce(cluster subject_id) collinear constraints(1)
}
 */

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
	
	/* Selfish */
	clogit a payoff if treatment_id == "`l'", group(id) iter(50) vce(rob)
	est store S`l'
	local deltaS`l' = _b[payoff]
	local se_deltaS`l' = _se[payoff]
	
	/* Altruism */
	clogit a payoff alpha if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraint(1)
	est store A`l'
	local alphaA`l' = _b[alpha]
	local se_alphaA`l' = _se[alpha]

	/* Social expectation - Norm */
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(rob)
	est store N`l'
	local deltaN`l' = _b[payoff]
	local gammaN`l' = _b[mean_app]
	local se_deltaN`l' = _se[payoff]
	local se_gammaN`l' = _se[mean_app]
	
	/* Behindness averse */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraints(1 2)
	est store BA`l'
	local rhoBA`l' = _b[rho]
	local sigmaBA`l' = _b[sigma]
	local se_rhoBA`l' = _se[rho]
	local se_sigmaBA`l' = _se[sigma]
	
	/* Charity prone */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraints(1 4)
	est store CP`l'
	local rhoCP`l' = _b[rho]
	local sigmaCP`l' = _b[sigma]
	local se_rhoCP`l' = _se[rho]
	local se_sigmaCP`l' = _se[sigma]
	
	/* Competitive */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraints(1 3)
	est store CO`l'
	local rhoCO`l' = _b[rho]
	local sigmaCO`l' = _b[sigma]
	local se_rhoCO`l' = _se[rho]
	local se_sigmaCO`l' = _se[sigma]

	/* Difference averse */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(50) vce(rob) collinear constraint(1 5)
	est store DA`l'
	local rhoDA`l' = _b[rho]
	local sigmaDA`l' = _b[sigma]
	local se_rhoDA`l' = _se[rho]
	local se_sigmaDA`l' = _se[sigma]
	
}

foreach l of local levels {
estimates stats N`l' CO`l' A`l' BA`l' CP`l' DA`l' S`l' 
matrix temp = r(S)
local N_AIC`l' = temp[1,5]
local CO_AIC`l' = temp[2,5]
local A_AIC`l' = temp[3,5]
local BA_AIC`l' = temp[4,5]
local CP_AIC`l' = temp[5,5]
local DA_AIC`l' = temp[6,5]
local S_AIC`l' = temp[7,5]
}

log close


/* STAMPA TABELLA COEFFICIENTS IN FORMATO LOG */

log using stata_COEFF_ONG.log, replace

foreach l of local levels {
  di "`l' `deltaS`l'' `alphaA`l'' `deltaN`l'' `gammaN`l'' `rhoBA`l'' `sigmaBA`l'' `rhoCP`l'' `sigmaCP`l'' `rhoCO`l'' `sigmaCO`l'' `rhoDA`l'' `sigmaDA`l''" 
 }

log close
 
 
  /* STAMPA TABELLA STD DEV IN FORMATO LOG */ 
log using stata_SE_ONG.log, replace

foreach l of local levels {
  di "`l' `se_deltaS`l'' `se_alphaA`l'' `se_deltaN`l'' `se_gammaN`l'' `se_rhoBA`l'' `se_sigmaBA`l'' `se_rhoCP`l'' `se_sigmaCP`l'' `se_rhoCO`l'' `se_sigmaCO`l'' `se_rhoDA`l'' `se_sigmaDA`l'' "
 } 

log close


/* STAMPA TABELLA AIC IN FORMATO LOG */ 

log using stata_AIC_ONG.log, replace
 
foreach l of local levels {
  di "`l' `N_AIC`l'' `CO_AIC`l'' `A_AIC`l'' `BA_AIC`l'' `CP_AIC`l'' `DA_AIC`l'' `S_AIC`l''"
 }

 
 log close
 
 
* Norm model *
/*
foreach l of local levels {
	
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(cluster subject_id)
}
*/
