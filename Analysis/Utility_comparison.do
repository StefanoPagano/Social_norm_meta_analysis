set more off
* RUN ANDREA *

cd "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis"
import delimited "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\data_utility.csv", clear
log using "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\stata.log", replace

* RUN STEFANO *
/*cd "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis"
import delimited "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis/data_utility.csv", clear

log using /Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis/stata.log
*/
* DG *
preserve 
drop if game_type != "DG"

* Fehr and schmidt model *
* set constraint for IA *
constraint 1 payoff = 1
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

*local i = 1
foreach l of local levels {
	di "treatment -> `l' "
	
	/* Selfish */
	clogit a payoff if treatment_id == "`l'", group(id) vce(rob)
	est store S`l'
	local deltaS`l' = _b[payoff]
	local se_deltaS`l' = _se[payoff]
	
	/* Altruism */
	clogit a payoff alpha if treatment_id == "`l'", group(id) vce(rob) collinear constraint(1)
	est store A`l'
	local alphaA`l' = _b[alpha]
	local se_alphaA`l' = _se[alpha]

	/* Social expectation */
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(rob)
	est store N`l'
	local deltaN`l' = _b[payoff]
	local gammaN`l' = _b[mean_app]
	local se_deltaN`l' = _se[payoff]
	local se_gammaN`l' = _se[mean_app]
	
	/* Charness Rabin 2002 */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) vce(rob) collinear constraints(1)
	est store CR`l'
	local rhoCR`l' = _b[rho]
	local sigmaCR`l' = _b[sigma]
	local se_rhoCR`l' = _se[rho]
	local se_sigmaCR`l' = _se[sigma]
	*local ++i
}

foreach l of local levels {
estimates stats N`l' CR`l' A`l' S`l'
matrix temp = r(S)
local N_AIC`l' = temp[1,5]
local CR_AIC`l' = temp[2,5]
local A_AIC`l' = temp[3,5]
local S_AIC`l' = temp[4,5]
}

foreach l of local levels {
  di "`l', `deltaN`l'', `gammaN`l'', `se_deltaN`l'', `se_gammaN`l'', `rhoCR`l'', `sigmaCR`l'', `se_rhoCR`l'', `se_sigmaCR`l'', `alphaA`l'', `se_alphaA`l'', `deltaS`l'', `se_deltaS`l''"
 }

foreach l of local levels {
  di "`l', `N_AIC`l'', `CR_AIC`l'', `A_AIC`l'', `S_AIC`l''"
 }

 
 log close
* Norm model *
/*
foreach l of local levels {
	
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(cluster subject_id)
}
*/
