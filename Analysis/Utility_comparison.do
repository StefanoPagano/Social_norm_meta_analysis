cd "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis"
import delimited C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\data_utility.csv, clear

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

*local i = 1
foreach l of local levels {
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) vce(cluster subject_id) collinear constraints(1)
	est store IA`l'
	
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(cluster subject_id)
	est store N`l'
	
	*local ++i
}

foreach l of local levels {
estimates stats IA`l' N`l'
}


* Norm model *
/*
foreach l of local levels {
	
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(cluster subject_id)
}
*/
