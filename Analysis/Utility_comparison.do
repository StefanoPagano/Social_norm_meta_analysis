
* RUN ANDREA *
/*
cd "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis"
import delimited "C:\Users\aguido\Documents\GitHub\Social_norm_meta_analysis\Analysis\data_utility.csv", clear
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
	di "treatment -> `l' "
	
	/* Social expectation */
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(bootstrap, reps(500))
	est store N`l'
	
	/* Charness Rabin 2002 */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) vce(bootstrap, reps(500)) collinear constraints(1)
	est store CR`l'
	
	*local ++i
}

foreach l of local levels {
estimates stats N`l' CR`l'
}


* Norm model *
/*
foreach l of local levels {
	
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(cluster subject_id)
}
*/
