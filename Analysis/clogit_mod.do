/*TEST ANDREA*/
cap prog drop myconditional_logit3
program myconditional_logit3
args todo b lnL
version 14
tempname a
tempvar  den xb p last somma
mleval `a' = `b', eq(1) scalar
mleval `xb' = `b', eq(2)
quietly{
by id : gen double `somma' = exp(`xb' + (`a')*$ciao)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end
global ciao x2
ml model d0 myconditional_logit3 (a: ) (Eq1: choice = x1 , nocons)
ml maximize, iter(5)

/*OUR VARIABLES - KW*/
set more off
clear all

import delimited "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis/data_utility.csv", encoding(ISO-8859-1)

keep if treatment_id=="2013Kru001_1a"

sort id scenarios

cap prog drop myconditional_logit3
program myconditional_logit3
args todo b lnL
version 14
tempname boh
tempvar  den xb p last somma
mleval `boh' = `b', eq(1) scalar
mleval `xb' = `b', eq(2)
quietly{
by id : gen double `somma' = exp(`xb' + (`boh')*$norm)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end
global norm mean_app
ml model d0 myconditional_logit3 (norme: ) (Eq1: a = payoff , nocons)
ml maximize, iter(10)

/*OUR VARIABLES - CHARNESS*/
set more off
clear all

import delimited "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis/data_utility.csv", encoding(ISO-8859-1)

keep if treatment_id=="2016Kim003_7"
drop if subject_id=="2016Kim003_7_2222"

sort id scenarios

* Charness Rabin 2002 *
gen r = payoff > endowment/2
gen s = payoff < endowment/2
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

constraint 1 payoff = 1

cap prog drop myconditional_logit3
program myconditional_logit3
args todo b lnL
version 14
tempname boh
tempname boh2
tempvar  den xb p last somma
mleval `boh' = `b', eq(1) scalar
mleval `boh2' = `b', eq(2) scalar
mleval `xb' = `b', eq(3)

quietly{
by id : gen double `somma' = exp(`xb' + (`boh')*$rho + (`boh2')*$sigma)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end
global rho rho
global sigma sigma
ml model d0 myconditional_logit3 (boh: ) (boh2: ) (Eq1: a = payoff , nocons), collinear constraint(1)
ml maximize, iter(10)

clogit a payoff rho sigma, iter(50) group(id) collinear constraint(1)
