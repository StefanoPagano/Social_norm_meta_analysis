** this program allows to put non-linear constraints to clogit function **

** first part allows to estimate KW model by adding a restriction to gamma (g>0)

** second part allows to estimate CR model with several restrictions:
          - 
/*Basic clogit*/
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

import delimited "data_utility.csv", encoding(ISO-8859-1)

keep if treatment_id=="2013Kru001_1a"

sort id scenarios

cap prog drop KW_gamma_positive
program KW_gamma_positive
args todo b lnL
version 14
tempname gamma
tempvar  den xb p last somma
mleval `gamma' = `b', eq(1) scalar
mleval `xb' = `b', eq(2)
quietly{
by id : gen double `somma' = exp(`xb' + (`gamma')*$norm)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end
global norm mean_app
ml model d0 KW_gamma_positive (norme: ) (Eq1: a = payoff , nocons)
ml maximize, iter(10)

/*OUR VARIABLES - CHARNESS*/
set more off
clear all

import delimited "data_utility.csv", encoding(ISO-8859-1)

keep if treatment_id=="2007Lis165_1a"
drop if subject_id=="2016Kim003_7_2222"

sort id scenarios

* Charness Rabin 2002 *
gen r = payoff > endowment/2
gen s = payoff < endowment/2
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

constraint 1 payoff = 1

cap prog drop difference_averse
program difference_averse
args todo b lnL
version 14
tempname R S
tempvar  den xb p last somma
mleval `R' = `b', eq(1) scalar
mleval `S' = `b', eq(2) scalar
mleval `xb' = `b', eq(3)

quietly{
by id : gen double `somma' = exp(`xb' + invlogit(`R')*$rho - exp(`S')*$sigma)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end
global rho rho
global sigma sigma
ml model d0 difference_averse (R: ) (S: ) (Eq1: a = payoff , nocons), collinear constraint(1)
ml maximize, iter(10)


cap prog drop competitive
program competitive
args todo b lnL
version 14
tempname R S
tempvar  den xb p last somma
mleval `R' = `b', eq(1) scalar
mleval `S' = `b', eq(2) scalar
*mleval `diff' = `R' - `S'
mleval `xb' = `b', eq(3)

quietly{
by id : gen double `somma' = exp(`xb' +(exp(`S') - exp(`R'-`S'))*$rho -exp(`S')*$sigma)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end
global rho rho
global sigma sigma
ml model d0 competitive (R: ) (S: ) (Eq1: a = payoff , nocons), collinear constraint(1)
ml maximize, iter(30)

nlcom -exp([R]_cons)
/*clogit a payoff rho sigma, iter(50) group(id) collinear constraint(1)*/
