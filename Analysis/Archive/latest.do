clear all

* KW with gamma positive * 

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

* CR for difference averse * 
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

* CR for competitive *
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
by id : gen double `somma' = exp(`xb' -(exp(`R'-`S')+ exp(`S'))*$rho -exp(`S')*$sigma)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end


*****

* Estimate KW norm follower
import delimited "data_utility.csv", encoding(ISO-8859-1)
keep if game_type=="DG"
levelsof treatment_id, local(levels)

** treatment = 2007Lis165_1a
foreach l of local levels {
preserve
keep if treatment_id=="`l'"
sort id scenarios
global norm mean_app
ml model d0 KW_gamma_positive (norme: ) (Eq1: a = payoff , nocons)
ml maximize, iter(10)
est sto N_`l'
est stat
restore
}

* Estimate CR difference averse
gen r = payoff > endowment/2
gen s = payoff < endowment/2
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

constraint 1 payoff = 1

foreach l of local levels {
preserve
keep if treatment_id=="`l'"
sort id scenarios
global rho rho
global sigma sigma
ml model d0 competitive (R: ) (S: ) (Eq1: a = payoff , nocons), collinear constraint(1)
ml maximize, iter(30)
*global norm mean_app
*ml model d0 difference_averse (norme: ) (Eq1: a = payoff , nocons)
*ml maximize, iter(10)
est sto N_`l'
est stat
restore
}
