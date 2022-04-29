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

/*OUR VARIABLES*/

clear all

constraint 1 payoff = 1
import delimited "/Users/Stefano/Documents/GitHub/Social_norm_meta_analysis/Analysis/data_utility.csv", encoding(ISO-8859-1)
keep if treatment_id=="2019Cha026_1"

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
ml model d0 myconditional_logit3 (boh: ) (Eq1: a = payoff , nocons)
ml maximize, iter(5)
