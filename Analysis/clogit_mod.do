cap prog drop myconditional_logit3
program myconditional_logit3
args todo b lnL
version 14
tempname a
tempvar  den xb p last somma
mleval `a' = `b', eq(1) scalar
mleval `xb' = `b', eq(2)
quietly{
by id : gen double `somma' = exp(`xb' + (`a')*$x2)
by id : egen double `den' = sum((`somma'))
gen double `p' = (`somma')/`den'
mlsum `lnL' = $ML_y1*log(`p') if $ML_y1==1
if (`todo'==0 | `lnL' > =.) exit

}
end
global x2 x2
ml model d0 myconditional_logit3 (a: ) (Eq1: choice = x1 , nocons)
ml maximize, iter(5)
