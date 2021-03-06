clear all
set seed 157
set obs 2000
gen id = _n
local n_choices =5
expand `n_choices'
bys id : gen alternative = _n

gen x1 =  runiform(-2,2)
gen x2 =  runiform(-2,2)

matrix betas = (0.5 ,2)
global individuals = "id"

mata: 
// Calls from Stata the matrix "betas"
betas =st_matrix("betas")
// Generates a view of all attributes (M) x*
st_view(X = ., ., "x*")
// Generates a view individuals id 
st_view(panvar = ., ., st_global("individuals"))
// Extemely usefull utilities for panel data 
paninfo = panelsetup(panvar, 1)     
npanels = panelstats(paninfo)[1] 
   
// Looping over individuals (n)
for(n=1; n <= npanels; ++n) { 
    // Extract only the submatrix of individual n
    x_n = panelsubmatrix(X, n, paninfo) 
    
    // Linear utility
    util_n =betas :* x_n
    util_sum =rowsum(util_n) 
    U_exp = exp(util_sum)
    // Probability of each alternative
    p_i =     U_exp :/ colsum(U_exp)
    
    // Probability balance of alternatives
    cum_p_i =runningsum(p_i)    
    rand_draws = J(rows(x_n),1,uniform(1,1)) 
    pbb_balance = rand_draws:<cum_p_i
    cum_pbb_balance = runningsum(pbb_balance)
    choice_n = (cum_pbb_balance:== J(rows(x_n),1,1))
    
    // Storing each individual choice.
    if (n==1) Y =choice_n    
    else       Y = Y \ choice_n    
}
   
// Creates a new Stata variable called "choice"    
idx = st_addvar("float", "choice")
// save the content of Y on "choice" Stata variable
st_store(., idx, Y)
     
end
