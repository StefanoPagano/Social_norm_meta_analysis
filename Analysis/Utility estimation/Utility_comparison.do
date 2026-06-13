set more off
clear all

*******************************
*******************************
** 1. Descriptive statistics **
*******************************
*******************************

** Import data
cd "C:\Users\andrea\OneDrive\Documents\github\Social_norm_meta_analysis\Analysis\Utility estimation"
import delimited "Data\new_data_utility2025-07-23.csv", clear

* DG *
drop if game_type != "DG"

preserve
tempfile temp

** adjust for those papers with action space as subset of total endowment **
** 2017Gac013 **
replace endowment = 4 if paper_id == "2017Gac013"
drop if paper_id=="2017Gac013" & scenarios > 4
** 2017Del037 **
replace scenarios = round(scenarios*endowment, 1) if paper_id == "2017Del037"

gen coop = scenarios/endowment

** smooth **
replace coop = round(coop,1) if paper_id == "2017Del037"

** sd app (uncertainty) normalization **
summ sd_app
local sd_max = r(max)
gen sd_app_norm = 2*(sd_app/`sd_max') - 1

set scheme lean1
collapse (mean) mean_app sd_app_norm, by(coop)
gen db = 2
save `temp' 
restore

* generate db variable for graph *
gen db = 1

** adjust for those papers with action space as subset of total endowment **
replace endowment = 4 if paper_id == "2017Gac013"
drop if paper_id=="2017Gac013" & scenarios > 4

** 2017Del037 **
replace scenarios = round(scenarios*endowment, 1) if paper_id == "2017Del037"

* generate cooperation variable *
gen coop = scenarios/endowment

** smooth **
replace coop = round(coop,1) if paper_id == "2017Del037"

append using `temp'
twoway (hist coop if a==1 & db==1, percent xtitle("% Endowment") yaxis(2) yscale(range(0) axis(1))) /// 
(line mean_app coop if db==2, yaxis(1)) ///
(line sd_app_norm coop if db==2, yaxis(1)), ///
	xtitle("% Endowment") ///
ytitle("Appropriateness / Uncertainty") yline(0) yscale(range(0) axis(2)) legend( pos(12) label (1 "Choices") label (2 "Appropriateness") label( 3 "Uncertainty") rows(1))
graph export "Output\Figures\hist_coop_mean_app.pdf", replace


* Table 1
tab treatment_id if a==1
***************************
***************************
** 2. Utility estimation **
***************************
***************************

clear all
cd "C:\Users\andrea\OneDrive\Documents\GitHub\Social_norm_meta_analysis\Analysis\"
import delimited "Utility estimation\Data\new_data_utility2025-07-23.csv", clear

* DG *
drop if game_type != "DG"

* set constraint for social preferences models *
constraint 1 payoff = 1
constraint 2 rho = 0
constraint 3 sigma = rho
constraint 4 sigma = 0

levelsof treatment_id, local(levels)

** adjust for specific papers **
replace payoff = endowment - round(endowment*scenario,1) if paper_id=="2017Del037"

* Charness Rabin 2002 *
gen r = payoff > endowment/2
gen s = payoff < endowment/2
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

** Adjustments for specific papers **
drop if paper_id == "2017Gac013" & scenarios >4

*******************************
*******************************
** 2.1 Paper-wise estimation **
*******************************
*******************************

capture mkdir "Utility estimation\Output\Data"

postfile results ///
	str40 treatment_id ///
	deltaS se_deltaS ///
	deltaN se_deltaN gammaN se_gammaN ///
	rhoDA se_rhoDA sigmaDA se_sigmaDA ///
	rhoFU se_rhoFU sigmaFU se_sigmaFU gammaFU se_gammaFU ///
	S_AIC N_AIC DA_AIC FU_AIC nobs ///
	using "Utility estimation\Output\Data\results_DG.dta", replace

foreach l of local levels {
	di "treatment -> `l'"

	/* S - Selfish */
	clogit a payoff if treatment_id == "`l'", group(id) iter(50) vce(rob)
	est store S`l'
	local deltaS    = _b[payoff]
	local se_deltaS = _se[payoff]

	/* N - Social Norm */
	clogit a payoff mean_app if treatment_id == "`l'", group(id) vce(rob)
	est store N`l'
	local deltaN    = _b[payoff]
	local gammaN    = _b[mean_app]
	local se_deltaN = _se[payoff]
	local se_gammaN = _se[mean_app]

	/* DA - Difference Averse */
	clogit a payoff rho sigma if treatment_id == "`l'", group(id) iter(200) vce(rob) collinear constraint(1)
	est store DA`l'
	local rhoDA      = _b[rho]
	local sigmaDA    = _b[sigma]
	local se_rhoDA   = _se[rho]
	local se_sigmaDA = _se[sigma]

	/* FU - Full model */
	clogit a payoff rho sigma mean_app if treatment_id == "`l'", group(id) iter(200) vce(rob) collinear constraint(1)
	est store FU`l'
	local rhoFU      = _b[rho]
	local sigmaFU    = _b[sigma]
	local gammaFU    = _b[mean_app]
	local se_rhoFU   = _se[rho]
	local se_sigmaFU = _se[sigma]
	local se_gammaFU = _se[mean_app]

	qui tab id if e(sample)
	local nobs = r(r)

	estimates stats N`l' DA`l' S`l' FU`l'
	matrix temp = r(S)
	local N_AIC  = temp[1,5]
	local DA_AIC = temp[2,5]
	local S_AIC  = temp[3,5]
	local FU_AIC = temp[4,5]

	post results ("`l'") ///
		(`deltaS') (`se_deltaS') ///
		(`deltaN') (`se_deltaN') (`gammaN') (`se_gammaN') ///
		(`rhoDA') (`se_rhoDA') (`sigmaDA') (`se_sigmaDA') ///
		(`rhoFU') (`se_rhoFU') (`sigmaFU') (`se_sigmaFU') (`gammaFU') (`se_gammaFU') ///
		(`S_AIC') (`N_AIC') (`DA_AIC') (`FU_AIC') (`nobs')
}

postclose results
tempfile maindata
save `maindata'
use "Utility estimation\Output\Data\results_DG.dta", clear
export delimited "Utility estimation\Output\Data\results_DG.csv", replace
use `maindata', clear

** Norm uncertainty models **

postfile results_nu ///
	str40 treatment_id ///
	basedeltaNU se_basedeltaNU basegammaNU se_basegammaNU baseetaNU se_baseetaNU ///
	baserhoNU se_baserhoNU basesigmaNU se_basesigmaNU ///
	deltaNU se_deltaNU gammaNU se_gammaNU etaNU se_etaNU nuNU se_nuNU ///
	rhoNU se_rhoNU sigmaNU se_sigmaNU ///
	baseNU_AIC NU_AIC nobs_NU ///
	using "Utility estimation\Output\Data\results_NU_DG.dta", replace

foreach l of local levels {
	di "treatment -> `l'"

	/* NU base - without interaction */
	clogit a payoff mean_app sd_app rho sigma if treatment_id == "`l'", group(id) iter(200) vce(rob) collinear constraint(1)
	est store baseNU`l'
	local basedeltaNU    = _b[payoff]
	local basegammaNU    = _b[mean_app]
	local baseetaNU      = _b[sd_app]
	local se_basedeltaNU = _se[payoff]
	local se_basegammaNU = _se[mean_app]
	local se_baseetaNU   = _se[sd_app]
	local baserhoNU      = _b[rho]
	local basesigmaNU    = _b[sigma]
	local se_baserhoNU   = _se[rho]
	local se_basesigmaNU = _se[sigma]
	estimates stats baseNU`l'
	matrix tempB = r(S)
	local baseNU_AIC = tempB[1,5]

	/* NU - with interaction */
	clogit a payoff c.mean_app##c.sd_app rho sigma if treatment_id == "`l'", group(id) iter(200) vce(rob) collinear constraint(1)
	est store NU`l'
	local deltaNU    = _b[payoff]
	local gammaNU    = _b[mean_app]
	local etaNU      = _b[sd_app]
	local nuNU       = _b[c.mean_app#c.sd_app]
	local se_deltaNU = _se[payoff]
	local se_gammaNU = _se[mean_app]
	local se_etaNU   = _se[sd_app]
	local se_nuNU    = _se[c.mean_app#c.sd_app]
	local rhoNU      = _b[rho]
	local sigmaNU    = _b[sigma]
	local se_rhoNU   = _se[rho]
	local se_sigmaNU = _se[sigma]

	qui tab id if e(sample)
	local nobs_NU = r(r)

	estimates stats NU`l'
	matrix temp = r(S)
	local NU_AIC = temp[1,5]

	post results_nu ("`l'") ///
		(`basedeltaNU') (`se_basedeltaNU') (`basegammaNU') (`se_basegammaNU') (`baseetaNU') (`se_baseetaNU') ///
		(`baserhoNU') (`se_baserhoNU') (`basesigmaNU') (`se_basesigmaNU') ///
		(`deltaNU') (`se_deltaNU') (`gammaNU') (`se_gammaNU') (`etaNU') (`se_etaNU') (`nuNU') (`se_nuNU') ///
		(`rhoNU') (`se_rhoNU') (`sigmaNU') (`se_sigmaNU') ///
		(`baseNU_AIC') (`NU_AIC') (`nobs_NU')
}

postclose results_nu
use "Utility estimation\Output\Data\results_NU_DG.dta", clear
export delimited "Utility estimation\Output\Data\results_NU_DG.csv", replace
use `maindata', clear

 
*****************************************
*****************************************
**2 2.2 Models for representative agent **
*****************************************
*****************************************
* overall model *
eststo clear
eststo :clogit a payoff, group(id) vce(rob)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(rob) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(rob)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(rob) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\overall_models.tex", label replace aic bic se

eststo model2 :clogit a payoff rho sigma, group(id) iter(50) collinear constraint(1) 
eststo model3 :clogit a payoff mean_app, group(id) iter(50) collinear
eststo model4 :clogit a payoff rho sigma mean_app, group(id) iter(50) collinear constraint(1)
suest model2 model3 model4 
** comparing coefficients **
// measure how much of the weight previously attributed to rho and sigma is captured by delta
test [model2_a]rho= [model4_a]rho
test [model2_a]sigma= [model4_a]sigma

/*
eststo clear
eststo :clogit a payoff, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(cluster paper_id) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(cluster paper_id)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster paper_id) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\overall_models_cluster.tex", label replace aic bic se 
*/

* models with norm uncertainty *
eststo clear
eststo :clogit a payoff mean_app sd_app, group(id) iter(50) vce(rob) collinear
eststo :clogit a payoff rho sigma mean_app sd_app, group(id) iter(50) vce(rob) collinear constraint(1)
eststo :clogit a payoff c.mean_app##c.sd_app, group(id) iter(50) vce(rob) collinear
eststo :clogit a payoff rho sigma c.mean_app##c.sd_app, group(id) iter(50) vce(rob) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\uncertainty_models.tex", label replace aic bic se

eststo clear
eststo :clogit a payoff mean_app sd_app, group(id) iter(50) vce(cluster treatment_id) collinear
eststo :clogit a payoff rho sigma mean_app sd_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
eststo :clogit a payoff c.mean_app##c.sd_app, group(id) iter(50) vce(cluster treatment_id) collinear
eststo :clogit a payoff rho sigma c.mean_app##c.sd_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
esttab using "Utility estimation\Output\Tables\uncertainty_models_cluster.tex", label replace aic bic se

preserve
collapse (mean) mean_app (mean) sd_app, by(treatment_id scenarios)
pwcorr mean_app sd_app
restore

** Linear Regressions on donations
** Comments: when analyzing behavior using simple regressions
preserve	
keep if a == 1
gen perc_e = scenarios / endowment
gen high = 0
replace high = 1 if perc_e>0.5

tobit perc_e mean_app, vce(cluster treatment_id ) ul(1) ll(0)
tobit perc_e sd_app, vce(cluster treatment_id ) ul(1) ll(0)
tobit perc_e mean_app sd_app if perc_e <=0.5, vce(cluster treatment_id ) ul(0.5) ll(0)
tobit perc_e mean_app sd_app if perc_e >0.5, vce(cluster treatment_id ) ul(1) ll(0.5)

*tobit perc_e c.mean_app##c.st_app, vce(cluster treatment_id ) ul(1) ll(0)
*tobit perc_e c.mean_app##c.st_app if perc_e <=0.5, vce(cluster treatment_id ) ll(0) ul(0.5)
*tobit perc_e c.mean_app##c.st_app if perc_e >0.5, vce(cluster treatment_id ) ll(0.5) ul(1)
*metobit perc_e mean_app if perc_e <=0.5 ||treatment_id:
*metobit perc_e mean_app st_app if perc_e <=0.5 ||treatment_id:
*metobit perc_e c.mean_app##c.st_app if perc_e <=0.5 ||treatment_id:, ul(0.5) ll(0)


gen zero=0
replace zero = 1 if perc_e == 0
logit zero mean_app, vce(cluster treatment_id)
logit zero mean_app sd_app, vce(cluster treatment_id)
logit zero c.mean_app##c.sd_app, vce(cluster treatment_id)

gen half=0
replace half = 1 if perc_e > 0
logit half mean_app, vce(cluster treatment_id)
logit half mean_app sd_app, vce(cluster treatment_id)
logit half c.mean_app##c.sd_app, vce(cluster treatment_id)

restore
*eststo clear
*eststo :clogit a payoff mean_app, group(id) vce(bootstrap, strata(treatment_id))
*eststo :clogit a payoff mean_app st_app, group(id) iter(50) vce(cluster treatment_id) collinear
*eststo :clogit a payoff rho sigma mean_app st_app, group(id) iter(50) vce(bootstrap, strata(treatment_id)) collinear constraint(1)
*esttab using uncertainty_models.tex, label replace aic bic se 

*eststo :clogit a payoff mean_app, group(id) iter(50) collinear constraint(1)
*eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) collinear constraint(1)
*suest est5 est6, vce(cluster treatment_id)
*test [est6_a]mean_app= [est5_a]mean_app

*restore


********* NOT SURE TO INCLUDE THIS **************
preserve
* ToG game *
drop if game_type != "ToG"

replace endowment = 5 if treatment_id == "2013Kru001_1b"
replace scenarios = scenarios - 5 if treatment_id == "2013Kru001_1b" | treatment_id == "2019Cha026_3"
replace choice = choice - 5 if treatment_id == "2013Kru001_1b" | treatment_id == "2019Cha026_3"
replace payoff = payoff + 5 if treatment_id == "2019Cha026_3"

* set constraint for social preferences models *
constraint 1 payoff = 1
constraint 2 rho = 0
constraint 3 sigma = rho
constraint 4 sigma = 0

levelsof treatment_id, local(levels)

* Charness Rabin 2002 *
gen r = payoff > endowment
gen s = payoff < endowment
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

* overall model *
eststo clear
eststo :clogit a payoff, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
esttab using take_or_give_overall_models.tex, label replace aic bic se 

restore

preserve
* Donation game *
drop if game_type != "Donation Game"

* set constraint for social preferences models *
constraint 1 payoff = 1
constraint 2 rho = 0
constraint 3 sigma = rho
constraint 4 sigma = 0

levelsof treatment_id, local(levels)

* Charness Rabin 2002 *
gen r = payoff > endowment/2
gen s = payoff < endowment/2
gen rho = endowment*r-2*payoff*r
gen sigma = endowment*s-2*payoff*s
gen alpha = endowment - 2*payoff

* overall model *
eststo clear
eststo :clogit a payoff, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1) 
eststo :clogit a payoff mean_app, group(id) vce(cluster treatment_id)
eststo :clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)
esttab using take_or_give_overall_models.tex, label replace aic bic se 
}
restore
*/
