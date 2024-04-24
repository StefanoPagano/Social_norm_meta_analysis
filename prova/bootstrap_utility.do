* create stata readable files from raw dataset

* ISSUE : 2016Kim003_7 has no beliefs data for odd numbered scenarios (interpolation). Find why in the originating codes. check lines 467 of individual_between.R

clear all
*cd "../"
*import delimited using "../File_DB/Output/Treatment.csv", clear
*gen treatment_id = paperid + "_" + treatmentcode

import delimited using "../prova/Subjects_beliefs.csv", clear
keep if treatment_id == "2007Lis165_1a" | treatment_id == "2007Lis165_1b" | treatment_id == "2012Laz164_3" | treatment_id == "2013Kru001_1a" | treatment_id == "2013Kru001_1b" |  treatment_id == "2018Her061_9" | treatment_id == "2019Cha026_1" | treatment_id == "2019Cha026_3" | treatment_id == "2020And089_1" | treatment_id == "2020And089_2" | treatment_id == "2020Bas115_2a" | treatment_id == "2016Kim003_7"
tempfile beliefs
save `beliefs'

import delimited using "../File_DB/Output/Subjects_choices.csv", clear
keep if treatment_id == "2007Lis165_1a" | treatment_id == "2007Lis165_1b" | treatment_id == "2012Laz164_3" | treatment_id == "2013Kru001_1a" | treatment_id == "2013Kru001_1b" |  treatment_id == "2018Her061_9" | treatment_id == "2019Cha026_1" | treatment_id == "2019Cha026_3" | treatment_id == "2020And089_1" | treatment_id == "2020And089_2" | treatment_id == "2020Bas115_2a" | treatment_id == "2016Kim003_7"
tempfile choices
save `choices'

mat v = J(200,3,0)
mat j = J(200,3,0)
mat k = J(200,3,0)

forvalues i = 1(1)200{

* create table of beliefs aggregated by paper for all papers (between-subjects and within-subjects)
use `beliefs', clear
sample 80, by(treatment_id)
collapse (mean) mean_app=kw_normative (sd) sd_app=kw_normative, by(treatment_id scenarios)
tempfile beliefs2
save `beliefs2' 

* create comprehensive table with choices and beliefs
use `choices', clear
merge m:m treatment_id scenarios using `beliefs2', keep(match master)

keep if treatment_id == "2007Lis165_1a" | treatment_id == "2007Lis165_1b" | treatment_id == "2012Laz164_3" | treatment_id == "2013Kru001_1a" | treatment_id == "2013Kru001_1b" | treatment_id == "2016Kim003_7" | treatment_id == "2018Her061_9" | treatment_id == "2019Cha026_1" | treatment_id == "2019Cha026_3" | treatment_id == "2020And089_1" | treatment_id == "2020And089_2" | treatment_id == "2020Bas115_2a"
tempfile final_data
save `final_data'

* DG *
*preserve 
drop if game_type != "DG"
gen payoff = endowment - scenarios
egen id = group(subject_id)

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
    
	* FULL MODEL *
    clogit a payoff rho sigma mean_app sd_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)

    mat v[`i',1] = _b[sd_app]

	mat v[`i',2] = _se[sd_app]
	
	matrix t = r(table)
	mat v[`i',3] = t[4,5]
	
	* Norm plus uncertain MODEL *
    clogit a payoff mean_app sd_app, group(id) iter(50) vce(cluster treatment_id) collinear 

    mat j[`i',1] = _b[sd_app]

	mat j[`i',2] = _se[sd_app]
	
	matrix t = r(table)
	mat j[`i',3] = t[4,3]

	* Norm model *
    clogit a payoff rho sigma mean_app, group(id) iter(50) vce(cluster treatment_id) collinear constraint(1)

    mat k[`i',1] = _b[mean_app]

	mat k[`i',2] = _se[mean_app]
	
	matrix t = r(table)
	mat k[`i',3] = t[4,4]

}

log using "../prova/output.log", replace
matlist v
matlist j
matlist k
log close
