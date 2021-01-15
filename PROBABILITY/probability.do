
*************************************************************
/*** Chapter 5: Probability  **/
*************************************************************

*************************************************************
/*** Clear memory ***/
*************************************************************
clear all

*************************************************************
*************************************************************
/*** 5.1 Probability ***/
*************************************************************
*************************************************************

*************************************************************
/** 5.1.1 Frequentist versus Bayesian **/
*************************************************************

*************************************************************
/** 5.1.2 Definition and Axioms **/
*************************************************************

*************************************************************
/** 5.1.3 Permutations **/
*************************************************************
* create data set with 50 observations
clear
set obs 50
generate k = _n

* calculate the log denominator and numerator
generate logdenom = k * log(365) + lnfactorial(365 - k)
generate lognumer = lnfactorial(365)

* probability that at least two have the same birthday
generate pr = 1 - exp(lognumer - logdenom)

* plot probability
scatter pr k, yline(.5) xtitle("Number of people") ///
	ytitle("Probability that at least two" "people have the same birthday") ///
	msymbol(oh)

*************************************************************
/** 5.1.4 Sampling with and without Replacement **/
*************************************************************
clear
* create data set and number observations
set obs 365 
generate day = _n 
* set seed to ensure replicability
set seed 12345

* specify the number of people to sample and simulations
local k = 23
local sims = 1000
* create event counter 
local event = 0

* loop through simulations
forvalues i = 1 / `sims' {
	preserve
		* draw k samples
			bsample `k'
		* count unique values drawn, saved in r(r)
			quietly tabulate day
		* count if event occurred using if command
			* if there are duplicates, the number of unique birthdays
			* will be less than the number of birthdays, which is k
			if r(r) < `k' {
				local event = `event' + 1	
			}
	restore
}

* fraction of trials where at least two birthdays are the same
display `event' / `sims'

*************************************************************
/** 5.1.5 Combinations **/
*************************************************************
display %12.0f comb(84,6)

*************************************************************
*************************************************************
/*** 5.2 Conditional Probability ***/
*************************************************************
*************************************************************

*************************************************************
/** 5.2.1 Conditional, Marginal, and Joint Probabilities **/
*************************************************************
cd probability
use FLvoters, clear
drop if missing(race, gender, age)
describe, short

tabulate race
tabulate gender

tabulate race gender, column nofreq
tabulate race gender, cell nofreq
  
generate agegroup = .
	replace agegroup = 1 if age <= 20
	replace agegroup = 2 if age > 20 & age <= 40
	replace agegroup = 3 if age > 40 & age <= 60
	replace agegroup = 4 if age > 60 & age < .

egen genderrace = group(gender race), label 
tabulate genderrace agegroup, cell nofreq

count if race=="black" & gender=="f" & agegroup == 4

* joint probability of black female over 60
count if race == "black" & gender == "f" & agegroup == 4
scalar blackfem60 = r(N)
display blackfem60 / _N

* marginal probability of being over 60
count if agegroup == 4
scalar age60 = r(N)
display age60 / _N

* P(black and female | above 60)
display blackfem60 / age60

* two-way joint probability table for age group and gender
tabulate agegroup gender, cell nofreq
count if agegroup == 4 & gender == "f"
scalar fem60 = r(N)

* P(above 60 and female)
display fem60 / _N
* P(black | female and above 60)
display blackfem60 / fem60 

*************************************************************
/** 5.2.2 Independence **/
*************************************************************
estpost tabulate race gender
matrix list e(pct)

* transpose cell percentages
matrix pct = e(pct)'
* show transposed matrix to demonstrate
matrix list pct
* joint probability of being female, by race
matrix joint_rf = pct[1..6,1] / 100
* marginal probability of being female
scalar margin_f = pct[7,1] / 100

* transpose column percentages
matrix colpct = e(colpct)'
* marginal probability of race
matrix margin_r = colpct[15..20,1] / 100

* calculate P(race) * P(female)
matrix product_rf = margin_r * margin_f
* combine matrices
matrix joint_marg = joint_rf, product_rf

* save columns as variables
svmat joint_marg

* scatter plot
scatter joint_marg1 joint_marg2, msymbol(Oh) || function y=x, range(0 .4) ///
 	xtitle("P(race) * P(female)") ytitle("P(race and female)") ///
	legend(off) 

* joint probability of being 60+ female
quietly estpost tabulate agegroup gender 
matrix pct2 = e(pct)'
scalar joint_f60 = pct2[4,1] / 100

* marginal probability for 60+
scalar margin_60 = pct2[14,1]

* joint probability for 60+ female and race
quietly estpost tabulate genderrace agegroup
matrix pct3 = e(pct)'
matrix joint_60rf = pct3[40..45,1] / 100

* joint independence
matrix j_product_60rf = (product_rf * margin_60) / 100
matrix joint_ind = joint_60rf, j_product_60rf
svmat joint_ind

* conditional independence, given female
matrix cond_60rf =  joint_60rf / margin_f 
matrix c_product_60rf =(joint_rf / margin_f) * (joint_f60 / margin_f)
matrix cond_ind = cond_60rf, c_product_60rf
svmat cond_ind

scatter joint_ind2 joint_ind1, msymbol(Oh) || function y=x, range(0 .3) ///
	 xtitle("P(race and above 60 and female)") ///
	 ytitle("P(race) * P(above 60) * P(female)") ///
	 title("Joint independence") legend(off) ///
	 name(joint_ind, replace)
scatter cond_ind2 cond_ind1, msymbol(Oh) || function y=x, range(0 .3) ///
	xtitle("P(race and above 60 | female)") ///
	ytitle("P(race | female) * P(above 60 | female)") ///
	 title("Marginal independence") legend(off) ///
	 name(cond_ind, replace)
graph combine joint_ind cond_ind

clear
set obs 1000
* define locals for door options and Monty's choice
local doors "goat goat car"
local monty = "goat"
* create variables to store switch decision
generate noswitch = ""
generate switch = ""

forvalues i = 1/ `=_N' {
	* randomly choose the initial door
	    local first = runiformint(1, 3)
	* identify selection from doors local for each observation
	    quietly replace noswitch = "`: word `first' of `doors''" if _n == `i'
	* store selected door in nos local for each observation
	    quietly local nos = noswitch[`i']
	* remove selected door from doors local, save revised list as remain
	    quietly local remain : list doors - nos
	* Monty chooses one door with a goat, remove "goat" from remain local
	    quietly replace switch = "`: list remain - monty'" if _n == `i'
}
tabulate noswitch
tabulate switch

*************************************************************
/** 5.2.3 Bayes’ Rule **/
*************************************************************

*************************************************************
/** 5.2.4 Predicting Race Using Surname and Residence Location **/
*************************************************************
use names, clear
describe, short

merge 1:m surname using FLVoters, keep(3)
drop _merge
drop if missing(race)

replace race = "api" if race == "asian" 
replace race = "others" if race =="other" | race == "native"
egen rmax = rowmax(pctwhite - pctothers)

levelsof race, local(race_cat) clean
generate correct = 0
foreach v of local race_cat  {
	replace correct = 1 if race == "`v'" & pct`v' == rmax
}
tabulate correct if race == "white"

summarize correct if race == "white"

* black
tabulate correct if race == "black"
* Hispanic
tabulate correct if race == "hispanic"
* Asian
tabulate correct if race == "api"

* whites false discovery rate
tabulate correct if pctwhite == rmax

* black false discovery rate
tabulate correct if pctblack == rmax

* Hispanic false discovery rate
tabulate correct if pcthispanic == rmax

* Asian false discovery rate
tabulate correct if pctapi == rmax

save FL, replace

use FLCensusVTD, clear
* compute race proportions in Florida
quietly estpost summarize white black api hispanic others [fweight = totalpop]
matrix raceprop = e(mean)'
matrix list raceprop

save FLCensus, replace

use names, clear
summarize count 
scalar tot = r(sum)

* P(surname | race) = P(race | surname) * P(surname) / P(race)
generate namewhite = (pctwhite / 100) * (count / tot) / matrix(raceprop[1,1])
generate nameblack = (pctblack / 100) * (count / tot) / matrix(raceprop[2,1])
generate nameapi = (pctapi / 100) * (count / tot) / matrix(raceprop[3,1])
generate namehispanic = (pcthispanic / 100) * (count / tot) / matrix(raceprop[4,1])
generate nameothers = (pctothers / 100) * (count / tot) / matrix(raceprop[5,1])
keep surname name*
merge 1:m surname using FL
drop if missing(race)
drop _merge

merge m:1 county vtd using FLCensus, keep(3)
drop _merge
generate nameresidence = (namewhite * white) + (nameblack * black) + ///
    (namehispanic * hispanic) + (nameapi * api) + (nameothers * others)

generate prewhite = namewhite * white / nameresidence
generate preblack = nameblack * black / nameresidence
generate prehispanic = namehispanic * hispanic / nameresidence
generate preapi = nameapi * api / nameresidence
generate preothers = nameothers * others / nameresidence

egen pre_rmax = rowmax(prewhite - preothers)
replace race = "api" if race == "asian" 
replace race = "others" if race =="other" | race== "native"

levelsof race, local(race_cat) clean
generate pre_correct = 0
foreach v of local race_cat {
	replace pre_correct = 1 if race=="`v'" & pre`v' == pre_rmax
}

* white
tabulate pre_correct if race == "white"
* blacks
tabulate pre_correct if race == "black"
* Hispanic
tabulate pre_correct if race == "hispanic"
* Asian
tabulate pre_correct if race == "api"

* proportion of blacks among those with surname "White"
summarize pctblack if surname == "WHITE"
* predicted probability of being black given residence location
summarize preblack if surname == "WHITE"

* white false discovery rate
tabulate pre_correct if prewhite == pre_rmax
* black false discovery rate
tabulate pre_correct if preblack == pre_rmax
* Hispanic false discovery rate
tabulate pre_correct if prehispanic == pre_rmax
* Asian false discovery rate
tabulate pre_correct if preapi == pre_rmax

*************************************************************
*************************************************************
/*** 5.3 Random Variables and Probability Distributions ***/
*************************************************************
*************************************************************

*************************************************************
/** 5.3.1 Random Variables **/
*************************************************************

*************************************************************
/** 5.3.2 Bernoulli and Uniform Distributions **/
*************************************************************
clear
set obs 1000
scalar p = .5  // success probabilities
generate x = runiform() 
list x in 1/6
* Bernoulli; turn TRUE / FALSE to 1/0
generate y = cond(x < p, 1, 0)
list y in 1/6
* close to success probability p, proportion of 1s vs 0s
summarize y  
scalar var_y = r(Var)

*************************************************************
/** 5.3.3 Binomial Distribution **/
*************************************************************
* PMF when x = 2, n = 3, p = 0.5
display binomialp(3, 2, .5)

* CDF when x = 1, n = 3, p = 0.5
display binomial(3, 1, .5)

* number of voters who turn out
foreach n of numlist 1000 10000 100000 {
	display "With `n' voters " binomialp(`n', `=`n' / 2', .5)
}

*************************************************************
/** 5.3.4 Normal Distribution **/
*************************************************************
* plus minus one standard deviation from the mean
display normal(1) - normal(-1)
* plus minus two standard deviations from the mean
display normal(2) - normal(-2)

scalar mu = 5
scalar sigma = 2
* plus minus one standard deviation from the mean
display normal((mu + sigma - mu) / sigma) - normal((mu - sigma - mu) /sigma)
* plus minus two standard deviations from the mean
display normal((mu + 2 * sigma - mu) / sigma) - normal((mu - 2 * sigma - mu) / sigma)

use pres0812, clear
regress obama12z obama08z

predict e, resid
egen e_std = std(e)
summarize e
scalar sd_e = r(sd)

histogram e_std, addplot(function normalden(x), range(-3 3)) ///
   	xlabel(-3/3) width(1) start(-3) ///
	xtitle("Standardized residuals")  ///
   	title("Distribution of standardized residuals") ///
	fcolor(none) color(black) legend(off) name(e1, replace)
qnorm e_std, xlabel(-3/3) ylabel(-3/3) ///
	xtitle("Theoretical quantiles") ///
	ytitle("Sample quantiles") ///
	title("Normal Q-Q plot") msymbol(oh) ///
	name(e2, replace)  
graph combine e1 e2

summarize obama08z if state == "CA"
scalar ca08 = r(mean)
display ca08
scalar ca12 = _b[obama08z] * ca08
display ca12
display 1 - normal((ca08 - ca12) / sd_e)

summarize obama08z if state == "TX"
scalar tx08 = r(mean)
display tx08
scalar tx12 = _b[obama08z] * tx08
display tx12
display 1 - normal((tx08 - tx12) / sd_e)

*************************************************************
/** 5.3.5 Expectation and Variance **/
*************************************************************
* theoretical variance: p was set to 0.5 earlier
display p * (1 - p)
* sample variance using scalar from `y' generated earlier
display var_y

*************************************************************
/** 5.3.6 Predicting Election Outcomes with Uncertainty **/ 
*************************************************************
use pres08, clear
* two-party vote share
generate p = obama / (obama + mccain)

set obs 1000
generate obamaev = . 
forvalues i = 1/1000 {
	* sample number of votes for Obama in each state
	    quietly generate draws = rbinomial(1000, p)
	* sum state's Electoral College votes if Obama wins majority
	    quietly summarize ev if draws > 500
	    quietly replace obamaev = r(sum) if _n == `i'
	* reset draws variable
	   quietly drop draws
}

histogram obamaev, xline(364, lcolor(blue)) ///
  	xtitle("Obama's Electoral College votes") ///
	title("Prediction of election outcome") width(10) ///
	fcolor(none) color(black)  	
  
tabstat obamaev, statistics(mean median)

* probability of binomial random variable taking greater than n/2 votes
generate pred_ev = ev * binomialtail(1000, 501, p)
tabstat pred_ev, statistics(sum)

* theoretical variance
generate pb = binomialtail(1000, 501, p)
generate variance1 = pb * (1 - pb) * ev^2
summarize variance1
display r(sum)

* theoretical standard deviation
display sqrt(r(sum))
* approximate variance & standard deviation using Monte Carlo draws
tabstat obamaev, statistics(variance sd)

*************************************************************
*************************************************************
/*** 5.4 Large Sample Theorems ***/
*************************************************************
*************************************************************

*************************************************************
/** 5.4.1 The Law of Large Numbers **/
*************************************************************
clear
set obs 5000
set seed 12345
generate xbin = rbinomial(10, .2)
generate xsum = sum(xbin)
generate meanbin = xsum / _n

generate xunif = runiform()
generate meanunif = sum(xunif) / _n

gen id = _n
line meanbin id, yline(2, lpattern(dash)) /// 
	ylabel(1(.5)3) yscale(range(1 3)) xtitle("Sample size") ///
	ytitle("Sample mean") title("Binomial(10, 0.2)") ///
	name(bin, replace)
line meanunif id, yline(.5, lpattern(dash)) ylabel(0(.2)1) ///
	xtitle("Sample size") ytitle("Sample mean") ///
  	title("Uniform(0,1)") name(unif, replace) 
graph combine bin unif

*************************************************************
/** 5.4.2 The Central Limit Theorem **/
*************************************************************
/*
program name 
    command1
    command2
    ...
    commandN
end
*/

* define command that calculates z-scores and returns as r-class results
program zscore, rclass
	clear
	syntax, obs(integer) 
	set obs `obs'
	* binomial distribution, n = 10 and probability = .2
	    generate x = rbinomial(10, .2)
	    summarize x, meanonly
	    scalar xmean = r(mean)
	   return scalar zbinomial = (xmean - 2) / sqrt(1.6 / `obs')
	* uniform distribution, 0 to 1 interval
	    generate xu = runiform()
	    summarize xu, meanonly
	    scalar xumean = r(mean)
	    return scalar zunif = (xumean - 0.5) / (sqrt(1 / (12 * `obs')))	
end
simulate zbin = r(zbinomial) zunif = r(zunif), reps(1000) nodots: zscore, obs(5000)

* plot results
histogram zbin, bin(40) addplot(function y=normalden(x, 0, 1), range(-3 3)) ///
	ylabel(0(.1).6) xtitle("z-score") title("Binomial(0.2, 10)") ///
	legend(off) name(bin1, replace)
histogram zunif, bin(40) addplot(function y=normalden(x, 0, 1), range(-3 3)) ///
	ylabel(0(.1).6) xtitle("z-score") title("Uniform(0, 1)") ///
	legend(off) name(unif1, replace)
graph combine bin1 unif1, note("n = 5000")

***********************************************************	
/*** Return to main qss directory ***/
***********************************************************
cd ..
graph close _all
