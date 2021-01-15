
*************************************************************
/*** Chapter 2: Causality ***/
*************************************************************

*************************************************************
/*** Clear memory ***/
*************************************************************
clear all

*************************************************************
*************************************************************
/*** 2.1 Racial Discrimination in the Labor Market ***/
*************************************************************
*************************************************************
cd causality
use resume, clear

describe

list * in 1/6
local fname = firstname[2]

generate id = _n

modes firstname
modes firstname, nmodes(5)

tab1 sex race call
tabulate race call


* overall callback rate: total callbacks divided by the sample size
tabulate race call, row
tabulate race, summarize(call)

*************************************************************
*************************************************************
/*** 2.2 Subsetting the Data in Stata ***/
*************************************************************
*************************************************************

*************************************************************
/** 2.2.1 Relational Operators **/
*************************************************************
display 4 == 4
display 4 > 3
display  ("Hello" == "hello") // Stata is case sensitive
display  ("Hello" != "hello")

generate abovefour = 1 if id >= 5
list id abovefour in 1/10

*************************************************************
/** 2.2.2 Logical Operators **/
*************************************************************
* FALSE & TRUE
display (5 == 4) & (5 < 10)
* TRUE & TRUE
display (5 > 4) & (5 < 10)

* FALSE | TRUE 
display (5 == 4) | (5 < 10)
* FALSE | FALSE
display (5 == 4) | (5 == 10)

* TRUE & FALSE & TRUE
display (5 > 1) & (5 == 6) & (7 < 10)

* (TRUE | FALSE) & FALSE - the parentheses evaluate to TRUE
display (5 > 4 | 5 == 3) & (7 < 5)

* TRUE | (FALSE & FALSE) - the parentheses evaluate to FALSE
display (5 > 4) | (5 == 3 & 7 < 5)

*************************************************************
/** 2.2.3 Simple Conditional Statements and Variable Creation **/
*************************************************************
generate BlackFemale = cond(race == "black" & sex == "female", 1, 0)

replace BlackFemale = . if race == "" | sex == ""

* confirming the accuracy of our code 
tabulate race sex if BlackFemale == 0
tabulate race sex if BlackFemale == 1

generate type = .
replace type = 1 if race == "black" & sex == "female"
replace type = 2 if race == "black" & sex == "male"
replace type = 3 if race == "white" & sex == "female"
replace type = 4 if race == "white" & sex == "male"

label define typecat 1 "Black Female" 2 "Black Male" 3 "White Female" 4 "White Male"
label values type typecat

* number of observations for each level
tabulate type
tabulate type, nolabel 

egen type2 = group(race sex), label
tabulate type2

*************************************************************
/** 2.2.4 Subsetting Using Conditions **/
*************************************************************
* callback rate for black-sounding names
summarize call if race == "black"
tabulate call if race == "black"

* callback rate for females with black-sounding names
summarize call if race == "black" & sex == "female"
summarize call if BlackFemale == 1

*************************************************************
/** 2.2.5 Preserving and Transforming Data Sets **/
*************************************************************
* original number of observations
summarize call

* subset blacks only
preserve
	keep if race == "black"
	summarize call
	tabulate race
restore

preserve
	collapse call, by(sex race)
	list		
restore

*************************************************************
*************************************************************
/*** 2.3 Causal Effects and the Counterfactual ***/
*************************************************************
*************************************************************
* restore data to original order and view first observation
sort id
list firstname call in 1

*************************************************************
*************************************************************
/*** 2.4 Randomized Controlled Trials ***/
*************************************************************
*************************************************************

*************************************************************
/** 2.4.1 The Role of Randomization **/
*************************************************************

*************************************************************
/** 2.4.2 Social Pressure and Voter Turnout **/
*************************************************************
use social, clear 
codebook, compact

* turnout for each group
tabulate messages, summarize(primary2006) 

* turnout for control group
summarize primary2006 if messages == "Control"
return list

*  subtract control group turnout from each group
generate votediff = primary2006 - r(mean)
tabulate messages if message!="Control", summarize(votediff)

generate age = 2006 - yearofbirth 
tabstat age primary2004 hhsize, by(message) statistics(mean)

*************************************************************
*************************************************************
/*** 2.5 Observational Studies ***/
*************************************************************
*************************************************************

*************************************************************
/** 2.5.1 Minimum Wage and Unemployment **/
*************************************************************
use minwage, clear 
describe
codebook, compact 

* create a dichotomous state variable
generate state = cond(location == "PA", "PA", "NJ")

* proportion of restaurants whose wage is less than $5.05
generate minwagebefore = cond(wagebefore < 5.05, 1, 0)
generate minwageafter = cond(wageafter < 5.05, 1, 0)
tabstat minwagebefore minwageafter, by(state) statistics(mean)

* create a variable for proportion of full-time employees in NJ and PA
generate fullpropafter = fullafter / (fullafter + partafter) 

* compute the difference-in-means
summarize fullpropafter if state == "NJ"
scalar fullpropafterNJ = r(mean)

summarize fullpropafter if state == "PA"
scalar fullpropafterPA = r(mean)

display fullpropafterNJ - fullpropafterPA

*************************************************************
/** 2.5.2 Confounding Bias **/
*************************************************************
tabulate chain state, column nofreq

* subset Burger King only, by state
summarize fullpropafter if state == "NJ" & chain == "burgerking" 
scalar njbk = r(mean)
summarize fullpropafter if state == "PA" & chain == "burgerking" 
scalar pabk = r(mean)

* Burger King only NJ vs PA comparison of full-time employee rates
display njbk - pabk

* subset Burger King, by NJ location
summarize fullpropafter if state == "NJ" & chain == "burgerking" & ///
	(location != "shoreNJ" & location != "centralNJ")
scalar njbk_subset = r(mean)

* Burger King only and excluding NJ shore and central NJ
display njbk_subset - pabk

*************************************************************
/** 2.5.3 Before-and-After and Difference-in-Differences Designs **/
*************************************************************
* full-time employment proportion in the previous period for NJ
generate fullpropbefore = fullbefore / (fullbefore + partbefore) 
summarize fullpropbefore if state == "NJ"
scalar fullpropbeforeNJ = r(mean)

* mean difference between before and after the minimum wage increase
display fullpropafterNJ - fullpropbeforeNJ

* full-time employment proportion in the previous period for PA
summarize fullpropbefore if state == "PA"
scalar fullpropbeforePA = r(mean)

* differences in differences using mean difference between before and after, by state
display (fullpropafterNJ - fullpropbeforeNJ) -  (fullpropafterPA - fullpropbeforePA)

*************************************************************
*************************************************************
/*** 2.6 Descriptive Statistics for a Single Variable ***/
*************************************************************
*************************************************************

*************************************************************
/** 2.6.1 Quantiles **/
*************************************************************
* cross-section comparison between NJ and PA median
summarize fullpropafter if state == "NJ", detail
scalar medafterNJ = r(p50)

summarize fullpropafter if state == "PA", detail
scalar medafterPA = r(p50)

display medafterNJ - medafterPA

* before and after comparison
summarize fullpropbefore if state == "NJ", detail
scalar medbeforeNJ = r(p50)

display medafterNJ - medbeforeNJ

* median difference-in-differences
summarize fullpropbefore if state == "PA", detail
scalar medbeforePA = r(p50)

display (medafterNJ- medbeforeNJ) - (medafterPA - medbeforePA)

* summary shows quartiles as well as minimum, maximum, and mean
summarize wagebefore if state == "NJ", detail
summarize wageafter if state == "NJ", detail

* interquartile range 
tabstat wagebefore wageafter if state == "NJ", statistics(iqr) columns(statistics)

centile wagebefore if state == "NJ", centile(0(10)100)
centile wageafter if state == "NJ", centile(0(10)100)

*************************************************************
/** 2.6.2 Standard Deviation **/
*************************************************************
egen sqNJ = mean((fullpropafter - fullpropbefore)^2) if state == "NJ"
generate rmsNJ = sqrt(sqNJ)

egen meanNJ = mean(fullpropafter - fullpropbefore) if state == "NJ"
summarize rmsNJ meanNJ

* standard deviation and variance
tabstat fullpropbefore fullpropafter, statistics(sd var) by(state) long nototal

***********************************************************	
/*** Return to main qss directory ***/
***********************************************************
cd ..
graph close _all

