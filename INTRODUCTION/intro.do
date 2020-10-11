
*************************************************************
*************************************************************
/*** 1.3 Introduction to Stata ***/
*************************************************************
*************************************************************

*************************************************************
/** 1.3.1 Arithmetic Operations **/
*************************************************************
display 5 + 3
display 5 - 3
display 5 / 3
display 5 ^ 3
display 5 * (10 - 3)
display sqrt(4)

*************************************************************
/** 1.3.2 Variables **/
*************************************************************
clear
input year worldpop
1950 2525779 
1960 3026003 
1970 3691173 
1980 4449049 
1990 5320817 
2000 6127700 
2010 6916183 
end

list

describe 
codebook

generate pm = worldpop / 1000
list pm

sort year
generate lagworldpop = worldpop[_n - 1]
generate pctincrease = ((worldpop - lagworldpop) / lagworldpop) * 100
list pctincrease
rename pm popmillion

recode year (1950 1960 1970 1980 = 1) (1990 2000 2010 = 0), generate(coldwar)

generate coldwar2 = inlist(year, 1950 1960 1970 1980)

*************************************************************
/** 1.3.3 Labels **/
*************************************************************
label variable worldpop "World population (thousands)"
label variable popmillion "Population in millions"
label variable pctincrease "Percentage increase for each decade"

label define cwlabel 0 "After Cold War" 1 "During Cold War"

label values coldwar cwlabel 
codebook coldwar

label define cwlabel 0 "Post-Cold War" 1 "Cold War", modify
list coldwar

lookfor pop

*************************************************************
/** 1.3.4 Describing the Data **/
*************************************************************
summarize year worldpop pctincrease

tabulate coldwar

tabulate year coldwar

tabulate coldwar, summarize(popmillion)

*************************************************************
/** 1.3.5 Data Files **/
*************************************************************
cd introduction

use UNpop, clear

import delimited using UNpop.csv, clear

*************************************************************
/** 1.3.6 Merging Data Sets in Stata **/
*************************************************************
merge 1:1 year using UNsubpop

*************************************************************
/** 1.3.8 Programming and Learning Tips **/
*************************************************************
* run example do-file
quietly do example.do

***********************************************************	
/*** Return to main qss directory ***/
***********************************************************
clear all
graph close _all
