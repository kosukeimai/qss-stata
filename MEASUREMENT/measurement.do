
*************************************************************
/*** Chapter 3: Measurement ***/
*************************************************************

*************************************************************
/*** Clear memory ***/
*************************************************************
clear all

*************************************************************
*************************************************************
/*** 3.1 Measuring Civilian Victimization during Wartime ***/
*************************************************************
*************************************************************
cd measurement
use afghan, clear
* summaries for variables of interest
summarize age educyears employed
tabulate income 

* cross-tabulation of proportions
tabulate violentexpisaf violentexptaliban, cell nofreq

count if violentexpisaf~=.& violentexptaliban~=.
local c = r(N)
count if violentexpisaf==0 & violentexptaliban==0
local c1 = `=round((`=r(N)'/`c')*100,.1)'
count if violentexpisaf==0 & violentexptaliban==1
local c2 = `=round((`=r(N)'/`c')*100,.1)'
count if violentexpisaf==1 & violentexptaliban==0
local c3 = `=round((`=r(N)'/`c')*100,.1)'
count if violentexpisaf==1 & violentexptaliban==1
local c4 = `=round((`=r(N)'/`c')*100,.1)'
local r1 = `=round(`c3'+`c4',1)'
local r2 = `=round(`c2'+`c4',1)'
local c4a = `=round(`c4',1)'

*************************************************************
*************************************************************
/*** 3.2 Handling Missing Data in Stata ***/
*************************************************************
*************************************************************
tabulate violentexpisaf violentexptaliban, cell nofreq missing
count if violentexpisaf==.
local isaf = `=ceil(round((r(N)/_N)*100, .01))'
count if violentexptaliban==.
local tal = `=ceil(round((r(N)/_N)*100, .01))'
count if violentexptaliban==. & violentexpisaf==.
local both = `=round((r(N)/_N)*100, .01)'

*************************************************************
/** 3.2.1 Missings Package **/
*************************************************************
* number of observations missing values
missings report violentexpisaf violentexptaliban

missings dropobs violentexpisaf violentexptaliban, force
tabulate violentexpisaf violentexptaliban, missing

missings tag income, generate(miss_income) 
tabulate miss_income

*************************************************************
*************************************************************
/*** 3.3 Visualizing the Univariate Distribution ***/
*************************************************************
*************************************************************

*************************************************************
/** 3.3.1 Bar plot **/
*************************************************************
* victimization by ISAF
tabulate violentexpisaf, missing

* make bar plots by specifying a certain range for y-axis
graph bar, over(violentexpisaf, relabel(1 "No harm" 2 "Harm" 3 "No response")) missing ///
   ylabel(0(10)70) ytitle("Proportion of the respondents") ///
   title("Civilian victimization by the ISAF") 

* repeat the same for the victimization by Taliban
tabulate violentexptaliban, missing

graph bar, over(violentexptaliban, relabel(1 "No harm" 2 "Harm" 3 "No response")) missing ///
   ylabel(0(10)70) ytitle("Proportion of the respondents") ///
   title("Civilian victimization by the Taliban") 

*************************************************************
/** 3.3.2 Histogram **/ 
************************************************************* 
histogram age, xtitle("Age") ///
	title("Distribution of respondent's age") ///
	fcolor(none) color(black) 

* add a vertical line representing median
summarize educyears, detail
histogram educyears, discrete xline(`r(p50)') ///
	xtitle("Years of education") ///
	title("Distribution of respondent's education") ///
	text(.45 2.5 "median") fcolor(none) color(black)  

*************************************************************
/** 3.3.3 Box plot **/
*************************************************************
graph box age, ytitle("Age") title("Distribution of age") 

graph box age, ytitle("Age") title("Distribution of age") ///
	text(55 90 "`=ustrunescape("\u23AB")'" "`=ustrunescape("\u23AC")'" ///
		"`=ustrunescape("\u23AD")'", size(vhuge) color(navy*.9)) ///
	text (55 95 "1.5" "IQR", color(navy*.9)) ///
	text(30 90 "`=ustrunescape("\u23AB")'" "`=ustrunescape("\u23AC")'" ///
		"`=ustrunescape("\u23AD")'", size(huge) color(navy*.9)) ///
	text (30 95 "IQR", color(navy*.9)) ///
	text (20 7 "lower" "quartile", color(navy*.9)) ///
	text (30 7 "median", color(navy*.9)) ///
	text (40 7 "upper" "quartile", color(navy*.9))

graph box educyears, over(province) ytitle("Years of education")  ///
	title("Education by province") 

tabstat violentexptaliban violentexpisaf, by(province) statistics(mean)

*************************************************************
/** 3.3.4 Printing and Saving Graphs **/
*************************************************************
* saving or printing a graph
graph save educyears.pdf, replace	
graph box educyears, over(province) ytitle("Years of education") ///
	title("Education by province") ///
	saving(educyears, replace) 

* example alteration with display and opening a saved graph
graph display, scheme(s1mono)
graph use educyears.gph 

* combining graphs
histogram age, ylabel(0(.01).04) ///
	xtitle("Age") title("Distribution of respondent's age") ///
	fcolor(none) color(black) name(age, replace)
histogram educyears, discrete ///
	xtitle("Years of education") ///
	title("Distribution of respondent's education") ///
	fcolor(none) color(black) name(educ, replace)
graph combine age educ 

graph close _all

preserve
	clear 
	set obs 101
	generate logx = (_n/10)-5.1
	generate x = exp(logx)
	twoway line logx x , xtitle("x") ytitle("log x") name(logx, replace)

	use afghan-village.dta, clear
	generate logpop = log(population)
	generate pop1000 = population / 1000
	histogram pop1000, fcolor(none) color(black)  xtitle("Population (in thousands)") ylabel(0(.1).4) name(h1,replace)
	histogram logpop, fcolor(none) color(black) xtitle("log population") ylabel(0(.1).4) name(h2, replace)
	graph combine h1 h2 , rows(1) name(h, replace)
	graph combine logx h  
restore

*************************************************************
*************************************************************
/*** 3.4 Survey Sampling ***/
*************************************************************
*************************************************************

*************************************************************
/** 3.4.1 The Role of Randomization **/
*************************************************************
* load village data
use afghan-village, clear
* box plots for altitude and log population
graph box altitude, over(villagesurveyed, relabel(1 "Nonsampled" 2 "Sampled")) ///
   ytitle("Altitude (meters)") name(alt, replace)
generate logpop = log(population)
graph box logpop, over(villagesurveyed, relabel(1 "Nonsampled" 2 "Sampled")) ///
   ytitle("log population") name(pop, replace)  
graph combine alt pop
graph close _all

use afghan, clear
* create binary variable flagging missing values
generate missvioltaliban = missing(violentexptaliban)
generate missviolisaf = missing(violentexpisaf)

* view nonresponse rate, by province
tabstat missvioltaliban missviolisaf, by(province) statistics(mean)

summarize listresponse if listgroup =="ISAF"
scalar isaf = r(mean)
summarize listresponse if listgroup =="control"
scalar control = r(mean)
display isaf - control
local sup = round((scalar(isaf) - scalar(control)) * 100,1)

tabulate listresponse listgroup

*************************************************************
*************************************************************
/*** 3.5 Measuring Political Polarization ***/
*************************************************************
*************************************************************
twoway pcarrowi 0 -1 0 1  0 1 0 -1 , lcolor(black) mcolor(black) || pcarrowi  -1 0 1 0  1 0 -1 0 , lcolor(black) mcolor(black) || scatteri -.75 -.35  , msymbol(T ) msize(large) || scatteri -.1 .3 , msymbol(X) msize(huge) || scatteri .5 .5 , msize(large) || scatteri -.75 -.35 -.1 .3, msymbol(i) connect(line) lpattern(dot) lcolor(black) || scatteri -.1 .3 .5 .5 , msymbol(i) connect(line) lpattern(dash) lcolor(black) legend(off) text(.6 .6 "Status quo") text(-.9 -.35 "Proposal" , place(9)) text(-.2 .4 "Ideal point") text(.15 .75 "Economic conservatism", place(0)) text(-.15 -.75 "Economic liberalism" , place(0)) text(.9 .27 "Racial conservatism", place(0)) text(-.9 .25 "Racial liberalism", place(0))

*************************************************************
*************************************************************
/*** 3.6 Summarizing Bivariate Relationships ***/
*************************************************************
*************************************************************

*************************************************************
/** 3.6.1 Scatter Plot **/
*************************************************************
* load the data
use congress, clear
* scatterplots for the 80th and 112th Congresses
scatter dwnom2 dwnom1 if party == "Democrat" & congress == 80 || ///
	scatter dwnom2 dwnom1 if party == "Republican" & congress == 80, ///
	xlabel(-1.5(.5)1.5) ylabel(-1.5(.5)1.5) ///
	xtitle("Economic liberalism/conservatism") ytitle( "Racial liberalism/conservatism")  ///
	title("80th Congress") text(1 -0.75 "Democrats") text(-1 1 "Republicans") ///
	msymbol(T) mcolor(red) legend(off) name(congress80, replace)
scatter dwnom2 dwnom1 if party == "Democrat" & congress == 112 || ///
	scatter dwnom2 dwnom1 if party == "Republican" & congress == 112, ///
	xlabel(-1.5(.5)1.5) ylabel(-1.5(.5)1.5) ///
	xtitle("Economic liberalism/conservatism") ytitle( "Racial liberalism/conservatism") ///
	title("112th Congress") text(1 -1 "Democrats") text(-1 1 "Republicans") ///
	msymbol(T) mcolor(red) legend(off) name(congress112, replace)
graph combine congress80 congress112

* party median for each congress
bysort congress party: egen partymedian = median(dwnom1)

line partymedian congress if party=="Democrat", lpattern(dash) || ///
   line partymedian congress if party=="Republican", ///
   xlabel(80(5)115) ylabel(-1(.5)1) lcolor(blue) lpattern(solid) ///
   xtitle("Congress") ytitle("DW-NOMINATE score (1st dimension)") ///
   text(-.6 110 "Democratic" "Party") text(.85 110 "Republican" "Party") ///
   legend(off) 

*************************************************************
/** 3.6.2 Correlation **/
*************************************************************
generate demmedian = partymedian if party == "Democrat"
generate repmedian = partymedian if party == "Republican"   
collapse demmedian repmedian, by(congress)

* create year variable 
egen year = fill(1948(2)2012)

merge 1:1 year using USGini

generate polar = repmedian - demmedian

* time-series plots for partisan difference and Gini coefficient
scatter polar year, xlabel(1950(10)2010) ///
    xtitle("Year") ytitle("Republican median - Democratic median") ///
    title("Political polarization") msymbol(Oh) name(polar, replace)
scatter gini year,  xlabel(1950(10)2010) ylabel(0.35(.02)0.45) ///
    xtitle("Year") ytitle("Gini coefficient") ///
    title("Income inequality") msymbol(Oh) name(gini, replace)
 graph combine polar gini

correlate gini polar

*************************************************************
/** 3.6.3 Quantile–Quantile Plot **/
*************************************************************
* reload original congress data
use congress, clear
* party distributions on dimension 2
histogram dwnom2 if party=="Democrat" & congress==112, ///
   xtitle("Racial liberalism/conservatism dimension") ///
   bin(12) title("Democrats") fcolor(none) color(black) name(d112, replace) 
histogram dwnom2 if party=="Republican" & congress==112, ///
   xtitle("Racial liberalism/conservatism dimension") ///
   bin(12) title("Republicans") fcolor(none) color(black) name(r112, replace) 
graph combine d112 r112

generate demdwnom2 = dwnom2 if party=="Democrat"
generate repdwnom2 = dwnom2 if party=="Republican"

qqplot repdwnom2 demdwnom2 if congress==112, ///
  xlabel(-1.5(.5)1.5) ylabel(-1.5(.5)1.5) ///
  ytitle("Republicans") xtitle("Democrats") title("Racial liberalism/conservatism dimension")
graph close _all

*************************************************************
*************************************************************
/*** 3.7 Clustering ***/
*************************************************************
*************************************************************

*************************************************************
/** 3.7.1 The k-means Algorithm **/
*************************************************************
* k-means with 2 clusters, by Congress
cluster kmeans dwnom1 dwnom2 if congress == 80, k(2) ///
	start(krandom(12345)) name(kdwnom80)
cluster kmeans dwnom1 dwnom2 if congress == 112, k(2) ///
	start(krandom(12345)) name(kdwnom112)

* final centroids 
tabstat dwnom1 dwnom2, by(kdwnom80)
tabstat dwnom1 dwnom2, by(kdwnom112)

* number of observations for each cluster by party 
tabulate party kdwnom80
tabulate party kdwnom112

* k-means with 4 clusters
cluster kmeans dwnom1 dwnom2 if congress==80, k(4) ///
	start(krandom(12345)) name(k4dwnom80)
cluster kmeans dwnom1 dwnom2 if congress==112, k(4) ///
	start(krandom(12345)) name(k4dwnom112) 

* plotting results - 80th Congress
scatter dwnom2 dwnom1 if k4dwnom80==1 || scatter dwnom2 dwnom1 if k4dwnom80==2 || ///
   scatter dwnom2 dwnom1 if k4dwnom80==3 || scatter dwnom2 dwnom1 if k4dwnom80==4, ///
   xtitle("Economic liberalism/conservatism") ytitle("Racial liberalism/conservatism") ///
   title("80th Congress") legend(off) name(cluster80, replace)
* plotting results - 112th Congress
scatter dwnom2 dwnom1 if k4dwnom112==1 || scatter dwnom2 dwnom1 if k4dwnom112==2 || ///
    scatter dwnom2 dwnom1 if k4dwnom112==3 || scatter dwnom2 dwnom1 if k4dwnom112==4, ///
    xtitle("Economic liberalism/conservatism") ytitle("Racial liberalism/conservatism") ///
    title("112th Congress") legend(off) name(cluster112, replace)
 graph combine cluster80 cluster112
   
***********************************************************	
/*** Return to main qss directory ***/
***********************************************************
cd ..
graph close _all
