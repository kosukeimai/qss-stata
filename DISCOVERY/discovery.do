clear all
set seed 12345
set maxvar 10000
nwclear

*************************************************************
/*** Chapter 7: Discovery ***/
*************************************************************

*************************************************************
*************************************************************
/*** 7.1 Network Data ***/
*************************************************************
*************************************************************

*************************************************************
/** 7.1.1 Marriage Network in Renaissance Florence **/
*************************************************************
cd discovery
use florentine, clear
* the first column "family" represents row names
list family - castellan in 1/5

egen marriage = rowtotal(acciaiuol - tornabuon)
list family marriage

*************************************************************
/** 7.1.2 Undirected Graph and Centrality Measures **/
*************************************************************
* declare network data
nwset acciaiuol - tornabuon, undirected name(marriage)

nwplot marriage, label(family) 

* count number of edges (degree)
nwdegree, generate(_degree)
* show family and degree columns
list family _degree

nwcloseness, unconnected(16) generate(close_out far_out near_out)
list family near_out close_out far_out

generate avg_edge = 1 / (near_out * 15)
list family avg_edge
  
nwbetween, generate(between) nosym
list family between

nwplot marriage, size(near_out) ///
	label(family) title("Closeness (nonnormalized)", margin(b = 4)) ///
	labelopt(mlabposition(12) mlabgap(2.0))  ///
	legend(off) name(near, replace)

nwplot marriage, size(between) ///
	label(family) title("Betweenness", margin(b = 4)) ///
	labelopt(mlabposition(12) mlabgap(2.0)) ///
	legend(off) name(between, replace) 

*************************************************************
/** 7.1.3 Twitter Following Network **/
*************************************************************
* create directed network
use twitter-following, clear
nwset following followed, edgelist name(twitter) 
tempfile twitter
save `twitter'
* merge with senator data
use twitter-senator, clear 
rename screen_name _nodelab
merge 1:1 _nodelab using `twitter'
save `twitter', replace

*************************************************************
/** 7.1.4 Directed Graph and Centrality **/
*************************************************************
nwdegree twitter, generate(degree) in(nofreq) out(nofreq)

gsort -_indegree -_outdegree 
list _nodelab name party state _indegree _outdegree in 1/3
gsort -_outdegree -_indegree
list _nodelab name party state _indegree _outdegree in 1/3

* create reverse network
use twitter-following, clear
nwset followed following, edgelist name(reverse_twitter) directed
use `twitter', clear

* calculate closeness measures
nwcloseness twitter, nosym unconnected(91) generate(close_out far_out near_out)
nwcloseness reverse_twitter, nosym unconnected(91) generate(close_in far_in near_in)

nwbetween twitter, generate(bw_dir)
nwbetween twitter, generate(bw_undir) nosym

scatter near_out near_in if party == "R", msymbol(O) mcolor(red) || ///
   scatter near_out near_in if party == "D", msymbol(T) mcolor(blue) || ///
   scatter near_out near_in if party == "I", msymbol(X) mcolor(black) ///
   xtitle("Incoming path") ytitle("Outgoing path") title("Closeness") ///
   legend(off) name(near, replace)
scatter bw_undir bw_dir if party == "R", msymbol(O) mcolor(red) || ///
   scatter bw_undir bw_dir if party == "D", msymbol(T) mcolor(blue) || ///
   scatter bw_undir bw_dir if party == "I", msymbol(X) mcolor(black) ///
   xtitle("Directed") ytitle("Undirected") title("Betweenness") ///
   legend(off) name(btw, replace)
 graph combine near btw  

/*
nwsummarize A
local n = r(nodes)
nwdegree A, generate(prdeg) valued
generate _pr = 1 / `n' if _n <= `n'
generate _prpre = _pr
forvalues i =1/`n' {
	generate n`i' = net`i' *_prpre / _outprdeg
	sum n`i'
	replace _pr  = (1 - `d') / r(N) + `d' * r(sum) in `i'
	drop n`i'
}

while condition {
    command1
    command2
    ...
    commandN
}
*/

program pagerank 
quietly {
	syntax  anything, [d(real .85)] 
	tempvar _prpre _dif  
	nwsummarize `anything'
	local n = r(nodes)
	nwdegree `anything', generate(prdeg) valued
	generate _pr = 1 / `n' if _n <= `n'
	generate `_prpre' = _pr
	scalar diff = 1
	while scalar(diff) > .001 {
		replace `_prpre' = _pr
		forvalues i =1/`n' {
			generate _ndeg = net`i' * `_prpre' / _outprdeg
			summarize _ndeg
			replace _pr  = (1 - `d') / r(N) + `d' * r(sum) in `i'
			drop _ndeg
		}
		summarize _pr
		replace _pr = _pr / r(sum)
		generate `_dif' = abs(_pr - `_prpre')
		summarize `_dif'
		scalar diff = r(max)
		drop `_dif'
	}
}
end

clear
set seed 12345
set obs 10
* create and view adjacency matrix
generate name1 = char(runiformint(65, 70))
generate name2 = char(runiformint(65, 70))
list name1 name2
* declare network named "websites" and run PageRank
nwset name1 name2, edgelist directed name(websites)
pagerank websites
list _nodelab _pr if _nodelab~=""

use `twitter', clear
pagerank twitter

nwplot twitter, size(_pr) arrowfactor(0)  ///
	color(party, colorpalette(blue white red)) ///
	lineopt(lcolor(gs12)) legend(off)

*************************************************************
/** 7.2 Spatial Data **/
*************************************************************

*************************************************************
/** 7.2.1 The 1854 Cholera Outbreak in London**/
*************************************************************

*************************************************************
/** 7.2.2 Spatial Data in Stata **/
*************************************************************
shp2dta using cb_2017_us_state_500k, database(usdb) ///
	coordinates(uscoord) genid(id) replace

use usdb, clear
drop if inlist(NAME, "Guam", "Alaska", "Hawaii", "Puerto Rico", "American Samoa", ///
	"United States Virgin Islands") | regexm(NAME, "Mariana")
save usdb, replace

use uscities, clear
list in 1/5
generate STUSPS = substr(name, -2, 2)
merge m:1 STUSPS using usdb 
keep if _merge == 3
drop _merge

generate poprel = pop / 1000000
spmap using uscoord if capital == 2, id(id) point(select(keep if capital == 2) ///
	by(capital) x(v6) y(lat) proportional(poprel) prange(0 8) ///
	psize(absolute)) title("US state capitals") gsize(2.6)

gsort STUSPS -pop
by STUSPS: generate citysize = _n
replace id = . if capital != 2

spmap using uscoord if STUSPS == "CA", id(id) ///
	label(select(keep if citysize <= 7 & STUSPS == "CA") ///
		x(v6) y(lat)  label(name) gap(*3 ..) ///
		size(*1.25 ..) position(2) length(21)) ///
	point(select(keep if citysize <= 7 & STUSPS == "CA") ///
		by(citysize) x(v6) y(lat)) ///
	title("Largest cities of California") gsize(3.5)

use uscoord, clear
count if _X ~= . & _Y ~= .
list in 1/5    

*************************************************************
/** 7.2.3 United States Presidential Elections **/
*************************************************************
use pres08, clear
rename statename NAME
* binary win and vote share
generate demwin = obama > mccain
generate demvote = obama / (obama + mccain)
merge 1:1 NAME using usdb 
keep if _merge == 3

* US as red and blue states
spmap demwin using uscoord, id(id) fcolor(red blue) ///
	title("US as red and blue states") legend(off) name(usblue, replace)
* US in gradients of blue
spmap demvote using uscoord, id(id) clnumber(8) fcolor(Blues) ///
	title("US as Democratic vote share") legend(off) ///
	name(demshare, replace)
graph combine usblue demshare

*************************************************************
/** 7.2.4 Expansion of Walmart **/
*************************************************************
use walmart, clear
merge 1:1 _n using usdb

generate propsize = cond(type == 3, 3, .5)
spmap using uscoord, id(id) point(by(type) ///
	x(_X) y(_Y) fcolor(green*.8 blue*.5 red*.8) /// 
	size(small) proportional(propsize) legenda(on)) ///
	legend(size(medlarge)) gsize(2.6)

*************************************************************
/** 7.2.5 Animation in Stata **/
*************************************************************
generate openyear =  year(date(opendate, "YMD"))
forvalues i = 1975(10)2005  {
	spmap using uscoord, id(id) point(select(keep if openyear <=  `i') ///
	by(type) x(_X) y(_Y)  fcolor(green*.8 blue*.5 red*.8) /// 
        size(small)  proportional(propsize)) ///
	legend(size(medlarge)) title("`i'") name(yr`i', replace) 
}
graph combine yr1975 yr1985 yr1995 yr2005

mkdir graphs
levelsof openyear, local(y)
local count = 1
foreach i of local y {
	spmap using uscoord, id(id) point(select(keep if openyear <= `i') ///
	by(type) x(_X) y(_Y) fcolor(green*.8 blue*.5 red*.8) /// 
 	size(small) proportional(propsize) legenda(on)) ///
	legend(size(medlarge)) title("`i'")
	
	graph export graphs/wm`count'.png, replace
	local count = `count' + 1
 }

 /*
winexec "C:\Program Files\ffmpeg\bin\ffmpeg.exe" -i graphs/wm%d.png -b:v 512k graphs/wm.mpg
sleep 10000
winexec "C:\Program Files\ffmpeg\bin\ffmpeg.exe" -r 1 -i graphs/wm.mpg -t 43 -r 1 graphs/wm.gif

shell /usr/local/bin/ffmpeg -i graphs/wm%d.png -b:v 512k graphs/wm.mpg
sleep 10000
shell /usr/local/bin/ffmpeg -r 1 -i graphs/wm.mpg -t 43 -r 1 -y graphs/wm.gif

* get list of graphs using Unix
local input: dir "" files "*.png"
shell convert -delay 150 -loop 0 `macval(input)' wm.gif
*/

*************************************************************
/** 7.3 Textual Data **/
*************************************************************

*************************************************************
/** 7.3.1 The Disputed Authorship of The Federalist Papers **/
*************************************************************
clear
* change directory and create temporary file
cd federalist
tempfile fedpapers
save `fedpapers', emptyok

* loop through all essays
fs fp*.txt
foreach f in `r(files)' {
    clear
    set obs 1 
    generate file = "`f'"
    generate strL txt = fileread(`"`f'"')
    generate fedno =  substr(file, 3, 2)
	destring fedno, replace
    append using `fedpapers'
    save `"`fedpapers'"', replace
}

use `fedpapers', clear

* make lower case, remove any characters except alpha-numeric and whitespace
txttool txt, replace
generate fed = txt

* remove stopwords and stem words
txttool fed, stopwords("stopwordexample.txt") stem replace

* remove numbers
replace fed = ustrregexra(fed, "[0-9]+", "")
save fedpapers, replace

* truncated output of essay no. 10
list fed if fedno == 10

*************************************************************
/** 7.3.2 Topic Discovery **/
*************************************************************
preserve
	keep fed fedno
	save freq, replace
	wordfreq using freq.dta
	gsort -freq
	list word in 1 / 10
	erase freq.dta
restore

clear
tempfile termfreq
save `termfreq', emptyok

* loop over all 85 papers for word frequency
forvalues i = 1 / 85 {
	use fedpapers, clear 
	* keep one essay and compute frequency
	  keep if fedno == `i'
	  keep fed
	  save freq, replace
	  wordfreq using freq.dta
	 * create paper number variable
	  generate fedno = `i'
	* append with other essays
	  append using `termfreq'
  	  save `termfreq', replace
}
use `termfreq', clear

* data cleaning
replace word = ustrregexra(word,`"[^a-zA-Z]"',"")
drop if word == ""
drop if length(word) < 3

gsort fedno -freq 
list word in 1 / 10 if fedno == 1

* federalist paper Nos. 12 and 24
local papers "12 24"
foreach paper of local papers {
preserve
	keep if fedno == `paper'
	gsort -freq 
	keep in 1 / 20
	set seed 12345
	generate xunif = runiformint(1,40)
	generate yunif = runiformint(1,40)
	levelsof freq, local(f)
	local s  ""
	foreach n of local f {
		local s `"`s' (scatter yunif xunif if freq == `n', msymbol(i) mlabpos(0) mlab(word) mlabsize(`n'))"'
	}
	twoway `s', xscale(noline) yscale(noline) ///
		xtitle("") ytitle("") xlabel(none) ylabel(none) ///
		graphregion(margin(l+15 r+15 t+5 b+10) color(white)) ///
		title("Federalist Paper No. `paper'", size(huge)) ///
		legend(off) name(fed`paper', replace)	
restore
}
graph combine fed12 fed24 

sort word fedno
by word: egen df = count(fedno)
bysort fedno: egen tf = sum(freq)
generate tfidf = freq * (ln(85 / df) / ln(2))
generate tfidfw = (freq / tf) * (ln(85 / df) / ln(2))

gsort fedno -tfidfw
by fedno : generate tf_rank = _n
list word tfidfw if fedno == 12 & tf_rank <= 10
list word tfidfw if fedno == 24 & tf_rank <= 10

*************************************************************
/** 7.3.3 Document-Term Matrix and Clusters **/
*************************************************************
generate hamilton = fedno==1 | inrange(fedno, 6, 9) | ///
	inrange(fedno, 11, 13) | inrange(fedno, 15, 17) | ///
	inrange(fedno, 21, 36) | inrange(fedno, 59, 61) | ///
	inrange(fedno, 65, 85)	
keep if hamilton == 1 
keep fedno word tfidfw
reshape wide tfidfw, i(fedno) j(word) string
* replace missing values with zero
foreach var of varlist tfidf* {
	replace `var' = 0 if `var' == .
} 

cluster kmeans tfidfw*,  k(4) start(krandom(12345)) keepcenters iter(5)
keep if _clus == .
generate cluster = _n

local vars tfidfw*
forvalues i = 1 / 10 {
	* identify maximum row value and create variable to store name
	egen double maxval`i' = rowmax(`vars') 
	generate maxvar`i' = ""
	* identify which variable has the maximum value and store name
	foreach var of varlist `vars' {
		replace maxvar`i' = "`var'" if `var' == maxval`i'
		replace `var' = .a if `var' == maxval`i'
	}
	* remove tfidf prefix
	replace maxvar`i' = substr(maxvar`i', 7, .)
}
* combine variable names into one string
egen topten = concat(maxvar*), p(", ")
list topten

*************************************************************
/** 7.3.4 Authorship Prediction **/
*************************************************************
use fedpapers, clear
keep fedno txt fed 
sort fedno
txttool txt, bagw replace 

generate hamilton = fedno == 1 | inrange(fedno, 6, 9) | ///
	inrange(fedno, 11, 13) | inrange(fedno, 15, 17) | ///
	inrange(fedno, 21, 36) | inrange(fedno, 59, 61) | ///
	inrange(fedno, 65, 85)
generate madison = fedno == 10 | fedno==14 | inrange(fedno, 37, 48) | fedno == 58

generate author = 1 if hamilton == 1
	replace author = -1 if madison == 1 
label define auth -1 "Madison" 1 "Hamilton"
label val author auth

keep txt-fed hamilton-author w_although w_always w_commonly ///
	w_consequently w_considerable w_enough w_there w_upon w_while w_whilst

* calculate term frequencies for each word
generate wdct = wordcount(txt)
local newlist "although always commonly consequently considerable enough there upon while whilst" 
foreach v of local newlist {
	generate tfm_`v' = (w_`v' / wdct) * 1000
}
tabstat tfm*, by(author) col(stat) long nototal

regress author tfm_upon tfm_there tfm_consequently tfm_whilst

predict authpred, xb

* subset to papers authored by Hamilton or Madison
summarize authpred if inlist(author, -1, 1)
* standard deviation
display r(sd)

*************************************************************
/** 7.3.5 Cross Validation **/
*************************************************************
generate d_authpred = cond(authpred < 0, -1, 1)
tabulate author d_authpred

generate excl_predict  = .
levelsof fedno if inlist(author, -1, 1), local(papers)
foreach i of local papers {
	regress author tfm_upon tfm_there tfm_consequently tfm_whilst if fedno != `i'
	predict auth_temp 
	replace excl_predict = auth_temp if fedno == `i'
	drop auth_temp
}

generate excl_predict2 = cond(excl_predict > 0, 1, -1)
tabulate author excl_predict2

list fedno authpred if inrange(fedno, 49, 57) | inrange(fedno, 62, 63)

scatter authpred fedno if hamilton == 1, msymbol(square) mcolor(gray) || ///
 	scatter authpred fedno if madison == 1, msymbol(O) mcolor(blue) || ///
	scatter authpred fedno if inrange(fedno, 49, 57) | ///
	inrange(fedno, 62, 63), msymbol(T) mcolor(black) ///
	yline(0, lpattern(dash) lcolor(black)) /// 
	xtitle("Federalist Papers")  ytitle("Predicted values") ///
	legend(label(1 "Hamilton") label(2 "Madison") label(3 "Disputed") cols(3)) 

***********************************************************	
/*** Return to main qss directory ***/
***********************************************************
cd ..
cd ..
graph close _all
