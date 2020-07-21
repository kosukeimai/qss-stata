
/*  File: UNpop.dta
	Author: Kosuke Imai
	The following code loads the UN population data and saves it as a Stata file
*/

set more off

* provide example of commenting command lines
use UNpop, clear // load data
save UNpop, replace // save and replace data file
cd .. // return to main qss-stata directory

* splitting a long command line
label define example_label 1 "Category 1" 2 "Category 2" ///
	3 "Category 3" 4 "Category 4" 5 "Category 5"
	
	
	
	
