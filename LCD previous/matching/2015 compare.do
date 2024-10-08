/*

Compare 2015 results pre/post edits

*/

cd "/Users/gretam/Documents/LCD/matching/Lancet2025/Jen/"
local files : dir . files "*.csv"

foreach f in `files' { 
	if regexm("`f'", "sum"){
		import delimited "`f'", varn(1) clear
		keep city city1 sum
		local g=subinstr("`f'", "_AllCities.csv", "", 1)
		save "`g'.dta", replace
	}
	else {
		import delimited "`f'", varn(1) clear
		keep city city1 mean
		local g=subinstr("`f'", "_AllCities.csv", "", 1)
		save "`g'.dta", replace
	}
}

cd "/Users/gretam/Documents/LCD/matching/Lancet2025/Greta/"
local files : dir . files "*.csv"

foreach f in `files' { 
	if regexm("`f'", "sum"){
		import delimited "`f'", varn(1) clear
		keep city city1 sum
		local g=subinstr("`f'", "_AllCities.csv", "", 1)
		save "`g'.dta", replace
	}
	else {
		import delimited "`f'", varn(1) clear
		keep city city1 mean
		local g=subinstr("`f'", "_AllCities.csv", "", 1)
		save "`g'.dta", replace
	}
}

cd "/Users/gretam/Documents/LCD/matching/Lancet2025/"
foreach file in 2015_Fall.dta 2015_Winter.dta 2015_Summer.dta 2015_Spring.dta {
	di "`file'"
	use "Jen/`file'", clear
	rename mean mean_jen
	merge 1:1 city1 using "Greta/`file'", assert(3) nogen
	gen diff=mean_jen-mean
	summ diff
}

use "Jen/2015_sum2_popsize.dta", clear
rename sum sum_jen
merge 1:1 city1 using "Greta/2015_sum2_popsize", assert(3) nogen
gen diff=sum_jen-sum
summ diff

import delimited "2015_Winter.csv", varn(1) clear
keep city1 mean
rename mean mean_output
save "2015_Winter.dta", replace

import delimited "2015_Winter_G.csv", varn(1) clear
keep city1 mean
rename mean mean_g
save "2015_Winter_G.dta", replace

import delimited "2015_Winter_J.csv", varn(1) clear
keep city1 mean
rename mean mean_j
save "2015_Winter_J.dta", replace


use "Jen/2015_Winter.dta", clear
rename mean mean_gee

merge 1:1 city1 using "2015_Winter_G.dta", assert(3) nogen
merge 1:1 city1 using "2015_Winter_J.dta", assert(3) nogen
merge 1:1 city1 using "2015_Winter.dta", assert(3) nogen

*will use gee file as default
gen diff_g=mean_gee-mean_g
gen diff_j=mean_gee-mean_j
gen diff_output=mean_gee-mean_output
summ diff*

list city1 if missing(diff_output)



